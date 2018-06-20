#include "Header.h"
#include "Io.h"
#include "Unify.h"

/*
 * memory management
 */

static Context_ptr context_avail;
static Trail_ptr trail_avail;
static long context_gets, context_frees, context_avails;
static long trail_gets, trail_frees, trail_avails;

#define MAX_MULTIPLIERS 250
static int multipliers[MAX_MULTIPLIERS];  /* (m[i]==0) => i is available */

/*************
 *
 *   next_available_multiplier()
 *
 *************/

static int next_available_multiplier()
{
    int i;

    for (i = 0; i < MAX_MULTIPLIERS; i++)
	if (multipliers[i] == 0) {
	    multipliers[i] = 1;
	    return(i);
	    }
    abend("next_available_multiplier, none.");
}  /* next_available_multiplier */

/*************
 *
 *    Context_ptr get_context()
 *
 *************/

Context_ptr get_context(void)
{
    Context_ptr p;
    int i;
    
    context_gets++;

    if (!context_avail) {
	p = tp_alloc(sizeof(struct context));
	for (i=0; i<MAX_VARS; i++)
	    p->terms[i] = NULL;
	}
    else {
	context_avails--;
	p = context_avail;
	context_avail = context_avail->contexts[0];
	}
    p->contexts[0] = NULL;
    p->multiplier = next_available_multiplier();
    p->partial_term = NULL;
    return(p);
}  /* get_context */

/*************
 *
 *    free_context()
 *
 *************/

void free_context(Context_ptr p)
{
#if 0
    /* for checking that contexts are really clear when freed */
    int i;
    for (i=0; i<MAX_VARS; i++) {
	if (p->terms[i]) {
	    printf("ERROR, context %x, term %d not null.\n",p->contexts[i], i);
	    p_term(p->terms[i]);
	    p->terms[i] = NULL;
	    }
	if (p->contexts[i]) {
	    printf("ERROR, context %x, context %d not null.\n",p->contexts[i], i);
	    p_term(p->terms[i]);
	    p->contexts[i] = NULL;
	    }
	}
#endif
    context_frees++;
    context_avails++;
    multipliers[p->multiplier] = 0;
    p->contexts[0] = context_avail;
    context_avail = p;
}  /* free_context */

/*************
 *
 *    Trail_ptr get_trail()
 *
 *************/

Trail_ptr get_trail(void)
{
    Trail_ptr p;
    
    trail_gets++;
    if (!trail_avail)
	p = tp_alloc(sizeof(struct trail));
    else {
	trail_avails--;
	p = trail_avail;
	trail_avail = trail_avail->next;
	}
    p->next = NULL;
    return(p);
}  /* get_trail */

/*************
 *
 *    free_trail()
 *
 *************/

void free_trail(Trail_ptr p)
{
    trail_frees++;
    trail_avails++;
    p->next = trail_avail;
    trail_avail = p;
}  /* free_trail */

/*************
 *
 *   print_unify_mem()
 *
 *************/

void print_unify_mem(FILE *fp, int heading)
{
    if (heading)
	fprintf(fp, "  type (bytes each)        gets      frees     in use      avail      bytes\n");

    fprintf(fp, "context (%4d)      %11ld%11ld%11ld%11ld%9.1f K\n", sizeof(struct context), context_gets, context_frees, context_gets - context_frees, context_avails, (((context_gets - context_frees) + context_avails) * sizeof(struct context)) / 1024.);
    fprintf(fp, "trail (%4d)        %11ld%11ld%11ld%11ld%9.1f K\n", sizeof(struct trail), trail_gets, trail_frees, trail_gets - trail_frees, trail_avails, (((trail_gets - trail_frees) + trail_avails) * sizeof(struct trail)) / 1024.);

}  /* print_unify_mem */

/*************
 *
 *   p_unify_mem()
 *
 *************/

void p_unify_mem()
{
    print_unify_mem(stdout, 1);
}  /* p_unify_mem */

/*
 *  end of memory management
 */

/*************
 *
 *    int unify(t1, c1, t2, c2, trail_address)
 *
 *        Attempt to unify t1 in context c1 with t2 in context c2.  
 *    If successful, return 1 and and a pointer to the trail (a record
 *    of the substitutions).  The trail is extended by adding new
 *    entries to the front, and the front is returned.  On entry,
 *    *trail_address must be either NULL or the result of a previous
 *    call to unify.  If unification fails, the trail is unchanged.
 *    A context is a substitution table along with a multiplier for
 *    the variables.  The multiplier need not be present for
 *    unification, but it is needed for `apply'.
 *
 *        An example of its use:
 *
 *             c1 = get_context();
 *             c2 = get_context();
 *             tr = NULL;
 *             if (unify(t1, c1, t2, c2, &tr)) {
 *                 print_subst(stdout, c1);
 *                 print_subst(stdout, c2);
 *                 print_trail(stdout, tr);
 *                 t3 = apply(t1, c1);
 *                 t4 = apply(t2, c2);
 *                 printf("apply substitution: ");
 *                 print_term(stdout, t3); printf(" ");
 *                 print_term(stdout, t4); printf("\n");
 *                 clear_subst_1(tr);
 *                 zap_term(t3);
 *                 zap_term(t4);
 *                 }
 *             else
 *                 printf("unify fails\n");
 *             free_context(c1);
 *             free_context(c2);
 *
 *************/

int unify(Term_ptr t1, Context_ptr c1,
          Term_ptr t2, Context_ptr c2, Trail_ptr *trp)
{
    Trail_ptr tpos, tp, t3;
    int vn1, vn2;

    DEREFERENCE(t1, c1)  /* dereference macro */

    DEREFERENCE(t2, c2)  /* dereference macro */

    /* Now, neither t1 nor t2 is a bound variable. */

    if (VARIABLE(t1)) {
	vn1 = t1->symbol;
	if (VARIABLE(t2)) {
	    /* both t1 and t2 are variables */
	    if (vn1 == t2->symbol && c1 == c2)
		return(1);  /* identical */
	    else {
		BIND_TR(vn1, c1, t2, c2, trp)
		return(1);
		}
	    }
	else {
	    /* t1 variable, t2 not variable */
	    if (occur_check(vn1, c1, t2, c2)) {
		BIND_TR(vn1, c1, t2, c2, trp)
		return(1);
		}
	    else
		return(0);  /* failed occur_check */
	    }
	}

    else if (VARIABLE(t2)) {
	/* t2 variable, t1 not variable */
	vn2 = t2->symbol;
	if (occur_check(vn2, c2, t1, c1)) {
	    BIND_TR(vn2, c2, t1, c1, trp)
	    return(1);
	    }
	else
	    return(0);  /* failed occur_check */
	}
    
    else if (t1->symbol != t2->symbol)
	return(0);  /* fail because of symbol clash */

    else if (t1->arity == 0)
	return(1);

    else {  /* both complex with same functor */
	int i, arity;

	tpos = *trp;  /* save trail position in case of failure */
	
	i = 0; arity = t1->arity;
	while (i < arity && unify(t1->args[i], c1, t2->args[i], c2, trp))
	    i++;

	if (i == arity)
	    return(1);
	else {  /* restore trail and fail */
	    tp = *trp;
	    while (tp != tpos) {
		tp->context->terms[tp->varnum] = NULL;
		tp->context->contexts[tp->varnum] = NULL;
		t3 = tp;
		tp = tp->next;
		free_trail(t3);
		}
	    *trp = tpos;
	    return(0);
	    }
	}
}  /* unify */

/*************
 *
 *    int occur_check(varnum, var_context, term, term_context)
 *
 *    Return 0 iff variable occurs in term under substitution
 *       (including var==term).
 *
 *************/

int occur_check(int vn, Context_ptr vc, Term_ptr t, Context_ptr c)
{
    if (!c)
	return(1);
    else if (VARIABLE(t)) {  /* variable */
	int tvn;
        tvn = t->symbol;
        if (tvn == vn && c == vc)
            return(0);  /* fail occur_check here */
        else if (c->terms[tvn] == NULL)
            return(1);  /* uninstantiated variable */
        else
            return(occur_check(vn, vc, c->terms[tvn], c->contexts[tvn]));
        }
    else {  /* constant or complex */
	int i;
	for (i = 0; i < t->arity; i++)
	    if (!occur_check(vn, vc, t->args[i], c))
		return(0);
	return(1);
        }
}  /* occur_check */

/*************
 *
 *    int match(t1, c1, t2, trail_address) -- one-way unification.
 *
 *        Match returns 1 if t2 is an instance of {t1 in context c1}.
 *    This is not a very general version, but it is useful for
 *    demodulation and subsumption.  It assumes that the variables
 *    of t1 and t2 are separate, that none of the variables in t2
 *    have been instantiated, and that none of those t2's variables
 *    will be instantiatied.  Hence, there is no context for t2,
 *    no need to dereference more than one level, and no need for
 *    an occur_check.
 *
 *        The use of the trail is the same as in `unify'.
 *
 *************/

int match(Term_ptr t1, Context_ptr c1, Term_ptr t2, Trail_ptr *trp)
{
    int vn;

    if (VARIABLE(t1)) {
	vn = t1->symbol;
	if (c1->terms[vn] == NULL) {
	    BIND_TR(vn, c1, t2, NULL, trp)
	    return(1);
	    }
	else
	    return(term_ident(c1->terms[vn], t2));
	}
    else if (VARIABLE(t2))
	return(0);
    else  /* neither term is a variable */
	if (t1->symbol != t2->symbol)
	    return(0);  /* fail because of symbol clash */
	else {
	    Trail_ptr tpos, tp, t3;
	    int i, arity;

	    tpos = *trp;  /* save trail position in case of failure */
	    i = 0; arity = t1->arity;
	    while (i < arity && match(t1->args[i], c1, t2->args[i], trp))
		i++;
	    if (i == arity)
		return(1);
	    else {  /* restore from trail and fail */
		tp = *trp;
		while (tp != tpos) {
		    tp->context->terms[tp->varnum] = NULL;
		    t3 = tp;
		    tp = tp->next;
		    free_trail(t3);
		    }
		*trp = tpos;
		return(0);
		}
	    }
}  /* match */

/*************
 *
 *    Term_ptr apply(term, context) -- Apply a substitution to a term.
 *
 *    Apply always succeeds and returns a pointer to the
 *    instantiated term.
 *
 *************/

Term_ptr apply(Term_ptr t, Context_ptr c)
{
    Term_ptr t2;

    DEREFERENCE(t, c)

    /* A NULL context is ok.  It happens when c is built by match. */
    /* If the context is NULL, then apply just copies the term.    */
    
    t2 = get_term(t->arity);

    if (VARIABLE(t)) {
	if (!c)
	    t2->symbol = t->symbol;
	else
	    t2->symbol = c->multiplier * MAX_VARS + t->symbol;
	return(t2);
	}
    else {  /* constant or complex term */
	int i;
	t2->symbol = t->symbol;
	for (i = 0; i < t->arity; i++)
	    t2->args[i] = apply(t->args[i], c);
	return(t2);
	}
}  /* apply */

/*************
 *
 *    apply_substitute(beta, c_from, t, into_term, c_into)
 *
 *    This routine is similar to apply, except that when it reaches the into
 *    term, the appropriate instance of beta is returned.  It is typically used
 *    in building a paramoldulant.
 *
 *************/

Term_ptr apply_substitute(Term_ptr beta, Context_ptr c_from, Term_ptr t,
			  Term_ptr into_term, Context_ptr c_into)
{
    if (t == into_term)
	return(apply(beta, c_from));
    else if (VARIABLE(t))
	return(apply(t, c_into));
    else {
	Term_ptr t2;
	int i;
        t2 = get_term(t->arity);
        t2->symbol = t->symbol;
	for (i = 0; i < t->arity; i++)
	    t2->args[i] = apply_substitute(beta, c_from, t->args[i], into_term, c_into); 
	return(t2);
	}
}  /* apply_substitute */

/*************
 *
 *    Term_ptr apply_basic(term, context) -- Apply a substitution to a term.
 *
 *    Apply always succeeds and returns a pointer to the
 *    instantiated term.
 *
 *    This "basic" version is used for "basic" paramodulation.
 *    If the into term has the "nonbasic" mark, or if the into term
 *    is a variable that gets instantiated, the result is also marked.
 *
 *************/

Term_ptr apply_basic(Term_ptr t, Context_ptr c)
{
    Term_ptr raw, t2;

    raw = t;  /* save nondereferenced term */

    DEREFERENCE(t, c)

    t2 = get_term(t->arity);
    if (VARIABLE(t)) {
	if (!c)
	    t2->symbol = t->symbol;
	else
	    t2->symbol = c->multiplier * MAX_VARS + t->symbol;
	return(t2);
	}
    else {  /* constant or complex term */
	int i;

	t2->symbol = t->symbol;
	for (i = 0; i < t->arity; i++)
	    t2->args[i] = apply_basic(t->args[i], c);

	/* If the original input is a variable, the result
	   and all of its nonvariable subterms are marked nonbasic;
	   else if the dereferenced input is nonbasic, the root of the
	   result is marked as nonbasic.
	   */

	if (VARIABLE(raw))
	    all_nonbasic(t2);
	else if (term_nonbasic(t))
	    set_term_nonbasic(t2);

	return(t2);
	}
}  /* apply_basic */

/*************
 *
 *    apply_substitute_basic(beta, c_from, t, into_term, c_into)
 *
 *    This routine is similar to apply, except that when it reaches the into
 *    term, the appropriate instance of beta is returned.  It is typically used
 *    in building a paramoldulant.
 *
 *    This "basic" version is used for "basic" paramodulation.
 *    If the into term has the "nonbasic" mark, or if the into term
 *    is a variable that gets instantiated, the result is also marked.
 *    Also, "nonbasic" marks elsewhere in the term are copied.
 *
 *************/

Term_ptr apply_substitute_basic(Term_ptr beta, Context_ptr c_from, Term_ptr t,
			  Term_ptr into_term, Context_ptr c_into)
{
    if (t == into_term)
	return(apply_basic(beta, c_from));
    else if (VARIABLE(t))
	return(apply_basic(t, c_into));
    else {
	Term_ptr t2;
	int i;
        t2 = get_term(t->arity);
	if (term_nonbasic(t))
	    set_term_nonbasic(t2);
        t2->symbol = t->symbol;
	for (i = 0; i < t->arity; i++)
	    t2->args[i] = apply_substitute_basic(beta, c_from, t->args[i],
						 into_term, c_into); 
	return(t2);
	}
}  /* apply_substitute_basic */

/*************
 *
 *    clear_subst_2(trail_1, trail_2) -- Clear part of a substitution.
 *
 *    It is assumed that trail_2 (possibly NULL) is a subtrail
 *    of trail_1. This routine clears entries starting at trail_1,
 *    up to (but not including) trail_2.
 *
 *************/

void clear_subst_2(Trail_ptr t1, Trail_ptr t2)
{
    Trail_ptr t3;
    while (t1 != t2) {
	t1->context->terms[t1->varnum] = NULL;
	t1->context->contexts[t1->varnum] = NULL;
	t3 = t1;
	t1 = t1->next;
	free_trail(t3);
	}
}  /* clear_subst_2 */

/*************
 *
 *    clear_subst_1(trail_1) -- Clear a substitution.
 *
 *        It is assumed that trail_2 (including NULL) is a subtrail
 *    of trail_1. This routine clears entries starting at trail_1,
 *    up to (but not including) trail_2.
 *
 *************/

void clear_subst_1(Trail_ptr t1)
{
    Trail_ptr t3;
    while (t1) {
	t1->context->terms[t1->varnum] = NULL;
	t1->context->contexts[t1->varnum] = NULL;
	t3 = t1;
	t1 = t1->next;
	free_trail(t3);
	}
}  /* clear_subst_1 */

/*************
 *
 *    print_context(file_ptr, context)
 *
 *************/

void print_context(FILE *fp, Context_ptr c)
{
#if 1
    int i;
    struct term *t = get_term(0);

    if (!c)
	fprintf(fp, "Substitution NULL.\n");
    else {
	fprintf(fp, "Substitution, multiplier %d\n", c->multiplier);
	for (i=0; i< MAX_VARS; i++)
	    if (c->terms[i] != NULL) {
		t->symbol = i;
		print_term(fp, t);
		fprintf(fp, " [0x%x] -> ", (unsigned) c);
		print_term(fp, c->terms[i]);
		fprintf(fp, " [0x%x:%d]\n", (unsigned) c->contexts[i], c->contexts[i]->multiplier);
		}
	if (c->partial_term) {
	    printf("partial_term: ");
	    print_term(fp, c->partial_term);
	    printf("\n");
	    }
	free_term(t);
	}
#else
    int i;

    if (!c)
	fprintf(fp, "Substitution NULL.\n");
    else {
	fprintf(fp, "Substitution in context %x, multiplier %d\n", (unsigned) c, c->multiplier);
	
	for (i=0; i< MAX_VARS; i++)
	    if (c->terms[i] != NULL) {
		fprintf(fp, "v%d -> ", i);
		print_term(fp, c->terms[i]);
		fprintf(fp, " context %x\n", (unsigned) c->contexts[i]);
		}
	if (c->partial_term) {
	    printf("partial_term: ");
	    print_term(fp, c->partial_term);
	    printf("\n");
	    }
	}
#endif
}  /* print_context */

/*************
 *
 *    p_context(context)
 *
 *************/

void p_context(Context_ptr c)
{
    print_context(stdout, c);
}  /* p_context */

/*************
 *
 *    print_trail(file_ptr, context)
 *
 *************/

void print_trail(FILE *fp, Trail_ptr t)
{
    Trail_ptr t2;
    fprintf(fp, "Trail:");
    t2 = t;
    while (t2 != NULL) {
	fprintf(fp, " <%d,%x>", t2->varnum, (unsigned) t2->context);
	t2 = t2->next;
	}
    fprintf(fp, ".\n");
}  /* print_trail */

/*************
 *
 *    p_trail(context)
 *
 *************/

void p_trail(Trail_ptr t)
{
    print_trail(stdout, t);
}  /* p_trail */

