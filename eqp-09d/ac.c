#include "Header.h"
#include "Io.h"
#include "Order.h"
#include "Unify.h"
#include "Ac.h"

/* #define DEBUG */

/*************
 *
 *   p_binding()
 *
 *************/

static void p_binding(int vnum, Context_ptr vc, Term_ptr t, Context_ptr tc)
{
  Term_ptr vt, ti;
  vt = get_term(0);
  vt->symbol = vnum;
  ti = apply(vt, vc);
  printf("[");
  print_term(stdout,vt);
  printf(",0x%x:%d] -> [", (unsigned) vc, vc->multiplier);
  print_term(stdout,t);
  printf(",0x%x:%d] (", (unsigned) tc, tc->multiplier);
  print_term(stdout,ti);
  printf(")\n");
  free_term(vt);
  free_term(ti);
}  /* p_binding */

/*************
 *
 *    flatten_deref
 *
 *    Given a term (t) with AC functor and a context (tc), fill in an
 *    array (a) with the flattened arguments.  Variable arguments are
 *    dereferenced, and a second array (ac) is filled in with the
 *    corresponding contexts.  The given term is not changed.
 *    The index (*ip) must be initialized by the calling routine.
 *
 *    Note: the objects being manipulated are pairs <term,context>,
 *    where the context determines the values of the variables in the term.
 *    If context is NULL, the variables are treated as constants.
 *
 *************/

static void flatten_deref(Term_ptr t, Context_ptr tc,
			  Term_ptr *a, Context_ptr *ac, int *ip)
{
    Term_ptr t1;
    Context_ptr c1;
    int sn, i;

    sn = t->symbol;

    for (i = 0; i < t->arity; i++) {
	t1 = t->args[i];
	c1 = tc;
	DEREFERENCE(t1, c1);

        if (t1->symbol == sn)
            flatten_deref(t1, c1, a, ac, ip);
	else {
	    if (*ip >= MAX_AC_ARGS) {
		p_term(t1);
		abend("in flatten_deref, too many arguments.");
		}
	    else {
		a[*ip] = t1;
		ac[*ip] = c1;
		}
	    (*ip)++;
	    }
        }
}  /* flatten_deref(t) */

/*************
 *
 *    compare_ncv_context -- compare terms, taking context into account.
 *
 *    Compare terms.  NAME < COPMLEX < VARIABLE.
 *    Constants by symbol; complex lexicographically; vars by (context,varnum)
 *    Ignore AC functors.
 *
 *    Return LESS_THAN, SAME_AS, or GREATER_THAN.
 *
 *************/

static int compare_ncv_context(Term_ptr t1, Term_ptr t2,
			       Context_ptr c1, Context_ptr c2)
{
    int rc, m1, m2;

    if (CONSTANT(t1) && !CONSTANT(t2))
	rc = LESS_THAN;
    else if (!CONSTANT(t1) && CONSTANT(t2))
	rc = GREATER_THAN;
    else if (COMPLEX(t1) && VARIABLE(t2))
	rc = LESS_THAN;
    else if (VARIABLE(t1) && COMPLEX(t2))
	rc = GREATER_THAN;

    /* Now we know they are the same type. */

    else if VARIABLE(t1) {
	/* both variables */

	m1 = (c1 ? c1->multiplier : -INT_MAX);
	m2 = (c2 ? c2->multiplier : -INT_MAX);

	if (m1 < m2)
	    rc = LESS_THAN;
	else if (m1 > m2)
	    rc = GREATER_THAN;
	else if (t1->symbol < t2->symbol)
	    rc = LESS_THAN;
	else if (t1->symbol > t2->symbol)
	    rc = GREATER_THAN;
	else
	    rc = SAME_AS;
	}
    else {
	if (t1->symbol < t2->symbol)
	    rc = LESS_THAN;
	else if (t1->symbol > t2->symbol)
	    rc = GREATER_THAN;
	else {
	    int i;
	    for (i = 0, rc = SAME_AS; i < t1->arity && rc == SAME_AS; i++)
		rc = compare_ncv_context(t1->args[i], t2->args[i], c1, c2);
	    }
	}
    return(rc);
}  /* compare_ncv_context */

/*************
 *
 *    sort_ac(a, c, n)
 *
 *    Sort an array of terms and their associated contexts.
 *    I intend for the number of terms to be small, so this is 
 *    a quadratic sort.
 *
 *************/

static void sort_ac(Term_ptr *a, Context_ptr *c, int n)
{
    int i, j, min_i;
    Term_ptr min_t;
    Context_ptr min_c;

    for (i = 0; i < n-1; i++) {
        min_t = a[i];
	min_c = c[i];
        min_i = i;
        for (j = i+1; j < n; j++) {
            if (compare_ncv_context(a[j], min_t, c[j], min_c) == LESS_THAN) {
                min_t = a[j];
                min_c = c[j];
                min_i = j;
                }
            }
        if (min_i != i) {
            a[min_i] = a[i];
            a[i] = min_t;
	    c[min_i] = c[i];
	    c[i] = min_c;
            }
        }

}  /* sort_ac */

/*************
 *
 *    void elim_con_context
 *
 *    Eliminate common terms, taking context into account.
 *    Eliminated terms are just set to NULL.
 *
 *************/

static void elim_con_context(Term_ptr *a1, Term_ptr *a2,
			     Context_ptr *c1, Context_ptr *c2, int n1, int n2)
{
    int i1, i2, rc;

    i1 = i2 = 0;
    while (i1 < n1 && i2 < n2) {
	rc = compare_ncv_context(a1[i1], a2[i2], c1[i1], c2[i2]);
	if (rc == SAME_AS) {
	    a1[i1] = NULL; c1[i1] = NULL; i1++;
	    a2[i2] = NULL; c2[i2] = NULL; i2++;
	    }
	else if (rc == LESS_THAN)
	    i1++;
	else
	    i2++;
	}
}  /* elim_con_context */

/*************
 *
 *    void ac_mult_context
 *
 *    With an array of terms, eliminate NULL positions, and collapse
 *    duplicates into one, building a corresponding array of multiplicities.
 *
 *************/

static void ac_mult_context(Term_ptr *a, Context_ptr *c, int *mults, int *np)
{
    int it, im, i, m, j, n; 

    n = *np;
    im = 0;
    it = 0;
    while (it < n) {
	if (!a[it])
	    it++;
	else {
	    i = it+1;
	    m = 1;
	    while (i < n && a[i] &&
		   compare_ncv_context(a[it], a[i], c[it], c[i]) == SAME_AS) {
		a[i] = NULL;
		c[i] = NULL;
		m++;
		i++;
		}
	    mults[im++] = m;
	    it = i;
	    }
	}
    for (i = n-1; i >= 0; i--) {
	if (!a[i]) {
	    for (j = i; j < n-1; j++) {
		a[j] = a[j+1];
		c[j] = c[j+1];
		}
	    n--;
	    }
	}
    if (n != im)
	abend("in ac_mult_context, n!=im.");
    *np = n;
}  /* ac_mult_context */

/*************
 *
 *    ac_prepare
 *
 *    Final preparation for diophantine solver.  Fill in the arrays:
 *
 *        ab -          coefficients for both terms
 *        constraints -    0 for variable with context
 *                         -(varnum+1) for variable w/o context (unbindable)
 *                         else symbol number
 *        terms         the arguments
 *        contexts      contexts for arguments 
 *
 *************/

static void ac_prepare(Term_ptr *a1, Term_ptr *a2,
		       Context_ptr *c1, Context_ptr *c2,
		       int *mults1, int *mults2, int n1, int n2, int *ab,
		       int *constraints,
		       Term_ptr *terms, Context_ptr *contexts)
{
    int i;
    
    if (n1+n2 > MAX_COEF)
	abend("in ac_prepare, too many arguments.");

    for (i = 0; i < n1; i++) {
	ab[i] = mults1[i];
	if (VARIABLE(a1[i]))
	    constraints[i] = (c1[i] ? 0 : (-a1[i]->symbol) - 1);
	else
	    constraints[i] = a1[i]->symbol;
	terms[i] = a1[i];
	contexts[i] = c1[i];
	}

    for (i = 0; i < n2; i++) {
	ab[i+n1] = mults2[i];
	if (VARIABLE(a2[i]))
	    constraints[i+n1] = (c2[i] ? 0 : (-a2[i]->symbol) - 1);
	else
	    constraints[i+n1] = a2[i]->symbol;
	terms[i+n1] = a2[i];
	contexts[i+n1] = c2[i];
	}
}  /* ac_prepare */

/*************
 *
 *    set_up_basis_terms
 *
 *    Given the basis solutions, fill in a corresponding array of 
 *    partial terms to be used for building substitutions.
 *    This is done once for the basis, to avoid rebuilding the terms
 *    for each subset.
 *
 *    NOTE: the terms are not well-formed.  Each has form f(ti,NULL),
 *    so that it's quick to make, e.g., f(t1,f(t2,f(t3,t4))).
 *
 *************/

static void set_up_basis_terms(int sn, int (*basis)[MAX_COEF], int num_basis,
			int length, Term_ptr (*basis_terms)[MAX_COEF])
{
    Term_ptr t1, t2, t3;
    int i, j, k;

    for (i = 0; i < num_basis; i++)
	for (j = 0; j < length; j++) {
	    if (basis[i][j] == 0)
		basis_terms[i][j] = NULL;
	    else {
		t1 = get_term(0);
		t1->symbol = i;
		for (k = 2; k <= basis[i][j]; k++) {
		    t2 = get_term(0);
		    t2->symbol = i;
		    t3 = get_term(2);
		    t3->symbol = sn;
		    t3->args[0] = t2;
		    t3->args[1] = t1;
		    t1 = t3;
		    }
		t2 = get_term(2);
		t2->symbol = sn;
		t2->args[0] = t1;
		t2->args[1] = NULL;
		basis_terms[i][j] = t2;
		}
	    }
}  /* set_up_basis_terms */

#define GO        1
#define SUCCESS   2
#define EXHAUSTED 4
#define FAILURE   3

/*************
 *
 *    unify_ac
 *
 *    Associative-commutative Unification.  t1 and t2 have the same ac functor.
 *    (t1, c1, t2, c2, are dereferenced terms from bt.)
 *
 *    If c1 and c2 are different they must have different multipliers!!!
 *
 *************/

int unify_ac(Term_ptr t1, Context_ptr c1,
	     Term_ptr t2, Context_ptr c2, Bt_node_ptr bt)
{
    Term_ptr t3, t4, ti;
    Bt_node_ptr sub_problems, bt2, bt3;
    Ac_position_ptr ac;
    Term_ptr a1[MAX_AC_ARGS], a2[MAX_AC_ARGS];
    Context_ptr ac1[MAX_AC_ARGS], ac2[MAX_AC_ARGS], ci;
    int mults1[MAX_AC_ARGS], mults2[MAX_AC_ARGS];
    int ab[MAX_COEF], num_basis, n1, n2;
    int i, j, length, vn, ok;
    int status, continuation;

    continuation = bt->alternative;

    if (!continuation) {  /* If first call, set up dioph eq and solve. */
	
	Stats[AC_INITIATIONS]++;
	bt->alternative = ASSOC_COMMUTE;

	ac = get_ac_position();
	bt->ac = ac;
	ac->c3 = get_context();
	ac->superset_limit = Parms[AC_SUPERSET_LIMIT].val;

#ifdef DEBUG
        printf("\nunify_ac, new problem:\n");
        printf("    "); print_term(stdout, t1); printf(" [0x%x:%d]\n", (unsigned) c1, c1->multiplier);
        printf("    "); print_term(stdout, t2); printf(" [0x%x:%d]\n", (unsigned) c2, c2->multiplier);
        printf("    c3 context is [0x%x:%d]\n", (unsigned) ac->c3, ac->c3->multiplier);

#endif

	n1 = 0;
	flatten_deref(t1,c1,a1,ac1,&n1);   /* put args in a1, incl. deref */
	sort_ac(a1, ac1, n1);              /* sort args */
	n2 = 0;
	flatten_deref(t2,c2,a2,ac2,&n2);  
	sort_ac(a2, ac2, n2);
	
	elim_con_context(a1, a2, ac1, ac2, n1, n2);  /* elim. common terms */
	
	ac_mult_context(a1, ac1, mults1, &n1);    /* get multiplicity */
	ac_mult_context(a2, ac2, mults2, &n2);

	if (n1 == 0 && n2 == 0) {
	    /* Input terms are identical modulo AC.  */
            /* Succeed with no alternatives.         */
	    free_context(ac->c3);
	    free_ac_position(bt->ac);
	    bt->ac = NULL;
	    bt->alternative = 0;
	    status = SUCCESS;
	    }
	else {
	    
	    ac_prepare(a1, a2, ac1, ac2, mults1, mults2, n1, n2, ab,
		       ac->constraints, ac->args, ac->arg_contexts);

	    ok = dio(ab,n1,n2,ac->constraints,ac->basis,&(ac->num_basis));

	    num_basis = ac->num_basis;
	    length = n1 + n2;
	    
	    if (ok == 1 && num_basis > 0) {
		/* if solutions, store data in ac_position */
		ac->m = n1;
		ac->n = n2;
		
		/* prepare for combination search */
		
		set_up_basis_terms(t1->symbol, ac->basis, num_basis,
				   n1+n2, ac->basis_terms);
		
		status = GO;
		}
	    else {
		status = FAILURE;
		if (ok == -1) {
		    printf("basis too big for %d %d.\n", n1, n2);
#if 0
		    p_term(t1); printf(" ");
		    p_term(t2); printf("\n");
		    p_ac_basis(ac->basis, ac->num_basis, n1, n2);
		    /* print out args2 */
		    for (i = 0; i < n2; i++)
			p_term(a2[i]);
		    exit(34);
#endif		    
		    }
		}
	    }
	}

    else {  /* continuation */

	Stats[AC_CONTINUATIONS]++;
	ac = bt->ac;
#ifdef DEBUG
        printf("\nunify_ac, continuation:\n");
        printf("    "); print_term(stdout, t1); printf(" [0x%x:%d]\n", (unsigned) c1, c1->multiplier);
        printf("    "); print_term(stdout, t2); printf(" [0x%x:%d]\n", (unsigned) c2, c2->multiplier);
        printf("    c3 context is [0x%x:%d]\n", (unsigned) ac->c3, ac->c3->multiplier);

#endif
	if (ac->sub_position) { /* if subproblems pending */
	    ac->sub_position = unify_bt_next(ac->sub_position);
	    status = (ac->sub_position ? SUCCESS : GO);
	    }
	else
	    status = GO;
	num_basis = ac->num_basis;
	length = ac->m + ac->n;
	}

    while (status == GO) {

	if (continuation) {

	    /* Undo bindings from previous combination. */

	    for (i = length-1; i >= 0; i--) {

		ti = ac->args[i]; ci = ac->arg_contexts[i];

		if (ci && VARIABLE(ti)) {
		    vn = ti->symbol;
#ifdef DEBUG
		    printf("<-<-<- clearing (ci) ");
		    fflush(stdout);
		    p_binding(vn, ci, ci->terms[vn], ci->contexts[vn]);
#endif
		    ci->terms[vn] = NULL;
		    ci->contexts[vn] = NULL;
		    }

		else if (CONSTANT(ti) || (!ci && VARIABLE(ti))) {
#ifdef DEBUG
		    printf("<-<-<- clearing (c3)"); print_term(stdout, ac->new_terms[i]);
		    p_binding(ac->new_terms[i]->symbol, ac->c3, ac->c3->terms[ac->new_terms[i]->symbol],
			      ac->c3->contexts[ac->new_terms[i]->symbol]);
#endif
		    ac->c3->terms[ac->new_terms[i]->symbol] = NULL;
		    ac->c3->contexts[ac->new_terms[i]->symbol] = NULL;
		    }
		}
	    }

        /* Get first or next relevant subset of the basis solutions.
	 * A parameter limits the number of combinations (and makes AC
	 * unification incomplete).  -1 means that there is no limit.
	 * 0 means that no supsersets are allowed, 1 means that supersets
         * with one additional element are allowed, etc..  Also, if there
         * is a limit, then at most MAX_COMBOS combinations will be returned.
	 */

	if (ac->superset_limit < 0)
	  ok = next_combo_a(length, ac->basis, num_basis, ac->constraints,
			    ac->combo, ac->sum, !continuation);
	else
	  ok = next_combo_a1(length, ac->basis, num_basis, ac->constraints,
			     ac->combo, ac->sum, !continuation, ac->combos,
			     &(ac->combos_remaining), ac->superset_limit);
#ifdef DEBUG
	printf("    ----Combination? %s\n", ok ? "YES" : "NO");
#endif

	if (ok) {

            /* We now have a potential unifier.  It's not guaranteed,
             * because it may have subterms to be unified.
             */

	    sub_problems = bt3 = NULL;

            /* A variable is associated with each row of the basis.
             * ac->combo is the current subst of the rows.
	     *
	     * Loop through columns, building a term (t4) for each.
	     */

	    for (i = 0; i < length; i++) {
		t4 = NULL;
		/* Loop through rows, building t4. */
		for (j = 0; j < num_basis; j++) {
		    if (ac->combo[j]) {
			t3 = ac->basis_terms[j][i];
			if (t3) {
			    if (!t4)
				t4 = t3->args[0];
			    else {
				t3->args[1] = t4;
				t4 = t3;
				}
			    }
			}
		    }
		ac->new_terms[i] = t4;
#ifdef DEBUG
		printf("    ---- arg %d goes with ", i);
		p_term(t4);
#endif
		
		/* t4 must now be unified with args[i].
		 * switch args[i]
		 *   variable: just bind it.
		 *   constant: bind t4 (which is a variable in this case).
		 *   complex:  add t4=args[i] to the set of subproblems.
		 */

		ti = ac->args[i]; ci = ac->arg_contexts[i];

		if (ci && VARIABLE(ti)) {
		    vn = ti->symbol;
		    ci->terms[vn] = t4;
		    ci->contexts[vn] = ac->c3;
#ifdef DEBUG
		    printf("->->->-> binding (ci) ");
		    p_binding(vn, ci, t4, ac->c3);
#endif
		    }
		else if (CONSTANT(ti) || (!ci && VARIABLE(ti))) {
		    ac->c3->terms[t4->symbol] = ti;
		    ac->c3->contexts[t4->symbol] = ci;
#ifdef DEBUG
		    printf("->->->-> binding (c3)");
		    p_binding(t4->symbol, ac->c3, ti, ci);
#endif
		    }
		else {
		    bt2 = get_bt_node();
		    bt2->prev = bt3;
		    if (bt3)
			bt3->next = bt2;
		    else
			sub_problems = bt2;
		    bt2->t1 = t4;
		    bt2->c1 = ac->c3;
		    bt2->t2 = ti;
		    bt2->c2 = ci;
		    bt3 = bt2;
#ifdef DEBUG
		    printf("->->->-> subproblem  [");
		    print_term(stdout,t4);
		    printf(",0x%x:%d] :: [", (unsigned) ac->c3, ac->c3->multiplier);
		    print_term(stdout,ti);
		    printf(",0x%x:%d]\n", (unsigned) ci, ci->multiplier);
#endif
		    }
		}  /* for each arg */

	    if (sub_problems) {
		ac->sub_position = unify_bt_guts(sub_problems);
		if (ac->sub_position)
		    status = SUCCESS;
		else {
#ifdef DEBUG
		    printf("    subproblems failed; continuing\n");
#endif
		    continuation = 1;
		    status = GO;
		    }
		}
	    else {
		ac->sub_position = NULL;
		status = SUCCESS;
		}
	    }  /* if (ok) */
	else  /* There are no more combinations, so stop. */
	    status = EXHAUSTED;
	}  /* while (STATUS == go) */

    if (status == SUCCESS) {
#ifdef DEBUG
        printf("\nunify_ac, success:\n");
        printf("    "); print_term(stdout, t1); printf(" [0x%x:%d]\n", (unsigned) c1, c1->multiplier);
        printf("    "); print_term(stdout, t2); printf(" [0x%x:%d]\n", (unsigned) c2, c2->multiplier);
        printf("    c3 context is [0x%x:%d]\n", (unsigned) ac->c3, ac->c3->multiplier);
	{
	  Term_ptr s1, s2;
	  s1 = apply(t1, c1);
	  s2 = apply(t2, c2);
	  printf("    t1 instance: "); print_term_nl(stdout, s1);
	  printf("    t2 instance: "); print_term_nl(stdout, s2);
	  zap_term(s1);
	  zap_term(s2);
	}
	  

#endif
	return(1);
	}
    else {
	/* Free memory, clean up, and fail. */

#ifdef DEBUG
        printf("\nunify_ac, %s:\n", status == EXHAUSTED ? "finished" : "fail");
        printf("    "); print_term(stdout, t1); printf(" [0x%x:%d]\n", (unsigned) c1, c1->multiplier);
        printf("    "); print_term(stdout, t2); printf(" [0x%x:%d]\n", (unsigned) c2, c2->multiplier);
        printf("    c3 context is [0x%x]\n", (unsigned) ac->c3);

#endif
	if (status == EXHAUSTED) {
	    /* Delete all terms in basis_terms. */
	    for (i = 0; i < num_basis; i++)
		for (j = 0; j < length; j++)
		    if (ac->basis_terms[i][j]) {
			t2 = ac->basis_terms[i][j];
			zap_term(t2->args[0]);
			free_term(t2);
			}
	    }

	free_context(ac->c3);
	free_ac_position(bt->ac);
	bt->ac = NULL;
	bt->alternative = 0;
	return(0);
	}
    
}  /* unify_ac */

/*************
 *
 *    unify_ac_cancel
 *
 *    This routine should be called if the rest of a sequence of
 *    AC unifiers is not called for.  It clears substitutions as well
 *    frees memory.
 *
 *************/

void unify_ac_cancel(Ac_position_ptr ac)
{
    int i, j, length, vn;
    Context_ptr ci;
    Term_ptr t2, ti;
    Bt_node_ptr bt;

    length = ac->m + ac->n;
    
    /* Undo bindings from previous combination. */
    
    for (i = 0; i < length; i++) {
	
	ti = ac->args[i]; ci = ac->arg_contexts[i];
	
	if (ci && VARIABLE(ti)) {
	    vn = ac->args[i]->symbol;
	    ci->terms[vn] = NULL;
	    ci->contexts[vn] = NULL;
	    }
	
	else if (CONSTANT(ti) || (!ci && VARIABLE(ti))) {
	    ac->c3->terms[ac->new_terms[i]->symbol] = NULL;
	    ac->c3->contexts[ac->new_terms[i]->symbol] = NULL;
	    }
	}
    
    /* Delete all terms in basis_terms. */
    
    for (i = 0; i < ac->num_basis; i++)
	for (j = 0; j < length; j++)
	    if (ac->basis_terms[i][j]) {
		t2 = ac->basis_terms[i][j];
		zap_term(t2->args[0]);
		free_term(t2);
		}

    if (ac->sub_position) {
	/* unity_bt leaves you at the end of the list, so get to the start. */
	for (bt = ac->sub_position; bt->prev; bt = bt->prev);
	unify_bt_cancel(bt);
	}
    
    free_context(ac->c3);
    free_ac_position(ac);
    
}  /* unify_ac_cancel */

/*************
 *
 *   p_ac_position() - print ac_position
 *
 *************/

void p_ac_position(Ac_position_ptr ac, int n)
{

    if (!ac)
	printf("\nac_position is NULL.\n");
    else {
	printf("\nac_position, context c3:\n");
	p_context(ac->c3);
	printf("\nSub problems:\n");
	if (!ac->sub_position)
	    printf("none.\n");
	else {
	    Bt_node_ptr bt;
	    /* Get to the beginning of the list of subproblems. */
	    for (bt = ac->sub_position; bt->prev; bt = bt->prev);
	    for (; bt; bt = bt->next)
		p_bt_tree(bt, n);
	    }
	printf("end of ac_position, context c3:\n");
	}
}  /* p_ac_position */

