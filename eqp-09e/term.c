#include "Header.h"
#include "Symbols.h" /* var_nam(), sn_to_str() */

/*
 * memory management
 */

/*
 * Note that this is unusual, because there is an array of avail lists
 * for terms of different arities.  Terms with arities >= MAX_TERM_AVAIL-1
 * all go into the same list.
 */

#define MAX_TERM_AVAIL 5

static Term_ptr term_avail[MAX_TERM_AVAIL];
static long term_gets, term_frees, term_avails, term_mem;

/*************
 *
 *   Term_ptr get_term(arity)
 *
 *************/

Term_ptr get_term(int arity)
{
    Term_ptr p;

    term_gets++;
    
    if (arity == 0) {
	if (term_avail[0]) {
	    term_avails--;
	    p = term_avail[0];
	    term_avail[0] = (Term_ptr) p->args;
	    }
	else {
	    p = tp_alloc(sizeof(struct term));
	    p->arity = 0;
	    term_mem += sizeof(struct term);
	    }
	p->args = NULL;
	}
	
    else if (arity < MAX_TERM_AVAIL-1) {
	if (term_avail[arity]) {
	    term_avails--;
	    p = term_avail[arity];
	    term_avail[arity] = p->args[0];
	    }
	else {
	    p = tp_alloc(sizeof(struct term));
	    p->args = tp_alloc(sizeof(Term_ptr) * arity);
	    p->arity = arity;
	    term_mem += sizeof(struct term) + sizeof(Term_ptr) * arity;
	    }
	}
    else {  /* must use multiple-arity list */
	Term_ptr t2;
	for (p = term_avail[MAX_TERM_AVAIL-1], t2 = NULL;
	     p && p->arity != arity;
	     t2 = p, p = p->args[0]);
	if (p) {
	    term_avails--;
	    if (t2)
		t2->args[0] = p->args[0];
	    else
		term_avail[MAX_TERM_AVAIL-1] = p->args[0];
	    }
	else {
	    p = tp_alloc(sizeof(struct term));
	    p->args = tp_alloc(sizeof(Term_ptr) * arity);
	    p->arity = arity;
	    term_mem += sizeof(struct term) + sizeof(Term_ptr) * arity;
	    }
	}

    p->symbol = 0;
    p->bits = 0;
    p->fpa_id = 0;
    p->containing_clause = NULL;
    return(p);
}  /* get_term */

/*************
 *
 *    free_term()
 *
 *************/

void free_term(Term_ptr p)
{
    term_frees++;
    term_avails++;
    if (p->arity == 0) {
	p->args = (struct term **) term_avail[0];
	term_avail[0] = p;
	}
    else if (p->arity < MAX_TERM_AVAIL-1) {
	p->args[0] = term_avail[p->arity];
	term_avail[p->arity] = p;
	}
    else {
	p->args[0] = term_avail[MAX_TERM_AVAIL-1];
	term_avail[MAX_TERM_AVAIL-1] = p;
	}
}  /* free_term */

/*************
 *
 *   print_term_mem()
 *
 *************/

void print_term_mem(FILE *fp, int heading)
{
    if (heading)
	fprintf(fp, "  type (bytes each)        gets      frees     in use      avail      bytes\n");

    fprintf(fp, "term (%4d)         %11ld%11ld%11ld%11ld%9.1f K\n", (int) sizeof(struct term), term_gets, term_frees, term_gets - term_frees, term_avails, term_mem / 1024.);

}  /* print_term_mem */

/*************
 *
 *   p_term_mem()
 *
 *************/

void p_term_mem()
{
    print_term_mem(stdout, 1);
}  /* p__mem */

/*
 *  end of memory management
 */

/*************
 *
 *    zap_term(term)
 *
 *    Free a term and all of its subterms.
 *
 *************/

void zap_term(Term_ptr t)
{
    int i;
    for (i = 0; i < t->arity; i++)
	zap_term(t->args[i]);
    free_term(t);
}  /* zap_term */

/*************
 *
 *    int term_ident(term1, term2) -- Compare two terms.
 *
 *    If identical return(1); else return(0).  The bits
 *    field is not checked.
 *
 *************/

int term_ident(Term_ptr t1, Term_ptr t2)
{
    if (t1->symbol != t2->symbol)
	return(0);
    else {
	int i;
	for (i = 0; i < t1->arity; i++)
	    if (!term_ident(t1->args[i], t2->args[i]))
		return(0);
	return(1);
	}
}  /* term_ident */  

/*************
 *
 *    Term_ptr copy_term(term) -- Return a copy of the term.
 *
 *    The bits field is not copied.
 *
 *************/

Term_ptr copy_term(Term_ptr t)
{
    Term_ptr t2;
    int i;

    t2 = get_term(t->arity);
    t2->symbol = t->symbol;
    for (i = 0; i < t->arity; i++)
	t2->args[i] = copy_term(t->args[i]);
    return(t2);
}  /* copy_term */

/*************
 *
 *    Term_ptr copy_term_bits(term) -- Return a copy of the term.
 *
 *    The bits field is copied.
 *
 *************/

Term_ptr copy_term_bits(Term_ptr t)
{
    Term_ptr t2;
    int i;

    t2 = get_term(t->arity);
    t2->symbol = t->symbol;
    t2->bits = t->bits;
    for (i = 0; i < t->arity; i++)
	t2->args[i] = copy_term_bits(t->args[i]);
    return(t2);
}  /* copy_term_bits */

/*************
 *
 *    int set_vars_term(term, varnames)
 *
 *    If too many variables, return(0).
 *
 *************/

int set_vars_term(Term_ptr t, char **varnames)
{
    if (CONSTANT(t) && var_name(sn_to_str(t->symbol))) {
	int i;
	for (i = 0;
	     i < MAX_VARS && varnames[i] && varnames[i] != sn_to_str(t->symbol);
	     i++) ;
	if (i == MAX_VARS)
	    return(0);
	else {
	    if (!varnames[i])
		varnames[i] = sn_to_str(t->symbol);
	    t->symbol = i;
	    return(1);
	    }
	}
    else {
	int i, rc;
	for (i = 0, rc = 1; i < t->arity && rc; i++)
	    rc = set_vars_term(t->args[i], varnames);
	return(rc);
	}
}  /* set_vars_term */

/*************
 *
 *    int set_vars(term)
 *
 *        Decide which of the names are really variables, and make them
 *    into variables.  (This routine is used only on input terms.)
 *
 *    If too many variables, return(0); else return(1).
 *
 *************/

int set_vars(Term_ptr t)
{
    char *varnames[MAX_VARS];
    int i;

    for (i=0; i<MAX_VARS; i++)
	varnames[i] = NULL;
    return(set_vars_term(t, varnames));
}  /* set_vars */

/*************
 *
 *    int ground_term(t) -- is a term ground?
 *
 *************/

int ground_term(Term_ptr t)
{
    if (VARIABLE(t))
	return(0);
    else {
	int i;
	for (i = 0; i < t->arity; i++)
	    if (!ground_term(t->args[i]))
		return(0);
	return(1);
	}
}  /* ground_term */

/*************
 *
 *   biggest_variable()
 *
 *************/

int biggest_variable(Term_ptr t)
{
    if (VARIABLE(t))
	return(t->symbol);
    else {
	int i, max, v;
	for (max = -1, i = 0; i < t->arity; i++) {
	    v = biggest_variable(t->args[i]);
	    max = (v > max ? v : max);
	    }
	return(max);
	}
}  /* biggest_variable */

/*************
 *
 *    symbol_count
 *
 *************/

int symbol_count(Term_ptr t)
{
    int i, count;
    for (i = 0, count = 0; i < t->arity; i++)
	count += symbol_count(t->args[i]);
    return(count+1);
}  /* symbol_count  */

/*************
 *
 *     int occurs_in(t1, t2) -- Does t1 occur in t2 (including t1==t1)?
 *
 *     term_ident is used to check identity.
 *
 *************/

int occurs_in(Term_ptr t1, Term_ptr t2)
{
    if (term_ident(t1, t2))
	return(1);
    else {
	int i;
	for (i = 0; i < t2->arity; i++)
	    if (occurs_in(t1, t2->args[i]))
		return(1);
	return(0);
	}
}  /* occurs_in */

/*************
 *
 *    build_binary_term
 *
 *************/

Term_ptr build_binary_term(int sn, Term_ptr t1, Term_ptr t2)
{
    Term_ptr t;

    t = get_term(2);
    t->symbol = sn;
    t->args[0] = t1;
    t->args[1] = t2;
    return(t);
}  /* build_binary_term */

/*************
 *
 *   copy_nonbasic_marks(t1, t2)
 *
 *   Assume that t1 and t2 are identical except for marks.
 *
 *************/

void copy_nonbasic_marks(Term_ptr t1, Term_ptr t2)
{
    int i;
    
    if (term_nonbasic(t1))
	set_term_nonbasic(t2);
    for (i = 0; i < t1->arity; i++)
	copy_nonbasic_marks(t1->args[i], t2->args[i]);
}  /* copy_nonbasic_marks */

/*************
 *
 *   all_nonbasic(t)
 *
 *   Give t and all nonvariable subterms the "nonbasic" mark.
 *
 *************/

void all_nonbasic(Term_ptr t)
{
    if (!VARIABLE(t)) {
	int i;
	set_term_nonbasic(t);
	for (i = 0; i < t->arity; i++)
	    all_nonbasic(t->args[i]);
	}
}  /* all_nonbasic */

/*************
 *
 *   clear_all_nonbasic(t)
 *
 *   Remove all "nonbasic" marks from t and its subterms.
 *
 *************/

void clear_all_nonbasic(Term_ptr t)
{
    int i;
    clear_term_nonbasic(t);
    for (i = 0; i < t->arity; i++)
	clear_all_nonbasic(t->args[i]);
}  /* clear_all_nonbasic */

/*************
 *
 *   mark_term_disabled()
 *
 *************/

void mark_term_disabled(Term_ptr t)
{
    int i;
    set_term_disabled(t);
    for (i = 0; i < t->arity; i++)
	mark_term_disabled(t->args[i]);
}  /* mark_term_disabled */

/*************
 *
 *   set_term_scratch()
 *
 *************/

void set_term_scratch(Term_ptr t)
{
    SET_BIT(t->bits, SCRATCH_BIT);
}  /* set_term_scratch */

/*************
 *
 *   clear_term_scratch()
 *
 *************/

void clear_term_scratch(Term_ptr t)
{
    CLEAR_BIT(t->bits, SCRATCH_BIT);
}  /* clear_term_scratch */

/*************
 *
 *   term_scratch()
 *
 *************/

int term_scratch(Term_ptr t)
{
    return(TP_BIT(t->bits, SCRATCH_BIT));
}  /* term_scratch */

/*************
 *
 *   set_term_oriented()
 *
 *************/

void set_term_oriented(Term_ptr t)
{
    SET_BIT(t->bits, ORIENTED_BIT);
}  /* set_term_oriented */

/*************
 *
 *   clear_term_oriented()
 *
 *************/

void clear_term_oriented(Term_ptr t)
{
    CLEAR_BIT(t->bits, ORIENTED_BIT);
}  /* clear_term_oriented */

/*************
 *
 *   term_oriented()
 *
 *************/

int term_oriented(Term_ptr t)
{
    return(TP_BIT(t->bits, ORIENTED_BIT));
}  /* term_oriented */

/*************
 *
 *   set_term_nonbasic()
 *
 *************/

void set_term_nonbasic(Term_ptr t)
{
    SET_BIT(t->bits, NONBASIC_BIT);
}  /* set_term_nonbasic */

/*************
 *
 *   clear_term_nonbasic()
 *
 *************/

void clear_term_nonbasic(Term_ptr t)
{
    CLEAR_BIT(t->bits, NONBASIC_BIT);
}  /* clear_term_nonbasic */

/*************
 *
 *   term_nonbasic()
 *
 *************/

int term_nonbasic(Term_ptr t)
{
    return(TP_BIT(t->bits, NONBASIC_BIT));
}  /* term_nonbasic */

/*************
 *
 *   set_term_disabled()
 *
 *************/

void set_term_disabled(Term_ptr t)
{
    SET_BIT(t->bits, DISABLED_BIT);
}  /* set_term_disabled */

/*************
 *
 *   clear_term_disabled()
 *
 *************/

void clear_term_disabled(Term_ptr t)
{
    CLEAR_BIT(t->bits, DISABLED_BIT);
}  /* clear_term_disabled */

/*************
 *
 *   term_disabled()
 *
 *************/

int term_disabled(Term_ptr t)
{
    return(TP_BIT(t->bits, DISABLED_BIT));
}  /* term_disabled */

