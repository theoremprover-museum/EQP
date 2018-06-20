#include "Header.h"
#include "List.h"
#include "Symbols.h"
#include "Io.h"
#include "Order.h"

/*************
 *
 *    int sym_precedence(symbol_1, symbol_2)
 *
 *    Return SAME_AS, GREATER_THAN, LESS_THAN, or NOT_COMPARABLE.
 *
 *************/

static int sym_precedence(int symbol_1, int symbol_2)
{
    int p1, p2;

    if (symbol_1 == symbol_2)
	return(SAME_AS);
    else {
	p1 = sn_to_node(symbol_1)->lex_val;
	p2 = sn_to_node(symbol_2)->lex_val;
	
	if (p1 == INT_MAX || p2 == INT_MAX)
	    return(NOT_COMPARABLE);
	else if (p1 > p2)
	    return(GREATER_THAN);
	else if (p1 < p2)
	    return(LESS_THAN);
	else
	    return(SAME_AS);
	}
}  /* sym_precedence */

/*************
 *
 *    int lrpo_status(symbol)
 *
 *************/

static int lrpo_status(int symbol)
{
    return(sn_to_node(symbol)->lrpo_status);
}  /* lrpo_status */

/*************
 *
 *    int lrpo_lex(t1, t2) -- Is t1 > t2 ?
 *
 *    t1 and t2 have same functor and the functor has lr status.
 *
 *************/

static int lrpo_lex(Term_ptr t1, Term_ptr t2)
{
    int i, ok;
    int arity = t1->arity;

    /* First skip over any identical arguments. */

    for (i = 0; i < arity && term_ident(t1->args[i],t2->args[i]); i++);

    if (i == arity)
	return(0);  /* t1 and t2 identical */
    else if (lrpo(t1->args[i], t2->args[i])) {
	/* return (t1 > each remaining arg of t2) */
	for (ok = 1, i++; ok && i < arity; i++)
	    ok = lrpo(t1, t2->args[i]);
	return(ok);
	}
    else {
	/* return (there is a remaining arg of t1 s.t. arg == t2 or arg > t2) */
	for (ok = 0, i++; !ok && i < arity; i++)
	    ok = (term_ident(t1->args[i], t2) || lrpo(t1->args[i], t2));
	return(ok);
	}
}  /* lrpo_lex */

/*************
 *
 *    int num_occurrences(t_arg, t) -- How many times does t_arg occur
 *    as an argument of t?
 *
 *************/

static int num_occurrences(Term_ptr t_arg, Term_ptr t)
{
    int i, n;

    for (i = 0, n = 0; i < t->arity; i++)
       if (term_ident(t->args[i], t_arg))
	   n++;
    return(n);
}  /* num_occurrences */

/*************
 *
 *   Gen_ptr_ptr set_multiset_diff(t1, t2) 
 *
 *   Construct the multiset difference, then return the set of that.
 *   Result must be deallocated by caller with zap_term.
 *
 *   In other words, viewing a term as a multiset of its arguments,
 *   find the set of t1's arguments that have more occurrences in
 *   t1 than in t2.
 *
 *************/

static Gen_ptr_ptr set_multiset_diff(Term_ptr t1, Term_ptr t2)
{
    Gen_ptr_ptr set, prev, curr;
    int i, j;
    
    set = NULL; prev = NULL;
    for (i = 0; i < t1->arity; i++) {

	/* Check iff this is the first occurrence of the arg. */

	for (j = 0; j != i && !term_ident(t1->args[i],t1->args[j]); j++);

        if (i == j && num_occurrences(t1->args[i],t1) > num_occurrences(t1->args[i],t2)) {
	    curr = get_gen_ptr();
	    curr->u.t = t1->args[i];
	    if (prev)
                prev->next = curr;
	    else
	        set = curr;
	    prev = curr;
	    }
	}
    return(set);
}  /* set_multiset_diff */

/*************
 *
 *   int lrpo_multiset(t1, t2) -- Is t1 > t2 in the lrpo multiset ordering?
 *
 *   t1 and t2 have functors with the same precedence.
 *
 *   let n(a,t) be the number of occurrences of a (as an argument) in t.
 *
 *   t1 >(multiset) t2 iff for each arg a2 of t2 with n(a2,t2) > n(a2,t1),
 *   there is an arg a1 of t1 such that  n(a1,t1) > n(a1,t2), and a1>a2.
 *
 *************/

static int lrpo_multiset(Term_ptr t1, Term_ptr t2)
{
    Gen_ptr_ptr s1, s2, p1, p2;
    int ok;

    s1 = set_multiset_diff(t1, t2);
    s2 = set_multiset_diff(t2, t1);
    /*
     * return (s2 not empty and foreach arg a2 of s2
     * there is an arg a1 of s1 such that lrpo(a1, a2)).
     */
    if (!s2)
       ok = 0;
    else {
	for (p2 = s2, ok = 1; p2 && ok; p2 = p2->next)
	    for (p1 = s1, ok = 0; p1 && !ok; p1 = p1->next)
		ok = lrpo(p1->u.t, p2->u.t);
	}
    while (s1) {
	p1 = s1; s1 = s1->next; free_gen_ptr(p1);
	}
    while (s2) {
	p1 = s2; s2 = s2->next; free_gen_ptr(p1);
	}
    return(ok);
}  /* lrpo_multiset */

/*************
 *
 *    int lrpo(t1, t2)
 *                      
 *    Is t1 > t2 in the lexicographic recursive path ordering?
 *
 *************/

int lrpo(Term_ptr t1, Term_ptr t2)
{
    if (VARIABLE(t1))
	/* varaiable never greater than anything */
	return(0);  

    else if (VARIABLE(t2))
	/* t1 > variable iff t1 properly contains that variable */
	return(occurs_in(t2, t1));

    else if (t1->symbol == t2->symbol &&
             lrpo_status(t1->symbol) == LRPO_LR_STATUS)
	return(lrpo_lex(t1, t2));

    else {
	int p, i, ok;

	p = sym_precedence(t1->symbol, t2->symbol);

	if (p == SAME_AS)
	    return(lrpo_multiset(t1, t2));

	else if (p == GREATER_THAN) {
	    /* return (t1 > each arg of t2) */
	    for (ok = 1, i = 0; ok && i < t2->arity; i++)
		ok = lrpo(t1, t2->args[i]);
	    return(ok);
	    }

	else {  /* LESS_THEN or NOT_COMPARABLE */
	    /* return (there is an arg of t1 s.t. arg == t2 or arg > t2) */
	    for (ok = 0, i = 0; !ok && i < t1->arity; i++)
		ok = term_ident(t1->args[i], t2) || lrpo(t1->args[i], t2);

	    return(ok);
	    }
	}
}  /* lrpo */

/*************
 *
 *   int lrpo_greater(t1, t2) - Is t1 > t2 in the lexicographic
 *                              recursive path ordering?
 *
 *    Time this routine.
 *
 *************/

int lrpo_greater(Term_ptr t1, Term_ptr t2)
{
    int rc;

    CLOCK_START(LRPO_TIME)
    rc = lrpo(t1,t2);
    CLOCK_STOP(LRPO_TIME)
    return(rc);

}  /* lrpo_greater */

/*************
 *
 *    merge_sort
 *
 *************/

void merge_sort(void **a,    /* array to sort */
		void **w,    /* work array */
		int start,   /* index of first element */
		int end,     /* index of last element */
		int (*comp_proc) (void *, void *))
{
    int mid, i, i1, i2, e1, e2;

    if (start < end) {
	mid = (start+end)/2;
	merge_sort(a, w, start, mid, comp_proc);
	merge_sort(a, w, mid+1, end, comp_proc);
	i1 = start; e1 = mid;
	i2 = mid+1; e2 = end;
	i = start;
	while (i1 <= e1 && i2 <= e2) {
	    if ((*comp_proc)(a[i1], a[i2]) == LESS_THAN)
		w[i++] = a[i1++];
	    else
		w[i++] = a[i2++];
	    }

	if (i2 > e2)
	    while (i1 <= e1)
		w[i++] = a[i1++];
	else
	    while (i2 <= e2)
		w[i++] = a[i2++];

	for (i = start; i <= end; i++)
	    a[i] = w[i];
	}
}  /* merge_sort */

/*************
 *
 *    int term_compare_ncv(t1, t2) -- Compare two terms.
 *
 *    NAME < COMPLEX < VARIABLE; within type, lexicographic by symbol.
 *    Return SAME_AS, GREATER_THEN, or LESS_THAN.
 *
 *************/

int term_compare_ncv(Term_ptr t1, Term_ptr t2)
{
    int  rc;

    if (COMPLEX(t1) && COMPLEX(t2)) {
	if (t1->symbol == t2->symbol) {
	    int i;
	    for (rc = SAME_AS, i = 0; rc == SAME_AS && i < t1->arity; i++)
		rc = term_compare_ncv(t1->args[i], t2->args[i]);
	    }
	else if (t1->symbol > t2->symbol)
	    rc = GREATER_THAN;
	else
	    rc = LESS_THAN;
	}
    else if (COMPLEX(t1)) {
	if (CONSTANT(t2))
	    rc = GREATER_THAN;
	else
	    rc = LESS_THAN;
	}
    else if (COMPLEX(t2)) {
	if (CONSTANT(t1))
	    rc = LESS_THAN;
	else
	    rc = GREATER_THAN;
	}
    else {
	/* neither is complex, so just look at symbols (var >= 0, constant < 0) */
	if (t1->symbol == t2->symbol)
	    rc = SAME_AS;
	else if (t1->symbol > t2->symbol)
	    rc = GREATER_THAN;
	else
	    rc = LESS_THAN;
	}
    return(rc);
}  /* term_compare_ncv */

/*************
 *
 *    int term_compare_vf(t1, t2) -- Compare two terms.
 *
 *    VARIABLE smallest, rest lexicographically by symbol.
 *    Return SAME_AS, GREATER_THEN, or LESS_THAN.
 *
 *************/

int term_compare_vf(Term_ptr t1, Term_ptr t2)
{
    if (VARIABLE(t1))
        if (VARIABLE(t2))
            if (t1->symbol == t2->symbol)
                return(SAME_AS);
            else
                return(t1->symbol > t2->symbol ? GREATER_THAN : LESS_THAN);
        else
            return(LESS_THAN);

    else if (VARIABLE(t2))
        return(GREATER_THAN);

    else if (t1->symbol == t2->symbol) {
	int i, rc;
	for (rc = SAME_AS, i = 0; rc == SAME_AS && i < t1->arity; i++)
	    rc = term_compare_ncv(t1->args[i], t2->args[i]);
	return(rc);
	}
    else
	return(t1->symbol > t2->symbol ? GREATER_THAN : LESS_THAN);
}  /* term_compare_vf */  

