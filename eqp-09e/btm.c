/*
 * To do, to think about.
 *
 * 1. Recall that inputs to match_bt must be ac_canonical.
 * 2. Also must be c_canonical.
 *
 */

#include "Header.h"
#include "Symbols.h"
#include "Io.h"
#include "Order.h"
#include "Unify.h"
#include "Ac.h"

/* #define DEBUG */

#define POP       1
#define BACKTRACK 2
#define GO        3
#define SUCCESS   4
#define FAILURE   5

/* for mutuall recursion */
 int match_commute(Term_ptr t1, Context_ptr c1, Term_ptr t2, Bt_node_ptr bt);

/*
 * memory management
 */

static Bt_node_ptr bt_node_avail;
static Ac_position_ptr ac_position_avail;
static Ac_match_pos_ptr ac_match_pos_avail;
static Ac_match_free_vars_pos_ptr ac_match_free_vars_pos_avail;
static long bt_node_gets, bt_node_frees, bt_node_avails;
static long ac_position_gets, ac_position_frees, ac_position_avails;
static long ac_match_pos_gets, ac_match_pos_frees, ac_match_pos_avails;
static long ac_match_free_vars_pos_gets, ac_match_free_vars_pos_frees, ac_match_free_vars_pos_avails;

/*************
 *
 *    Bt_node_ptr get_bt_node()
 *
 *************/

Bt_node_ptr get_bt_node(void)
{
    Bt_node_ptr p;
    
    bt_node_gets++;
    if (bt_node_avail == NULL)
	p = tp_alloc(sizeof(struct bt_node));
    else {
	bt_node_avails--;
	p = bt_node_avail;
	bt_node_avail = bt_node_avail->next;
	}

    p->parent = NULL;
    p->first_child = NULL;
    p->last_child = NULL;
    p->next = NULL;
    p->prev = NULL;

    p->t1 = NULL;
    p->t2 = NULL;
    p->c1 = NULL;
    p->c2 = NULL;

    p->varnum = -1;
    p->cb = NULL;
    p->alternative = 0;
    p->partial = 0;

    return(p);
}  /* get_bt_node */

/*************
 *
 *    free_bt_node()
 *
 *************/

void free_bt_node(Bt_node_ptr p)
{
    bt_node_frees++;
    bt_node_avails++;
    p->next = bt_node_avail;
    bt_node_avail = p;
}  /* free_bt_node */

/*************
 *
 *    Ac_position_ptr get_ac_position()
 *
 *************/

Ac_position_ptr get_ac_position(void)
{
    Ac_position_ptr p;
    
    ac_position_gets++;
    if (ac_position_avail == NULL)
	p = tp_alloc(sizeof(struct ac_position));
    else {
	ac_position_avails--;
	p = ac_position_avail;
	ac_position_avail = ac_position_avail->next;
	}

    /* note, no initialization */

    return(p);
}  /* get_ac_position */

/*************
 *
 *    free_ac_position()
 *
 *************/

void free_ac_position(Ac_position_ptr p)
{
    ac_position_frees++;
    ac_position_avails++;
    p->next = ac_position_avail;
    ac_position_avail = p;
}  /* free_ac_position */

/*************
 *
 *    Ac_match_pos_ptr get_ac_match_pos()
 *
 *************/

static Ac_match_pos_ptr get_ac_match_pos(void)
{
    Ac_match_pos_ptr p;

    ac_match_pos_gets++;
    if (ac_match_pos_avail == NULL)
        p = tp_alloc(sizeof(struct ac_match_pos));
    else {
        ac_match_pos_avails--;
        p = ac_match_pos_avail;
        ac_match_pos_avail = ac_match_pos_avail->next;
        }

    /* note, no initialization */

    return(p);
}  /* get_ac_match_pos */

/*************
 *
 *    free_ac_match_pos()
 *
 *************/

static void free_ac_match_pos(Ac_match_pos_ptr p)
{
    ac_match_pos_frees++;
    ac_match_pos_avails++;
    p->next = ac_match_pos_avail;
    ac_match_pos_avail = p;
}  /* free_ac_match_pos */

/*************
 *
 *    Ac_match_free_vars_pos_ptr get_ac_match_free_vars_pos()
 *
 *************/

static Ac_match_free_vars_pos_ptr get_ac_match_free_vars_pos(void)
{
    Ac_match_free_vars_pos_ptr p;

    ac_match_free_vars_pos_gets++;
    if (ac_match_free_vars_pos_avail == NULL)
        p = tp_alloc(sizeof(struct ac_match_free_vars_pos));
    else {
        ac_match_free_vars_pos_avails--;
        p = ac_match_free_vars_pos_avail;
        ac_match_free_vars_pos_avail = ac_match_free_vars_pos_avail->next;
        }

    /* note, no initialization */

    return(p);
}  /* get_ac_match_free_vars_pos */

/*************
 *
 *    free_ac_match_free_vars_pos()
 *
 *************/

static void free_ac_match_free_vars_pos(Ac_match_free_vars_pos_ptr p)
{
    ac_match_free_vars_pos_frees++;
    ac_match_free_vars_pos_avails++;
    p->next = ac_match_free_vars_pos_avail;
    ac_match_free_vars_pos_avail = p;
}  /* free_ac_match_free_vars_pos */

/*************
 *
 *   print_ac_mem()
 *
 *************/

void print_ac_mem(FILE *fp, int heading)
{
    if (heading)
	fprintf(fp, "  type (bytes each)        gets      frees     in use      avail      bytes\n");

    fprintf(fp, "bt_node (%4d)      %11ld%11ld%11ld%11ld%9.1f K\n", sizeof(struct bt_node), bt_node_gets, bt_node_frees, bt_node_gets - bt_node_frees, bt_node_avails, (((bt_node_gets - bt_node_frees) + bt_node_avails) * sizeof(struct bt_node)) / 1024.);
    fprintf(fp, "ac_position (%4d)%11ld%11ld%11ld%11ld%9.1f K\n", sizeof(struct ac_position), ac_position_gets, ac_position_frees, ac_position_gets - ac_position_frees, ac_position_avails, (((ac_position_gets - ac_position_frees) + ac_position_avails) * sizeof(struct ac_position)) / 1024.);
    fprintf(fp, "ac_match_pos (%4d)%11ld%11ld%11ld%11ld%9.1f K\n", sizeof(struct ac_match_pos), ac_match_pos_gets, ac_match_pos_frees, ac_match_pos_gets - ac_match_pos_frees, ac_match_pos_avails, (((ac_match_pos_gets - ac_match_pos_frees) + ac_match_pos_avails) * sizeof(struct ac_match_pos)) / 1024.);
    fprintf(fp, "ac_match_free_vars_pos (%4d)\n                    %11ld%11ld%11ld%11ld%9.1f K\n", sizeof(struct ac_match_free_vars_pos), ac_match_free_vars_pos_gets, ac_match_free_vars_pos_frees, ac_match_free_vars_pos_gets - ac_match_free_vars_pos_frees, ac_match_free_vars_pos_avails, (((ac_match_free_vars_pos_gets - ac_match_free_vars_pos_frees) + ac_match_free_vars_pos_avails) * sizeof(struct ac_match_free_vars_pos)) / 1024.);

}  /* print_ac_mem */

/*************
 *
 *   p_ac_mem()
 *
 *************/

void p_ac_mem()
{
    print_ac_mem(stdout, 1);
}  /* p_ac_mem */

/*
 *  end of memory management
 */

/*************
 *
 *    flatten - given an AC term, store arguments in an array
 *
 *    The index (*ip) must be initialized by the calling routine.
 *
 *************/

void flatten(Term_ptr t, Term_ptr *a, int *ip)
{
    Term_ptr t1;
    int sn, i;

    sn = t->symbol;
    for (i = 0; i < t->arity; i++) {
	t1 = t->args[i];
        if (t1->symbol == sn)
            flatten(t1, a, ip);
	else {
	    if (*ip >= MAX_AC_ARGS) {
		p_term(t);
		abend("in flatten, too many arguments.");
		}
	    a[*ip] = t1;
	    (*ip)++;
	    }
        }
}  /* flatten */

/*************
 *
 *    flatten_mult
 *
 *    Flatten an AC term into an array, collapsing multiple occurrences
 *    into one, filling in a parallel array with multiplicities.
 *    Also return a count of the total number of arguments.
 *
 *    The index (*ip) must be initialized by the calling routine.
 *
 *************/

void flatten_mult(Term_ptr t, Term_ptr *a, int *m, int *ip, int *totp,
		  int (*comp_proc) (void *, void *))
{
    Term_ptr t1;
    int sn, i;

    sn = t->symbol;
    for (i = 0; i < t->arity; i++) {
	t1 = t->args[i];
        if (t1->symbol == sn)
            flatten_mult(t1, a, m, ip, totp, comp_proc);
	else {
	    (*totp)++;
	    if (*ip > 0 && (*comp_proc)(t1, a[(*ip)-1]) == SAME_AS)
		m[(*ip)-1]++;
	    else {
		if (*ip >= MAX_AC_ARGS) {
		    p_term(t);
		    abend("in flatten_mult, too many arguments.");
		    }
		a[*ip] = t1;
		m[*ip] = 1;
		(*ip)++;
		}
	    }
        }
}  /* flatten_mult */

/*************
 *
 *    void elim_com
 *
 *    Eliminate common terms.  Eliminated terms are just set to NULL.
 *
 *************/

void elim_com(Term_ptr *a1, Term_ptr *a2, int n1, int n2,
	      int (*comp_proc) (void *, void *))
{
    int i1, i2, rc;

    i1 = i2 = 0;
    while (i1 < n1 && i2 < n2) {
	rc = (*comp_proc)(a1[i1], a2[i2]);
	if (rc == SAME_AS) {
	    a1[i1] = NULL; i1++;
	    a2[i2] = NULL; i2++;
	    }
	else if (rc == LESS_THAN)
	    i1++;
	else
	    i2++;
	}
}  /* elim_com */

/*************
 *
 *    void ac_mult
 *
 *    With an array of terms, eliminate NULL positions, and collapse
 *    duplicates into one, building a corresponding array of multiplicities.
 *
 *************/

void ac_mult(Term_ptr *a, int *mults, int *np, int (*comp_proc) (void *, void *))
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
		   (*comp_proc)(a[it], a[i]) == SAME_AS) {
		a[i] = NULL;
		m++;
		i++;
		}
	    mults[im++] = m;
	    it = i;
	    }
	}
    for (i = n-1; i >= 0; i--) {
	if (!a[i]) {
	    for (j = i; j < n-1; j++)
		a[j] = a[j+1];
	    n--;
	    }
	}
    if (n != im)
	abend("in ac_mult, n!=im.");
    *np = n;
}  /* ac_mult */

/*************
 *
 *    right_associate(t)
 *
 *    Given a term (t) with a binary functor, say f, right associate the
 *    the binary tree with respect to f.  Do only the top of the tree,
 *    not subtrees under a symbol different from f.  After the reassociation,
 *    the term has the same (physical) top node.
 *
 *************/

void right_associate(Term_ptr t)
{
    Term_ptr fab, a, b, c, d;
    int sn;

    sn = t->symbol;

    if (t->args[1]->symbol == sn)
	right_associate(t->args[1]);

    if (t->args[0]->symbol == sn) {
	right_associate(t->args[0]);

	/* Let t be f(f(a,b),c). */

	fab = t->args[0];
	a = fab->args[0];
        b = fab->args[1];
	c = t->args[1];

	if (b->symbol != sn) {
	    /*  This is easy---just reassociate. */
	    t->args[0] = a;
	    t->args[1] = fab;
	    fab->args[0] = b;
	    fab->args[1] = c;
	    }
	else {
	    /* This is tricky---append the two lists w/o changing top node. */
	    for (d = b; d->args[1]->symbol == sn; d = d->args[1]);

	    t->args[0] = a;
	    t->args[1] = b;
	    fab->args[0] = d->args[1];
	    fab->args[1] = c;
	    d->args[1] = fab;
	    }
	}
}  /* right_associate */

/*************
 *
 *    ac_canonical
 *
 *    Put term t into AC canonical form (right associated and sorted).
 *    Terms are compared with the routine term_compare_ncv, which is
 *    CONSTANT < COMPLEX < VARIABLE, then lexicographic by symbol.
 *
 *    This is recursive, applies to all subterms.
 *
 *    Subterms with term_scratch are fully demodulated; this implies
 *    that they are ac_canonical.
 *
 *************/

void ac_canonical(Term_ptr t)
{
    Term_ptr args[MAX_AC_ARGS], work[MAX_AC_ARGS];
    Term_ptr t1;
    int n, i;

    if (!Internal_flags[AC_PRESENT])
	return;

    if (is_assoc_comm(t->symbol)) {
	/* Get array of arguments, sort, right assoc tree, then insert. */
	n = 0;
	flatten(t, args, &n);
	for (i = 0; i < n; i++)
	    if (!term_scratch(args[i])) /* if not already reduced (and canon) */
		ac_canonical(args[i]);

	merge_sort((void **) args, (void **) work, 0, n-1,
		   (int (*)(void*,void*)) term_compare_ncv);

	right_associate(t);

        for (t1 = t, i = 0; i < n-2; t1 = t1->args[1], i++) {
            t1->args[0] = args[i];
	    clear_term_scratch(t1); /* clear "reduced" flag, because changed. */
	    }
        t1->args[0] = args[n-2];
        t1->args[1] = args[n-1];
	}
    else {  /* Top functor is not AC, so just recurse on arguments. */
	for (i = 0; i < t->arity; i++)
	    ac_canonical(t->args[i]);
	}
}  /* ac_canonical */

/*************
 *
 *     check_ac_canonical(t)
 *
 *************/

int check_ac_canonical(Term_ptr t)
{
    Term_ptr t1;
    int rc;
    
    t1 = copy_term(t);
    ac_canonical(t1);
    rc = term_ident(t, t1);
    zap_term(t1);
    return(rc);
}  /* check_ac_canonical */

/*************
 *
 *    compact_args
 *
 *    Given an array of terms, remove holes by shifting left.
 *
 *************/

static void compact_args(Term_ptr *a, int *np)
{
    int avail, i;

    for (i = 0, avail = -1; i < *np; i++) {
	if (a[i] && avail != -1)
	    a[avail++] = a[i];
	else if (!a[i] && avail == -1)
	    avail = i;
	}
    if (avail != -1)
	*np = avail;
}  /* compact_args */

/*************
 *
 *    macbv_rec -- match (identically) all args of an AC term.
 *
 *    Called by match_ac_bound_vars.
 *
 *************/

static int macbv_rec(int ac_sn, Term_ptr t, Term_ptr *args2, int *mults2,
		     int *match2, int n2, int *bound_matches, int *bp)
{
    int i, available;
    
    if (!COMPLEX(t) || t->symbol != ac_sn) {
	for (i = 0; i < n2; i++) {
	    available = mults2[i] - match2[i];
	    if (available > 0 && term_ident(t, args2[i])) {
		match2[i]++;
		bound_matches[(*bp)++] = i;
		return(1);
		}
	    }
	return(0);
	}
    else {
	if (!macbv_rec(ac_sn,t->args[0],args2,mults2,match2,n2,
		       bound_matches,bp))
	    return(0);
	else
	    return(macbv_rec(ac_sn,t->args[1],args2,mults2,match2,n2,
			     bound_matches,bp));
	}
}  /* macbv_rec */

/*************
 *
 *    match_ac_bound_vars -- match (identically) a set of bound variables.
 *
 *    For each bound variable of args1, find an identical match in args2.
 *    If bound to an AC term t, (with same AC functor) find an identical
 *    match for each argument of t.  Record the positions of the matched
 *    terms in `bound_matches', so that they can be unmached on backtracking.
 *
 *************/

static int match_ac_bound_vars(int ac_sn, Term_ptr *args1, int n1,
      Term_ptr *args2, int *mults2, int *match2, int n2, int begin,
      int *bound_matches, int *bp, Context_ptr c1)
{
    int i, ok, vn;
    Term_ptr t;

    for (i=begin, ok=1, *bp=0; i < n1 && ok; i++) {
	vn = args1[i]->symbol;
	t = c1->terms[vn];
	if (t)
	    ok = macbv_rec(ac_sn,t,args2,mults2,match2,n2,bound_matches,bp);
	}
    if (!ok) {
	/* Subtract any matches that were made before failure. */
	for (i = 0; i < *bp; i++)
	    match2[bound_matches[i]] -= 1;
	*bp = 0;  /* Not really necessary, but helpful for debugging. */
	}
    return(ok);
}  /* match_ac_bound_vars */

/*************
 *
 *    set_up_free_vars
 *
 *    Build a list of the set of free variables in args1.  Each node
 *    contains the number of occurrences (coef) of the variable.
 *    Sort the list---nonincreasing coef.
 *
 *    Variables are partitioned into `free' and `bound' according to
 *    their state after all nonvariable terms have been matched.
 *    A variable is called `bound' iff it occurs in a nonvariable term.
 *   
 *    Since the partition does not change during backtracking, this
 *    routine needs to be called only once, after all nonvariable
 *    terms have been matched for the first time.
 *
 *************/

static void set_up_free_vars(Ac_match_pos_ptr ac, Context_ptr c1)
{
    Ac_match_free_vars_pos_ptr p1, p2;
    Term_ptr t;
    int i, temp;

    ac->free_first = NULL; ac->free_last = NULL;
    for (i = ac->last_a1_functor+1; i < ac->n1; i++) {
	t = ac->args1[i];
	if (c1->terms[t->symbol] == NULL) {
	    /* We have a free variable. */
	    for (p1=ac->free_first; p1 && p1->varnum!=t->symbol; p1=p1->next);
	    if (p1)
		(p1->coef)++;
	    else {
		p1 = get_ac_match_free_vars_pos();
		p1->varnum = t->symbol;
		p1->coef = 1;
		p1->next = NULL;
		p1->prev = ac->free_last;
		if (ac->free_last)
		    ac->free_last->next = p1;
		else
		    ac->free_first = p1;
		ac->free_last = p1;
		}
	    }
	}
    /* Now sort -- nonincreasing coefficients. */
    /* There won't be many, so use a quadratic sort. */
    p1 = ac->free_first;
    if (p1) {
	while (p1->next) {
	    for (p2 = p1->next; p2; p2 = p2->next) {
		if (p1->coef < p2->coef) {
		    temp = p2->coef;
		    p2->coef = p1->coef;
		    p1->coef = temp;
		    temp = p2->varnum;
		    p2->varnum = p1->varnum;
		    p1->varnum = temp;
		    }
		}
	    p1 = p1->next;
	    }
	}
}  /* set_up_free_vars */

/*************
 *
 *    unbind_free_var
 *
 *    This routine takes an `ac match free variable position' and
 *    unbinds the free variable.  If the variable is bound to a
 *    compound AC term that was created just for the binding,
 *    then the new parts of the term are deleted.
 *
 *************/

static void unbind_free_var(Ac_match_free_vars_pos_ptr pos, Context_ptr c)
{
    int i, j;
    Term_ptr t, t1;

    /* Free the temporary substitution term, if necessary. */

    /* First count how many nodes have to be deleted. */
    for (i = j = 0; i < pos->n; i++)
	if (pos->combo[i])
	    j++;
    
    t = c->terms[pos->varnum];
    for (i = 0; i < j-1; i++) {
	t1 = t->args[1];
	free_term(t);
	t = t1;
	}
    
    /* unbind variable */
    c->terms[pos->varnum] = NULL;
    
}  /* unbind_free_var */

/*************
 *
 *    free_var_match
 *
 *    Find the first or next match for a free variable.  If (match_all)
 *    then all remaining arguments of args2 must be matched.
 *    Otherwise, backtracking will produce matches in all combinations.
 *
 *************/

static int free_var_match(Ac_match_free_vars_pos_ptr pos, Term_ptr *args2,
  int *mults2, int *match2, int n2, Context_ptr c1, int symbol, int match_all)
              
{
    Term_ptr t;
    int i, j, k, n, ok, go, avail;

    t = c1->terms[pos->varnum];

    if (!t) {
	/* It is not a continuation, so set up everything. */

	/* Loop through args2, collecting targets, combinations of which */
        /* can be substituted for the current variable.                  */
        /* Example: current variable is 2x; terms available for          */
        /* matching are 4a, 3b, 2c, 1d; targets are a,a,b,c.             */
	n = 0;
	for (i = 0; i < n2; i++) {
	    avail = mults2[i] - match2[i];
	    if (match_all && (avail % pos->coef != 0))
		return(0); /* Fail, because there will be unmatched term(s) */
	    j = avail / pos->coef;  /* integer division */
	    for (k = 0; k < j; k++)
		pos->targets[n++] = i;
	    }

	pos->n = n;
	if (n == 0)
	    return(0);
	else {
	    for (i = 0; i < n; i++)
		pos->combo[i] = 1;
	    }
	}
    else {
	/* continutation */
	unbind_free_var(pos, c1);

	/* unmark args2 terms */
	for (i = 0; i < pos->n; i++)
	    if (pos->combo[i]) {
		match2[pos->targets[i]] -= pos->coef;
		}

	if (match_all) {
	    for (i = 0; i < pos->n; i++)
		pos->combo[i] = 0;
	    return(0);
	    }
	else {
	    go = 1;
	    while (go) {
		/* subtract 1 from  combo */
		for (i = (pos->n)-1; i >= 0 && pos->combo[i] == 0; i--)
		    pos->combo[i] = 1;
		if (i < 0)
		    return(0);
		else {
		    pos->combo[i] = 0;
		    /* Check redundancy condition. */
		    for (i = 0, ok = 1; i < (pos->n)-1 && ok; i++)
			if (pos->targets[i] == pos->targets[i+1] &&
			    pos->combo[i] < pos->combo[i+1])
			    ok = 0;
		    go = !ok;
		    }
		}

	    /* Now make sure that combo is not empty. */
	    for (i = 0, ok = 0; i < pos->n && !ok; i++)
		ok = pos->combo[i];
	    if (!ok)
		return(0);
	    }
	}

    /* All is well---we have a match for the current variable. */
    /* Build a temporary substitution term, if necessary. */
    /* Note order in which it is built---this makes it AC canonical. */

    t = NULL;
    for (i = pos->n-1; i >= 0; i--) 
	if (pos->combo[i]) {
	    if (!t)
		t = args2[pos->targets[i]];
	    else
		t = build_binary_term(symbol, args2[pos->targets[i]], t);
	    }

    /* Bind variable. */
    c1->terms[pos->varnum] = t;
    
    /* Mark args2 terms matched to the current variable. */
    for (i = 0; i < pos->n; i++)
	if (pos->combo[i])
	    match2[pos->targets[i]] += pos->coef;

    return(1);
	
}  /* free_var_match */

/*************
 *
 *    build_partial_term
 *
 *    When partial match has been found, this routine collects the
 *    unmatched arguments of args2 and builds and returns an AC term.
 *    The size of the new term is stored in the AC position so that
 *    it can easily be freed.
 *
 *************/

static Term_ptr build_partial_term(Ac_match_pos_ptr ac)
{
    int i, j, k, n;
    Term_ptr t;

    t = NULL; k = 0;
    for (i = 0; i < ac->n2; i++) {
	n = ac->mults2[i] - ac->match2[i];
	for (j = 0; j < n; j++) {
	    k++;
	    if (!t)
		t = ac->args2[i];
	    else
		t = build_binary_term(ac->t1->symbol, ac->args2[i], t);
	    }
	}
    ac->partial_term_size = k;
    return(t);
}  /* build_partial_term */

/*************
 *
 *    clear_partial_term
 *
 *    Remove the partial term from the substitution and free the
 *    appropriate parts fo the partial term.
 *
 *************/

static void clear_partial_term(Ac_match_pos_ptr ac)
{
    int i;
    Term_ptr t, t1;

    t = ac->c1->partial_term;
    ac->c1->partial_term = NULL;
    
    for (i = 0; i < ac->partial_term_size - 1; i++) {
	t1 = t->args[1];
	free_term(t);
	t = t1;
	}
    ac->partial_term_size = 0;
}  /* clear_partial_term */

#define GO_FUNCTORS   1
#define GO_BOUND_VARS 2
#define GO_FREE_VARS  3
#define SUCCESS       4
#define FAILURE       5

/*************
 *
 *    match_ac -- associative-commutative matching.
 *
 *    Get the first (bt->alternative == 0) or next AC matcher.
 *    I intend for this to be called from `match_bt_guts'.  
 *    It assumed that the root functors of the input terms are AC. 
 *
 *    Call match_ac_cancel(ac) if you quit before getting all matchers. 
 *
 *    t1 -- pattern term
 *    c1 -- context (substitution table) for t1
 *    t2 -- subject term
 *    bt -- backtrack position
 *
 *************/

static int match_ac(Term_ptr t1, Context_ptr c1, Term_ptr t2, Bt_node_ptr bt)
{
    int status, n1, n2, total2, i, ok, a1_pos, a2_pos;
    int free_var_forward;
    Term_ptr a1, a2;
    Ac_match_pos_ptr ac;
    Ac_match_free_vars_pos_ptr free_pos, p1, p2;
    Bt_node_ptr bt1;

    if (bt->alternative == 0) {  /* initialize, get first matcher */
	ac = get_ac_match_pos();
	bt->acm = ac;
	ac->t1 = t1; ac->t2 = t2; ac->c1 = c1;
	ac->free_first = NULL; ac->partial_term_size = 0;
	n1 = 0; n2 = 0; total2 = 0; 
        flatten(t1, ac->args1, &n1);
        flatten_mult(t2, ac->args2, ac->mults2, &n2, &total2,
		     (int (*)(void*,void*)) term_compare_ncv);
	if (n1 > total2)  /* fail if t1 has more arguments */
	    status = FAILURE;
	else {
	    /* Assume inputs are ac_canonical, so don't sort.       */
	    /* Don't bother to eliminate common arguments, because  */
	    /* It usually doesn't pay off.                          */
	    ac->n1 = n1; ac->n2 = n2;
	    for (i = 0; i < n1; i++)
		ac->match1[i] = -1;
	    for (i = 0; i < n2; i++)
		ac->match2[i] = 0;
	    for (i = 0; i < n1 && !VARIABLE(ac->args1[i]); i++);
	    ac->last_a1_functor = i-1;
	    a1_pos = 0; a2_pos = 0; bt1 = NULL;
	    status = GO_FUNCTORS;
	    }
	}
    else {  /* continuation, get next matcher */
	ac = bt->acm;
	if (bt->partial) {
	    printf("WARNING: partial match_ac on continuation.\n");
	    if (c1->partial_term)
		clear_partial_term(ac);
	    }
        n1 = ac->n1; n2 = ac->n2;
	if (n1 == 0 && n2 == 0)  /* vacuous success last time */
	    status = FAILURE;
	else {
	    free_pos = ac->free_last;
	    free_var_forward = 0;
	    status = GO_FREE_VARS;
	    }
	}
    
    while (status != SUCCESS && status != FAILURE) {
	while (status == GO_FUNCTORS) {
	    if (a1_pos > ac->last_a1_functor)
		status = GO_BOUND_VARS;
	    else if (a1_pos < 0)
		status = FAILURE;
	    else {
		if (bt1) {
		    /* remove arrow */
		    ac->match1[a1_pos] = -1;
		    ac->bt1[a1_pos] = NULL;
		    ac->match2[a2_pos]--;
		    /* Try for another match with this pair. */
		    bt1 = match_bt_next(bt1);
		    if (!bt1)
			a2_pos++;
		    }

		if (!bt1) {
		    /* Look for a match for a1, starting with a2. */
		    a1 = ac->args1[a1_pos];
		    while (!bt1 && a2_pos < ac->n2) {
			a2 = ac->args2[a2_pos];
			if (a1->symbol == a2->symbol &&
			    ac->match2[a2_pos] < ac->mults2[a2_pos])
			    bt1 = match_bt_first(a1, c1, a2, 0);
			if (!bt1)
			    a2_pos++;
			}
		    }

		if (bt1) {   /* We have a match: a1->a2. */
		    /* draw arrow */
		    ac->match1[a1_pos] = a2_pos;
		    ac->bt1[a1_pos] = bt1;
		    ac->match2[a2_pos]++;
		    a1_pos++; a2_pos = 0; bt1 = NULL;
		    }
		else {  /* back up */
		    a1_pos--;
		    a2_pos = ac->match1[a1_pos];
		    bt1 = ac->bt1[a1_pos];
		    }
		}
	    }  /* while GO_FUNCTORS */
	
	if (status == GO_BOUND_VARS) {
	    /* Try to macth (identically) bound variables. */
	    ok = match_ac_bound_vars(t1->symbol, ac->args1, n1,
				     ac->args2, ac->mults2, ac->match2, n2,
				     ac->last_a1_functor+1, ac->bound_matches,
				     &(ac->bound_count), c1);
	    if (ok) {
		free_pos = ac->free_first;
		free_var_forward = 1;
		status = GO_FREE_VARS;
		}
	    else {  /* backup */
		a1_pos = ac->last_a1_functor;
		if (a1_pos >= 0) {
		    a2_pos = ac->match1[a1_pos];
		    bt1 = ac->bt1[a1_pos];
		    }
		status = GO_FUNCTORS;
		}
	    }

	else if (status == GO_FREE_VARS) {
	    if (ac->free_first == NULL) {
		set_up_free_vars(ac, c1);
		free_pos = ac->free_first;
		}
	    while (free_pos) {
		if (free_var_match(free_pos, ac->args2, ac->mults2,
				ac->match2, ac->n2, c1, ac->t1->symbol,
				!bt->partial && free_pos->next == NULL)) {
		    free_pos = free_pos->next;
		    free_var_forward = 1;
		    }
		else {
		    free_pos = free_pos->prev;
		    free_var_forward = 0;
		    }
		}
	    if (free_var_forward) {
		/* Check for non-matched a2 terms. */
		for (i = 0, ok = 1; i < n2 && ok; i++)
		    ok = ac->mults2[i] == ac->match2[i];
		if (!ok) {
		    /*  Have at least 1 non-matched a2 term. */
		    if (bt->partial) {
			c1->partial_term = build_partial_term(ac);
			status = SUCCESS;
			}
		    else
			status = GO_FUNCTORS;  /* set up below */
		    }
		else
		    status = SUCCESS;
		}
	    else
		status = GO_FUNCTORS;

	    if (status == GO_FUNCTORS) {
		/* Unmark bound variable matches. */
		for (i = 0; i < ac->bound_count; i++)
		    ac->match2[ac->bound_matches[i]] -= 1;
		a1_pos = ac->last_a1_functor;
		if (a1_pos >= 0) {
		    a2_pos = ac->match1[a1_pos];
		    bt1 = ac->bt1[a1_pos];
		    }
		}
	    }  /* if GO_FREE_VARS */
	}  /* while !SUCCESS && !FAILURE */

    if (status == SUCCESS)
	bt->alternative = ASSOC_COMMUTE;
    else {
	/* free memory */
	p1 = ac->free_first;
	while (p1) {
	    p2 = p1;
	    p1 = p1->next;
	    free_ac_match_free_vars_pos(p2);
	    }
	free_ac_match_pos(ac);
	bt->alternative = 0;
	}
    return(status == SUCCESS);
}    /* match_ac */

/*************
 *
 *    match_ac_cancel
 *
 *    Free an AC match position.  This is to be used when you have obtained
 *    one or more AC matchers by calling match_ac, but you do not wish
 *    to backtrack to obtain additional AC matchers.  Do not call this
 *    routine if match_ac returned 0.
 *
 *************/

void match_ac_cancel(Ac_match_pos_ptr ac)
{
    Ac_match_free_vars_pos_ptr p1, p2;
    int i;
    
    for (i = 0; i <= ac->last_a1_functor; i++)
	match_bt_cancel(ac->bt1[i]);
    p1 = ac->free_first;
    while (p1) {
	unbind_free_var(p1, ac->c1);
	p2 = p1;
	p1 = p1->next;
	free_ac_match_free_vars_pos(p2);
	}
    if (ac->partial_term_size > 0)
	clear_partial_term(ac);
    free_ac_match_pos(ac);
}  /* match_ac_cancel */

/*************
 *
 *    p_acm -- print an ac match position.
 *
 *************/

void p_acm(Ac_match_pos_ptr ac)
{
    int i;
    Ac_match_free_vars_pos_ptr p;
    
    print_term(stdout, ac->t1); printf(" "); p_term(ac->t2);
    for (i = 0; i < ac->n1; i++) {
        print_term(stdout, ac->args1[i]);
	printf(" %d ",ac->match1[i]);
	}
    printf("\n");
    for (i = 0; i < ac->n2; i++) {
        print_term(stdout, ac->args2[i]);
	printf(" <%d,%d> ",ac->mults2[i],ac->match2[i]);
	}
    printf("\n");

    printf("last_a1_functor=%d.\n",ac->last_a1_functor);
    printf("free vars list <symbol,coef>:\n");
    for (p = ac->free_first; p; p = p->next) {
	printf("<%d,%d>, ", p->varnum, p->coef);
	for (i = 0; i < p->n; i++) {
	    print_term(stdout,ac->args2[p->targets[i]]);
	    printf(":%d ",p->combo[i]);
	    }
	printf("\n");
	}
    printf("\n");
	
}  /* p_acm */

/*************
 *
 *    Bt_node_ptr match_bt_backup(bt)
 *
 *    Back up (freeing nodes) to the most recent node with an alternative.
 *
 *************/

static Bt_node_ptr match_bt_backup(Bt_node_ptr bt1)
{
    Bt_node_ptr bt2, bt3;

    while (bt1 && !bt1->alternative) {

	if (bt1->cb) {  /* unbind variable */
	    bt1->cb->terms[bt1->varnum] = NULL;
	    bt1->cb->contexts[bt1->varnum] = NULL;
	    }
	
	if (bt1->prev) {
	    bt1 = bt1->prev;
	    while (bt1->last_child)
		bt1 = bt1->last_child;
	    }
	else {
	    bt2 = bt1;
	    bt1 = bt1->parent;

	    while (bt2) {
		bt3 = bt2;
		bt2 = bt2->next;
		free_bt_node(bt3);
		}

	    if (bt1)
		bt1->first_child = bt1->last_child = NULL;
	    }
	}
    
    return(bt1);
	
}  /* match_bt_backup */

/*************
 *
 *    match_bt_guts
 *
 *    Main loop for backtracking matching.
 *
 *************/

static Bt_node_ptr match_bt_guts(Bt_node_ptr bt1)
{
    Term_ptr t1, t2;
    Context_ptr c1;
    int vn1, status;
    Bt_node_ptr bt2, bt3;

    status = GO;

    while (status == GO) {

    	t1 = bt1->t1;
	t2 = bt1->t2;
	c1 = bt1->c1;

	if (bt1->alternative == COMMUTE) {
	    if (match_commute(t1, c1, t2, bt1))
		    status = POP;
		else
		    status = BACKTRACK;
	    }
	else if (bt1->alternative == ASSOC_COMMUTE) {
	    if (match_ac(t1, c1, t2, bt1))
		    status = POP;
		else
		    status = BACKTRACK;
	    }
	else if (VARIABLE(t1)) {
	    vn1 = t1->symbol;
	    if (c1->terms[vn1]) {
		if (term_ident(c1->terms[vn1], t2))
		    status = POP;
		else
		    status = BACKTRACK;
		}
	    else {
		BIND_BT(vn1, c1, t2, NULL, bt1)
		status = POP;
		}
	    }
	
	else if (VARIABLE(t2))
	    status = BACKTRACK;

	else if (t1->symbol != t2->symbol)
	    status = BACKTRACK;
	
	else if (CONSTANT(t1))
	    status = POP;
	
	else {  /* both COMPLEX with same functor (and same arity) */
	    int arity = t1->arity;

	    if (arity == 2 && is_commutative(t1->symbol)) {
		if (match_commute(t1, c1, t2, bt1))
		    status = POP;
		else
		    status = BACKTRACK;
		}
	    else if (arity == 2 && is_assoc_comm(t1->symbol)) {
		if (match_ac(t1, c1, t2, bt1))
		    status = POP;
		else
		    status = BACKTRACK;
		}
	    else {
		int i;
		/* Set up children corresponding to args of <t1,t2>. */
		/* Order not important for correctness. */
		/* AC kids last for efficiency, but keep in order otherwise. */
		bt3 = NULL;
		for (i = 0; i < arity; i++) {

		    bt2 = get_bt_node();
		    bt2->t1 = t1->args[i];
		    bt2->t2 = t2->args[i];
		    bt2->c1 = c1;
		    bt2->c2 = NULL;
		    bt2->parent = bt1;

		    if (is_assoc_comm(bt2->t1->symbol)) {
			/* insert at end */
			bt2->prev = bt1->last_child;
			if (bt1->last_child)
			    bt1->last_child->next = bt2;
			else
			    bt1->first_child = bt2;
			bt1->last_child = bt2;
			}
		    else {
			if (bt3) {
			    /* insert after bt3 */
			    bt2->next = bt3->next;
			    bt2->prev = bt3;
			    bt3->next = bt2;
			    if (bt2->next)
				bt2->next->prev = bt2;
			    else
				bt1->last_child = bt2;
			    }
			else {
			    /* insert at beginning */
			    bt2->next = bt1->first_child;
			    if (bt2->next)
				bt2->next->prev = bt2;
			    else
				bt1->last_child = bt2;
			    bt1->first_child = bt2;
			    }
			bt3 = bt2;
			}
		    }

		bt1 = bt1->first_child;
		status = GO;
		}
	    }
	
	if (status == POP) {
	    while (!bt1->next && bt1->parent)
		bt1 = bt1->parent;
	    if (!bt1->next)
		status = SUCCESS;
	    else {
		bt1 = bt1->next;
		status = GO;
		}
	    }
	else if (status == BACKTRACK) {
	    bt1 = match_bt_backup(bt1);
	    if (bt1)
		status = GO;
	    else
		status = FAILURE;
	    }
	}
    return(bt1);
}  /* match_bt_guts */

/*************
 *
 *    match_commute
 *
 *    Commutative matching.  t1 and t2 have the same commutative functor.
 *    There is nothing fancy here.
 *
 *************/

 int match_commute(Term_ptr t1, Context_ptr c1, Term_ptr t2, Bt_node_ptr bt)
			 
{
    Bt_node_ptr bt1, bt2;

    if (bt->alternative == 0) {  /* first call */
	bt->alternative = COMMUTE;
	bt->flipped = 0;

	/* Set up 2 subproblems, then match guts. */

	bt1 = get_bt_node();  bt2 = get_bt_node();
	bt1->next = bt2; bt2->prev = bt1;
	bt1->c1 = c1; bt1->c2 = NULL;
	bt2->c1 = c1; bt2->c2 = NULL;
	bt1->t1 = t1->args[0]; bt1->t2 = t2->args[0];
	bt2->t1 = t1->args[1]; bt2->t2 = t2->args[1];

	bt->position_bt = match_bt_guts(bt1);
	}
    else  /* continuation */
	bt->position_bt = match_bt_next(bt->position_bt);

    if (!bt->position_bt && !bt->flipped) {

	/* Set up 2 subproblems, with t2 flipped, then match guts. */

	bt1 = get_bt_node();  bt2 = get_bt_node();
	bt1->next = bt2; bt2->prev = bt1;
	bt1->c1 = c1; bt1->c2 = NULL;
	bt2->c1 = c1; bt2->c2 = NULL;
	bt1->t1=t1->args[0]; bt1->t2=t2->args[1];
	bt2->t1=t1->args[1]; bt2->t2=t2->args[0];

	bt->flipped = 1;
	bt->position_bt = match_bt_guts(bt1);
	}

    if (bt->position_bt)
	return(1);
    else {
	bt->alternative = 0;
	return(0);
	}
    
}  /* match_commute */

/*************
 *
 *    match_bt_first
 *
 *    This is backtracking matching, to be used when there
 *    can be more than one unifier.  This version handles (any number of)
 *    commutative and associative-commutative function symbols.
 *
 *    The flag `partial' says that if the top level is AC, then
 *    not all arguments of t2 have to be matched.  The non-matched
 *    args are put in c1->partial_term.  Partial matches are allowed
 *    for the top level only.  This is useful for AC rewriting.
 *
 *    If any AC terms are in t1 or t2, then both t1 and t2 should be
 *    in `ac_canonical' form before the call, that is, AC terms are
 *    right associated and sorted.  Also, if any C terms are in t1 or t2,
 *    they must be c_canonical.
 *
 *    Get first unifier.  Return position for match_bt_next calls.
 *    Here is an example of its use:
 *
 *        c1 = get_context();
 *        bt = match_bt_first(t1, c1, t2, 0);
 *        while (bt) {
 *            t3 = apply(t1, c1);
 *            zap_term(t3);
 *            bt = match_bt_next(bt);
 *            }
 *        free_context(c1);
 *
 *    If you quit before NULL is returned, call match_bt_cancel(bt)
 *    to clear substitutions and free memory.
 *
 *************/

Bt_node_ptr match_bt_first(Term_ptr t1, Context_ptr c1, Term_ptr t2,
			   int partial)
{
    Bt_node_ptr bt;

    bt = get_bt_node();
    bt->t1 = t1; bt->t2 = t2; bt->c1 = c1; bt->c2 = NULL;
    bt->partial = partial;
    return(match_bt_guts(bt));

}  /* match_bt */

/*************
 *
 *    match_bt_next -- see match_bt_first
 *
 *    Get next unifier.  Return position for subsequent calls.
 *
 *************/

Bt_node_ptr match_bt_next(Bt_node_ptr bt1)
{

    /* Go to last node in tree, then back up to a node with an alternative. */

    while (bt1->next)
	bt1 = bt1->next;
    while (bt1->last_child)
	bt1 = bt1->last_child;

    bt1 = match_bt_backup(bt1);

    if (bt1)
	return(match_bt_guts(bt1));
    else
	return(NULL);
}  /* match_bt_next */

/*************
 *
 *    match_bt_cancel
 *
 *    This routine should be called if the rest of a sequence of
 *    unifiers is not called for.  It clears substitutions and
 *    frees memory.
 *
 *************/

void match_bt_cancel(Bt_node_ptr bt)
{
    Bt_node_ptr bt1, bt2;

    for (bt1 = bt; bt1; ) {

	match_bt_cancel(bt1->first_child);
	
	if (bt1->alternative == COMMUTE)
	    /* match_bt_guts leaves us at the second child. */
	    match_bt_cancel(bt1->position_bt->prev);
	else if (bt1->alternative == ASSOC_COMMUTE) {
	    match_ac_cancel(bt1->acm);
	    }
	else if (bt1->cb) {
	    bt1->cb->terms[bt1->varnum] = NULL;
	    bt1->cb->contexts[bt1->varnum] = NULL;
	    }
	bt2 = bt1;
	bt1 = bt1->next;
	free_bt_node(bt2);
	}
}  /* match_bt_cancel */

