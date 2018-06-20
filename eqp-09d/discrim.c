#include "Header.h"
#include "List.h"
#include "Symbols.h"
#include "Unify.h"
#include "Discrim.h"

/*

This file contains code for two different types of discrimination tree
indexing.  Both types are for finding generalizations of a query term,
as when finding rewrite rules or subsuming literals.

The first type binds variables during the indexing and cannot handle
(in any way) commutative or AC function symbols.

The second type (whose routines usually have "wild" in the name) does
not bind variables (variables are wild cards) and handles AC function
symbols in two different ways, depending on the flag INDEX_AC_ARGS.
If the flag is clear, AC function symbols are simply treated as
constants.  If the flag is set, two levels of indexing occur on
arguments of AC symbols.  

The justification for the AC indexing is as follows.  Let t1 and t2 be
terms with the same AC symbol at their roots.  If t2 is an instance of t1, then
number_of_args(t1) <= number_of_args(t2), and
number_of_nonvariable_args(t1) <= number_of_non_variable_args(t2).
These two conditions correspond to the two levels of indexing for AC symbols.

*/

#define GO        1
#define BACKTRACK 2
#define SUCCESS   3
#define FAILURE   4

#define DVAR(d)  (d->symbol >= 0)
 
/*
 * memory management
 */

static Discrim_ptr discrim_avail;
static Flat_ptr flat_avail;
static Discrim_pos_ptr discrim_pos_avail;
static long discrim_gets, discrim_frees, discrim_avails;
static long flat_gets, flat_frees, flat_avails;
static long discrim_pos_gets, discrim_pos_frees, discrim_pos_avails;

/*************
 *
 *    Discrim_ptr get_discrim()
 *
 *************/

static Discrim_ptr get_discrim(void)
{
    Discrim_ptr p;

    discrim_gets++;
    if (!discrim_avail)
        p = tp_alloc(sizeof(struct discrim));
    else {
        discrim_avails--;
        p = discrim_avail;
        discrim_avail = discrim_avail->next;
        }

    p->next = NULL;
    p->u.kids = NULL;
    p->symbol = 0;
    p->ac_type = 0;

    return(p);
}  /* get_discrim */

/*************
 *
 *    free_discrim()
 *
 *************/

static void free_discrim(Discrim_ptr p)
{
    discrim_frees++;
    discrim_avails++;
    p->next = discrim_avail;
    discrim_avail = p;
}  /* free_discrim */

/*************
 *
 *    Flat_ptr get_flat()
 *
 *************/

static Flat_ptr get_flat(void)
{
    Flat_ptr p;

    flat_gets++;
    if (!flat_avail)
        p = tp_alloc(sizeof(struct flat));
    else {
        flat_avails--;
        p = flat_avail;
        flat_avail = flat_avail->next;
        }

#if 0  /* assume these will be set by the caller? */
    p->t = NULL;
    p->prev = NULL;
    p->last = NULL;
#endif

    p->next = NULL;
    p->alternatives = NULL;
    p->bound = 0;
    p->place_holder = 0;

    return(p);
}  /* get_flat */

/*************
 *
 *    free_flat()
 *
 *************/

static void free_flat(Flat_ptr p)
{
    flat_frees++;
    flat_avails++;
    p->next = flat_avail;
    flat_avail = p;
}  /* free_flat */

/*************
 *
 *    Discrim_pos_ptr get_discrim_pos()
 *
 *************/

static Discrim_pos_ptr get_discrim_pos(void)
{
    Discrim_pos_ptr p;

    discrim_pos_gets++;
    if (!discrim_pos_avail)
        p = tp_alloc(sizeof(struct discrim_pos));
    else {
        discrim_pos_avails--;
        p = discrim_pos_avail;
        discrim_pos_avail = (Discrim_pos_ptr) discrim_pos_avail->data;
        }

    p->subst = NULL;
    p->data = NULL;
    p->f = NULL;

    return(p);
}  /* get_discrim_pos */

/*************
 *
 *    free_discrim_pos()
 *
 *************/

static void free_discrim_pos(Discrim_pos_ptr p)
{
    discrim_pos_frees++;
    discrim_pos_avails++;
    p->data = (Gen_ptr_ptr ) discrim_pos_avail;
    discrim_pos_avail = p;
}  /* free_discrim_pos */

/*************
 *
 *   print_discrim_mem()
 *
 *************/

void print_discrim_mem(FILE *fp, int heading)
{
    if (heading)
	fprintf(fp, "  type (bytes each)        gets      frees     in use      avail      bytes\n");

    fprintf(fp, "discrim (%4d)      %11ld%11ld%11ld%11ld%9.1f K\n", sizeof(struct discrim), discrim_gets, discrim_frees, discrim_gets - discrim_frees, discrim_avails, (((discrim_gets - discrim_frees) + discrim_avails) * sizeof(struct discrim)) / 1024.);
    fprintf(fp, "flat (%4d)         %11ld%11ld%11ld%11ld%9.1f K\n", sizeof(struct flat), flat_gets, flat_frees, flat_gets - flat_frees, flat_avails, (((flat_gets - flat_frees) + flat_avails) * sizeof(struct flat)) / 1024.);
    fprintf(fp, "discrim_pos (%4d)  %11ld%11ld%11ld%11ld%9.1f K\n", sizeof(struct discrim_pos), discrim_pos_gets, discrim_pos_frees, discrim_pos_gets - discrim_pos_frees, discrim_pos_avails, (((discrim_pos_gets - discrim_pos_frees) + discrim_pos_avails) * sizeof(struct discrim_pos)) / 1024.);

}  /* print_discrim_mem */

/*************
 *
 *   p_discrim_mem()
 *
 *************/

void p_discrim_mem()
{
    print_discrim_mem(stdout, 1);
}  /* p_discrim_mem */

/*
 *  end of memory management
 */

/*************
 *
 *   discrim_init()
 *
 *************/

Discrim_ptr discrim_init()
{
    return(get_discrim());
}  /* discrim_init */

/*************
 *
 *   discrim_dealloc(d)
 *
 *************/

void discrim_dealloc(Discrim_ptr d)
{
    if (d->u.kids)
	abend("in discrim_dealloc, nonempty index.");
    else
	free_discrim(d);
}  /* discrim_dealloc */

/*************
 *
 *     print_discrim_tree(fp, d, n, depth)
 *
 *************/

static void print_discrim_tree(FILE *fp, Discrim_ptr d, int n, int depth)
{
    int arity, i;

    for (i = 0; i < depth; i++)
	printf("  ");

    if (depth == 0)
	fprintf(fp, "\nroot");
    else if (DVAR(d))
	fprintf(fp, "v%d", d->symbol);
    else
	fprintf(fp, "%s", sn_to_str(d->symbol));

    if (n == 0) {
	Gen_ptr_ptr p;
	for (i = 0, p = d->u.data; p; i++, p = p->next);
	fprintf(fp, ": leaf node with %d objects.\n", i);
	}
    else {
	Discrim_ptr d1;
	fprintf(fp, "\n");
	for (d1 = d->u.kids; d1; d1 = d1->next) {
	    if (d1->symbol >= 0)
		arity = 0;
	    else
		arity = sn_to_arity(d1->symbol);
	    print_discrim_tree(fp, d1, n+arity-1, depth+1);
	    }
	}
}  /* print_discrim_tree */

/*************
 *
 *   print_discrim_index()
 *
 *************/

void print_discrim_index(FILE *fp, Discrim_ptr d)
{
    print_discrim_tree(fp, d, 1, 0);
}  /* print_discrim_index */

/*************
 *
 *   p_discrim_index()
 *
 *************/

void p_discrim_index(Discrim_ptr d)
{
    print_discrim_index(stdout, d);
}  /* p_discrim_index */

/*************
 *
 *   Discrim_ptr discrim_insert_rec(t, d)
 *
 *   Return node of d corresp. to end of term t.  If it does
 *   not exist, add nodes to t so that it does exist.
 *
 *************/

static Discrim_ptr discrim_insert_rec(Term_ptr t, Discrim_ptr d)
{
    Discrim_ptr d1, d2, prev;
    int symbol, i;

    if (VARIABLE(t)) {
	d1 = d->u.kids;
	prev = NULL;
	symbol = t->symbol;
	while (d1 && DVAR(d1) && d1->symbol < symbol) {
	    prev = d1;
	    d1 = d1->next;
	    }
	if (!d1 || !DVAR(d1) || d1->symbol != symbol) {
	    d2 = get_discrim();
	    d2->symbol = t->symbol;
	    d2->next = d1;
	    if (!prev)
		d->u.kids = d2;
	    else
		prev->next = d2;
	    return(d2);
	    }
	else  /* found node */
	    return(d1);
	}

    else {  /* constant || complex */
	d1 = d->u.kids;
	prev = NULL;
	/* arities fixed: handle both NAME and COMPLEX */
	symbol = t->symbol;
	while (d1 && DVAR(d1)) {  /* skip variables */
	    prev = d1;
	    d1 = d1->next;
	    }
	while (d1 && d1->symbol < symbol) {
	    prev = d1;
	    d1 = d1->next;
	    }
	if (!d1 || d1->symbol != symbol) {
	    d2 = get_discrim();
	    d2->symbol = symbol;
	    d2->next = d1;
	    d1 = d2;
	    }
	else
	    d2 = NULL;  /* new node not required at this level */

	for (i = 0; i < t->arity; i++)
	    d1 = discrim_insert_rec(t->args[i], d1);

        if (d2)  /* link in new subtree (possibly a leaf) */
	    if (!prev)
		d->u.kids = d2;
	    else
		prev->next = d2;
	    
	return(d1);  /* d1 is leaf corresp. to end of input term */
	}
}  /* discrim_insert_rec */

/*************
 *
 *    discrim_insert(t, root, object)
 *
 *    Insert into a discrimination tree index for finding more general
 *    terms.
 *      t:      key (a term)
 *      root:   the discrimination tree
 *      object: the object to be inserted
 *    If object is not a pointer to a term, cast it into one.
 *
 *************/

void discrim_insert(Term_ptr t, Discrim_ptr root, void *object)
{
    Discrim_ptr d;
    Gen_ptr_ptr gp1, gp2;

    d = discrim_insert_rec(t, root);
    gp1 = get_gen_ptr();
    gp1->u.v = object;

    /* Install at end of list. */
    if (d->u.data == NULL)
	d->u.data = gp1;
    else {
	for (gp2 = d->u.data; gp2->next; gp2 = gp2->next);
	gp2->next = gp1;
	}

}  /* discrim_insert */

/*************
 *
 *    Discrim_ptr discrim_end(t, d, path_p)
 *
 *    Given a discrimination tree (or a subtree) and a term, return the 
 *    node in the tree that corresponds to the last symbol in t (or NULL
 *    if the node doesn't exist).  *path_p is a list that is extended by
 *    this routine.  It is a list of pointers to the
 *    nodes in path from the parent of the returned node up to imd. 
 *    (It is useful for deletions, because nodes do not have pointers to
 *    parents.) 
 *
 *************/

static Discrim_ptr discrim_end(Term_ptr t, Discrim_ptr d, Gen_ptr_ptr *path_p)
{
    Discrim_ptr d1;
    Gen_ptr_ptr dp;
    int symbol, sym;

    /* add current node to the front of the path list. */

    dp = get_gen_ptr();
    dp->u.v = (void *) d;
    dp->next = *path_p;
    *path_p = dp;

    if (VARIABLE(t)) {
	d1 = d->u.kids;
	symbol = t->symbol;
	while (d1 && DVAR(d1) && d1->symbol < symbol) 
	    d1 = d1->next;

	if (!d1 || !DVAR(d1) || d1->symbol != symbol)
	    return(NULL);
	else   /* found node */
	    return(d1);
	}

    else {  /* constant || complex */
	d1 = d->u.kids;
	sym = t->symbol;  /* arities fixed: handle both NAME and COMPLEX */
	while (d1 && DVAR(d1))  /* skip variables */
	    d1 = d1->next;
	while (d1 && d1->symbol < sym)
	    d1 = d1->next;

	if (!d1 || d1->symbol != sym)
	    return(NULL);
	else {
	    int i;
	    for (i = 0; d1 && i < t->arity; i++)
		d1 = discrim_end(t->args[i], d1, path_p);
	    return(d1);
	    }
	}
}  /* discrim_end */

/*************
 *
 *    discrim_delete(t, root, object)
 *
 *************/

void discrim_delete(Term_ptr t, Discrim_ptr root, void *object)
{
    Discrim_ptr end, d2, d3, parent;
    Gen_ptr_ptr tp1, tp2;
    Gen_ptr_ptr dp1, path;

    /* First find the correct leaf.  path is used to help with  */
    /* freeing nodes, because nodes don't have parent pointers. */

    path = NULL;
    end = discrim_end(t, root, &path);
    if (!end)
	abend("in discrim_delete, can't find end.");

    /* Free the pointer in the leaf-list */

    tp1 = end->u.data;
    tp2 = NULL;
    while(tp1 && tp1->u.v != object) {
	tp2 = tp1;
	tp1 = tp1->next;
	}
    if (!tp1)
	abend("in discrim_delete, can't find term.");

    if (!tp2)
	end->u.data = tp1->next;
    else
	tp2->next = tp1->next;
    free_gen_ptr(tp1);

    if (end->u.data == NULL) {
        /* free tree nodes from bottom up, using path to get parents */
	end->u.kids = NULL;  /* probably not necessary */
	dp1 = path;
	while (end->u.kids == NULL && end != root) {
	    parent = (Discrim_ptr) dp1->u.v;
	    dp1 = dp1->next;
	    d2 = parent->u.kids;
	    d3 = NULL;
	    while (d2 != end) {
		d3 = d2;
		d2 = d2->next;
		}
	    if (!d3)
		parent->u.kids = d2->next;
	    else
		d3->next = d2->next;
	    free_discrim(d2);
	    end = parent;
	    }
	}

    /* free path list */

    while (path) {
	dp1 = path;
	path = path->next;
	free_gen_ptr(dp1);
	}

}  /* discrim_delete */

/*************
 *
 *    discrim_retrieve_leaf(t_in, root, subst, ppos)
 *
 *************/

Gen_ptr_ptr discrim_retrieve_leaf(Term_ptr t_in, Discrim_ptr root,
				      Context_ptr subst, Flat_ptr *ppos)
{
    Flat_ptr f, f1, f2, f_save;
    Term_ptr t;
    Discrim_ptr d;
    int symbol, match, bound, status;

    f = *ppos;  /* Don't forget to reset before return. */
    t = t_in;
    f_save = NULL;

    if (t) {  /* if first call */
        d = root->u.kids;
        if (d) {
	    f = get_flat();
	    f->t = t;
	    f->last = f;
	    f->prev = NULL;
	    f->place_holder = (COMPLEX(t));
            status = GO;
	    }
        else
            status = FAILURE;
        }
    else
        status = BACKTRACK;

    while (status == GO || status == BACKTRACK) {
	if (status == BACKTRACK) {
	    while (f && !f->alternatives) {  /* clean up HERE??? */
		if (f->bound) {
                    subst->terms[f->varnum] = NULL;
		    f->bound = 0;
		    }
		f_save = f;
		f = f->prev;
		}
	    if (f) {
		if (f->bound) {
                    subst->terms[f->varnum] = NULL;
		    f->bound = 0;
		    }
		d = f->alternatives;
		f->alternatives = NULL;
		status = GO;
		}
	    else
		status = FAILURE;
	    }

	if (status == GO) {
	    match = 0;
	    while (!match && d && DVAR(d)) {
		symbol = d->symbol;
		if (subst->terms[symbol]) { /* if already bound */
		    match = term_ident(subst->terms[symbol], f->t);
		    bound = 0;
		    }
		else { /* bind variable in discrim tree */
		    match = 1;
		    subst->terms[symbol] = f->t;
		    bound = 1;
		    }
		if (!match)
		    d = d->next;
		}
	    if (match) {
		/* push alternatives */
		f->alternatives = d->next;
		f->bound = bound;
		f->varnum = symbol;
		f = f->last;
		}
	    else if (VARIABLE(f->t))
		status = BACKTRACK;
	    else {
		symbol = f->t->symbol;
		while (d && d->symbol < symbol)
		    d = d->next;
		if (!d || d->symbol != symbol)
		    status = BACKTRACK;
		else if (f->place_holder) {
		    int i;
		    /* insert skeleton in place_holder */
		    f1 = get_flat();
		    f1->t = f->t;
		    f1->prev = f->prev;
		    f1->last = f;
		    f_save = f1;
		    if (f1->prev)
			f1->prev->next = f1;
		    
		    t = f->t;
		    for (i = 0; i < t->arity; i++) {
			if (i < t->arity-1)
			    f2 = get_flat();
			else
			    f2 = f;
			f2->place_holder = COMPLEX(t->args[i]);
			f2->t = t->args[i];
			f2->last = f2;
			f2->prev = f1;
			f1->next = f2;
			f1 = f2;
			}
		    f = f_save;
		    }
		}
	    if (status == GO) {
		if (f->next) {
		    f = f->next;
		    d = d->u.kids;
		    }
		else
		    status = SUCCESS;
		}
	    }
	}
    if (status == SUCCESS) {
	*ppos = f;
	return(d->u.data);
	}
    else {
	/* Free flats. */
	while (f_save) {
	    f1 = f_save;
	    f_save = f_save->next;
	    free_flat(f1);
	    }
	return(NULL);
	}
	   
}  /* discrim_retrieve_leaf */

/*************
 *
 *    discrim_retrieve_first(t, root, subst, ppos)
 *
 *    Get the first object associated with a term more general than t.
 *
 *    Remember to call discrim_cancel(*ppos) if you don't want the
 *    whole sequence.
 *
 *************/

void *discrim_retrieve_first(Term_ptr t, Discrim_ptr root, Context_ptr subst, Discrim_pos_ptr *ppos)
{
    Gen_ptr_ptr tp;
    Flat_ptr f;
    Discrim_pos_ptr gp;

    tp = discrim_retrieve_leaf(t, root, subst, &f);
    if (!tp)
	return(NULL);
    else {
	gp = get_discrim_pos();
	gp->subst = subst;
	gp->f = f;
	gp->data = tp;
	*ppos = gp;
	return(tp->u.v);
	}
}  /* discrim_retrieve_first */

/*************
 *
 *    discrim_retrieve_next(ppos)
 *
 *    Get the next object associated with a term more general than t.
 *
 *    Remember to call discrim_cancel(*ppos) if you don't want the
 *    whole sequence.
 *
 *************/

void *discrim_retrieve_next(Discrim_pos_ptr pos)
{
    Gen_ptr_ptr tp;
    
    tp = pos->data->next;
    if (tp) {  /* if any more terms in current leaf */
	pos->data = tp;
	return(tp->u.v);
	}
    else {  /* try for another leaf */
	tp = discrim_retrieve_leaf((Term_ptr) NULL, (Discrim_ptr) NULL,
				   pos->subst, &(pos->f));
	if (tp) {
	    pos->data = tp;
	    return(tp->u.v);
	    }
	else {
	    free_discrim_pos(pos);
	    return(NULL);
	    }
	}
}  /* discrim_retrieve_next */

/*************
 *
 *    discrim_cancel(pos)
 *
 *************/

void discrim_cancel(Discrim_pos_ptr pos)
{
    Flat_ptr f1, f2;

    f1 = pos->f;
    while (f1) {
	if (f1->bound)
	    pos->subst->terms[f1->varnum] = NULL;
	f2 = f1;
	f1 = f1->prev;
	free_flat(f2);
	}
    free_discrim_pos(pos);
}  /* discrim_cancel */

/********************************* WILD ******************************/

/*************
 *
 *     num_ac_args(t, symbol)
 *
 *************/

static int num_ac_args(struct term *t, int symbol)
{
    if (!COMPLEX(t) || t->symbol != symbol)
        return(1);
    else
        return(num_ac_args(t->args[0], symbol) +
               num_ac_args(t->args[1], symbol));
}  /* num_ac_args */

/*************
 *
 *     num_ac_nv_args(t, symbol)
 *
 *************/

static int num_ac_nv_args(struct term *t, int symbol)
{
    if (!COMPLEX(t) || t->symbol != symbol)
        return(VARIABLE(t) ? 0 : 1);
    else
        return(num_ac_nv_args(t->args[0], symbol) +
               num_ac_nv_args(t->args[1], symbol));
}  /* num_ac_nv_args */

/*************
 *
 *     print_discrim_wild_tree()
 *
 *************/

static void print_discrim_wild_tree(FILE *fp, Discrim_ptr d, int n, int depth)
{
    int arity, i;

    for (i = 0; i < depth; i++)
	fprintf(fp, "  ");

    if (depth == 0)
	fprintf(fp, "\nroot");
    else if (d->ac_type == AC_ARG_TYPE)
	fprintf(fp, "AC %d args", d->symbol);
    else if (d->ac_type == AC_NV_ARG_TYPE)
	fprintf(fp, "AC %d NV args", d->symbol);
    else if (DVAR(d))
	fprintf(fp, "*");
    else
	fprintf(fp, "%s", sn_to_str(d->symbol));

    if (n == 0) {
	Gen_ptr_ptr p;
	for (i = 0, p = d->u.data; p; i++, p = p->next);
	fprintf(fp, ": leaf node with %d objects.\n", i);
	}
    else {
	Discrim_ptr d1;
	fprintf(fp, "\n");
	for (d1 = d->u.kids; d1; d1 = d1->next) {
	    
	    if (d1->ac_type == AC_ARG_TYPE || d1->ac_type == AC_NV_ARG_TYPE)
		arity = 0;
	    else if (DVAR(d1))
		arity = 0;
	    else if (!Flags[INDEX_AC_ARGS].val && is_assoc_comm(d1->symbol))
	        arity = 0;
	    else
		arity = sn_to_arity(d1->symbol);
	    print_discrim_wild_tree(fp, d1, n+arity-1, depth+1);
	    }
	}
}  /* print_discrim_wild_tree */

/*************
 *
 *   print_discrim_wild_index()
 *
 *************/

void print_discrim_wild_index(FILE *fp, Discrim_ptr d)
{
    print_discrim_wild_tree(fp, d, 1, 0);
}  /* print_discrim_wild_index */

/*************
 *
 *   p_discrim_wild_index()
 *
 *************/

void p_discrim_wild_index(Discrim_ptr d)
{
    print_discrim_wild_index(stdout, d);
}  /* p_discrim_wild_index */

/*************
 *
 *     discrim_wild_insert_ac(t, d)
 *
 *************/

Discrim_ptr discrim_wild_insert_ac(Term_ptr t, Discrim_ptr d)
{
    int num_args, num_nv_args;
    Discrim_ptr d1, d2, dnew, prev;

    num_args = num_ac_args(t, t->symbol);

    for (d1 = d->u.kids, prev = NULL;
	 d1 && d1->symbol < num_args;
	 prev = d1, d1 = d1->next);
    if (!d1 || d1->symbol != num_args) {
	dnew = get_discrim();
	dnew->ac_type = AC_ARG_TYPE;
	dnew->symbol = num_args;
	dnew->next = d1;
	if (prev)
	    prev->next = dnew;
	else
	    d->u.kids = dnew;
	d1 = dnew;
	}

    num_nv_args = num_ac_nv_args(t, t->symbol);

    for (d2 = d1->u.kids, prev = NULL;
	 d2 && d2->symbol < num_nv_args;
	 prev = d2, d2 = d2->next);
    if (!d2 || d2->symbol != num_nv_args) {
	dnew = get_discrim();
	dnew->ac_type = AC_NV_ARG_TYPE;
	dnew->symbol = num_nv_args;
	dnew->next = d2;
	if (prev)
	    prev->next = dnew;
	else
	    d1->u.kids = dnew;
	d2 = dnew;
	}
    return(d2);
    
}  /* discrim_wild_insert_ac */

/*************
 *
 *    Discrim_ptr discrim_wild_insert_rec(t, d)
 *
 *************/

static Discrim_ptr discrim_wild_insert_rec(Term_ptr t, Discrim_ptr d)
{
    Discrim_ptr d1, prev, d2;
    int sym;

    if (VARIABLE(t)) {
	d1 = d->u.kids;
	if (!d1 || !DVAR(d1)) {
	    d2 = get_discrim();
	    d2->symbol = 0;    /* HERE */
	    d2->next = d1;
	    d->u.kids = d2;
	    return(d2);
	    }
	else  /* found node */
	    return(d1);
	}

    else {  /* constant || complex */
	d1 = d->u.kids;
	prev = NULL;
	/* arities fixed: handle both NAME and COMPLEX */
	sym = t->symbol;
	if (d1 && DVAR(d1)) {  /* skip variable */
	    prev = d1;
	    d1 = d1->next;
	    }
	while (d1 && d1->symbol < sym) {
	    prev = d1;
	    d1 = d1->next;
	    }
	if (!d1 || d1->symbol != sym) {
	    d2 = get_discrim();
	    d2->symbol = sym;
	    d2->next = d1;
	    d1 = d2;
	    }
	else
	    d2 = NULL;  /* new node not required at this level */

	if (is_assoc_comm(t->symbol)) {
	    if (Flags[INDEX_AC_ARGS].val)
		d1 = discrim_wild_insert_ac(t, d1);
	    /* If flag is clear, AC symbol is treated like a NAME. */
	    }

	else {
	    int i;
	    for (i = 0; i < t->arity; i++)
		d1 = discrim_wild_insert_rec(t->args[i], d1);
	    }

        if (d2)  /* link in new subtree (possibly a leaf) */
	    if (!prev)
		d->u.kids = d2;
	    else
		prev->next = d2;
	    
	return(d1);  /* d1 is leaf corresp. to end of input term */
	}
}  /* discrim_wild_insert_rec */

/*************
 *
 *    discrim_wild_insert(t, root, object)
 *
 *    Insert into a discrimination tree index for finding more general
 *    terms.
 *      t:      key
 *      root:   the discrimination tree
 *      object: the object to be inserted  (void *)
 *
 *************/

void discrim_wild_insert(Term_ptr t, Discrim_ptr root, void *object)
{
    Discrim_ptr d;
    Gen_ptr_ptr gp1, gp2;

    d = discrim_wild_insert_rec(t, root);
    gp1 = get_gen_ptr();
    gp1->u.v = object;

    /* Install at end of list. */
    if (d->u.data == NULL)
	d->u.data = gp1;
    else {
	for (gp2 = d->u.data; gp2->next; gp2 = gp2->next);
	gp2->next = gp1;
	}

}  /* discrim_wild_insert */

/*************
 *
 *    Discrim_ptr discrim_wild_end(t, is, path_p)
 *
 *    Given a discrimination tree (or a subtree) and a term, return the 
 *    node in the tree that corresponds to the last symbol in t (or NULL
 *    if the node doesn't exist).  *path_p is a list that is extended by
 *    this routine.  It is a list of pointers to the
 *    nodes in path from the parent of the returned node up to imd. 
 *    (It is needed for deletions, because nodes do not have pointers to
 *    parents.) 
 *
 *************/

static Discrim_ptr discrim_wild_end(Term_ptr t, Discrim_ptr d,
				    Gen_ptr_ptr *path_p)
{
    Discrim_ptr d1;
    Gen_ptr_ptr p;
    int sym;

    /* add current node to the front of the path list. */

    p = get_gen_ptr();
    p->u.v = (void *) d;
    p->next = *path_p;
    *path_p = p;

    if (VARIABLE(t)) {
	d1 = d->u.kids;
	if (d1 && DVAR(d1))
	    return(d1);
	else
	    return(NULL);
	}

    else {  /* constant || complex */
	d1 = d->u.kids;
	sym = t->symbol;
	if (d1 && DVAR(d1))  /* skip variables */
	    d1 = d1->next;
	while (d1 && d1->symbol < sym)
	    d1 = d1->next;

	if (!d1 || d1->symbol != sym)
	    return(NULL);
	else if (is_assoc_comm(t->symbol)) {
	    if (!Flags[INDEX_AC_ARGS].val)
		return(d1);
	    else {
		int num_args, num_nv_args;
		Discrim_ptr d2, d3;
		
		num_args = num_ac_args(t, t->symbol);
		num_nv_args = num_ac_nv_args(t, t->symbol);
		
		for (d2 = d1->u.kids; d2 && d2->symbol != num_args; d2 = d2->next);
		if (!d2)
		    return(NULL);
		else {
		    for (d3 = d2->u.kids; d3 && d3->symbol != num_nv_args; d3 = d3->next);
		    if (!d3)
			return(NULL);
		    else {
			p = get_gen_ptr();
			p->u.v = (void *) d1;
			p->next = *path_p;
			*path_p = p;
			p = get_gen_ptr();
			p->u.v = (void *) d2;
			p->next = *path_p;
			*path_p = p;
			return(d3);
			}
		    }
		}
	    }
	else {
	    int i;
	    for (i = 0; d1 && i < t->arity; i++)
		d1 = discrim_wild_end(t->args[i], d1, path_p);
	    return(d1);
	    }
	}
}  /* discrim_wild_end */

/*************
 *
 *    discrim_wild_delete(t, root, object)
 *
 *************/

void discrim_wild_delete(Term_ptr t, Discrim_ptr root, void *object)
{
    Discrim_ptr end, i2, i3, parent;
    Gen_ptr_ptr tp1, tp2;
    Gen_ptr_ptr isp1, path;

    /* First find the correct leaf.  path is used to help with  */
    /* freeing nodes, because nodes don't have parent pointers. */

    path = NULL;
    end = discrim_wild_end(t, root, &path);
    if (!end)
	abend("in discrim_wild_delete, can't find end.");

    /* Free the pointer in the leaf-list */

    tp1 = end->u.data;
    tp2 = NULL;
    while(tp1 && tp1->u.v != object) {
	tp2 = tp1;
	tp1 = tp1->next;
	}
    if (!tp1)
	abend("in discrim_wild_delete, can't find term.");

    if (!tp2)
	end->u.data = tp1->next;
    else
	tp2->next = tp1->next;
    free_gen_ptr(tp1);

    if (!end->u.data) {
        /* free tree nodes from bottom up, using path to get parents */
	end->u.kids = NULL;  /* not really necessary */
	isp1 = path;
	while (!end->u.kids && end != root) {
	    parent = (Discrim_ptr) isp1->u.v;
	    isp1 = isp1->next;
	    i2 = parent->u.kids;
	    i3 = NULL;
	    while (i2 != end) {
		i3 = i2;
		i2 = i2->next;
		}
	    if (!i3)
		parent->u.kids = i2->next;
	    else
		i3->next = i2->next;
	    free_discrim(i2);
	    end = parent;
	    }
	}

    /* free path list */

    while (path) {
	isp1 = path;
	path = path->next;
	free_gen_ptr(isp1);
	}

}  /* discrim_wild_delete */

/*************
 *
 *    discrim_wild_retrieve_leaf(t_in, root, ppos)
 *
 *************/

Gen_ptr_ptr discrim_wild_retrieve_leaf(Term_ptr t_in, Discrim_ptr root,
					   Flat_ptr *ppos)
{
    Flat_ptr f, f1, f2, f_save;
    Term_ptr t;
    Discrim_ptr d;
    int symbol, status;

    f = *ppos;  /* Don't forget to reset before return. */
    t = t_in;

    if (t) {  /* if first call */
        d = root->u.kids;
        if (d) {
	    f = get_flat();
	    f->t = t;
	    f->last = f;
	    f->prev = NULL;
	    f->next = NULL;

	    if (Flags[INDEX_AC_ARGS].val)
		f->place_holder = COMPLEX(t);
	    else
		f->place_holder = (COMPLEX(t) && !is_assoc_comm(t->symbol));
				   
            status = GO;
	    }
        else
            status = FAILURE;
        }
    else
        status = BACKTRACK;

    while (status == GO || status == BACKTRACK) {

	/* Three things determine the state at this point.      */
        /* 1. d is the current node in the discrimination tree. */
	/* 2. f is the current node in the stack of flats.      */
	/* 3. status is either GO or BACKTRACK.                 */

	if (status == BACKTRACK) {
	    /* Back up to a place with an aternative branch. */
	    while (f && !f->alternatives) {
		f_save = f;
		f = f->prev;
		}
	    if (f) {
		d = f->alternatives;
		f->alternatives = NULL;
		status = GO;
		}
	    else {
		/* Free stack of flats and fail. */
		while (f_save) {
		    f1 = f_save;
		    f_save = f_save->next;
		    free_flat(f1);
		    }
		status = FAILURE;
		}
	    }

	if (status == GO) {
	    if (d && d->ac_type == AC_ARG_TYPE) {
		if (d->symbol <= f->num_ac_args)
		    f->alternatives = d->next;
		else
		    status = BACKTRACK;
		}
	    else if (d && d->ac_type == AC_NV_ARG_TYPE) {
		if (d->symbol <= f->num_ac_nv_args)
		    f->alternatives = d->next;
		else
		    status = BACKTRACK;
		}
	    else if (d && DVAR(d)) {
		/* push alternatives */
		f->alternatives = d->next;
		f = f->last;
		}
	    else if (VARIABLE(f->t))
		status = BACKTRACK;
	    else {
		symbol = f->t->symbol;
		while (d && d->symbol < symbol)
		    d = d->next;
		if (!d || d->symbol != symbol)
		    status = BACKTRACK;
		else if (f->place_holder) {

		    /* Insert skeleton in place_holder.  This is tricky, because 
		     * someone's "last" pointer may be pointing to f.  Therefore,
		     * f becomes the last argument of the skeleton.
		     */

		    if (is_assoc_comm(f->t->symbol)) {
			/* Can't get here if INDEX_AC_ARGS flag is clear. */
			f1 = get_flat();
			f1->t = f->t;
			f1->prev = f->prev;
			f1->last = f;
			if (f1->prev)
			    f1->prev->next = f1;

			f2 = get_flat();
			f2->prev = f1;
			f2->last = f2;
			f2->next = f;
			f->prev = f2;
			f1->next = f2;
			f->last = f;

			/* Now, f2 is the AC_ARGS node, and f is the AC_NV_ARGS node. */
			f2->num_ac_args = num_ac_args(f1->t, f1->t->symbol);
			f->num_ac_nv_args = num_ac_nv_args(f1->t, f1->t->symbol);

			f = f1;
			}

		    else {  /* non AC case */
			int i;
			
			f1 = get_flat();
			f1->t = f->t;
			f1->prev = f->prev;
			f1->last = f;
			f_save = f1;
			if (f1->prev)
			    f1->prev->next = f1;

			t = f->t;
			for (i = 0; i < t->arity; i++) {
			    if (i < t->arity-1)
				f2 = get_flat();
			    else
				f2 = f;
			    if (Flags[INDEX_AC_ARGS].val)
				f2->place_holder = COMPLEX(t->args[i]);
			    else
				f2->place_holder = (COMPLEX(t->args[i]) &&
						    !is_assoc_comm(t->args[i]->symbol));
			    f2->t = t->args[i];
			    f2->last = f2;
			    f2->prev = f1;
			    f1->next = f2;
			    f1 = f2;
			    }
			f = f_save;
			}
		    }
		}
	    if (status == GO) {
		if (f->next) {
		    f = f->next;
		    d = d->u.kids;
		    }
		else
		    status = SUCCESS;
		}
	    }
	}  /* while */
    if (status == SUCCESS) {
	*ppos = f;
	return(d->u.data);
	}
    else
	return(NULL);
	   
}  /* discrim_wild_retrieve_leaf */

/*************
 *
 *    discrim_wild_retrieve_first(t, root, ppos)
 *
 *    Get the first object associated with a term more general than t.
 *
 *    Remember to call discrim_wild_cancel(*ppos, context) if you don't want the
 *    whole sequence.
 *
 *************/

void *discrim_wild_retrieve_first(Term_ptr t, Discrim_ptr root, Discrim_pos_ptr *ppos)
{
    Gen_ptr_ptr tp;
    Flat_ptr f;
    Discrim_pos_ptr gp;

    tp = discrim_wild_retrieve_leaf(t, root, &f);
    if (!tp)
	return(NULL);
    else {
	gp = get_discrim_pos();
	gp->f = f;
	gp->data = tp;
	*ppos = gp;
	return(tp->u.v);
	}
}  /* discrim_wild_retrieve_first */

/*************
 *
 *    discrim_wild_retrieve_next(pos)
 *
 *    Get the next object associated with a term more general than t.
 *
 *    Remember to call discrim_wild_cancel(*ppos, context) if you don't want the
 *    whole sequence.
 *
 *************/

void *discrim_wild_retrieve_next(Discrim_pos_ptr pos)
{
    Gen_ptr_ptr tp;
    
    tp = pos->data->next;
    if (tp) {  /* if any more terms in current leaf */
	pos->data = tp;
	return(tp->u.v);
	}
    else {  /* try for another leaf */
	tp = discrim_wild_retrieve_leaf((Term_ptr) NULL,
				   (Discrim_ptr) NULL, &(pos->f));
	if (tp) {
	    pos->data = tp;
	    return(tp->u.v);
	    }
	else {
	    free_discrim_pos(pos);
	    return(NULL);
	    }
	}
}  /* discrim_wild_retrieve_next */

/*************
 *
 *    discrim_wild_cancel(pos)
 *
 *************/

void discrim_wild_cancel(Discrim_pos_ptr pos)
{
    Flat_ptr f1, f2;

    f1 = pos->f;
    while (f1) {
	f2 = f1;
	f1 = f1->prev;
	free_flat(f2);
	}
    free_discrim_pos(pos);
}  /* discrim_wild_cancel */
