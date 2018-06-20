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

/*************
 *
 *    unify_bt_first
 *
 *    This is backtracking unification, to be used when there
 *    can be more than one unifier.  This version handles (any number of)
 *    commutative and associative-commutative function symbols.
 *
 *    Get first unifier.  Return position for unify_bt_next calls.
 *    This procedure can also be used for matching, because a NULL
 *    context causes the corresponding term to be treated as ground.
 *    
 *    Here is an example of its use:
 *
 *        c1 = get_context();
 *        c2 = get_context();
 *        bt = unify_bt_first(t1, c1, t2, c2);
 *        while (bt) {
 *            t3 = apply(t1, c1);
 *            t4 = apply(t2, c2);
 *            zap_term(t3);
 *            zap_term(t4);
 *            bt = unify_bt_next(bt);
 *            }
 *        free_context(c1);
 *        free_context(c2);
 *
 *************/

Bt_node_ptr unify_bt_first(Term_ptr t1, Context_ptr c1, Term_ptr t2, Context_ptr c2)
{
    Bt_node_ptr bt;

    bt = get_bt_node();
    bt->t1 = t1; bt->t2 = t2; bt->c1 = c1; bt->c2 = c2;
    return(unify_bt_guts(bt));

}  /* unify_bt */

/*************
 *
 *    unify_bt_next
 *
 *    Get next unifier.  Return position for subsequent calls.
 *
 *************/

Bt_node_ptr unify_bt_next(Bt_node_ptr bt1)
{

    /* Go to last node in tree, then back up to a node with an alternative. */

    while (bt1->next)
	bt1 = bt1->next;
    while (bt1->last_child)
	bt1 = bt1->last_child;

    bt1 = unify_bt_backup(bt1);

    if (bt1)
	return(unify_bt_guts(bt1));
    else
	return(NULL);
}  /* unify_bt_next */

/*************
 *
 *    unify_bt_cancel
 *
 *    This routine should be called if the rest of a sequence of
 *    unifiers is not called for.  It clears substitutions as well
 *    frees memory.
 *
 *************/

void unify_bt_cancel(Bt_node_ptr bt)
{
    Bt_node_ptr bt1, bt2;

    for (bt1 = bt; bt1; ) {

	unify_bt_cancel(bt1->first_child);
	
	if (bt1->alternative == COMMUTE)
	    unify_bt_cancel(bt1->position_bt);
	else if (bt1->alternative == ASSOC_COMMUTE) {
	    unify_ac_cancel(bt1->ac);
	    }
	else if (bt1->cb) {
	    bt1->cb->terms[bt1->varnum] = NULL;
	    bt1->cb->contexts[bt1->varnum] = NULL;
	    }
	bt2 = bt1;
	bt1 = bt1->next;
	free_bt_node(bt2);
	}
}  /* bt_node */

/*************
 *
 *    unify_bt_guts
 *
 *    Main loop for backtracking unification.
 *
 *************/

Bt_node_ptr unify_bt_guts(Bt_node_ptr bt1)
{
    Term_ptr t1, t2;
    Context_ptr c1, c2;
    int vn1, vn2, status;
    Bt_node_ptr bt2, bt3;

    status = GO;
    while (status == GO) {
    	t1 = bt1->t1;
	t2 = bt1->t2;
	c1 = bt1->c1;
	c2 = bt1->c2;
	DEREFERENCE(t1, c1)
	DEREFERENCE(t2, c2)

#ifdef DEBUG
	printf("guts loop (derefed) ");
	p_term(t1); printf(" %d ",   c1 ? c1->multiplier : -2);
        p_term(t2); printf(" %d \n", c2 ? c2->multiplier : -2);
#endif	    
	
	if (bt1->alternative == COMMUTE) {
	    if (unify_commute(t1, c1, t2, c2, bt1))
		    status = POP;
		else
		    status = BACKTRACK;
	    }
	else if (bt1->alternative == ASSOC_COMMUTE) {
	    if (unify_ac(t1, c1, t2, c2, bt1))
		    status = POP;
		else
		    status = BACKTRACK;
	    }
	else if (c1 && VARIABLE(t1)) {
	    vn1 = t1->symbol;
	    if (VARIABLE(t2)) {
		if (vn1 == t2->symbol && c1 == c2)
		    status = POP;
		else {
#ifdef DEBUG 
	  printf("BIND: v%d, c%d\n", vn1, c1->multiplier);
	  fflush(stdout);
#endif
		    BIND_BT(vn1, c1, t2, c2, bt1)
		    status = POP;
		    }
		}
	    else {
		/* t1 variable, t2 not variable */
		Stats[BT_OCCUR_CHECKS]++;
		if (occur_check(vn1, c1, t2, c2)) {
#ifdef DEBUG
	  printf("BIND: v%d, c%d\n", vn1, c1->multiplier);
	  fflush(stdout);
#endif
		    BIND_BT(vn1, c1, t2, c2, bt1)
		    status = POP;
		    }
		else
		    status = BACKTRACK;
		}
	    }
	
	else if (c2 && VARIABLE(t2)) {
	    /* t2 variable, t1 not variable */
	    vn2 = t2->symbol;
	    Stats[BT_OCCUR_CHECKS]++;
	    if (occur_check(vn2, c2, t1, c1)) {
#ifdef DEBUG
	  printf("BIND: v%d, c%d\n", vn2, c2->multiplier);
	  fflush(stdout);
#endif
		BIND_BT(vn2, c2, t1, c1, bt1)
		status = POP;
		}
	    else
		status = BACKTRACK;
	    }

	else if (t1->arity != t2->arity || t1->symbol != t2->symbol)
	    status = BACKTRACK;

	else if (CONSTANT(t1))
	    status = POP;
	
	else {  /* both COMPLEX with same functor (and same arity) */

	    if (is_commutative(t1->symbol)) {
		if (unify_commute(t1, c1, t2, c2, bt1))
		    status = POP;
		else
		    status = BACKTRACK;
		}
	    else if (is_assoc_comm(t1->symbol)) {
		if (unify_ac(t1, c1, t2, c2, bt1))
		    status = POP;
		else
		    status = BACKTRACK;
		}
	    else {
		/* Set up children corresponding to args of <t1,t2>.
		 * Order not important for correctness.
		 * AC kids last for efficiency, but keep in order otherwise.
		 */
		int i;
		bt3 = NULL;

		for (i = 0; i < t1->arity; i++) {

		    bt2 = get_bt_node();
		    bt2->t1 = t1->args[i];
		    bt2->t2 = t2->args[i];
		    bt2->c1 = c1;
		    bt2->c2 = c2;
		    bt2->parent = bt1;

		    if (is_assoc_comm(t1->args[i]->symbol)) {
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
	    bt1 = unify_bt_backup(bt1);
	    if (bt1)
		status = GO;
	    else
		status = FAILURE;
	    }
	}
    return(bt1);
}  /* unify_bt_guts */

/*************
 *
 *    Bt_node_ptr unify_bt_backup(bt)
 *
 *    Back up (freeing nodes) to the most recent node with an alternative.
 *
 *************/

Bt_node_ptr unify_bt_backup(Bt_node_ptr bt1)
{
    Bt_node_ptr bt2, bt3;

    while (bt1 && !bt1->alternative) {

	if (bt1->cb) {  /* unbind variable */
#ifdef DEBUG
	  printf("CLEAR: v%d, c%d\n", bt1->varnum, bt1->cb->multiplier);
	  fflush(stdout);
#endif
	    bt1->cb->terms[bt1->varnum] = NULL;
	    bt1->cb->contexts[bt1->varnum] = NULL;
	    bt1->cb = NULL;
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
	
}  /* unify_bt_backup */

/*************
 *
 *    unify_commute
 *
 *    Commutative unification.  t1 and t2 have the same commutative functor.
 *
 *    t1, c1, t2, c2, are dereferenced terms from bt.
 *
 *************/

int unify_commute(Term_ptr t1, Context_ptr c1, Term_ptr t2, Context_ptr c2, Bt_node_ptr bt)
{
    Bt_node_ptr bt1, bt2;

    if (bt->alternative == 0) {  /* first call */
	bt->alternative = COMMUTE;
	bt->flipped = 0;

	/* Set up 2 subproblems, then unify guts. */

	bt1 = get_bt_node();  bt2 = get_bt_node();
	bt1->next = bt2; bt2->prev = bt1;
	bt1->c1 = c1; bt1->c2 = c2;
	bt2->c1 = c1; bt2->c2 = c2;
	bt1->t1=t1->args[0]; bt1->t2=t2->args[0];
	bt2->t1=t1->args[1]; bt2->t2=t2->args[1];

	bt->position_bt = unify_bt_guts(bt1);
	}
    else  /* continuation */
	bt->position_bt = unify_bt_next(bt->position_bt);

    if (!bt->position_bt && !bt->flipped) {

	/* Set up 2 subproblems, with t2 flipped, then unify guts. */

	bt1 = get_bt_node();  bt2 = get_bt_node();
	bt1->next = bt2; bt2->prev = bt1;
	bt1->c1 = c1; bt1->c2 = c2;
	bt2->c1 = c1; bt2->c2 = c2;
	bt1->t1=t1->args[0]; bt1->t2=t2->args[1];
	bt2->t1=t1->args[1]; bt2->t2=t2->args[0];

	bt->flipped = 1;
	bt->position_bt = unify_bt_guts(bt1);
	}

    if (bt->position_bt)
	return(1);
    else {
	bt->alternative = 0;
	return(0);
	}
    
}  /* unify_commute */

/*************
 *
 *    p_bt_tree -- print a bt tree (This could be improved!)
 *
 *************/

void p_bt_tree(Bt_node_ptr bt, int n)
{
    int i;
    Bt_node_ptr curr, prev;

    if (bt == NULL) 
	printf("bt tree NULL.\n");
    else {
	printf("\n" );
	for (i = 0; i < n%100; i++)
	    printf("----");
	printf(" bt_tree: %d\n", n);

	print_term(stdout, bt->t1); printf(" [ 0x%x ]\n", (unsigned) bt->c1);
	print_term(stdout, bt->t2); printf(" [ 0x%x ]\n", (unsigned) bt->c2);
	p_context(bt->c1);
	p_context(bt->c2);

	if (bt->alternative == ASSOC_COMMUTE) {
	    p_ac_position(bt->ac, n+100);
	    }
	
	prev = NULL;
	for (curr = bt->first_child; curr; curr = curr->next) {
	    if (curr->parent != bt)
		printf("parent error\n");
	    if (curr->prev != prev)
		printf("prev error\n");
	    p_bt_tree(curr, n+1);
	    prev = curr;
	    }
	if (bt->last_child != prev)
	    printf("last error\n");
	printf(" end of bt_tree: %d\n", n);
	}
   
}  /* p_bt_tree */

