#define IN_MAIN  /* so that global vars in header.h will not be external */
#include "Header.h"
#include "List.h"
#include "Io.h"
#include "Unify.h"
#include "Discrim.h"
#include "Fpa.h"
#include "Clause.h"
#include "Ac.h"
#include "Eqp.h"
#include "Interp.h"

extern Interp_ptr Interpretation;

/*************
 *
 *   clause_test()
 *
 *************/

void clause_test(Gen_ptr_ptr p1)
{
    Gen_ptr_ptr p2, p3;
    Term_ptr t1, t2, t3, t4;
    Clause_ptr c1, c2, c3, c4;
    
    for(p2 = p1; p2; p2 = p2->next) {
	t1 = p2->u.t;
	printf("\n");
	print_term_nl(stdout, t1);
	c1 = term_to_clause(t1);
	print_clause(stdout, c1);
	printf("    pos:  %d\n", pos_clause(c1));
	printf("    neg:  %d\n", neg_clause(c1));
	printf("   unit:  %d\n", unit_clause(c1));
	printf("    num:  %d\n", literal_count(c1));
	t2 = clause_to_term(c1);
	printf("   back:  %d\n", term_ident(t1, t2));
	zap_clause(c1);
	zap_term(t1);
	zap_term(t2);
	}

    print_term_mem(stdout, 1);
    print_clause_mem(stdout, 0);

}  /* clause_test */
    
/*************
 *
 *   unify_test()
 *
 *************/

void unify_test(Gen_ptr_ptr p1)
{
    Gen_ptr_ptr p2, p3;
    Term_ptr t1, t2, t3, t4;
    Context_ptr c1 = get_context();
    Context_ptr c2 = get_context();
    Trail_ptr tr = NULL;
    
    printf("Here is the list.\n");
    for(p2 = p1; p2; p2 = p2->next)
	print_term_nl(stdout, p2->u.t);
    printf("end_of_list.\n\n");
    
    for (p2 = p1; p2; p2 = p2->next) {
	t1 = p2->u.t;
	
	for (p3 = p2; p3; p3 = p3->next) {
	    t2 = p3->u.t;
	    printf("\nTrying to unify: "); p_term(t1);
	    printf("               : "); p_term(t2); fflush(stdout);
	    tr = NULL;
	    if (unify(t1, c1, t2, c2, &tr)) {
		p_context(c1);
		p_context(c2);
		p_trail(tr);
		t3 = apply(t1, c1);
		t4 = apply(t2, c2);
		printf("apply substitution: "); p_term(t3);
		printf("                  : "); p_term(t4); fflush(stdout);
		clear_subst_1(tr);
		zap_term(t3);
		zap_term(t4);
		}
	    else
		printf("unify fails\n");
	    }
	}
    free_context(c1);
    free_context(c2);
    
    for(p2 = p1; p2; p2 = p2->next)
	zap_term(p2->u.t);

    print_term_mem(stdout, 1);
    print_unify_mem(stdout, 0);
}  /* unify_test */

/*************
 *
 *   btm_test()
 *
 *************/

void btm_test(Gen_ptr_ptr p1)
{
    Gen_ptr_ptr p2, p3;
    Term_ptr t1, t2, t3, t4;
    Context_ptr c1 = get_context();
    Bt_node_ptr bt;
    
    printf("Here is the list.\n");
    for(p2 = p1; p2; p2 = p2->next) {
	t1 = p2->u.t;
	printf("\ninput:    "); print_term_nl(stdout, t1);
	ac_canonical(t1);
	printf("ac_canon: "); print_term_nl(stdout, t1);
	}
    
    for (p2 = p1; p2; p2 = p2->next) {
	t1 = p2->u.t;
	
	for (p3 = p2->next; p3; p3 = p3->next) {
	    t2 = p3->u.t;
	    printf("\n********************\nTrying to btm: "); p_term(t1);
	    printf("             : "); p_term(t2); fflush(stdout);
	    bt = match_bt_first(t1, c1, t2, 0);
	    while (bt) {
		p_context(c1);
		t3 = apply(t1, c1);
		printf("apply substitution: "); p_term(t3); fflush(stdout);
		zap_term(t3);
		bt = match_bt_next(bt);
		}
	    }
	}
    free_context(c1);
    
    for(p2 = p1; p2; p2 = p2->next)
	zap_term(p2->u.t);

    print_term_mem(stdout, 1);
    print_ac_mem(stdout, 0);
}  /* btm_test */

/*************
 *
 *   btu_test()
 *
 *************/

void btu_test(Gen_ptr_ptr p1)
{
    Gen_ptr_ptr p2, p3;
    Term_ptr t1, t2, t3, t4;
    Context_ptr c1 = get_context();
    Context_ptr c2 = get_context();
    Bt_node_ptr bt;
    int count;
    
    printf("Here is the list.\n");
    for(p2 = p1; p2; p2 = p2->next) {
	t1 = p2->u.t;
	printf("\ninput:    "); print_term_nl(stdout, t1);
	ac_canonical(t1);
	printf("ac_canon: "); print_term_nl(stdout, t1);
	}
    
    for (p2 = p1; p2; p2 = p2->next) {
	t1 = p2->u.t;
	
	for (p3 = p2->next; p3; p3 = p3->next) {
	    t2 = p3->u.t;
	    printf("\n********************\nTrying to btu: "); p_term(t1);
	    printf("             : "); p_term(t2); fflush(stdout);
	    count = 0;
	    bt = unify_bt_first(t1, c1, t2, c2);
	    while (bt) {
		count++;
#if 1
		printf("\n##################\n");
		/* p_context(c1); p_context(c2); */
		t3 = apply(t1, c1);
		t4 = apply(t2, c2);
		printf("apply substitution: "); p_term(t3);
		printf("                  : "); p_term(t4); fflush(stdout);
		printf("##################\n");
		zap_term(t3);
		zap_term(t4);
		p_bt_tree(bt, 0);
#endif
		bt = unify_bt_next(bt);
		}
	    printf("Number of unifiers is %d.\n", count);
	    }
	}
    free_context(c1);
    free_context(c2);
    
    for(p2 = p1; p2; p2 = p2->next)
	zap_term(p2->u.t);

    print_term_mem(stdout, 1);
    print_ac_mem(stdout, 0);
}  /* btu_test */

/*************
 *
 *   btu_1_test()
 *
 *************/

void btu_1_test(Gen_ptr_ptr p1)
{
    Gen_ptr_ptr p2, p3;
    Term_ptr t0, t1, t2, t3, t4;
    Context_ptr c1 = get_context();
    Context_ptr c2 = get_context();
    Bt_node_ptr bt;
    int count;
    
    printf("Here is the list.\n");
    for(p2 = p1; p2; p2 = p2->next) {
	t1 = p2->u.t;
	printf("\ninput:    "); print_term_nl(stdout, t1);
	ac_canonical(t1);
	printf("ac_canon: "); print_term_nl(stdout, t1);
	}
    
    for (p2 = p1; p2; p2 = p2->next) {
	t0 = p2->u.t;
	t1 = t0->args[0];
	t2 = t0->args[1]->args[0];

	printf("\n********************\nTrying to btu: "); p_term(t1);
	printf("             : "); p_term(t2); fflush(stdout);
	count = 0;
	bt = unify_bt_first(t1, c1, t2, c2);
	while (bt) {
	    count++;
#if 1
	    p_context(c1);
	    p_context(c2);
	    t3 = apply(t1, c1);
	    t4 = apply(t2, c2);
	    printf("apply substitution: "); p_term(t3);
	    printf("                  : "); p_term(t4); fflush(stdout);
	    zap_term(t3);
	    zap_term(t4);
#endif
	    bt = unify_bt_next(bt);
	    }
	printf("Number of unifiers is %d.\n", count);
	}
    free_context(c1);
    free_context(c2);
    
    for(p2 = p1; p2; p2 = p2->next)
	zap_term(p2->u.t);

    print_term_mem(stdout, 1);
    print_ac_mem(stdout, 0);
}  /* btu_1_test */

/*************
 *
 *   discrim_test()
 *
 *************/

void discrim_test(Gen_ptr_ptr p0, Gen_ptr_ptr p1)
{
    Gen_ptr_ptr p2, p3;
    Discrim_ptr root;
    Discrim_pos_ptr pos;
    Term_ptr t1, t2;
    Context_ptr c = get_context();

    root = discrim_init();

    for (p2 = p0; p2; p2 = p2->next)
	discrim_insert(p2->u.t, root, p2->u.t);

    p_discrim_index(root);

    for (p2 = p1; p2; p2 = p2->next) {
	printf("\nquerying with: "); p_term(p2->u.t);
	t1 = discrim_retrieve_first(p2->u.t, root, c, &pos);
	while (t1) {
	    printf("               "); p_term(t1);
	    t1 = discrim_retrieve_next(pos);
	    }
	}

    for (p2 = p0; p2; p2 = p2->next)
	discrim_delete(p2->u.t, root, p2->u.t);

    for(p2 = p0; p2; p2 = p2->next)
	zap_term(p2->u.t);

    for(p2 = p1; p2; p2 = p2->next)
	zap_term(p2->u.t);

    discrim_dealloc(root);
    free_context(c);

    print_term_mem(stdout, 1);
    print_discrim_mem(stdout, 0);

}  /* discrim_test */

/*************
 *
 *   fpa_test()
 *
 *************/

void fpa_test(Gen_ptr_ptr p1)
{
    Gen_ptr_ptr p2, p3;
    Fpa_index_ptr index;
    Fpa_pos_ptr pos;
    Term_ptr t1, t2;
    Context_ptr c1 = get_context();
    Context_ptr c2 = get_context();

    index = fpa_init(100);

    for (p2 = p1; p2; p2 = p2->next)
	fpa_insert(p2->u.t, index);

    p_fpa_index(index);

    for (p2 = p1; p2; p2 = p2->next) {
	printf("\nquerying with: "); p_term(p2->u.t);
	t1 = fpa_retrieve_first(p2->u.t, index, MORE_GEN, c1, c2, &pos);
	while (t1) {
	    printf("               "); p_term(t1);
	    t1 = fpa_retrieve_next(pos);
	    }
	}

    for (p2 = p1; p2; p2 = p2->next)
	fpa_delete(p2->u.t, index);

    for(p2 = p1; p2; p2 = p2->next)
	zap_term(p2->u.t);

    fpa_dealloc(index);

    free_context(c1);
    free_context(c2);

    print_term_mem(stdout, 1);
    print_fpa_mem(stdout, 0);

}  /* fpa_test */

/*************
 *
 *   lrpo_test()
 *
 *************/

void lrpo_test(Gen_ptr_ptr p1)
{
    Gen_ptr_ptr p2, p3;
    Term_ptr t1, t2;

    for (p2 = p1; p2; p2 = p2->next) {
	t1 = p2->u.t;
	for (p3 = p1; p3; p3 = p3->next) {
	    t2 = p3->u.t;
	    print_term(stdout, t1); printf(" ");
	    print_term(stdout, t2);
	    printf("  %d\n", lrpo(t1,t2));
	    }
	}

    for(p2 = p1; p2; p2 = p2->next)
	zap_term(p2->u.t);

    print_term_mem(stdout, 1);

}  /* lrpo_test */

/*************
 *
 *   interp_test()
 *
 *************/

void interp_test(Gen_ptr_ptr p)
{
    List_ptr lst = get_list();
    List_pos_ptr pos;
    int val;

    term_list_to_literal_list(p, lst);

    for (pos = lst->first; pos; pos = pos->next)
	renumber_vars(pos->c);

    for (pos = lst->first; pos; pos = pos->next) {
	val = eval_clause(pos->c, Interpretation);
	printf("%s", val ? "True:  " : "FALSE: "); p_clause(pos->c);
	}

}  /* interp_test */

/*************
 *
 *    main
 *
 *************/

int main(int argc, char **argv)
{
    int rc, i;
    Gen_ptr_ptr p1, p2;

#if 1
    init();
#else
    for (i=0; i<MAX_STATS; i++)
	Stats[i] = 0;
    Stats[INIT_WALL_SECONDS] = wall_seconds();
    init_clocks();
    init_symbol_table();
    init_options();
    init_special_ops();
#endif

    read_preamble();

    if (Stats[INPUT_ERRORS] != 0) {
        fprintf(stderr, "\nInput errors were found.\007\n\n");
        printf("Input errors were found.\n");
        exit(INPUT_ERROR_EXIT);
	}

    p1 = read_list(stdin, &rc);

    if (rc == 0)
	btu_test(p1);

}  /* main */

