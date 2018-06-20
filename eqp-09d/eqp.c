/*  WARNING!! This code uses the general, multiliteral clause structure,
 *  but most of it assumes that all clauses are equality units.
 */

#include "Header.h"
#include "List.h"
#include "Symbols.h"
#include "Io.h"
#include "Unify.h"
#include "Ac.h"
#include "Discrim.h"
#include "Fpa.h"
#include "Clause.h"
#include "List.h"
#include "Paramod.h"
#include "Pindex.h"
#include "Demod.h"
#include "Eqp.h"
#include "Interp.h"

/****************** Global variables for eqp (EQ prover)  ************/

/* clause lists */

 List_ptr Usable;
 List_ptr Sos;
 List_ptr Demodulators;
 List_ptr Passive;
 List_ptr Disabled;

/* indexes */

 Discrim_ptr Discrim_demod; /* for demodulation */
 Discrim_ptr Discrim_pos;   /* positive literals for forward subsumption */
 Discrim_ptr Discrim_neg;   /* negative literals for forward subsumption */

 Fpa_index_ptr Fpa_terms;   /* for "from" paramodulation */
 Fpa_index_ptr Fpa_alphas;  /* for "into" paramodulation */
 Fpa_index_ptr Fpa_bd;      /* for back demodulation */
 Fpa_index_ptr Fpa_pos;     /* pos literals: back subsump. and unit conflict */
 Fpa_index_ptr Fpa_neg;     /* neg literals: back subsump. and unit conflict */

 Pair_index_ptr Para_pairs; /* paramodulation-by-pairs */
 Pair_index_ptr Para_pairs_breadth; /* paramodulation-by-pairs with ratio */

/* Macros to get the correct index from the literal. */

#define FS_INDEX(lit)  (lit->sign ? Discrim_pos : Discrim_neg)
#define LIT_INDEX(lit) (lit->sign ? Fpa_pos : Fpa_neg)
#define COMP_LIT_INDEX(lit) (lit->sign ? Fpa_neg : Fpa_pos)

/* To assign ID to clauses. */

 int Id_count;

/* To access clauses by ID. */

 Gen_ptr_ptr *Id_table;

/* Interpretation for semantic inference rules. */

 Interp_ptr Interpretation;


/****************** End global variables for acp (AC prover)  ************/

/*************
 *
 *    init() -- initialize global variables
 *
 *************/

void init(void)
{
    init_stats();
    init_options();
    init_symbol_table();
    init_special_ops();
}  /* init */

/*************
 *
 *    read_preamble()
 *
 *************/

void read_preamble(void)
{
    Term_ptr t;
    int rc, error, go;

    go = 1;
    t = read_term(stdin, &rc);
    while (go && (t || rc == 0)) {
        error = 0;
        if (!t)
            error = 1;
        else if (VARIABLE(t))
            error = 1;
        else if (t->arity == 0) {
	    if (str_ident("end_of_commands", sn_to_str(t->symbol))) {
                print_term(stdout, t); printf(".\n");
		go = 0;
		}
	    else
		error = 1;
	    }
        else if (str_ident("set", sn_to_str(t->symbol))) {
	    if (t->arity != 1 ||
		t->args[0]->arity != 0) {
                printf("ERROR, bad set command: ");
                print_term_nl(stdout, t);
                Stats[INPUT_ERRORS]++;
		}
	    else if (change_flag(stdout,sn_to_str(t->args[0]->symbol), 1) == -1)
		Stats[INPUT_ERRORS]++;
	    else
                print_term_nl(stdout, t);
            }
        else if (str_ident("clear", sn_to_str(t->symbol))) {
	    if (t->arity != 1 ||
		t->args[0]->arity != 0) {
                printf("ERROR, bad clear command: ");
                print_term_nl(stdout, t);
                Stats[INPUT_ERRORS]++;
		}
	    else if (change_flag(stdout,sn_to_str(t->args[0]->symbol), 0) == -1)
		Stats[INPUT_ERRORS]++;
	    else
                print_term_nl(stdout, t);
            }
        else if (str_ident("assign", sn_to_str(t->symbol))) {
	    if (t->arity != 2 ||
		t->args[0]->arity != 0 ||
		t->args[1]->arity != 0) {
                printf("ERROR, bad assign command: ");
                print_term_nl(stdout, t);
                Stats[INPUT_ERRORS]++;
		}
	    else {
		int n, rc;
		rc = str_int(sn_to_str(t->args[1]->symbol), &n);
		if (!rc) {
		    printf("ERROR, assign value must be an interger: ");
		    print_term_nl(stdout, t);
		    Stats[INPUT_ERRORS]++;
		    }
		else if (change_parm(stdout, sn_to_str(t->args[0]->symbol), n) == -1)
		    Stats[INPUT_ERRORS]++;
		else
		    print_term_nl(stdout, t);
		}
            }
        else if (str_ident("lex", sn_to_str(t->symbol))) {
            print_term(stdout, t);
            printf(".\n");
            if (!process_lex_command(t))
                Stats[INPUT_ERRORS]++;
	    ;
            }
        else if (str_ident("op", sn_to_str(t->symbol))) {
            print_term(stdout, t);
            printf(".\n");
            if (!process_op_command(t))
                Stats[INPUT_ERRORS]++;
            }
        else if (str_ident("assoc_comm", sn_to_str(t->symbol))) {
            print_term(stdout, t);
            printf(".\n");
            if (!process_ac_command(t))
                Stats[INPUT_ERRORS]++;
	    else {
		Internal_flags[AC_PRESENT] = 1;
		Internal_flags[BT_UNIFY] = 1;
		}
	    }
        else if (str_ident("commutative", sn_to_str(t->symbol))) {
            print_term(stdout, t);
            printf(".\n");
            if (!process_comm_command(t))
                Stats[INPUT_ERRORS]++;
	    else {
		Internal_flags[COMM_PRESENT] = 1;
		Internal_flags[BT_UNIFY] = 1;
		}
            }
        else if (str_ident("lrpo_multiset_status", sn_to_str(t->symbol))) {
            print_term(stdout, t);
            printf(".\n");
            if (!process_multiset_command(t))
                Stats[INPUT_ERRORS]++;
            }
        else if (str_ident("interpretation", sn_to_str(t->symbol))) {
            print_term(stdout, t);
            printf(".\n");
	    Interpretation = init_interp(t);
	    p_interp(Interpretation);
	    Internal_flags[INTERP_PRESENT] = 1;
            }
        else
            error = 1;

        if (error) {
            Stats[INPUT_ERRORS]++;
            if (t != NULL) {
                printf("ERROR, command not found: ");
                print_term(stdout, t); printf(".\n");
                }
            }
        if (t != NULL)
            zap_term(t);
	if (go)
	    t = read_term(stdin, &rc);
        }

}  /* read_preamble */

/*************
 *
 *    subsumes(c1, c2) -- ASSUME UNIT CLAUSES!!
 *
 *************/

int subsumes(Clause_ptr c1, Clause_ptr c2)
{
    Literal_ptr lit1, lit2;
    Context_ptr c;
    Bt_node_ptr bt_position;
    int subsumed;

    lit1 = c1->literals;
    lit2 = c2->literals;
    c = get_context();
    if (lit1->sign != lit2->sign)
	subsumed = 0;
    else {
	if (Internal_flags[BT_UNIFY]) {
	    bt_position = match_bt_first(lit1->atom, c, lit2->atom, 0);
	    if (bt_position) {
		subsumed = 1;
		match_bt_cancel(bt_position);
		}
	    else
		subsumed = 0;
	    }
	else {
	    Trail_ptr tr = NULL;
	    subsumed = match(lit1->atom, c, lit2->atom, &tr);
	    if (subsumed)
		clear_subst_1(tr);
	    }
	}
    free_context(c);
    return(subsumed);
}  /* subsumes */

/*************
 *
 *    subsume_list(c, l) -- ASSUME UNIT CLAUSES!!
 *
 *************/

Clause_ptr subsume_list(Clause_ptr c, List_ptr l)
{
    List_pos_ptr p;
    Clause_ptr subsumer;
    Literal_ptr lit1, lit2;
    Context_ptr s;

    lit1 = c->literals;
    s = get_context();
    for (p = l->last, subsumer = NULL; p && !subsumer; p = p->prev) {
	lit2 = p->c->literals;
	if (lit1->sign == lit2->sign) {
	    if (Internal_flags[BT_UNIFY]) {
		Bt_node_ptr bt_position;
		bt_position = match_bt_first(lit2->atom, s, lit1->atom, 0);
		if (bt_position) {
		    subsumer = p->c;
		    match_bt_cancel(bt_position);
		    }
		}
	    else {
		Trail_ptr tr = NULL;
		if (match(lit2->atom, s, lit1->atom, &tr)) {
		    subsumer = p->c;
		    clear_subst_1(tr);
		    }
		}
	    }
	}
    free_context(s);
    return(subsumer);
}  /* subsume_list */

/*************
 *
 *    conflict_list(c, l) -- ASSUME UNIT CLAUSES!!
 *
 *************/

Clause_ptr conflict_list(Clause_ptr c, List_ptr l)
{
    List_pos_ptr p;
    Clause_ptr conflictor;
    Literal_ptr lit1, lit2;
    Context_ptr c1, c2;

    lit1 = c->literals;
    c1 = get_context();
    c2 = get_context();
    for (p = l->last, conflictor = NULL; p && !conflictor; p = p->prev) {
	lit2 = p->c->literals;
	if (lit1->sign != lit2->sign) {
	    if (Internal_flags[BT_UNIFY]) {
		Bt_node_ptr bt_position;
		bt_position = unify_bt_first(lit1->atom, c1, lit2->atom, c2);
		if (bt_position) {
		    conflictor = p->c;
		    unify_bt_cancel(bt_position);
		    }
		}
	    else {
		Trail_ptr tr = NULL;
		if (unify(lit1->atom, c1, lit2->atom, c2, &tr)) {
		    conflictor = p->c;
		    clear_subst_1(tr);
		    }
		}
	    }
	}
    free_context(c1);
    free_context(c2);
    return(conflictor);
}  /* conflict_list */

/*************
 *
 *   functional_subsume(alpha, beta, depth)
 *
 *   Assume that alpha=beta is not subsumed by x = x.
 *   "depth" is for debugging only.
 *
 *************/

Clause_ptr functional_subsume(Term_ptr alpha, Term_ptr beta,
			      Clause_ptr original, int depth)
{
    /* First, make alpha=beta into a clause and see if that is subsumed. */

    Clause_ptr subsumer;
    Clause_ptr c = get_clause();
    Literal_ptr l = get_literal();
    Term_ptr t = get_term(2);

    c->literals = l; l->atom = t; l->sign = 1;
    t->symbol = str_to_sn("=", 2);
    t->args[0] = alpha; t->args[1] = beta;

    if (Flags[INDEX_FS].val) {
	Discrim_pos_ptr pos;
	Context_ptr subst;

	subst = get_context();
	subsumer = discrim_retrieve_first(c->literals->atom,
					  FS_INDEX(c->literals), subst, &pos);
	if (subsumer)
	    discrim_cancel(pos);
	free_context(subst);
	}
    else {
	subsumer = subsume_list(c, Passive);
	if (!subsumer)
	    subsumer = subsume_list(c, Usable);
	if (!subsumer)
	    subsumer = subsume_list(c, Sos);
	}

#if 0
	if (subsumer && depth > 1) {
	    printf("\nfunctional subsume: "); p_clause(subsumer);
	    printf("                    "); p_clause(original);
	    }
#endif

    free_clause(c);
    free_literal(l);
    free_term(t);

    if (subsumer) {
	return(subsumer);
	}
    else if (alpha->symbol != beta->symbol) {
	return(NULL);
	}
    else {
	Term_ptr ta, tb;
	int i, n;
	for (i = n = 0; i < alpha->arity; i++) {
	    if (term_ident(alpha->args[i], beta->args[i]))
		n++;
	    else {
		ta = alpha->args[i];
		tb = beta->args[i];
		}
	    }
	if (n == alpha->arity-1) {
	    return(functional_subsume(ta, tb, original, depth+1));
	    }
	else
	    return(NULL);
	}
}  /* functional_subsume */

/*************
 *
 *    forward_subsume(c)  -- ASSUME UNIT CLAUSES!!
 *
 *    Check against x=x, Passive, Usable, and Sos.
 *
 *    Return c (if subsumed by x=x), subsumer, or NULL.
 *
 *************/

Clause_ptr forward_subsume(Clause_ptr c)
{
    Term_ptr t1, t2;
    Clause_ptr subsumer;
    Literal_ptr lit;

    CLOCK_START(FOR_SUB_TIME)
    lit = c->literals;
    if (lit->sign && is_symbol(lit->atom->symbol, "=", 2)) {
	t1 = lit->atom->args[0];
	t2 = lit->atom->args[1];
	if (term_ident(t1,t2)) {
	    CLOCK_STOP(FOR_SUB_TIME)
	    return(c);
	    }
	else if (Flags[FUNCTIONAL_SUBSUME].val) {
	    Clause_ptr subsumer = functional_subsume(c->literals->atom->args[0],
					c->literals->atom->args[1], c, 1);
	    CLOCK_STOP(FOR_SUB_TIME)
	    return(subsumer);
	    }
	}

    if (Flags[INDEX_FS].val) {
	Discrim_pos_ptr pos;
	Context_ptr subst;

	subst = get_context();
	subsumer = discrim_retrieve_first(c->literals->atom,
					  FS_INDEX(c->literals), subst, &pos);
	if (subsumer)
	    discrim_cancel(pos);
	free_context(subst);
	}
    else {
	subsumer = subsume_list(c, Passive);
	if (!subsumer)
	    subsumer = subsume_list(c, Usable);
	if (!subsumer)
	    subsumer = subsume_list(c, Sos);
	}

    CLOCK_STOP(FOR_SUB_TIME)
    return(subsumer);

}  /* forward_subsume */

/*************
 *
 *    check_for_proof(c) -- ASSUME UNIT CLAUSES!!
 *
 *    Check against x=x, Passive, Usable, and Sos.
 *    Clean up and exit if a proof is found and #proofs >= max_proofs.
 *
 *************/

void check_for_proof(Clause_ptr c)
{
    Literal_ptr lit;
    Term_ptr t1, t2;
    Context_ptr c1, c2;
    Bt_node_ptr bt_position;
    Clause_ptr conflictor;
    int xx_proof;

    if (literal_count(c) != 1) return;

    CLOCK_START(CONFLICT_TIME)
    lit = c->literals;
    xx_proof = 0;

    if (!lit->sign && is_symbol(lit->atom->symbol, "=", 2)) {
        c1 = get_context();
        c2 = get_context();
	t1 = lit->atom->args[0];
	t2 = lit->atom->args[1];
	bt_position = unify_bt_first(t1, c1, t2, c1);
	if (bt_position) {
	    xx_proof = 1;
	    unify_bt_cancel(bt_position);
	    }
	free_context(c1);
	free_context(c2);
	}

    if (xx_proof)
	conflictor = NULL;
    else {
	if (Flags[INDEX_CONFLICT].val) {
	    Context_ptr s1, s2;
	    Fpa_pos_ptr pos;
	    Term_ptr found_atom, atom;

	    s1 = get_context();
	    s2 = get_context();
	    atom = c->literals->atom;
	    found_atom = fpa_retrieve_first(atom, COMP_LIT_INDEX(c->literals),
					    UNIFY, s1, s2, &pos);
					    
	    if (found_atom) {
		fpa_cancel(pos);
		conflictor = found_atom->containing_clause;
		}
	    else
		conflictor = NULL;
	    free_context(s1);
	    free_context(s2);
	    }
	else {
	    conflictor = conflict_list(c, Passive);
	    if (!conflictor)
		conflictor = conflict_list(c, Usable);
	    if (!conflictor)
		conflictor = conflict_list(c, Sos);
	    }
	}

    CLOCK_STOP(CONFLICT_TIME)
    if (xx_proof || conflictor) {

	fprintf(stderr, "---------------- PROOF ----------------");
	fprintf(stderr, "\007\n\n");
	if (conflictor)
            printf("\nUNIT CONFLICT from %d and %d", c->id, conflictor->id);
        else
            printf("\nUNIT CONFLICT from %d and x=x", c->id);

	fprintf(stdout, " at %6.2f seconds.\n", run_time() / 1000.);

	Stats[PROOFS]++;

        print_proof(stdout, c, conflictor, Id_table);
	fflush(stdout);

	if (Stats[PROOFS] >= Parms[MAX_PROOFS].val) {
	    if (Flags[PRINT_LISTS_AT_END].val) {
		fprintf(stdout, "\nUsable:\n");
		print_list(stdout, Usable);
		fprintf(stdout, "\nSos:\n");
		print_list(stdout, Sos);
		fprintf(stdout, "\nDemodulators:\n");
		print_list(stdout, Demodulators);
		}
	    output_stats(stdout);
#if 0
	    printf("Fpa_bd: "); print_fpa_index_summary(stdout, Fpa_bd);
	    printf("Fpa_terms: "); print_fpa_index_summary(stdout, Fpa_terms);
	    printf("Fpa_alphas: "); print_fpa_index_summary(stdout, Fpa_alphas);
	    printf("Fpa_pos: "); print_fpa_index_summary(stdout, Fpa_pos);
	    printf("Fpa_neg: "); print_fpa_index_summary(stdout, Fpa_neg);

#endif
	    exit(PROOF_EXIT);
	    }
	}
}  /* check_for_proof */

/*************
 *
 *    term_list_to_literal_list(p, l)
 *
 *************/

void term_list_to_literal_list(Gen_ptr_ptr p, List_ptr l)
{
    Gen_ptr_ptr p1;
    Term_ptr t;
    Literal_ptr lit;
    Clause_ptr c;

    while (p) {
	t = p->u.t;
	c = get_clause();
	lit = get_literal();
	c->literals = lit;
	lit->sign = !is_symbol(t->symbol, "-", 1);
	if (!lit->sign) {
	    lit->atom = t->args[0];
	    free_term(t);
	    }
	else
	    lit->atom = t;
	c->weight = -1;
	list_append(c, l);
	p1 = p;
	p = p->next;
	free_gen_ptr(p1);
	}
}  /* term_list_to_literal_list */

/*************
 *
 *     read_lists(fp, usable, sos, passive, demodulators)
 *
 *************/

void read_lists(FILE *fp, List_ptr usable, List_ptr sos, List_ptr passive,
		List_ptr demodulators)
{
    Term_ptr t;
    int rc;
    Gen_ptr_ptr p;

    t = read_term(stdin, &rc);

    while (t || rc == 0) {
        if (rc == 0)
	    Stats[INPUT_ERRORS]++;
        else if (!is_symbol(t->symbol, "list", 1)) {
	    printf("bad list command: ");
	    p_term(t);
	    Stats[INPUT_ERRORS]++;
	    }
	else {
	    p = read_list(fp, &rc);
	    Stats[INPUT_ERRORS] += rc;
	    if (str_ident(sn_to_str(t->args[0]->symbol), "usable"))
		term_list_to_literal_list(p, usable);
	    else if (str_ident(sn_to_str(t->args[0]->symbol), "sos"))
		term_list_to_literal_list(p, sos);
	    else if (str_ident(sn_to_str(t->args[0]->symbol), "passive"))
		term_list_to_literal_list(p, passive);
	    else if (str_ident(sn_to_str(t->args[0]->symbol), "demodulators"))
		term_list_to_literal_list(p, demodulators);
	    else {
		printf("bad list command: ");
		p_term(t);
		Stats[INPUT_ERRORS]++;
		}
	    }
	t = read_term(stdin, &rc);
	}
	
    if (Stats[INPUT_ERRORS] == 0) {
	printf("\nUsable:\n");
	p_list(usable);
	printf("\nSos:\n");
	p_list(sos);
	printf("\nDemodulators:\n");
	p_list(demodulators);
	printf("\nPassive:\n");
	p_list(passive);
	}
}  /* read_lists */

/*************
 *
 *     process_all_input()
 *
 *************/

void process_all_input(List_ptr usable, List_ptr sos, List_ptr passive,
		       List_ptr demodulators)
{
    List_pos_ptr p, q;
    Clause_ptr c;

    printf("\nStarting to process input.\n");

    Usable = get_list();
    Sos = get_list();
    Demodulators = get_list();
    Passive = passive; /* Passive doesn't change, so just move the pointer. */

    for (p = Passive->first; p; p = p->next) {
	c = p->c;

        renumber_vars(c);  /* This can make it non-ac-canonical. */
        ac_canonical(c->literals->atom);
	clause_up_pointers(c);
	c->id = ++Id_count;
        store_clause_by_id(c, Id_table);

	index_for_sub(c, 1);
	index_bs_conflict(c, 1);
	Stats[PASSIVE_SIZE]++;
	Stats[PASSIVE_INPUT]++;
	Stats[CLAUSES_INPUT]++;
	Stats[CLAUSES_KEPT_INPUT]++;
	c->weight = term_weight(c->literals->atom);
	}

    p = usable->first;
    while(p) {
	q = p;
	p = p->next;
	c = q->c;
	list_remove(c, usable);
	Stats[USABLE_INPUT]++;
	Stats[CLAUSES_INPUT]++;
	process_and_store_clause(c, 1, Usable);
	}
    free_list(usable);

    p = sos->first;
    while(p) {
	q = p;
	p = p->next;
	c = q->c;
	list_remove(c, sos);
	Stats[SOS_INPUT]++;
	Stats[CLAUSES_INPUT]++;
	process_and_store_clause(c, 1, Sos);
	}
    free_list(sos);

    p = demodulators->first;
    while (p) {
	q = p;
	p = p->next;
	c = q->c;
	ac_canonical(c->literals->atom);
	c->id = ++Id_count;
        store_clause_by_id(c, Id_table);
	Stats[DEMODULATORS_INPUT]++;
	Stats[CLAUSES_INPUT]++;
	Stats[CLAUSES_KEPT_INPUT]++;

	list_remove(c, demodulators);
	list_append(c, Demodulators);
	Stats[DEMODULATORS_SIZE]++;
        index_demod(c, 1);
	}
    free_list(demodulators);

    printf("\nAfter processing input:\n");
    printf("\nUsable:\n");
    p_list(Usable);
    printf("\nSos:\n");
    p_list(Sos);
    printf("\nDemodulators:\n");
    p_list(Demodulators);
    printf("\nPassive:\n");
    p_list(Passive);
    
}  /* process_all_input */

/*************
 *
 *     demodulate_cl()
 *
 *************/

void demodulate_cl(Clause_ptr c)
{
    Literal_ptr lit;
    Gen_ptr_ptr p1, p2, p3;

    CLOCK_START(DEMOD_TIME)
    p1 = NULL;
    for (lit = c->literals; lit; lit = lit->next) {
	if (Internal_flags[BT_UNIFY]) {
	    void *demods;
	    if (Flags[INDEX_BT_DEMOD].val)
		demods = Discrim_demod;
	    else
		demods = Demodulators;
	    lit->atom = demodulate_bt(lit->atom, demods, 0, &p1);
	    }
	else
	    lit->atom = demodulate(lit->atom, Discrim_demod, &p1);
	clear_demod_marks(lit->atom);
	}

    if (p1) {

	p1 = reverse_gen_list(p1, (Gen_ptr_ptr) NULL);

	p2 = get_gen_ptr();
	p2->u.i = DEMOD_RULE;
	p3 = get_gen_ptr();
	p3->u.i = -(gen_ptr_count(p1));  /* number of rewrites */

	p2->next = p3;
	p3->next = p1;
	
	for (p1 = c->justification; p1 && p1->next; p1 = p1->next);
	if (p1)
	    p1->next = p2;
	else
	    c->justification = p2;
	}
    CLOCK_STOP(DEMOD_TIME)
}  /* demodulate_cl */

/*************
 *
 *    back_demodulate(demod, input)
 *
 *************/


void back_demodulate(Clause_ptr demod, int input)
{
    List_pos_ptr p;
    List_ptr to_be_back_demodulated;
    Term_ptr alpha;
    Clause_ptr c, new_c;
    int i;
    Gen_ptr_ptr p1, p2;

    alpha = demod->literals->atom->args[0];

    CLOCK_START(BD_FIND_TIME)
    to_be_back_demodulated = get_bd_clauses(demod);
    CLOCK_STOP(BD_FIND_TIME)

    /* For each clause in to_be_back_demodulated, move it to */
    /* Disabled, copy it, and process the copy. */
    
    while (to_be_back_demodulated->first) {
	c = to_be_back_demodulated->first->c;

        list_remove(c, to_be_back_demodulated);

	if (!list_member(c, Disabled)) {

	    if (input || Flags[PRINT_BACK_DEMOD].val) {
		printf("    -> %d back demodulating %d.\n", demod->id, c->id);
		fflush(stdout);
		}
	    
	    if (input)
		Stats[CLAUSES_BACK_DEMODULATED_INPUT]++;
	    else
		Stats[CLAUSES_BACK_DEMODULATED]++;
	    
	    new_c = copy_clause(c);
	    if (Flags[BASIC_PARAMOD].val)
		copy_clause_nonbasic_marks(c, new_c);
	    p1 = get_gen_ptr();
	    p2 = get_gen_ptr();
	    new_c->justification = p1;
	    p1->next = p2;
	    p1->u.i = BACK_DEMOD_RULE;
	    p2->u.i = c->id;
	    
	    disable_clause(c);
	    
	    process_and_store_clause(new_c, input, Sos);
	    }
	}
    free_list(to_be_back_demodulated);

}  /* back_demodulate */

/*************
 *
 *    new_demodulator(demod, input)
 *
 *************/

void new_demodulator(Clause_ptr demod, int input)
{
    if (Flags[NO_DEMODULATION].val)
	return;

    Stats[input ? NEW_DEMODULATORS_INPUT : NEW_DEMODULATORS]++;

    list_append(demod, Demodulators);
    Stats[DEMODULATORS_SIZE]++;
    index_demod(demod, 1);

    if (input || Flags[PRINT_NEW_DEMOD].val) {
	printf("%d is a new demodulator.\n", demod->id);
	fflush(stdout);
	}

}  /* new_demodulator */

/*************
 *
 *   flip_eq_unit()
 *
 *************/

Clause_ptr flip_eq_unit(Clause_ptr c)
{
    Clause_ptr d;
    Term_ptr atom, t;
    Gen_ptr_ptr p1, p2;

    d = copy_clause(c);
    if (Flags[BASIC_PARAMOD].val)
	copy_clause_nonbasic_marks(c, d);
    atom = d->literals->atom;
    t = atom->args[0];
    atom->args[0] = atom->args[1];
    atom->args[1] = t;
    
    p1 = get_gen_ptr(); p1->u.i = FLIP_RULE;
    p2 = get_gen_ptr(); p2->u.i = c->id;
    d->justification = p1;
    p1->next = p2;

    return(d);
}  /* flip_eq_unit */

/*************
 *
 *   process_clause()
 *
 *   This is called for input and derived clauses.  Actually, "input"
 *   means anything before the search starts, including back demodulated
 *   and flipped input clauses.  "Input" clauses are not checked
 *   against max_weight, statistics are different, and more is output.
 *
 *   This routine demodulates the clause, and decides if it should be
 *   kept.  Return (kept ? c : NULL).
 *
 *************/

Clause_ptr process_clause(Clause_ptr c, int input)
{
    Clause_ptr subsumer;

    Stats[input ? CLAUSES_GENERATED_INPUT : CLAUSES_GENERATED]++;
    if (Flags[PRINT_GEN].val) {
        printf("Processing: ");
        p_clause(c);
        /* p_term_basic(c->literals->atom); */
	fflush(stdout);
        }
    
    /* ac_canonical not needed here; it happens during demodulation. */

    demodulate_cl(c);
    orient_eq_literals(c);
    
    CLOCK_START(WEIGH_TIME)
    c->weight = term_weight(c->literals->atom);
    CLOCK_STOP(WEIGH_TIME)
	
    if (!input && c->weight > Parms[MAX_WEIGHT].val) {
	zap_clause(c);
	c = NULL;
	Stats[CLAUSES_WT_DELETE]++;
	}
    else if (!input &&
	     Parms[MAX_VARIABLES].val < INT_MAX &&
	     distinct_vars(c) > Parms[MAX_VARIABLES].val) {
	zap_clause(c);
	c = NULL;
	Stats[CLAUSES_WT_DELETE]++;
	Stats[CLAUSES_VAR_DELETE]++;
	}
    else if (subsumer = forward_subsume(c)) {
	if (input || Flags[PRINT_FORWARD_SUBSUMED].val) {
	    fprintf(stdout, "clause forward subsumed: ");
	    print_clause(stdout, c); fflush(stdout);
	    }

	if (Flags[BASIC_PARAMOD].val) {
	    if (Internal_flags[AC_PRESENT]) {
		Literal_ptr l;
		for (l = subsumer->literals; l; l = l->next)
		    clear_all_nonbasic(l->atom);
		}
	    else
		intersect_nonbasic_marks(subsumer, c);
	    }

	Stats[input ? CLAUSES_FORWARD_SUBSUMED_INPUT
	            : CLAUSES_FORWARD_SUBSUMED]++;
	zap_clause(c);
	c = NULL;
	}
    else {
	renumber_vars(c);	/* This can make it non-ac-canonical. */

	if (Internal_flags[INTERP_PRESENT]) {
	    CLOCK_START(SEMANTICS_TIME)
	    c->interpreted_value = eval_clause(c, Interpretation);
	    CLOCK_STOP(SEMANTICS_TIME)

	    if (!input && 
		!check_semantic_inference(c, Interpretation, Id_table)) {
		Stats[SEMANTIC_PARAMOD_PRUNES]++;
		zap_clause(c);
		c = NULL;
		}
	    }
	
	if (c) {
	    /* Keep the clause. */

	    ac_canonical(c->literals->atom);
	    clause_up_pointers(c);
	    c->id = ++Id_count;
	    store_clause_by_id(c, Id_table);
	    Stats[input ? CLAUSES_KEPT_INPUT : CLAUSES_KEPT]++;
	    if (input || Flags[PRINT_KEPT].val) {
		fprintf(stdout, "\n** KEPT: ");
		print_clause(stdout, c);
		/* p_term_basic(c->literals->atom); */
		fflush(stdout);
		}
	    
	    check_for_proof(c);
	    }
	}
    return(c);
}  /* process_clause */

/*************
 *
 *    process_and_store_clause(c, input_flag, lst)
 *
 *************/

void process_and_store_clause(Clause_ptr c, int input, List_ptr lst)
{
    c = process_clause(c, input);

    if (c) {

	CLOCK_START(STORE_TIME)
	if (lst == Sos) {
	    insert_clause_by_weight(c, Sos);
	    Stats[SOS_SIZE]++;
	    }
	else {
	    list_append(c, Usable);   
	    Stats[USABLE_SIZE]++;
	    }

	if (lst == Usable || Flags[PARA_PAIRS].val)
	    index_for_paramod(c, 1);

	index_for_bd(c, 1);
	index_for_sub(c, 1);
	index_bs_conflict(c, 1);
	CLOCK_STOP(STORE_TIME)

	if (eq_can_be_demod(c->literals)) {
	    new_demodulator(c, input);
	    if (!Flags[DELAY_BACK_DEMOD].val && !Flags[NO_DEMODULATION].val)
		back_demodulate(c, input);
	    }
	if (eq_lit(c->literals) && !term_oriented(c->literals->atom)) {
	    Clause_ptr d = flip_eq_unit(c);
	    /* We will probably flip the flipped version, but
	     * subsumption will take care of that.
	     */
	    process_and_store_clause(d, input, lst);
	    }
	}
}  /* process_and_store_clause */

/*************
 *
 *   process_derived_clause()
 *
 *************/

void process_derived_clause(Clause_ptr c)
{
    process_and_store_clause(c, 0, Sos);
}  /* process_derived_clause */

/*************
 *
 *    find_given_clause()
 *
 *    Sos is kept sorted by weight.
 *
 *************/

static Clause_ptr find_given_clause(void)
{
    Clause_ptr c;

    if (!Sos->first)
        c = NULL;
    else {
	int r;
	r = Parms[PICK_GIVEN_RATIO].val;
	if (r == -1) {
	    /* Default: smallest weight (should be the first). */
	    c = Sos->first->c;
	    }
	else {
	    /* Ratio strategy! */
	    if (Stats[GIVEN] % (r+1) == 0) {
		/* Clause with smallest id. */
		List_pos_ptr p;
		c = NULL;
		for (p = Sos->first; p; p = p->next) {
		    if (!c || c->id > p->c->id)
			c = p->c;
		    }
		}
	    else
		c = Sos->first->c;
	    }
	}
    return(c);
}  /* find_given_clause */

/*************
 *
 *    find_pair()
 *
 *************/

static void find_pair(Clause_ptr *c1p, Clause_ptr *c2p)
{
    int r = Parms[PICK_GIVEN_RATIO].val;

    if (r == -1 || (Stats[GIVEN] % (r+1) != 0)) {
	/* Get the best available pair. */
	retrieve_pair(Para_pairs, c1p, c2p);
	while (*c1p && pair_already_used(*c1p, 0,
					 *c2p, 0,
					 Para_pairs_breadth)) {
#if 0
printf("BEST (used): %d %d.\n", *c1p ? (*c1p)->id : 0, *c2p ? (*c2p)->id : 0);
#endif	
	    retrieve_pair(Para_pairs, c1p, c2p);
	    }
#if 0
printf("BEST: %d %d.\n", *c1p ? (*c1p)->id : 0, *c2p ? (*c2p)->id : 0);
#endif	
	}
    else {
	/* Get the oldest available pair. */
	retrieve_pair(Para_pairs_breadth, c1p, c2p);
	while (*c1p && pair_already_used(*c1p, (*c1p)->weight,
					 *c2p, (*c2p)->weight,
					 Para_pairs)) {
#if 0
printf("breadth (used): %d %d.\n", *c1p ? (*c1p)->id : 0, *c2p ? (*c2p)->id : 0);
#endif	
	    retrieve_pair(Para_pairs_breadth, c1p, c2p);
	    }
#if 0
printf("breadth: %d %d.\n", *c1p ? (*c1p)->id : 0, *c2p ? (*c2p)->id : 0);
#endif	
	}

}  /* find_pair */

/*************
 *
 *   inferences_to_make()
 *
 *************/

int inferences_to_make(void)
{
    if (Flags[PARA_PAIRS].val)
	return(pairs_available(Para_pairs));
    else
	return(Sos->first != NULL);
}  /* inferences_to_make */

/*************
 *
 *   sos_test()
 *
 *************/

int sos_test(Clause_ptr c1, Clause_ptr c2)
{
    /* We can't just check that either is in Sos, because the
     * other might be in Disabled.
     */

    int c1_usable = list_member(c1, Usable);
    int c2_usable = list_member(c2, Usable);
    int c1_sos = list_member(c1, Sos);
    int c2_sos = list_member(c2, Sos);

    return((c1_usable || c1_sos) &&
	   (c2_usable || c2_sos) &&
	   (c1_sos || c2_sos));
}  /* sos_test */

/*************
 *
 *   make_inferences()
 *
 *   Assume that there are inferences to make.
 *
 *************/

void make_inferences(void)
{
    if (Flags[PARA_PAIRS].val) {
	Clause_ptr c1, c2;

	find_pair(&c1, &c2);
	if (c1) {
#if 0
	    printf("PAIR: %3d:%2d:%d   %3d:%2d:%d\n", c1->id, c1->weight, !list_member(c1,Disabled), c2->id, c2->weight, !list_member(c2,Disabled));
#endif
	    if (Stats[GIVEN] % Parms[REPORT_GIVEN].val == 0) {
		output_stats(stdout);
		fflush(stdout);
		}
	    if (sos_test(c1, c2)) {
		int t0, t1, g0, g1;
		if (Flags[PRINT_PAIRS].val) {
		    t0 = run_time(); g0 = Stats[CLAUSES_GENERATED];
		    printf("\nClause pair #%d: (pr_wt=%d) ",
			   Stats[GIVEN],
			   c1->weight + c2->weight);
		    print_clause(stdout, c1);
		    printf("                       ");
		    print_clause(stdout, c2);
		    fflush(stdout);
		    }
		para_from_into(c1, c2, ALL);
		para_from_into(c2, c1, ALL_BUT_TOP);

		if (Flags[PRINT_PAIRS].val) {
		    t1 = run_time(); g1 = Stats[CLAUSES_GENERATED];
		    printf("  The prev. pair used %.2f sec. and generated %d clauses.\n",
			   (t1-t0)/1000., g1-g0);
		    }
		}
	    }
	}

    else {  /* given clause */
	Clause_ptr given;

	given = find_given_clause();
	list_remove(given, Sos);
	Stats[SOS_SIZE]--;

	list_append(given, Usable);
	Stats[USABLE_SIZE]++;
	index_for_paramod(given, 1);

	if (Flags[PRINT_GIVEN].val) {
	    fprintf(stdout, "\ngiven clause #%d: ", Stats[GIVEN]);
	    print_clause(stdout, given);
	    fflush(stdout);
	    }
        if (Flags[DELAY_BACK_DEMOD].val && list_member(given, Demodulators)) {
	    back_demodulate(given, 0);
	    }

	if (Flags[INDEX_PARAMOD].val) {
	    para_with_indexing(given, Fpa_alphas, Fpa_terms);
	    }
	else {
	    List_pos_ptr p;
	    for (p = Usable->first; p; p = p->next) {
		para_from_into(given, p->c, ALL);
		para_from_into(p->c, given, ALL_BUT_TOP);
		}
	    }
        free_dangle_list();
	}
}  /* make_inferences */

/*************
 *
 *   eq_prover()
 *
 *   Return the exit value.
 *
 *************/

int eq_prover(void)
{
    List_ptr usable, sos, passive, demodulators;
    int  rc;

    /* Initialize */

    if (Internal_flags[BT_UNIFY]) {
	Flags[INDEX_PARAMOD].val = 0;
	Flags[INDEX_BD].val = 0;
	Flags[INDEX_FS].val = 0;
	Flags[INDEX_BS].val = 0;
	Flags[INDEX_CONFLICT].val = 0;
	/* (INDEX_BT_DEMOD is OK with BT_UNIFY) */
	/* (without BT_UNIFY, demodulators always indexed) */
	/* (if any ac or c symbols, BT_UNIFY will be set) */
	}

    Id_count = 0;
    Disabled = get_list();
    Id_table = init_clause_table_id();

    if (!Internal_flags[BT_UNIFY] || Flags[INDEX_BT_DEMOD].val)
	Discrim_demod = discrim_init();

    if (Flags[INDEX_FS].val) {
	Discrim_pos = discrim_init();
	Discrim_neg = discrim_init();
	}

    if (Flags[INDEX_PARAMOD].val) {
	Fpa_alphas = fpa_init(Parms[FPA_DEPTH].val);
	Fpa_terms = fpa_init(Parms[FPA_DEPTH].val);
	}

    if (Flags[INDEX_BD].val)
	Fpa_bd = fpa_init(Parms[FPA_DEPTH].val);

    if (Flags[INDEX_BS].val || Flags[INDEX_CONFLICT].val) {
	Fpa_pos = fpa_init(Parms[FPA_DEPTH].val);
	Fpa_neg = fpa_init(Parms[FPA_DEPTH].val);
	}

    if (Flags[PARA_PAIRS].val) {
	Para_pairs = init_pair_index(Parms[PAIR_INDEX_SIZE].val);
	Para_pairs_breadth = init_pair_index(1);
	}

    /* Read clauses into temporary lists. */

    usable = get_list();
    sos = get_list();
    demodulators = get_list();
    passive = get_list();

    CLOCK_START(INPUT_TIME)
    read_lists(stdin, usable, sos, passive, demodulators);
    auto_lex_order();  /* For symbols not explicitly set with lex([]). */

    if (Stats[INPUT_ERRORS] != 0) {
        fprintf(stderr, "\nInput errors were found.\007\n\n");
        printf("Input errors were found.\n");
        return(INPUT_ERROR_EXIT);
        }
    /* Process input and assign to ordinary lists. */
    process_all_input(usable, sos, passive, demodulators);
    CLOCK_STOP(INPUT_TIME)

#if 0
p_pair_index(Para_pairs);
p_pair_index(Para_pairs_breadth);
#endif
    
    /* --------- HERE IS THE MAIN LOOP --------- */

    while (Stats[GIVEN] < Parms[MAX_GIVEN].val &&
           run_time()/1000 < Parms[MAX_SECONDS].val &&
           inferences_to_make()) {

	Stats[GIVEN]++;
        make_inferences();

        }

    /* --------- END OF THE MAIN LOOP --------- */

    if (!inferences_to_make()) {
        printf("\nno more inferences to make.\n");
        fprintf(stderr,"\nno more inferences to make.\n\007");
        rc = SOS_EMPTY_EXIT;
        }
    else if (Stats[GIVEN] >= Parms[MAX_GIVEN].val) {
        printf("\nmax_given.\n");
        fprintf(stderr,"\nmax_given.\n\007");
        rc = MAX_GIVEN_EXIT;
        }
    else {
        printf("\nmax_seconds.\n");
        fprintf(stderr,"\nmax_seconds.\n\007");
        rc = MAX_SECONDS_EXIT;
        }
    return(rc);

}  /* eq_prover */

/*************
 *
 *    has_an_instance(t, alpha)
 *
 *************/

static int has_an_instance(Term_ptr t, Term_ptr alpha)
{
    int ok;
    Context_ptr c;

    c = get_context();

    if (Internal_flags[BT_UNIFY]) {
	Bt_node_ptr bt_position;
        bt_position = match_bt_first(alpha, c, t, 1);
        if (bt_position) {
	    match_bt_cancel(bt_position);
	    ok = 1;
	    }
	else
	    ok = 0;
	}
    else {
	Trail_ptr tr = NULL;
        ok = match(alpha, c, t, &tr);
	if (ok)
	    clear_subst_1(tr);
	}
    
    if (!ok) {
	int i;
	for (i = 0, ok = 0; i < t->arity && !ok; i++)
	    ok = has_an_instance(t->args[i], alpha);
	}
    free_context(c);
    return(ok);
}  /* has_an_instance */

/*************
 *
 *    get_bd_clauses(demod)
 *
 *    Return the list of clauses that can be rewritten with demod.
 *
 *************/

List_ptr get_bd_clauses(Clause_ptr demod)
{
    List_ptr lst;
    Term_ptr alpha;
    Clause_ptr c;

    alpha = demod->literals->atom->args[0];
    lst = get_list();

    if (Flags[INDEX_BD].val) {
	Fpa_pos_ptr pos;
	Term_ptr t;
	Context_ptr subst;

        subst = get_context();

	t = fpa_retrieve_first(alpha, Fpa_bd, INSTANCE, subst,
			       (Context_ptr) NULL, &pos);
	while (t) {
	    c = t->containing_clause;
	    if (c != demod && !list_member(c, lst))
		list_append(c, lst);
	    t = fpa_retrieve_next(pos);
	    }
        free_context(subst);

	}
    else {
	int i;
	List_pos_ptr p;

	for (i = 0; i < 2; i++) {
	    p = (i == 0 ? Usable->last : Sos->last);
	    while (p) {
		c = p->c;
		if (c != demod && has_an_instance(c->literals->atom, alpha))
		    list_append(c, lst);
		p = p->prev;
		}
	    }
	}

    return(lst);
}  /* get_bd_clauses */

/*************
 *
 *   index_bd_terms()
 *
 *************/

static void index_bd_terms(Term_ptr t, Fpa_index_ptr idx, int insert)
{
    int i;
    if (!VARIABLE(t)) {
	if (insert)
	    fpa_insert(t, idx);
        else
	    fpa_delete(t, idx);
	}
    for (i = 0; i < t->arity; i++)
	index_bd_terms(t->args[i], idx, insert);
}  /* index_into_terms */

/*************
 *
 *   index_for_bd()
 *
 *************/

void index_for_bd(Clause_ptr c, int insert)
{
    if (Flags[INDEX_BD].val) {
	Literal_ptr l;
	int i;
	for (l = c->literals; l; l = l->next) {
	    for (i = 0; i < l->atom->arity; i++)
		index_bd_terms(l->atom->args[i], Fpa_bd, insert);
	    }
	}
}  /* index_for_bd */

/*************
 *
 *   index_bs_conflict()
 *
 *************/

void index_bs_conflict(Clause_ptr c, int insert)
{
    if (Flags[INDEX_BS].val || Flags[INDEX_CONFLICT].val) {

	Literal_ptr l;

	for (l = c->literals; l; l = l->next) {
	    if (insert)
		fpa_insert(l->atom, LIT_INDEX(l));
	    else
		fpa_delete(l->atom, LIT_INDEX(l));
	    }
	}
}  /* index_bs_conflict */

/*************
 *
 *   index_for_sub()
 *
 *************/

void index_for_sub(Clause_ptr c, int insert)
{
    if (Flags[INDEX_FS].val) {
	Literal_ptr l;

	for (l = c->literals; l; l = l->next) {
	    if (insert)
		discrim_insert(l->atom, FS_INDEX(l), (void *) c);
	    else
		discrim_delete(l->atom, FS_INDEX(l), (void *) c);
	    }
	}
}  /* index_for_sub */

/*************
 *
 *   index_demod()
 *
 *************/

void index_demod(Clause_ptr c, int insert)
{
    Term_ptr alpha;

    alpha = c->literals->atom->args[0];
    if (Internal_flags[BT_UNIFY]) {
	if (Flags[INDEX_BT_DEMOD].val) {
	    if (insert)
		discrim_wild_insert(alpha, Discrim_demod, (void *) c);
	    else
		discrim_wild_delete(alpha, Discrim_demod, (void *) c);
	    }
	}
    else {
	if (insert)
	    discrim_insert(alpha, Discrim_demod, (void *) c);
	else
	    discrim_delete(alpha, Discrim_demod, (void *) c);
	}
}  /* index_demod */

/*************
 *
 *   index_into_terms()
 *
 *************/

static void index_into_terms(Term_ptr t, Fpa_index_ptr idx, int insert)
{
    int i;
    if (!VARIABLE(t)) {

	if (insert)
	    fpa_insert(t, idx);
        else
	    fpa_delete(t, idx);
	for (i = 0; i < t->arity; i++)
	    index_into_terms(t->args[i], idx, insert);
	}
}  /* index_into_terms */

/*************
 *
 *   index_for_paramod()
 *
 *   FOR NOW, index nonvariable terms (except right sides of
 *   pos eqs) for "from" paramod, and
 *   index left sides of eqs for "into" paramod.
 *
 *************/

void index_for_paramod(Clause_ptr c, int insert)
{
    if (Flags[PARA_PAIRS].val) {
	if (insert) {
	    insert_pair_index(c, c->weight, Para_pairs);
	    insert_pair_index(c, 0, Para_pairs_breadth);
	    }
	else {
	    delete_pair_index(c, c->weight, Para_pairs);
	    delete_pair_index(c, 0, Para_pairs_breadth);
	    }
	}
    else if (Flags[INDEX_PARAMOD].val) {
	Literal_ptr l;
	int i;
	for (l = c->literals; l; l = l->next) {
	    if (pos_eq_lit(l)) {
		if (insert)
		    fpa_insert(l->atom->args[0], Fpa_alphas);
		else
		    fpa_delete(l->atom->args[0], Fpa_alphas);
	    
		index_into_terms(l->atom->args[0], Fpa_terms, insert);
		}
	    else
		for (i = 0; i < l->atom->arity; i++)
		    index_into_terms(l->atom->args[i], Fpa_terms, insert);
	    }
	}
}  /* index_for_paramod */

/*************
 *
 *     insert_clause_by_weight(c, l)
 *
 *************/

void insert_clause_by_weight(Clause_ptr c, List_ptr l)
{
    List_pos_ptr p;

    for (p = l->first; p && c->weight >= p->c->weight; p = p->next);
    if (p)
	list_insert_before(c, p);
    else
	list_append(c, l);

}  /* insert_clause_by_weight */

/*************
 *
 *   disable_clause()
 *
 *   Remove from Usable, Sos, Demodulators, and/or Passive,
 *   unindex, and append to Disabled (if not already there).
 *
 *************/

void disable_clause(Clause_ptr c)
{
    CLOCK_START(DISABLE_TIME)
    if (list_member(c, Usable)) {
	index_for_bd(c, 0);
	index_for_sub(c, 0);
	index_bs_conflict(c, 0);
	index_for_paramod(c, 0);
	list_remove(c, Usable);
	Stats[USABLE_SIZE]--;
	}
    if (list_member(c, Sos)) {
	index_for_bd(c, 0);
	index_for_sub(c, 0);
	index_bs_conflict(c, 0);
	if (Flags[PARA_PAIRS].val)
	    index_for_paramod(c, 0);
	list_remove(c, Sos);
	Stats[SOS_SIZE]--;
	}
    if (list_member(c, Demodulators)) {
	index_demod(c, 0);
	list_remove(c, Demodulators);
	Stats[DEMODULATORS_SIZE]--;
	}
    if (list_member(c, Passive)) {
	index_for_sub(c, 0);
	index_bs_conflict(c, 0);
	list_remove(c, Passive);
	Stats[PASSIVE_SIZE]--;
	}
    if (!list_member(c, Disabled)) {
	Literal_ptr lit;
	for (lit = c->literals; lit; lit = lit->next)
	    mark_term_disabled(lit->atom);  
	
	list_append(c, Disabled);
	Stats[DISABLED_SIZE]++;
	}
    CLOCK_STOP(DISABLE_TIME)
}  /* disable_clause */

/*************
 *
 *   simplifiable_subst()
 *
 *************/

int simplifiable_subst(Context_ptr subst)
{
    int i, simp;
    Term_ptr v, t;
    void *demods;

    CLOCK_START(PRIME_PARAMOD_TIME);

    if (Internal_flags[BT_UNIFY]) {
	if (Flags[INDEX_BT_DEMOD].val)
	    demods = Discrim_demod;
	else
	    demods = Demodulators;
	}
    else
	demods = NULL;

    v = get_term(0);
    for (i = 0; i < MAX_VARS; i++) {
	if (subst->terms[i]) {
	    v->symbol = i;  /* This makes it variable vi. */
	    t = apply(v, subst);
	    ac_canonical(t);

	    if (Internal_flags[BT_UNIFY])
		simp = simplifiable_bt(t, 0, demods);
	    else
		simp = simplifiable(t, Discrim_demod);

	    if (simp) {
		/* printf("Simplifiable: "); p_term(t); */
		zap_term(v);
		zap_term(t);
		CLOCK_STOP(PRIME_PARAMOD_TIME);
		return(1);
		}
	    zap_term(t);
	    }
	}
    zap_term(v);
    CLOCK_STOP(PRIME_PARAMOD_TIME);
    return(0);
}  /* simplifiable_subst */

