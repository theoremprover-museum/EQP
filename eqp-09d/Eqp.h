#ifndef TP_EQP_H
#define TP_EQP_H

/****************** Global variables for eqp (equality prover)  ************/

/* clause lists */

extern struct list *Usable;
extern struct list *Sos;
extern struct list *Demodulators;
extern struct list *Passive;
extern struct list *Deleted_clauses;

/* indexes */

extern struct discrim *Demod_index;
extern struct fpa_index *Fpa_terms;
extern struct fpa_index *Fpa_alphas;

/* To assign ID to clauses. */

extern int Id_count;

/* To access clauses by ID. */

extern  Gen_ptr_ptr *Id_table;     


/* function prototypes from eqp.c */

void init(void);

void read_preamble(void);

int subsumes(Clause_ptr c1, Clause_ptr c2);

Clause_ptr subsume_list(Clause_ptr c, List_ptr l);

Clause_ptr conflict_list(Clause_ptr c, List_ptr l);

Clause_ptr functional_subsume(Term_ptr alpha, Term_ptr beta,
			      Clause_ptr original, int depth);

Clause_ptr forward_subsume(Clause_ptr c);

void check_for_proof(Clause_ptr c);

void term_list_to_literal_list(Gen_ptr_ptr p, List_ptr l);

void read_lists(FILE *fp, List_ptr usable, List_ptr sos, List_ptr passive,
		List_ptr demodulators);

void process_all_input(List_ptr usable, List_ptr sos, List_ptr passive,
		       List_ptr demodulators);

void demodulate_cl(Clause_ptr c);

void back_demodulate(Clause_ptr demod, int input);

void new_demodulator(Clause_ptr demod, int input);

Clause_ptr process_clause(Clause_ptr c, int input);

void process_and_store_clause(Clause_ptr c, int input, List_ptr lst);

void process_derived_clause(Clause_ptr c);

int inferences_to_make(void);

int sos_test(Clause_ptr c1, Clause_ptr c2);

void make_inferences(void);

int eq_prover(void);

List_ptr get_bd_clauses(Clause_ptr demod);

void index_for_bd(Clause_ptr c, int insert);

void index_bs_conflict(Clause_ptr c, int insert);

void index_for_sub(Clause_ptr c, int insert);

void index_demod(Clause_ptr c, int insert);

void index_for_paramod(Clause_ptr c, int insert);

void insert_clause_by_weight(Clause_ptr c, List_ptr l);

Clause_ptr demodulate_given(Clause_ptr given);

void disable_clause(Clause_ptr c);

int simplifiable_subst(Context_ptr subst);

#endif  /* ! TP_EQP_H */
