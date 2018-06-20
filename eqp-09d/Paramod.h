#ifndef TP_PARAMOD_H
#define TP_PARAMOD_H

/* positions into which to paramodulate */

#define ALL         0
#define TOP_ONLY    1
#define ALL_BUT_TOP 2

/* function prototypes from paramod.c */

Gen_ptr_ptr para_parents(Clause_ptr from_cl, Clause_ptr into_cl,
			      int from_ext, int into_ext);

void paramod(Clause_ptr from, Term_ptr from_atom, Clause_ptr into,
	     Term_ptr into_atom, Term_ptr into_term, int where, int psn);

void para_from_into(Clause_ptr c, Clause_ptr d, int where);

int alpha_is_ac(Literal_ptr lit);

int eq_lit(Literal_ptr lit);

int pos_eq_lit(Literal_ptr lit);

Term_ptr extend_atom(Term_ptr atom);

void zap_extended_atom(Term_ptr atom);

int poly1(Term_ptr t);

int term_weight(Term_ptr t);

void orient_eq_literals(Clause_ptr c);

void renum_vars_recurse(Term_ptr t, int varnums[]);

void renumber_vars(Clause_ptr c);

void add_vars(Term_ptr t, int v[]);

int multi_superset(Term_ptr t1, Term_ptr t2);

int eq_can_be_demod(Literal_ptr lit);

int orient_literal_lrpo(Literal_ptr lit);

void para_with_indexing(Clause_ptr c, Fpa_index_ptr idx_alpha,
			Fpa_index_ptr idx_into);

void basic_paramod_marks_term(Term_ptr t);

void basic_paramod_marks(Clause_ptr c);

void intersect_nonbasic_marks(Clause_ptr old, Clause_ptr new);

int basic_para_check(Term_ptr into_term);

int ordered_para_check(Term_ptr from_atom, Term_ptr alpha, Term_ptr beta, 
		       Context_ptr from_subst);

int prime_para_check(Context_ptr from_subst, Context_ptr into_subst);

void p_term_basic(Term_ptr t);


#endif  /* ! TP_PARAMOD_H */

