#ifndef TP_INTERP_H
#define TP_INTERP_H

#define MAX_SYMBOLS   200
#define MAX_VARS_EVAL   8

struct interp {
    Term_ptr t;  /* the term representation (from which this was built) */
    int size;   /* domain size */
    /* Array of pointers to tables, one for each constant, function,
       or predicate symbol (indexed by symbol number).  The arity and
       print symbol can be obtained from the symbol number.
    */
    int *tables[MAX_SYMBOLS];
    };

typedef struct interp *Interp_ptr;


/* function prototypes from interp.c */

Interp_ptr init_interp(Term_ptr t);

void print_interp(FILE *fp, Interp_ptr p);

void p_interp(Interp_ptr p);

int eval_term_ground(Term_ptr t, Interp_ptr p, int *vals);

int eval_clause_ground(Clause_ptr c, Interp_ptr p, int *vals);

int eval_clause(Clause_ptr c, Interp_ptr p);

int check_semantic_inference(Clause_ptr c, Interp_ptr interpretation,
			     Gen_ptr_ptr *id_table);

int semantic_para_parents_check(Clause_ptr from, Clause_ptr into);

#endif  /* ! TP_INTERP_H */
