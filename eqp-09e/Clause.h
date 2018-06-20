#ifndef TP_CLAUSE_H
#define TP_CLAUSE_H

#define UNIT_CONFLICT_RULE  -1
#define PARA_RULE           -2
#define PARA_FX_RULE        -3
#define PARA_IX_RULE        -4
#define PARA_FX_IX_RULE     -5
#define BACK_DEMOD_RULE     -6
#define DEMOD_RULE          -7
#define COPY_RULE           -8
#define FLIP_RULE           -9

struct literal {
    struct literal *next;
    struct term *atom;
    char sign;
    char type;
    };

typedef struct literal *Literal_ptr;

struct clause {
    int id;
    int weight;
    int interpreted_value;
    struct literal *literals;
    struct gen_ptr *justification;
    struct list_pos *containers;
    };

typedef struct clause *Clause_ptr;

struct list {
    struct list_pos *first, *last;
    struct list *next;  /* for avail list */
    };

typedef struct list *List_ptr;

struct list_pos {
    struct list_pos *prev, *next, *nocc;
    struct list *container;
    struct clause *c;
    };

typedef struct list_pos *List_pos_ptr;

#define CLAUSE_TAB_SIZE 1000

/* Types of literal */

#define ORDINARY_LIT   1
#define EQ_LIT         2
#define ORD_EQ_LIT     3
#define ANS_LIT        4
#define CONSTRAINT_LIT 5


/* function prototypes from clause.c */

Literal_ptr get_literal(void);

void free_literal(Literal_ptr p);

Clause_ptr get_clause(void);

void free_clause(Clause_ptr p);

List_ptr get_list(void);

void free_list(List_ptr p);

void free_list_pos(List_pos_ptr p);

void print_clause_mem(FILE *fp, int heading);

void p_clause_mem();

Gen_ptr_ptr *init_clause_table_id(void);

void store_clause_by_id(Clause_ptr c, Gen_ptr_ptr *tab);

void delete_clause_by_id(Clause_ptr c, Gen_ptr_ptr *tab);

Clause_ptr find_clause_by_id(int id, Gen_ptr_ptr *tab);

void print_clause_table_id(FILE *fp, Gen_ptr_ptr *tab);

void p_clause_table_id(Gen_ptr_ptr *tab);

void zap_clause(Clause_ptr c);

int pos_clause(Clause_ptr c);

int neg_clause(Clause_ptr c);

int literal_count(Clause_ptr c);

Literal_ptr literal_i(Clause_ptr c, int i);

int unit_clause(Clause_ptr c);

Clause_ptr copy_clause(Clause_ptr c);

void copy_clause_nonbasic_marks(Clause_ptr c1, Clause_ptr c2);

Clause_ptr term_to_clause(Term_ptr t);

Term_ptr clause_to_term(Clause_ptr c);

int clause_ident(Clause_ptr c1, Clause_ptr c2);

void list_append(Clause_ptr c, List_ptr l);

void list_prepend(Clause_ptr c, List_ptr l);

void list_insert_before(Clause_ptr c, List_pos_ptr pos);

void list_insert_after(Clause_ptr c, List_pos_ptr pos);

int list_remove(Clause_ptr c, List_ptr l);

int list_remove_all(Clause_ptr c);

int list_member(Clause_ptr c, List_ptr l);

void print_list(FILE *fp, List_ptr l);

void p_list(List_ptr l);

void list_zap(List_ptr l);

void list_check(List_ptr l);

int clause_in_list(Clause_ptr c, List_ptr l);

void clause_up_pointers(Clause_ptr c);

void print_clause(FILE *fp, Clause_ptr c);

void p_clause(Clause_ptr c);

void get_ancestors(Clause_ptr c, Gen_ptr_ptr *id_table, Gen_ptr_ptr *pp);

void get_para_parents(Clause_ptr c, Gen_ptr_ptr *id_table,
		      Clause_ptr *p1, Clause_ptr *p2);

void print_proof(FILE *fp, Clause_ptr c1, Clause_ptr c2, Gen_ptr_ptr *id_table, int id_count);

int biggest_variable_in_clause(Clause_ptr c);

int distinct_vars(Clause_ptr c);

#endif  /* ! TP_CLAUSE_H */
