#ifndef TP_TERM_H
#define TP_TERM_H

#define MAX_VARS 100   /* maximum # of distinct variables in clause */

typedef short Symbol_type;
typedef unsigned char Arity_type;
typedef unsigned char Bits_type;

struct term {
    Symbol_type symbol;
    Arity_type arity;
    Bits_type bits;
    unsigned long fpa_id;
    struct term **args;
    struct clause *containing_clause;
    };

typedef struct term *Term_ptr;

#define VARIABLE(t) ((t)->symbol >= 0)
#define CONSTANT(t) ((t)->symbol < 0 && (t)->arity == 0)
#define COMPLEX(t)  ((t)->arity > 0)

/* Bits field of term.
 * SCRATCH_BIT is by several operations to temporarily mark terms.
 * When using it, make sure that no other operation is using it, and
 * make sure to clear it when done. */

#define SCRATCH_BIT       01
#define ORIENTED_BIT      02  /* oriented eq. atom (in pos. or neg. lit) */
#define NONBASIC_BIT      04  /* blocked "into" term for "basic" paramod */
#define DISABLED_BIT     010  /* term occurs in disabled clause */

#define SET_BIT(bits, flag)    (bits = bits | flag)
#define CLEAR_BIT(bits, flag)  (bits = bits & ~flag)
#define TP_BIT(bits, flag)     (bits & flag)


/* function prototypes from term.c */

Term_ptr get_term(int arity);

void free_term(Term_ptr p);

void print_term_mem(FILE *fp, int heading);

void p_term_mem();

void zap_term(Term_ptr t);

int term_ident(Term_ptr t1, Term_ptr t2);

Term_ptr copy_term(Term_ptr t);

Term_ptr copy_term_bits(Term_ptr t);

int set_vars_term(Term_ptr t, char **varnames);

int set_vars(Term_ptr t);

int ground_term(Term_ptr t);

int biggest_variable(Term_ptr t);

int symbol_count(Term_ptr t);

int occurs_in(Term_ptr t1, Term_ptr t2);

Term_ptr build_binary_term(int sn, Term_ptr t1, Term_ptr t2);

void copy_nonbasic_marks(Term_ptr t1, Term_ptr t2);

void all_nonbasic(Term_ptr t);

void set_term_scratch(Term_ptr t);

void clear_term_scratch(Term_ptr t);

int term_scratch(Term_ptr t);

void set_term_oriented(Term_ptr t);

void clear_term_oriented(Term_ptr t);

int term_oriented(Term_ptr t);

void set_term_nonbasic(Term_ptr t);

void clear_term_nonbasic(Term_ptr t);

int term_nonbasic(Term_ptr t);

void set_term_disabled(Term_ptr t);

void clear_term_disabled(Term_ptr t);

int term_disabled(Term_ptr t);

void mark_term_disabled(Term_ptr t);

#endif  /* ! TP_TERM_H */
