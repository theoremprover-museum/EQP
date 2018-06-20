#ifndef TP_DISCRIM_H
#define TP_DISCRIM_H

/******** Discrimination Tree Indexing, more general terms. ******/


#define AC_ARG_TYPE     10  /* WARNING---these are used along with NAME, */
#define AC_NV_ARG_TYPE   9  /* COMPLEX, and VARIABLE in Header.h, so     */
                            /* they must be different from those.        */

struct discrim {
    struct discrim *next;  /* sibling */
    union {
        struct discrim *kids;    /* for internal nodes */
        struct gen_ptr *data;    /* for leaves */
        } u;
    Symbol_type symbol;  /* variable number or symbol number (as in term) */
    char ac_type;        /* for ac indexing */
    };

typedef struct discrim *Discrim_ptr;

struct flat {
    struct term *t;
    struct flat *prev, *next, *last;
    struct discrim *alternatives;
    int bound;
    int varnum;
    int place_holder;
    int num_ac_args;
    int num_ac_nv_args;
    };

typedef struct flat *Flat_ptr;

struct discrim_pos {         /* to save position in set of subsuming terms */
    struct context *subst;   /* substitution */
    struct gen_ptr *data;    /* ident. terms from leaf of discrim tree */
    struct flat *f;          /* stack of states for backtracking */
    };

typedef struct discrim_pos *Discrim_pos_ptr;



/* function prototypes from discrim.c */

void print_discrim_mem(FILE *fp, int heading);

void p_discrim_mem();

Discrim_ptr discrim_init();

void discrim_dealloc(Discrim_ptr d);

void print_discrim_index(FILE *fp, Discrim_ptr d);

void p_discrim_index(Discrim_ptr d);

void discrim_insert(Term_ptr t, Discrim_ptr root, void *object);

void discrim_delete(Term_ptr t, Discrim_ptr root, void *object);

void *discrim_retrieve_first(Term_ptr t, Discrim_ptr root, Context_ptr subst, Discrim_pos_ptr *ppos);

void *discrim_retrieve_next(Discrim_pos_ptr pos);

void discrim_cancel(Discrim_pos_ptr pos);

void print_discrim_wild_index(FILE *fp, Discrim_ptr d);

void p_discrim_wild_index(Discrim_ptr d);

Discrim_ptr discrim_wild_insert_ac(Term_ptr t, Discrim_ptr d);

void discrim_wild_insert(Term_ptr t, Discrim_ptr root, void *object);

void discrim_wild_delete(Term_ptr t, Discrim_ptr root, void *object);

void *discrim_wild_retrieve_first(Term_ptr t, Discrim_ptr root, Discrim_pos_ptr *ppos);

void *discrim_wild_retrieve_next(Discrim_pos_ptr pos);

void discrim_wild_cancel(Discrim_pos_ptr pos);

#endif  /* ! TP_DISCRIM_H */
