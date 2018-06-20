#ifndef TP_FPA_H
#define TP_FPA_H

/********** FPA/Path indexing ***********/

/* types of retrieval */

#define MORE_GEN   1
#define INSTANCE   2
#define UNIFY      3

#define FPA_SIZE 500  /* size of FPA hash tables */

struct fpa_index {
    int depth;       /* maximum term depth to which indexing is applied */
    struct fpa_head *table[FPA_SIZE];
    };

typedef struct fpa_index *Fpa_index_ptr;

struct fpa_head {            /* head of an FPA list */
    struct gen_ptr *list;         /* list of objects with path */
    struct fpa_head *next;        /* next FPA list */
    int *path;
    };

typedef struct fpa_head *Fpa_head_ptr;

struct fpa_tree {     /* for constructing FPA/path lookup tree */
    struct gen_ptr *list;    /* for leaves only */
    struct fpa_tree *left;    /* for INTERSECT and UNION nodes */
    struct fpa_tree *right;   /* for INTERSECT and UNION nodes */
    struct term *left_term;   /* for UNION nodes only */
    struct term *right_term;  /* for UNION nodes only */
    int type;                 /* INTERSECT, UNION, LEAF */
    int *path;                /* for debugging only */
    };

typedef struct fpa_tree *Fpa_tree_ptr;

struct fpa_pos {
    struct term *query_term;
    struct term *found_term;
    int type;
    struct fpa_tree *tree;         /* position in FPA query */
    struct context *subst_query;
    struct context *subst_found;
    struct trail *tr;              /* trail for unbinding variables */
    struct bt_node *bt_position;   /* for backtracking unify/match */
    struct fpa_pos *next;          /* for avail list only */
    };

typedef struct fpa_pos *Fpa_pos_ptr;


/* function prototypes from fpa.c */

void print_fpa_mem(FILE *fp, int heading);

void p_fpa_mem();

Fpa_index_ptr fpa_init(int depth);

void fpa_dealloc(Fpa_index_ptr p);

void fpa_insert(Term_ptr t, Fpa_index_ptr index);

void fpa_delete(Term_ptr t, Fpa_index_ptr index);

void print_fpa_index(FILE *fp, Fpa_index_ptr p);

void p_fpa_index(Fpa_index_ptr p);

void print_prop_tree(FILE *fp, Fpa_tree_ptr n, int depth);

void p_prop_tree(Fpa_tree_ptr n);

void print_path(FILE *fp, int *path);

void p_path(int *path);

Term_ptr fpa_retrieve_first(Term_ptr t, Fpa_index_ptr index, int type,
      Context_ptr subst_query, Context_ptr subst_found, Fpa_pos_ptr *ppos);

Term_ptr fpa_retrieve_next(Fpa_pos_ptr pos);

void fpa_cancel(Fpa_pos_ptr pos);

Term_ptr fpa_bt_retrieve_first(Term_ptr t, Fpa_index_ptr index, int type,
       Context_ptr subst_query, Context_ptr subst_found, Fpa_pos_ptr *ppos);

Term_ptr fpa_bt_retrieve_next(Fpa_pos_ptr pos);

void fpa_bt_cancel(Fpa_pos_ptr pos);

void print_fpa_index_summary(FILE *fp, Fpa_index_ptr p);

#endif  /* ! TP_FPA_H */
