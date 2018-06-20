#ifndef TP_AC_H
#define TP_AC_H

/******** Type of function symbol, types of unification ********/

#define COMMUTE       1
#define ASSOC_COMMUTE 2

/******** AC Unification ********/

#define MAX_COEF    250
#define MAX_BASIS   100   /* must be <= MAX_VARS, because rows are indexed. */
#define MAX_COMBOS  200   /* for superset-restricted AC unif. */
#define MAX_AC_ARGS 500   /* 2500 is enough for ALU problem */

/******** bind a variable, record binding in a bt_node ********/

#define BIND_BT(i, c1, t2, c2, bt) {  \
    c1->terms[i] = t2; c1->contexts[i] = c2; \
    bt->varnum = i; bt->cb = c1; }

/******** bt_node -- (backtrack node) for unification and matching ********/

/* In general, backtracking is handled by constructing and traversing     */
/* a tree built from nodes like this, rather than by recursively passing  */
/* the alternatives as an argument.                                       */

struct bt_node {

    struct bt_node *parent, *next, *prev, *first_child, *last_child;

    struct term *t1, *t2;         /* terms being unified or matched */
    struct context *c1, *c2;      /* respective contexts for variables */
                                  /* (for match, t1 is pattern, c2 is NULL) */

    int varnum;                   /* for unbinding when backtracking */
    struct context *cb;           /* for unbinding when backtracking */

    int alternative;              /* type of alternative (position) */

    /* for commutative unification */
    int flipped;
    struct bt_node *position_bt;  /* in sequence of alternatives */

    /* for AC unification */
    struct ac_position *ac;       /* position in sequence of AC unifiers */

    /* for AC matching */
    struct ac_match_pos *acm;     /* position in sequence of AC matchers */
    int partial;                  /* partial match for this pair */
   
    };

typedef struct bt_node *Bt_node_ptr;

/******** ac_position -- position in sequence of AC unifiers ********/

struct ac_position {
    int m, n, num_basis;              /* # of coefficients and size of basis */
    int basis[MAX_BASIS][MAX_COEF];
    int constraints[MAX_COEF];        /* 0 for vars, else symbol number */
    struct term *args[MAX_COEF];
    struct context *arg_contexts[MAX_COEF];
    struct term *new_terms[MAX_COEF]; /* substitution terms */
    int combo[MAX_BASIS];             /* current subset of basis solutions */
    int sum[MAX_COEF];                /* solution corresponding to combo */
    struct term *basis_terms[MAX_BASIS][MAX_COEF];
    struct context *c3;               /* table for new variables */
    struct bt_node *sub_position;     /* position in sub-unification problem */
    int superset_limit;               /* for superset-restricted AC unif. */
    int combos[MAX_COMBOS][MAX_BASIS];/* for superset-restricted AC unif. */
    int combos_remaining;             /* for superset-restricted AC unif. */
    struct ac_position *next;         /* for avail list only */
    };

typedef struct ac_position *Ac_position_ptr;

/******** ac_match_pos -- position in sequence of AC matchers ********/

struct ac_match_pos {
    struct term *t1, *t2;  /* t1 is pattern, t2 is subject */
    struct context *c1;    /* context for variables in t1  */
    int n1;                /* number of arguments in t1 */ 
    int n2;                /* size of set of set of args in t2 */
    struct term *args1[MAX_AC_ARGS], *args2[MAX_AC_ARGS];  /* the arguments */
           /* position in sequence of matches for complex args of args2 */
    struct bt_node *bt1[MAX_AC_ARGS];
           /* flags indicating which of args1 have been matched */
    int match1[MAX_AC_ARGS];
           /* integer indicating how many of each of args2 have been matched */
    int match2[MAX_AC_ARGS];
    int mults2[MAX_AC_ARGS];  /* multiplicities for args2 */
           /* indicates which of args2 are matched by bound vars in args1 */
    int bound_matches[MAX_AC_ARGS], bound_count;
    int last_a1_functor;   /* position of last non-variable arg in args1 */
           /* list of backtrack positions for free variables of args1 */
    struct ac_match_free_vars_pos *free_first, *free_last;
           /* # args of unmatched term---used for partial match */
    int partial_term_size;
    struct ac_match_pos *next;  /* for avail list only */
    };

typedef struct ac_match_pos *Ac_match_pos_ptr;

/******* backtrack node for free variables of args1 ********/

struct ac_match_free_vars_pos {
    int varnum;                 /* the index of the free variable */
    int coef;                   /* # of occurrences of the var in args1 */
    int targets[MAX_AC_ARGS];   /* terms in args2 that can go with variable */
    int n;                      /* number of tragets*/
    int combo[MAX_AC_ARGS];     /* current subset of the targets */
    struct ac_match_free_vars_pos *prev, *next;
    };

typedef struct ac_match_free_vars_pos *Ac_match_free_vars_pos_ptr;



/* function prototypes from btm.c */

Bt_node_ptr get_bt_node(void);

void free_bt_node(Bt_node_ptr p);

Ac_position_ptr get_ac_position(void);

void free_ac_position(Ac_position_ptr p);

void print_ac_mem(FILE *fp, int heading);

void p_ac_mem();

void flatten(Term_ptr t, Term_ptr *a, int *ip);

void flatten_mult(Term_ptr t, Term_ptr *a, int *m, int *ip, int *totp,
		  int (*comp_proc) (void *, void *));

void elim_com(Term_ptr *a1, Term_ptr *a2, int n1, int n2,
	      int (*comp_proc) (void *, void *));

void ac_mult(Term_ptr *a, int *mults, int *np, int (*comp_proc) (void *, void *));

void right_associate(Term_ptr t);

void ac_canonical(Term_ptr t);

int check_ac_canonical(Term_ptr t);

void p_acm(Ac_match_pos_ptr ac);

Bt_node_ptr match_bt_first(Term_ptr t1, Context_ptr c1, Term_ptr t2,
			   int partial);

Bt_node_ptr match_bt_next(Bt_node_ptr bt1);

void match_bt_cancel(Bt_node_ptr bt);


/* function prototypes from btu.c */

Bt_node_ptr unify_bt_first(Term_ptr t1, Context_ptr c1, Term_ptr t2, Context_ptr c2);

Bt_node_ptr unify_bt_next(Bt_node_ptr bt1);

void unify_bt_cancel(Bt_node_ptr bt);

Bt_node_ptr unify_bt_guts(Bt_node_ptr bt1);

Bt_node_ptr unify_bt_backup(Bt_node_ptr bt1);

int unify_commute(Term_ptr t1, Context_ptr c1, Term_ptr t2, Context_ptr c2, Bt_node_ptr bt);

void p_bt_tree(Bt_node_ptr bt, int n);


/* function prototypes from ac.c */

int unify_ac(Term_ptr t1, Context_ptr c1,
	     Term_ptr t2, Context_ptr c2, Bt_node_ptr bt);

void unify_ac_cancel(Ac_position_ptr ac);

void p_ac_position(Ac_position_ptr ac, int n);

/* function prototypes from dioph.c */

void p_ac_basis(int (*basis)[MAX_COEF], int num_basis, int m, int n);

int dio(int *ab, int m, int n, int *constraints, int (*basis)[MAX_COEF], int *num_basis);

int next_combo_a1(int length, int (*basis)[MAX_COEF], int num_basis, int *constraints, int *combo, int *sum, int start_flag, int (*combos)[MAX_BASIS], int *np, int ss_parm);

int next_combo_a(int length, int (*basis)[MAX_COEF], int num_basis, int *constraints, int *combo, int *sum, int start_flag);



#endif  /* ! TP_AC_H */
