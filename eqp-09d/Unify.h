#ifndef TP_UNIFY_H
#define TP_UNIFY_H

/******** dereference a variable ********/

#define DEREFERENCE(t, c) { int i; \
    while (c && t->symbol >= 0 && c->terms[i = t->symbol]) \
    { t = c->terms[i]; c = c->contexts[i]; } } 

/******** bind a variable, record binding in a trail ********/

#define BIND_TR(i, c1, t2, c2, trp) { struct trail *tr; \
    c1->terms[i] = t2; c1->contexts[i] = c2; \
    tr = get_trail(); tr->varnum = i; tr->context = c1; \
    tr->next = *trp; *trp = tr; }

/******** context -- substitution table ********/

struct context {
    struct term *terms[MAX_VARS];
    struct context *contexts[MAX_VARS];
    int multiplier;  /* needed for apply, not for unify or match */
    struct term *partial_term;  /* for ac matching */
    };

typedef struct context *Context_ptr;

/******** trail -- to record binding, so that it can be undone ********/

struct trail {
    struct trail *next;
    struct context *context;
    int varnum;
    };

typedef struct trail *Trail_ptr;


/* function prototypes from unify.c */

Context_ptr get_context(void);

void free_context(Context_ptr p);

Trail_ptr get_trail(void);

void free_trail(Trail_ptr p);

void print_unify_mem(FILE *fp, int heading);

void p_unify_mem();

int unify(Term_ptr t1, Context_ptr c1,
          Term_ptr t2, Context_ptr c2, Trail_ptr *trp);

int occur_check(int vn, Context_ptr vc, Term_ptr t, Context_ptr c);

int match(Term_ptr t1, Context_ptr c1, Term_ptr t2, Trail_ptr *trp);

Term_ptr apply(Term_ptr t, Context_ptr c);

Term_ptr apply_substitute(Term_ptr beta, Context_ptr c_from, Term_ptr t,
			  Term_ptr into_term, Context_ptr c_into);

Term_ptr apply_basic(Term_ptr t, Context_ptr c);

Term_ptr apply_substitute_basic(Term_ptr beta, Context_ptr c_from, Term_ptr t,
			  Term_ptr into_term, Context_ptr c_into);

void clear_subst_2(Trail_ptr t1, Trail_ptr t2);

void clear_subst_1(Trail_ptr t1);

void print_context(FILE *fp, Context_ptr c);

void p_context(Context_ptr c);

void print_trail(FILE *fp, Trail_ptr t);

void p_trail(Trail_ptr t);

#endif  /* ! TP_UNIFY_H */
