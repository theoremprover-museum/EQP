#ifndef TP_PINDEX_H
#define TP_PINDEX_H

/****************** Global variables for clause pair code ************/

struct pair_index {
    int finished;        /* set if nothing to retrieve */
    int n;               /* number of clause lists */
    int i, j;            /* working pair */
    int clause_min;      /* smallest wt of inserted clause */
    int new_clause_min;  /* smallest inserted wt since previous retrieval */
    List_ptr *lists;     /* clause lists */
    List_pos_ptr *top;
    List_pos_ptr *curr;
    struct pair_index *next;  /* for avail list */
    };

typedef struct pair_index *Pair_index_ptr;


/* function prototypes from pindex.c */

Pair_index_ptr init_pair_index(int n);

int pairs_available(Pair_index_ptr p);

void insert_pair_index(Clause_ptr c, int wt, Pair_index_ptr p);

void delete_pair_index(Clause_ptr c, int wt, Pair_index_ptr p);

void retrieve_pair(Pair_index_ptr p, Clause_ptr *cp1, Clause_ptr *cp2);

void p_pair_index(Pair_index_ptr p);

Pair_index_ptr get_pair_index(void);

void free_pair_index(Pair_index_ptr p);

void print_clause_pair_mem(FILE *fp, int heading);

void p_clause_pair_mem();

int pair_already_used(Clause_ptr c1, int weight1,
			   Clause_ptr c2, int weight2,
			   Pair_index_ptr p);

#endif  /* ! TP_PINDEX_H */
