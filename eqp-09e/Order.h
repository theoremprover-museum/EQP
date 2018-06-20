#ifndef TP_ORDER_H
#define TP_ORDER_H

#define LESS_THAN        1
#define GREATER_THAN     2
#define SAME_AS          3
#define NOT_COMPARABLE   4
#define NOT_GREATER_THAN 5
#define NOT_LESS_THAN    6

/* WARNING: for LRPO status, 0 is the default!! */

#define LRPO_LR_STATUS        0
#define LRPO_MULTISET_STATUS  1


/* function prototypes from lrpo.c */

int lrpo(Term_ptr t1, Term_ptr t2);

int lrpo_greater(Term_ptr t1, Term_ptr t2);

void merge_sort(void **a,    /* array to sort */
		void **w,    /* work array */
		int start,   /* index of first element */
		int end,     /* index of last element */
		int (*comp_proc) (void *, void *));

int term_compare_ncv(Term_ptr t1, Term_ptr t2);

int term_compare_vf(Term_ptr t1, Term_ptr t2);

#endif  /* ! TP_ORDER_H */
