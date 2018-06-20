#ifndef TP_LIST_H
#define TP_LIST_H

struct gen_ptr {     /* for constructing a list of integers or pointers */
    struct gen_ptr *next;
    union {
	int i;
	struct term *t;
	struct literal *l;
	struct clause *c;
	struct gen_ptr *p;
	void *v;
	} u;
    };

typedef struct gen_ptr *Gen_ptr_ptr;


/* function prototypes from list.c */

Gen_ptr_ptr get_gen_ptr(void);

void free_gen_ptr(Gen_ptr_ptr p);

void print_list_mem(FILE *fp, int heading);

void p_list_mem();

Gen_ptr_ptr reverse_gen_list(Gen_ptr_ptr p1, Gen_ptr_ptr p2);

Gen_ptr_ptr copy_gen_ptr_list(Gen_ptr_ptr p);

int gen_ptr_count(Gen_ptr_ptr p);

Gen_ptr_ptr read_list(FILE *fp, int *ep);

#endif  /* ! TP_LIST_H */
