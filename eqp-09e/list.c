#include "Header.h"
#include "Io.h"  /* for read_term() */
#include "List.h"

/*
 * memory management
 */

static Gen_ptr_ptr gen_ptr_avail;
static long gen_ptr_gets, gen_ptr_frees, gen_ptr_avails;

/*************
 *
 *    Gen_ptr_ptr get_gen_ptr()
 *
 *************/

Gen_ptr_ptr get_gen_ptr(void)
{
    Gen_ptr_ptr p;
    
    gen_ptr_gets++;
    if (!gen_ptr_avail)
	p = tp_alloc(sizeof(struct gen_ptr));
    else {
	gen_ptr_avails--;
	p = gen_ptr_avail;
	gen_ptr_avail = gen_ptr_avail->next;
	}
    p->u.t = NULL;
    p->next = NULL;
    return(p);
}  /* get_gen_ptr */

/*************
 *
 *    free_gen_ptr()
 *
 *************/

void free_gen_ptr(Gen_ptr_ptr p)
{
    gen_ptr_frees++;
    gen_ptr_avails++;
    p->next = gen_ptr_avail;
    gen_ptr_avail = p;
}  /* free_gen_ptr */

/*************
 *
 *   print_list_mem()
 *
 *************/

void print_list_mem(FILE *fp, int heading)
{
    if (heading)
	fprintf(fp, "  type (bytes each)        gets      frees     in use      avail      bytes\n");

    fprintf(fp, "gen_ptr (%4d)      %11ld%11ld%11ld%11ld%9.1f K\n", sizeof(struct gen_ptr), gen_ptr_gets, gen_ptr_frees, gen_ptr_gets - gen_ptr_frees, gen_ptr_avails, (((gen_ptr_gets - gen_ptr_frees) + gen_ptr_avails) * sizeof(struct gen_ptr)) / 1024.);

}  /* print_list_mem */

/*************
 *
 *   p_list_mem()
 *
 *************/

void p_list_mem()
{
    print_list_mem(stdout, 1);
}  /* p_list_mem */

/*
 *  end of memory management
 */

/*************
 *
 *     reverse_gen_list(p1, p2)
 *
 *************/

Gen_ptr_ptr reverse_gen_list(Gen_ptr_ptr p1, Gen_ptr_ptr p2)
{
    Gen_ptr_ptr p3;

    if (!p1)
	return(p2);
    else {
	p3 = p1->next;
	p1->next = p2;
	return(reverse_gen_list(p3, p1));
	}
}  /* reverse_gen_list */

/*************
 *
 *   copy_gen_ptr_list(p)
 *
 *************/

Gen_ptr_ptr copy_gen_ptr_list(Gen_ptr_ptr p)
{
    Gen_ptr_ptr start, p1, p2;

    start = NULL;
    p2 = NULL;
    for ( ; p; p = p->next) {
	p1 = get_gen_ptr();
	p1->u.v = p->u.v;
	if (p2)
	    p2->next = p1;
	else
	    start = p1;
	p2 = p1;
	}
    return(start);
}  /* copy_gen_ptr_list */

/*************
 *
 *   gen_ptr_count()
 *
 *************/

int gen_ptr_count(Gen_ptr_ptr p)
{
    int n;
    for (n = 0; p; p = p->next, n++);
    return(n);
}  /* gen_ptr_count */

/*************
 *
 *    Gen_ptr_ptr read_list(file_ptr, errors_ptr, integrate)
 *
 *    Read and return a list of terms.
 *
 *    The list must be terminated either with the term `end_of_list.'
 *    or with an actual EOF.
 *    Set errors_ptr to point to the number of errors found.
 *
 *************/

Gen_ptr_ptr read_list(FILE *fp, int *ep)
{
    Gen_ptr_ptr p1, p2, p3;
    Term_ptr t;
    int rc;

    *ep = 0;
    p3 = NULL;
    p2 = NULL;
    t = read_term(fp, &rc);
    while (rc == 0) {
	(*ep)++;
	t = read_term(fp, &rc);
	}

    /* keep going until t == NULL || t is end marker */

    while (t && (t->arity != 0 ||
		 !str_ident(sn_to_str(t->symbol), "end_of_list"))) {
	set_vars(t);
	p1 = get_gen_ptr();
	p1->u.t = t;
	if (p2 == NULL)
	    p3 = p1;
	else
	    p2->next = p1;
	p2 = p1;
	t = read_term(fp, &rc);
	while (rc == 0) {
	    (*ep)++;
	    t = read_term(fp, &rc);
	    }
	}
    if (!t)
	return(p3);
    else {
	zap_term(t);
	return(p3);
	}
}  /* read_list */

