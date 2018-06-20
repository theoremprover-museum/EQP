#include "Header.h"
#include "Symbols.h"
#include "Io.h"
#include "List.h"
#include "Clause.h"

/*
 * memory management
 */

static Literal_ptr literal_avail;
static Clause_ptr clause_avail;
static List_ptr list_avail;
static List_pos_ptr list_pos_avail;
static long literal_gets, literal_frees, literal_avails;
static long clause_gets, clause_frees, clause_avails;
static long list_gets, list_frees, list_avails;
static long list_pos_gets, list_pos_frees, list_pos_avails;

/*************
 *
 *    Literal_ptr get_literal()
 *
 *************/

Literal_ptr get_literal(void)
{
    Literal_ptr p;

    literal_gets++;
    if (!literal_avail)
        p = tp_alloc(sizeof(struct literal));
    else {
        literal_avails--;
        p = literal_avail;
        literal_avail = literal_avail->next;
        }
    p->next = NULL;
    p->atom = NULL;
    p->sign = 0;
    p->type = 0;
    return(p);
}  /* get_literal */

/*************
 *
 *    free_literal()
 *
 *************/

void free_literal(Literal_ptr p)
{
    literal_frees++;
    literal_avails++;
    p->next = literal_avail;
    literal_avail = p;
}  /* free_literal */

/*************
 *
 *    Clause_ptr get_clause()
 *
 *************/

Clause_ptr get_clause(void)
{
    Clause_ptr p;

    clause_gets++;
    if (!clause_avail)
        p = tp_alloc(sizeof(struct clause));
    else {
        clause_avails--;
        p = clause_avail;
        clause_avail = (Clause_ptr) clause_avail->literals;
        }
    p->literals = NULL;
    p->justification = NULL;
    p->containers = NULL;
    p->id = 0;
    p->weight = 0;
    p->interpreted_value = -1;

    return(p);
}  /* get_clause */

/*************
 *
 *    free_clause()
 *
 *************/

void free_clause(Clause_ptr p)
{
    clause_frees++;
    clause_avails++;
    p->literals = (Literal_ptr) clause_avail;
    clause_avail = p;
}  /* free_clause */

/*************
 *
 *    List_ptr get_list()
 *
 *************/

List_ptr get_list(void)
{
    List_ptr p;

    list_gets++;
    if (!list_avail)
        p = tp_alloc(sizeof(struct list));
    else {
        list_avails--;
        p = list_avail;
        list_avail = list_avail->next;
        }

    p->first = NULL;
    p->last = NULL;
    p->next = NULL;

    return(p);
}  /* get_list */

/*************
 *
 *    free_list()
 *
 *************/

void free_list(List_ptr p)
{
    list_frees++;
    list_avails++;
    p->next = list_avail;
    list_avail = p;
}  /* free_list */

/*************
 *
 *    List_pos_ptr get_list_pos()
 *
 *************/

List_pos_ptr get_list_pos(void)
{
    List_pos_ptr p;

    list_pos_gets++;
    if (!list_pos_avail)
        p = tp_alloc(sizeof(struct list_pos));
    else {
        list_pos_avails--;
        p = list_pos_avail;
        list_pos_avail = list_pos_avail->next;
        }

    p->prev = NULL;
    p->next = NULL;
    p->nocc = NULL;
    p->c = NULL;
    p->container = NULL;

    return(p);
}  /* get_list_pos */

/*************
 *
 *    free_list_pos()
 *
 *************/

void free_list_pos(List_pos_ptr p)
{
    list_pos_frees++;
    list_pos_avails++;
    p->next = list_pos_avail;
    list_pos_avail = p;
}  /* free_list_pos */

/*************
 *
 *   print_clause_mem()
 *
 *************/

void print_clause_mem(FILE *fp, int heading)
{
    if (heading)
	fprintf(fp, "  type (bytes each)        gets      frees     in use      avail      bytes\n");

    fprintf(fp, "literal (%4d)      %11ld%11ld%11ld%11ld%9.1f K\n", sizeof(struct literal), literal_gets, literal_frees, literal_gets - literal_frees, literal_avails, (((literal_gets - literal_frees) + literal_avails) * sizeof(struct literal)) / 1024.);
    fprintf(fp, "clause (%4d)       %11ld%11ld%11ld%11ld%9.1f K\n", sizeof(struct clause), clause_gets, clause_frees, clause_gets - clause_frees, clause_avails, (((clause_gets - clause_frees) + clause_avails) * sizeof(struct clause)) / 1024.);
    fprintf(fp, "list (%4d)         %11ld%11ld%11ld%11ld%9.1f K\n", sizeof(struct list), list_gets, list_frees, list_gets - list_frees, list_avails, (((list_gets - list_frees) + list_avails) * sizeof(struct list)) / 1024.);
    fprintf(fp, "list_pos (%4d)     %11ld%11ld%11ld%11ld%9.1f K\n", sizeof(struct list_pos), list_pos_gets, list_pos_frees, list_pos_gets - list_pos_frees, list_pos_avails, (((list_pos_gets - list_pos_frees) + list_pos_avails) * sizeof(struct list_pos)) / 1024.);

}  /* print_clause_mem */

/*************
 *
 *   p_clause_mem()
 *
 *************/

void p_clause_mem()
{
    print_clause_mem(stdout, 1);
}  /* p_clause_mem */

/*
 *  end of memory management
 */

/*
 *  The next set of routines is for a simple hash table to
 *  store and retrieve clauses by ID number.
 */

/*************
 *
 *    init_clause_table_id()
 *
 *    Allocate and return a clause-id table.
 *
 *************/

Gen_ptr_ptr *init_clause_table_id(void)
{
    int i;
    Gen_ptr_ptr *p;
    

    p = tp_alloc(CLAUSE_TAB_SIZE * sizeof(Gen_ptr_ptr *));
    
    for (i = 0; i < CLAUSE_TAB_SIZE; i++)
	p[i] = NULL;
    return(p);
}  /* init_clause_table_id */

/*************
 *
 *   store_clause_by_id(c, tab)
 *
 *   Store a clause in a clause-id table.
 *
 *************/

void store_clause_by_id(Clause_ptr c, Gen_ptr_ptr *tab)
{
    int i;
    Gen_ptr_ptr p1, p2, p3;

    i = c->id % CLAUSE_TAB_SIZE;
    for (p2=NULL, p1=tab[i]; p1 && p1->u.c->id < c->id; p2 = p1, p1 = p1->next);
    if (p1 && p1->u.c->id == c->id)
	abend("store_clause_by_id: clause already there.");
    p3 = get_gen_ptr();
    p3->u.c = c;
    p3->next = p1;
    if (p2)
	p2->next = p3;
    else
	tab[i] = p3;
}  /* store_clause_by_id */

/*************
 *
 *     delete_clause_by_id(c, tab)
 *
 *************/

void delete_clause_by_id(Clause_ptr c, Gen_ptr_ptr *tab)
{
    int i;
    Gen_ptr_ptr p1, p2;

    i = c->id % CLAUSE_TAB_SIZE;
    for (p2 = NULL, p1 = tab[i]; p1 && p1->u.c->id < c->id; p2 = p1, p1 = p1->next);
    if (!p1 || p1->u.c->id != c->id)
	abend("delete_clause_by_id: cannot find clause.");
    if (p2)
	p2->next = p1->next;
    else
	tab[i] = p1->next;
}  /* delete_clause_by_id */

/*************
 *
 *     find_clause_by_id(id, tab)
 *
 *     Given a clause ID and a table, retrieve the clause (or NULL).
 *
 *************/

Clause_ptr find_clause_by_id(int id, Gen_ptr_ptr *tab)
{
    int i;
    Gen_ptr_ptr p1;

    i = id % CLAUSE_TAB_SIZE;
    for (p1 = tab[i]; p1 && p1->u.c->id < id; p1 = p1->next);
    if (p1 && p1->u.c->id == id)
	return(p1->u.c);
    else {
	if (Stats[PROOFS] > 0)
	    printf("clause with id=%d not found.\n", id);
	return((Clause_ptr) NULL);
	}
}  /* find_clause_by_id */

/*************
 *
 *     print_clause_table_id(fp, tab)
 *
 *************/

void print_clause_table_id(FILE *fp, Gen_ptr_ptr *tab)
{
    int i;
    Gen_ptr_ptr p;

    fprintf(fp, "\nID clause table:\n");
    for (i=0; i<CLAUSE_TAB_SIZE; i++)
	for (p = tab[i]; p; p = p->next)
	    print_clause(fp, p->u.c);
	    
}  /* print_clause_table_id */

/*************
 *
 *     p_clause_table_id(tab)
 *
 *************/

void p_clause_table_id(Gen_ptr_ptr *tab)
{
    print_clause_table_id(stdout, tab);
}  /* p_clause_table_id */

/*************
 *
 *    zap_clause(c)
 *
 *    Free a clause (including justification list).  Nothing (e.g.,
 *    indexes) should refer to c or any of its subterms.
 *
 *************/

void zap_clause(Clause_ptr c)
{
    Gen_ptr_ptr p1, p2;
    Literal_ptr l1, l2;

    p1 = c->justification;
    while (p1) {
	p2 = p1;
	p1 = p1->next;
	free_gen_ptr(p2);
	}
    l1 = c->literals;
    while(l1) {
	zap_term(l1->atom);
	l2 = l1;
	l1 = l1->next;
	free_literal(l2);
	}
    free_clause(c);
}  /* zap_clause */

/*************
 *
 *   pos_clause(c) - Are all literals positive?
 *
 *************/

int pos_clause(Clause_ptr c)
{
    Literal_ptr l;
    int ok;
    for (l = c->literals, ok = 1; l && ok; l = l->next)
	ok = l->sign;
    return(ok);
}  /* pos_clause */

/*************
 *
 *   neg_clause(c) - Are all literals negative?
 *
 *************/

int neg_clause(Clause_ptr c)
{
    Literal_ptr l;
    int ok;
    for (l = c->literals, ok = 1; l && ok; l = l->next)
	ok = !l->sign;
    return(ok);
}  /* neg_clause */

/*************
 *
 *   literal_count(c) - Return the number of literals.
 *
 *************/

int literal_count(Clause_ptr c)
{
    Literal_ptr l;
    int n;
    for (l = c->literals, n = 0; l; l = l->next, n++);
    return(n);
}  /* literal_count */

/*************
 *
 *   literal_i() - Return the i-th literal (or NULL).
 *
 *************/

Literal_ptr literal_i(Clause_ptr c, int i)
{
    if (i < 1)
	return((Literal_ptr) NULL);
    else {
	Literal_ptr l = c->literals;
	int n = 0;
	while(l && n < i) {
	    n++;
	    if (n < i)
		l = l->next;
	    }
	return(l);
	}
}  /* literal_i */

/*************
 *
 *   unit_clause(c) - Does the clause have exactly one literal?
 *
 *************/

int unit_clause(Clause_ptr c)
{
    return(literal_count(c) == 1);
}  /* unit_clause */

/*************
 *
 *   copy_clause(c)
 *
 *   Do not copy id, weight, bits, or justification.
 *
 *************/

Clause_ptr copy_clause(Clause_ptr c)
{
    Clause_ptr d;
    Literal_ptr lit, new, prev;

    d = get_clause();
    for (lit = c->literals, prev = NULL; lit; lit = lit->next) {
	new = get_literal();
	if (prev)
	    prev->next = new;
	else
	    d->literals = new;
	new->sign = lit->sign;
	new->atom = copy_term(lit->atom);
	prev = new;
	}
    return(d);
}  /* copy_clause */

/*************
 *
 *   copy_clause_nonbasic_marks()
 *
 *************/

void copy_clause_nonbasic_marks(Clause_ptr c1, Clause_ptr c2)
{
    Literal_ptr l1, l2;
    for (l1 = c1->literals, l2 = c2->literals;
	 l1 && l2;
	 l1 = l1->next, l2 = l2->next)
	copy_nonbasic_marks(l1->atom, l2->atom);
}  /* copy_clause_nonbasic_marks */

/*************
 *
 *    term_to_literals(t, lits)
 *
 *************/

static Literal_ptr term_to_literals(Term_ptr t, Literal_ptr lits)
{
    Literal_ptr l;

    if (is_symbol(t->symbol, "|", 2)) {
	/* Note that we traverse right-to-left and add to the
         * front, so order is preserved.
         */
        l = term_to_literals(t->args[1], lits);
        l = term_to_literals(t->args[0], l);
	}
    else {
        l = get_literal();
        l->next = lits;
        l->sign = !is_symbol(t->symbol, "-", 1);
        if (l->sign)
            l->atom = copy_term(t);
        else
            l->atom = copy_term(t->args[0]);
        }
    return(l);
}  /* term_to_literals */

/*************
 *
 *    term_to_clause()
 *
 *************/

Clause_ptr term_to_clause(Term_ptr t)
{
    Clause_ptr c = get_clause();
    c->literals = term_to_literals(t, (Literal_ptr) NULL);
    return(c);
}  /* term_to_clause */

/*************
 *
 *    literals_to_term()
 *
 *************/

static Term_ptr literals_to_term(Literal_ptr l)
{
    Term_ptr t, t1, t2;

    if (l->sign)
        t1 = copy_term(l->atom);
    else {
        t1 = get_term(1);
        t1->symbol = str_to_sn("-", 1);
        t1->args[0] = copy_term(l->atom);
        }

    if (l->next) {
        t2 = literals_to_term(l->next);
        t = get_term(2);
        t->symbol = str_to_sn("|", 2);
	t->args[0] = t1;
	t->args[1] = t2;
        }
    else
        t = t1;

    return(t);
}  /* literals_to_term */

/*************
 *
 *    clause_to_term(c)
 *
 *************/

Term_ptr clause_to_term(Clause_ptr c)
{
    Term_ptr t;

    if (c->literals)
        t = literals_to_term(c->literals);
    else {
        t = get_term(0);
        t->symbol = str_to_sn("$F", 0);
        }
    return(t);
}  /* clause_to_term */

/*************
 *
 *   clause_ident()
 *
 *************/

int clause_ident(Clause_ptr c1, Clause_ptr c2)
{
    fprintf(stderr, "clause_ident not implemented\n");
    return(0);
}  /* clause_ident */

/*************
 *
 *     list_append(clause, list)
 *
 *************/

void list_append(Clause_ptr c, List_ptr l)
{
    List_pos_ptr p;

    p = get_list_pos();
    p->container = l;
    p->c = c;
    p->nocc = c->containers;
    c->containers = p;

    p->next = NULL;
    p->prev = l->last;
    l->last = p;
    if (p->prev)
	p->prev->next = p;
    else
	l->first = p;

}  /* list_append */

/*************
 *
 *     list_prepend(clause, list)
 *
 *************/

void list_prepend(Clause_ptr c, List_ptr l)
{
    List_pos_ptr p;

    p = get_list_pos();
    p->container = l;
    p->c = c;
    p->nocc = c->containers;
    c->containers = p;

    p->prev = NULL;
    p->next = l->first;
    l->first = p;
    if (p->next)
	p->next->prev = p;
    else
	l->last = p;
    
}  /* list_prepend */

/*************
 *
 *     list_insert_before(clause, pos)
 *
 *************/

void list_insert_before(Clause_ptr c, List_pos_ptr pos)
{
    List_pos_ptr p;

    p = get_list_pos();
    p->container = pos->container;
    p->c = c;
    p->nocc = c->containers;
    c->containers = p;

    p->next = pos;
    p->prev = pos->prev;
    pos->prev = p;
    if (p->prev)
	p->prev->next = p;
    else
	pos->container->first = p;
    
}  /* list_insert_before */

/*************
 *
 *     list_insert_after(clause, pos)
 *
 *************/

void list_insert_after(Clause_ptr c, List_pos_ptr pos)
{
    List_pos_ptr p;

    p = get_list_pos();
    p->container = pos->container;
    p->c = c;
    p->nocc = c->containers;
    c->containers = p;

    p->prev = pos;
    p->next = pos->next;
    pos->next = p;
    if (p->next)
	p->next->prev = p;
    else
	pos->container->last = p;
    
}  /* list_insert_after */

/*************
 *
 *     list_remove(clause, list)
 *
 *     Return 1 if deleted.
 *     Return 0 if it couldn't be deleted because it's not there.
 *
 *************/

int list_remove(Clause_ptr c, List_ptr l)
{
    List_pos_ptr p, prev;

    /* Find position from containment list of clause. */
    for (p = c->containers, prev = NULL;
	 p && p->container != l;
	 prev = p, p = p->nocc);
    
    if (p) {
	/* First update normal links. */
	if (p->prev)
	    p->prev->next = p->next;
	else
	    p->container->first = p->next;
	if (p->next)
	    p->next->prev = p->prev;
	else
	    p->container->last = p->prev;

	/* Now update containment links. */
	if (prev)
	    prev->nocc = p->nocc;
	else
	    c->containers = p->nocc;

	free_list_pos(p);
	return(1);
	}
    else
	return(0);
}  /* list_remove */

/*************
 *
 *     list_remove_all(clause)
 *
 *     Remove from all lists.  Return the number of lists
 *     from which it was removed.
 *
 *************/

int list_remove_all(Clause_ptr c)
{
    List_pos_ptr p, p2;
    int i = 0;

    p = c->containers;
    while (p) {
	i++;
	if (p->prev)
	    p->prev->next = p->next;
	else
	    p->container->first = p->next;
	if (p->next)
	    p->next->prev = p->prev;
	else
	    p->container->last = p->prev;
	p2 = p;
	p = p->nocc;
	free_list_pos(p2);
	}
    c->containers = NULL;
    return(i);
}  /* list_remove_all */

/*************
 *
 *     list_member(c, l)
 *
 *************/

int list_member(Clause_ptr c, List_ptr l)
{
    List_pos_ptr p;
    int found;

    for (p = c->containers, found = 0; p && !found; p = p->nocc) {
	if (p->container == l)
	    found = 1;
	}
    return(found);
}  /* list_member */

/*************
 *
 *    print_list(fp, l)
 *
 *************/

void print_list(FILE *fp, List_ptr l)
{
    List_pos_ptr p;

    for(p = l->first; p; p = p->next)
	print_clause(fp, p->c);
    fprintf(fp, "end_of_list.\n");
}  /* print_list */

/*************
 *
 *    p_list(l)
 *
 *************/

void p_list(List_ptr l)
{
    print_list(stdout, l);
}  /* p_list */

/*************
 *
 *     list_zap(l)
 *
 *     For each element, remove it, and if it occurs nowhere else, delete it.
 *
 *************/

void list_zap(List_ptr l)
{
    List_pos_ptr p;
    Clause_ptr c;

    p = l->first;
    while (p) {
	c = p->c;
	p = p->next;
	list_remove(c, l);
	if (!c->containers)
	    zap_clause(c);
	}
}  /* list_zap */

/*************
 *
 *     list_check(l)
 *
 *************/

void list_check(List_ptr l)
{
    List_pos_ptr p;

    for (p = l->first; p; p = p->next) {
	if (p->container != l)
	    printf("error0\n");
	if (p->next) {
	    if (p->next->prev != p)
		printf("error1\n");
	    }
	else if (p != l->last)
	    printf("error2\n");
	if (p->prev) {
	    if (p->prev->next != p)
		printf("error3\n");
	    }
	else if (p != l->first)
	    printf("error4\n");
	}
}  /* list_check */

/*************
 *
 *   clause_in_list(c, l)
 *
 *************/

int clause_in_list(Clause_ptr c, List_ptr l)
{
    List_pos_ptr p;
    int found;

    for (p = l->first, found = 0; p && !found; p = p->next)
	found = clause_ident(c, p->c);
    return(found);
}  /* clause_in_list */

/*************
 *
 *   clause_up_pointers_term()
 *
 *************/

static void clause_up_pointers_term(Term_ptr t, Clause_ptr c)
{
    int i;
    t->containing_clause = c;
    for (i = 0; i < t->arity; i++)
	clause_up_pointers_term(t->args[i], c);
}  /* clause_up_pointers_term */

/*************
 *
 *   clause_up_pointers()
 *
 *************/

void clause_up_pointers(Clause_ptr c)
{
    Literal_ptr l;
    for (l = c->literals; l; l = l->next)
	clause_up_pointers_term(l->atom, c);
}  /* clause_up_pointers */

/*************
 *
 *    print_clause(fp, lit)
 *
 *************/

void print_clause(FILE *fp, Clause_ptr c)
{
    Term_ptr t;
    Gen_ptr_ptr p;
    int i, n;

    if (Internal_flags[INTERP_PRESENT]) {
	char *s;
	switch (c->interpreted_value) {
	  case 0: s = "FALSE"; break;
	  case 1: s = "TRUE "; break;
	  default: s = "?????"; 
	    }
	fprintf(fp,"%s ", s);
	}

    fprintf(fp,"%d (wt=%d) [", c->id, c->weight);

    p = c->justification;
    while (p) {

	if (p != c->justification)
	    fprintf(fp, ",");

	switch (p->u.i) {
	  case BACK_DEMOD_RULE:
	    p = p->next;
	    fprintf(fp, "back_demod(%d)", p->u.i);
	    break;
	  case COPY_RULE:
	    p = p->next;
	    fprintf(fp, "copy(%d)", p->u.i);
	    break;
	  case FLIP_RULE:
	    p = p->next;
	    fprintf(fp, "flip(%d)", p->u.i);
	    break;

	  case DEMOD_RULE:
	    p = p->next;
	    n = -(p->u.i);
	    fprintf(fp, "demod([");
	    for (i = 0; i < n; i++) {
		p = p->next;
		fprintf(fp, "%s%d", (i == 0 ? "" : ","), p->u.i);
		}
	    fprintf(fp, "])");
	    break;

	  case UNIT_CONFLICT_RULE:
	    p = p->next;
	    fprintf(fp, "conflict(%d,%d)", p->u.i, p->next->u.i);
	    p = p->next;
	    break;

	  case PARA_RULE:
	    p = p->next;
	    fprintf(fp, "para(%d,%d)", p->u.i, p->next->u.i);
	    p = p->next;
	    break;

	  case PARA_FX_RULE:
	    p = p->next;
	    fprintf(fp, "para(%d',%d)", p->u.i, p->next->u.i);
	    p = p->next;
	    break;

	  case PARA_IX_RULE:
	    p = p->next;
	    fprintf(fp, "para(%d,%d')", p->u.i, p->next->u.i);
	    p = p->next;
	    break;

	  case PARA_FX_IX_RULE:
	    p = p->next;
	    fprintf(fp, "para(%d',%d')", p->u.i, p->next->u.i);
	    p = p->next;
	    break;

	  default:
	    fprintf(fp, "??(%d)", p->u.i);
	    }

	p = p->next;
	}

    fprintf(fp,"] ");

    t = clause_to_term(c);
    print_term(fp, t);
    fprintf(fp, ".\n");
    zap_term(t);

}  /* print_clause */

/*************
 *
 *    p_clause(c)
 *
 *************/

void p_clause(Clause_ptr c)
{
    print_clause(stdout, c);
}  /* p_clause */

/*************
 *
 *    get_ancestors
 *
 *************/

void get_ancestors(Clause_ptr c, Gen_ptr_ptr *id_table, Gen_ptr_ptr *pp)
{
    Gen_ptr_ptr p1, p2, p3;

    if (!c) {
	p2 = get_gen_ptr();
	p2->u.c = NULL;
	p2->next = *pp;
	*pp = p2;
	}
    else {
	p1 = *pp;
	p3 = NULL;
	while (p1 && (!p1->u.c || p1->u.c->id < c->id)) {
	    p3 = p1;
	    p1 = p1->next;
	    }
	if (!p1 || p1->u.c->id > c->id) {
	    p2 = get_gen_ptr();
	    p2->u.c = c;
	    if (!p3) {
		p2->next = *pp;
		*pp = p2;
		}
	    else {
		p2->next = p3->next;
		p3->next = p2;
		}
	    
	    for (p1 = c->justification; p1; p1 = p1->next)
		if (p1->u.i > 0)
		    get_ancestors(find_clause_by_id(p1->u.i, id_table),
				  id_table, pp);
	    }
	}
}  /* get_ancestors */

/*************
 *
 *   get_para_parents()
 *
 *************/

void get_para_parents(Clause_ptr c, Gen_ptr_ptr *id_table,
		      Clause_ptr *p1, Clause_ptr *p2)
{
    Gen_ptr_ptr g1;

    *p1 = NULL;
    *p2 = NULL;

    g1 = c->justification;
    if (!g1)
	return;
    else if (g1->u.i != PARA_RULE && g1->u.i != PARA_FX_RULE &&
	     g1->u.i != PARA_IX_RULE && g1->u.i != PARA_FX_IX_RULE)
	return;
    else {
	*p1 = find_clause_by_id(g1->next->u.i, id_table);
	*p2 = find_clause_by_id(g1->next->next->u.i, id_table);
	}
}  /* get_para_parents */

/*************
 *
 *    print_proof
 *
 *************/

void print_proof(FILE *fp, Clause_ptr c1, Clause_ptr c2, Gen_ptr_ptr *id_table)
{
    Gen_ptr_ptr p1, p2;
    Clause_ptr c;

    fprintf(fp, "\n---------------- PROOF ----------------\n\n");
    p1 = NULL;
    get_ancestors(c1, id_table, &p1);
    if (c2)
	get_ancestors(c2, id_table, &p1);
    while (p1) {
	c = p1->u.c;
	if (c)
	    print_clause(fp, c);
	else
	    fprintf(fp, "CLAUSE NOT FOUND, PROOF IS INCOMPLETE.\n");
	p2 = p1;
	p1 = p1->next;
	free_gen_ptr(p2);
        }
    fprintf(fp, "\n------------ end of proof -------------\n\n");
}  /* print_proof */

/*************
 *
 *   biggest_variable_in_clause(c)
 *
 *************/

int biggest_variable_in_clause(Clause_ptr c)
{
    Literal_ptr lit;
    int max, v;
    
    for (max = -1, lit = c->literals; lit; lit = lit->next) {
	v = biggest_variable(lit->atom);
	max = (v > max ? v : max);
	}
    return(max);
}  /* biggest_variable_in_clause */

/*************
 *
 *    void distinct_vars_rec(t, a, max) -- called by distinct_vars
 *
 *************/

static void distinct_vars_rec(Term_ptr t, int *a, int *max)
{
    int i, vn;

    if (VARIABLE(t)) {
	vn = t->symbol;
	for (i = 0; i < MAX_VARS && a[i] != -1 && a[i] != vn; i++);
	if (i != MAX_VARS && a[i] == -1) {
	    a[i] = vn;
	    *max = i+1;
	    }
	}
    else {
	for (i = 0; i < t->arity && *max < MAX_VARS; i++)
	    distinct_vars_rec(t->args[i], a, max);
	}
}  /* distinct_vars_rec */

/*************
 *
 *    int distinct_vars(c) -- number of variables in a clause.
 *
 *    if >= MAX_VARS, return MAX_VARS.
 *
 *************/

int distinct_vars(Clause_ptr c)
{
    Literal_ptr lit;
    int a[MAX_VARS], i, max;

    for (i = 0; i < MAX_VARS; i++)
	a[i] = -1;

    for (lit = c->literals, max = 0; lit; lit = lit->next)
	distinct_vars_rec(lit->atom, a, &max);

    return(max);

}  /* distinct_vars */

