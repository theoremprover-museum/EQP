#include "Header.h"
#include "Symbols.h" /* var_nam(), sn_to_str() */
#include "List.h"
#include "Clause.h"
#include "Interp.h"

/*************
 *
 *   init_interp()
 *
 *   Given a term presenting an interpretation, put it into the special
 *   data structure used for fast access.  A sample term is
 *
 *   interpretation(3, [
 *       function(e, 0),
 *       function(*(_,_), [0,1,2,1,2,0,2,0,1]),
 *       predicate(p(_), [1,0,1])
 *       ])
 *
 *************/

Interp_ptr init_interp(Term_ptr t)
{
    Interp_ptr p;
    int number_of_operations, domain_size, rc, i, arity, j, n, sym_num, val;
    int function, *table; 
    Term_ptr f, l1, l2;

    if (t->arity != 2) 
	abend("bad interp, arity");

    p = tp_alloc(sizeof(struct interp));
    p->t = t;

    for (i = 0; i < MAX_SYMBOLS; i++)
	p->tables[i] = NULL;

    rc = str_int(sn_to_str(t->args[0]->symbol), &domain_size);

    if (!rc)
	abend("bad interp, domain size not integer");
    else if (domain_size < 1)
	abend("bad interp, domain size < 1");

    number_of_operations = list_length(t->args[1]);
    if (number_of_operations == -1)
	abend("bad interp, number of operations");

    p->size = domain_size;

    for (i = 1, l1 = t->args[1]; i <= number_of_operations; i++, l1 = l1->args[1]) {
	f = l1->args[0];
	if (is_symbol(f->symbol, "function", 2))
	    function = 1;
	else if (is_symbol(f->symbol, "relation", 2))
	    function = 0;
	else
	    abend("bad interp, bad function");

	sym_num = -(f->args[0]->symbol);
	if (sym_num >= MAX_SYMBOLS)
	    abend("bad interp, symbol number too big");
	arity = f->args[0]->arity;
	if (arity > 3)
	    abend("bad interp, arity > 3");
	/* n = domain_size^arity */
	for (j = 0, n = 1; j < arity; j++, n = n * domain_size);
	l2 = f->args[1];
	if (list_length(l2) != n)
	    abend("bad interp, bad list");
	p->tables[sym_num] = tp_alloc(n * sizeof(int));
	table = p->tables[sym_num];
	for (j = 0; j < n; j++, l2 = l2->args[1]) {
	    rc = str_int(sn_to_str(l2->args[0]->symbol), &val);
	    if (!rc)
		abend("bad interp, list element not integer");
	    else if (function && (val < 0 || val > domain_size-1))
		abend("bad interp, function element out of range");
	    else if (!function && (val < 0 || val > 1))
		abend("bad interp, relation element out of range");
	    else
		table[j] = val;
	    }
	}
    return(p);
}  /* init_interp */

/*************
 *
 *   print_interp()
 *
 *************/

void print_interp(FILE *fp, Interp_ptr p)
{
    int i, j, n, arity, *table;

    fprintf(fp, "\nInterpretation, domain_size=%d.\n", p->size);

    for (i = 0; i < MAX_SYMBOLS; i++) {
	table = p->tables[i];
	if (table) {
	    fprintf(fp, "%s : [", sn_to_str(-i));
	    arity = sn_to_arity(-i);
	    for (j = 0, n = 1; j < arity; j++, n = n * p->size);
	    for (j = 0; j < n-1; j++)
		fprintf(fp, "%d,", table[j]);
	    fprintf(fp, "%d]\n", table[n-1]);
	    }
	}
}  /* print_interp */

/*************
 *
 *   p_interp()
 *
 *************/

void p_interp(Interp_ptr p)
{
    print_interp(stdout, p);
}  /* p_interp */

/*************
 *
 *   eval_term_ground()
 *
 *   This works for arity <= 3.  It works for terms and atoms.
 *
 *************/

int eval_term_ground(Term_ptr t, Interp_ptr p, int *vals)
{
    if (VARIABLE(t)) {
	return(vals[t->symbol]);
	}
    else {
	int sym_num = -(t->symbol);
	int *table = p->tables[sym_num];
	int n = p->size;
	int v0, v1, v2;

	if (table == NULL) {
	    printf("ready to abend: "); p_term(t);
	    abend("eval_ground_term, symbol not in interpretation");
	    }

	switch (t->arity) {
	  case 0:
	    return(table[0]);
	    break;
	  case 1:
	    v0 = eval_term_ground(t->args[0], p, vals);
	    return(table[v0]);
	    break;
	  case 2:
	    v0 = eval_term_ground(t->args[0], p, vals);
	    v1 = eval_term_ground(t->args[1], p, vals);
	    return(table[v0 * n + v1]);
	    break;
	  case 3:
	    v0 = eval_term_ground(t->args[0], p, vals);
	    v1 = eval_term_ground(t->args[1], p, vals);
	    v2 = eval_term_ground(t->args[2], p, vals);
	    return(table[v0 * n * n + v1 * n + v2]);
	    break;
	  default:
	    abend("eval_term_ground, arity too big");
	    }
	}
}  /* eval_term_ground */

/*************
 *
 *   eval_clause_ground()
 *
 *   Given a ground clause and an interpretation,
 *   return "at least one literal is true in the interpretation".
 *
 *************/

int eval_clause_ground(Clause_ptr c, Interp_ptr p, int *vals)
{
    Literal_ptr lit;
    int atom_val, true_literal;

    true_literal = 0;
    for (lit = c->literals; lit && !true_literal; lit = lit->next) {

	if (is_symbol(lit->atom->symbol, "=", 2))
	    atom_val = (eval_term_ground(lit->atom->args[0], p, vals) ==
		   eval_term_ground(lit->atom->args[1], p, vals));
	else
	    atom_val = eval_term_ground(lit->atom, p, vals);

	true_literal = (lit->sign ? atom_val : !atom_val);
	}
    return(true_literal);
}  /* eval_clause_ground */

/*************
 *
 *   eval_clause()
 *
 *   Given a clause and an interpretation,
 *   return "all instances are true in the interpretation".
 *
 *   A limit of 8 variables is wired into the code.
 *
 *************/

int eval_clause(Clause_ptr c, Interp_ptr p)
{
    int v, vals[MAX_VARS_EVAL], n, i, j;
    int v0, v0_lim, v1, v1_lim, v2, v2_lim, v3, v3_lim, v4, v4_lim, v5, v5_lim;
    int v6, v6_lim, v7, v7_lim;

    n = p->size;

    v = biggest_variable_in_clause(c);

    if (v >= MAX_VARS_EVAL)
	abend("eval clause: variable too big");

#if 0
    printf("\nEvaluating clause, biggest_var=%d ", v); p_clause(c);
#endif

    v0_lim = n;
    v1_lim = n;
    v2_lim = n;
    v3_lim = n;
    v4_lim = n;
    v5_lim = n;
    v6_lim = n;
    v7_lim = n;

    switch (v) {  /* note no breaks */
      case -1: v0_lim = 1;
      case 0:  v1_lim = 1;
      case 1:  v2_lim = 1;
      case 2:  v3_lim = 1;
      case 3:  v4_lim = 1;
      case 4:  v5_lim = 1;
      case 5:  v6_lim = 1;
      case 6:  v7_lim = 1;
      case 7:  ;
	}

    j = 0;

    for (v0 = 0; v0 < v0_lim; v0++) { vals[0] = v0;
    for (v1 = 0; v1 < v1_lim; v1++) { vals[1] = v1;
    for (v2 = 0; v2 < v2_lim; v2++) { vals[2] = v2;
    for (v3 = 0; v3 < v3_lim; v3++) { vals[3] = v3;
    for (v4 = 0; v4 < v4_lim; v4++) { vals[4] = v4;
    for (v5 = 0; v5 < v5_lim; v5++) { vals[5] = v5;
    for (v6 = 0; v6 < v6_lim; v6++) { vals[6] = v6;
    for (v7 = 0; v7 < v7_lim; v7++) { vals[7] = v7;

        j++;				      

        if (!eval_clause_ground(c, p, vals)) {
#if 0
	    printf("False for values: ");
	    for (i = 0; i <= v; i++)
		printf("v%d=%d ", i, vals[i]);
	    printf("\n");
#endif
	    return(0);
	    }

    }}}}}}}}

#if 0
    printf("Clause is true in this interpretation (%d instances).\n", j);
#endif

    return(1);
    
}  /* eval_clause */

/*************
 *
 *   false_positive_unit()
 *
 *************/

int false_positive_unit(Clause_ptr c)
{
    return (c->interpreted_value == 0 && 
	    pos_clause(c) &&
	    literal_count(c) == 1);
}  /* false_positive_unit */

/*************
 *
 *   check_semantic_inference()
 *
 *************/

int check_semantic_inference(Clause_ptr c, Interp_ptr interpretation,
			     Gen_ptr_ptr *id_table)
{
    Clause_ptr p1; 
    Clause_ptr p2;

    if (!Internal_flags[INTERP_PRESENT])
	return(1);

    get_para_parents(c, id_table, &p1, &p2);

    /* The inferene is ok if both parents are false positive units or 
       if the child and one parent are false.
    */

    if (!p1 && !p2)
	return(1);

    else if (false_positive_unit(p1) && false_positive_unit(p2))
	return(1);

    else if ((p1->interpreted_value == 0 || p2->interpreted_value == 0) &&
	     c->interpreted_value == 0)
	return(1);
    else
	return(0);

}  /* check_semantic_inference */

/*************
 *
 *   semantic_para_parents_check()
 *
 *   OK if one of the expecting parents is false in the interpretation.
 *
 *************/

int semantic_para_parents_check(Clause_ptr from, Clause_ptr into)
{
    if (!Internal_flags[INTERP_PRESENT])
	return(1);
    else
	return(from->interpreted_value == 0 || into->interpreted_value == 0);

}  /* semantic_para_parents_check */

