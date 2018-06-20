/* To do: 
 *   + from and into nonoriented equations.
 */

/* Notes:
 *   + ordered_paramod works only for indexed paramod 
 *   + basic_paramod works only for indexed paramod 
 *   + prime_paramod works for ALL paramod
 */

#include "Header.h"
#include "List.h"
#include "Symbols.h"
#include "Io.h"
#include "Unify.h"
#include "Ac.h"
#include "Clause.h"
#include "Fpa.h"
#include "Order.h"
#include "Paramod.h"

/*************
 *
 *   para_parents()
 *
 *************/

Gen_ptr_ptr para_parents(Clause_ptr from_cl, Clause_ptr into_cl,
			      int from_ext, int into_ext)
{
    Gen_ptr_ptr p1, p2, p3;

    p1 = get_gen_ptr();
    p2 = get_gen_ptr();
    p3 = get_gen_ptr();
    p1->next = p2;
    p2->next = p3;
    p2->u.i = from_cl->id;
    p3->u.i = into_cl->id;

    if (from_ext && into_ext)
	p1->u.i = PARA_FX_IX_RULE;
    else if (from_ext)
	p1->u.i = PARA_FX_RULE;
    else if (into_ext)
	p1->u.i = PARA_IX_RULE;
    else
	p1->u.i = PARA_RULE;

    return(p1);
}  /* para_parents */

/*************
 *
 *   key_check()
 *
 *************/

int key_check(Clause_ptr from, Clause_ptr into)
{
    int key = Parms[PARA_KEY].val;

    if (key == 0)
	return(1);
    else
	return(key == into->id);
}  /* key_check */

/*************
 *
 *     paramod(from, from_atom, into, into_atom, into_term, where)
 *
 *     If an AC extension is being used, then from (into) is the
 *     unextended literal, and from_atom (into_atom) is the
 *     extension.  If an extension is not being used, then
 *     from->atom==from_atom (into->atom==into_atom).
 *
 *************/

void paramod(Clause_ptr from, Term_ptr from_atom, Clause_ptr into,
	     Term_ptr into_atom, Term_ptr into_term, int where, int psn)
{
    int i;

#if 0
    printf("%d%s -> %d%s: %s.\n",
	   from->id, from->literals->atom==from_atom ? " " : "'",
	   into->id, into->literals->atom==into_atom ? " " : "'",
	   where == ALL ? "all" : (where == TOP_ONLY ? "top_only" : "all_but_top"));
#endif

    if (!key_check(from, into))
	return;

    if (VARIABLE(into_term))
	return;

    if (COMPLEX(into_term) && (where == ALL || where == ALL_BUT_TOP)) {
	for (i = 0; i < into_term->arity; i++)
	    paramod(from, from_atom, into, into_atom, into_term->args[i],
		    ALL, into_term->symbol);
	}

     if (!basic_para_check(into_term)) {
	Stats[BASIC_PARAMOD_PRUNES]++;
	return;
	}

    else if ((where == ALL || where == TOP_ONLY) &&
	     (into_term->symbol != psn || !is_assoc_comm(into_term->symbol))) {

	Context_ptr c_from, c_into;
	Bt_node_ptr unify_position;
	Term_ptr alpha, beta;
	Clause_ptr para;
	Literal_ptr lit;

	c_from = get_context();
	c_into = get_context();
	alpha = from_atom->args[0];
	beta = from_atom->args[1];

	unify_position = unify_bt_first(alpha, c_from, into_term, c_into);
	while (unify_position)
	    {
	    if (!prime_para_check(c_from, c_into))
		Stats[PRIME_PARAMOD_PRUNES]++;  /* skip this inference */
	    else
		{
		para = get_clause();
		para->justification = para_parents(from, into,
						   from->literals->atom != from_atom,
						   into->literals->atom != into_atom);
		lit = get_literal();
		para->literals = lit;
		lit->sign = into->literals->sign;
		if (Flags[BASIC_PARAMOD].val)
		    lit->atom = apply_substitute_basic(beta, c_from, into_atom,
						 into_term, c_into);
		else
		    lit->atom = apply_substitute(beta, c_from, into_atom,
						 into_term, c_into);
		    
		CLOCK_STOP(PARAMOD_TIME)
 	        process_derived_clause(para);
		CLOCK_START(PARAMOD_TIME)
		}
	    unify_position = unify_bt_next(unify_position);
	    }
	free_context(c_from);
	free_context(c_into);
	}
	
}  /* paramod */

/*************
 *
 *   para_from_into(c, d, where)
 *
 *   assume where is ALL or ALL_BUT_TOP
 *
 *************/

void para_from_into(Clause_ptr c, Clause_ptr d, int where)
{
    Literal_ptr cl, dl;
    Term_ptr c_ext_atom, d_ext_atom;
    int from_c;

    /* If semantics are on, skip inference if both parents true. */

    if (!semantic_para_parents_check(c, d)) {
	Stats[SEMANTIC_PARAMOD_PRUNES]++;
	return;
	}

    CLOCK_START(PARAMOD_TIME)

    cl = c->literals;
    dl = d->literals;

    from_c = pos_eq_lit(cl);

    if (from_c)
	paramod(c, cl->atom, d, dl->atom, dl->atom->args[0], where, 0);

    if (Internal_flags[AC_PRESENT] && Flags[AC_EXTEND].val) {
	
	/* Note that negative equalities are not extended. */
	
	c_ext_atom = (from_c && alpha_is_ac(cl)) ? extend_atom(cl->atom) : NULL;
	d_ext_atom = (pos_eq_lit(dl) && alpha_is_ac(dl)) ? extend_atom(dl->atom) : NULL;
	
	if (c_ext_atom)
	    paramod(c, c_ext_atom, d, dl->atom, dl->atom->args[0], where, 0);
	
	/* Note that when going into extensions, go into top level only. */
	
	if (c_ext_atom && d_ext_atom && where == ALL)
	    paramod(c, c_ext_atom, d, d_ext_atom, d_ext_atom->args[0], TOP_ONLY, 0);
	
	if (c_ext_atom)
	    zap_extended_atom(c_ext_atom);
	if (d_ext_atom)
	    zap_extended_atom(d_ext_atom);
	}
	
    CLOCK_STOP(PARAMOD_TIME)

}  /* para_from_into */

/*************
 *
 *     alpha_is_ac(lit)
 *
 *************/

int alpha_is_ac(Literal_ptr lit)
{
    return(is_symbol(lit->atom->symbol,"=",2) && is_assoc_comm(lit->atom->args[0]->symbol));
}  /* alpha_is_ac */

/*************
 *
 *     eq_lit(lit)
 *
 *************/

int eq_lit(Literal_ptr lit)
{
    return(is_symbol(lit->atom->symbol, "=", 2));
}  /* eq_lit */

/*************
 *
 *     pos_eq_lit(lit)
 *
 *************/

int pos_eq_lit(Literal_ptr lit)
{
    return(lit->sign && is_symbol(lit->atom->symbol, "=", 2));
}  /* pos_eq_lit */

/*************
 *
 *     extend_atom(lit)
 *
 *************/

Term_ptr extend_atom(Term_ptr atom)
{
    Term_ptr  t,  a,  b,  ax,  bx;

    t = get_term(2); t->symbol = atom->symbol;
    a = get_term(2); a->symbol = atom->args[0]->symbol;
    b = get_term(2); b->symbol = a->symbol;

    ax = get_term(0); ax->symbol = MAX_VARS-1;
    bx = get_term(0); bx->symbol = MAX_VARS-1;

    t->args[0] = a; t->args[1] = b;
    a->args[0] = atom->args[0]; a->args[1] = ax;
    b->args[0] = atom->args[1]; b->args[1] = bx;

    return(t);
}  /* extend_atom */

/*************
 *
 *     zap_extended_atom(atom)
 *
 *************/

void zap_extended_atom(Term_ptr atom)
{
    free_term(atom->args[1]->args[1]);
    free_term(atom->args[1]);

    free_term(atom->args[0]->args[1]);
    free_term(atom->args[0]);

    free_term(atom);
}  /* zap_extended_atom */

/*************
 *
 *     poly1(t) -- Stickel's function for x^3=x ring problem (CADE '84)
 *
 *************/

int poly1(Term_ptr t)
{
    if (VARIABLE(t))
	return(2);
    else if (CONSTANT(t))
	return(2);
    else {
        if (is_symbol(t->symbol, "=", 2))
	    return(poly1(t->args[0]) + poly1(t->args[1]) + 1);
	else if (is_symbol(t->symbol, "+", 2))
	    return(poly1(t->args[0]) + poly1(t->args[1]) + 1);
	else if (is_symbol(t->symbol, "f", 2))
	    return(poly1(t->args[0]) + poly1(t->args[1]) + 1);
	else if (is_symbol(t->symbol, "*", 2))
	    return(poly1(t->args[0]) * poly1(t->args[1]));
	else if (is_symbol(t->symbol, "m", 1))
	    return(poly1(t->args[0]) * 7 + 1);
	else
	    return(-INT_MAX);
	}
}  /* poly1 */

/*************
 *
 *     term_weight(t)
 *
 *************/

int term_weight(Term_ptr t)
{
    int wt;

    switch (Parms[WEIGHT_FUNCTION].val) {
      case 0: wt = symbol_count(t); break;
      case 1: wt = poly1(t); break;
      default: abend("in term_weight, weight_function out of range.");
	}
    return(wt);
}  /* term_weight */

/*************
 *
 *    orient_eq_literals(c)
 *
 *************/

void orient_eq_literals(Clause_ptr c)
{
    Literal_ptr lit;
    Term_ptr t0, t1;
    int w0, w1, oriented, flipped, n;

    CLOCK_START(ORIENT_EQ_TIME)
    
    for (lit = c->literals, n = 1; lit; lit = lit->next, n++) {
	flipped = 0;
	if (Flags[LRPO].val)
	    flipped = orient_literal_lrpo(lit);
	else {
	    if (eq_lit(lit)) {
		t0 = lit->atom->args[0];
		t1 = lit->atom->args[1];
		w0 = term_weight(t0);
		w1 = term_weight(t1);
		if (w1 > w0) {
		    flipped = 1;
		    lit->atom->args[0] = t1;
		    lit->atom->args[1] = t0;
		    }
		if (w0 != w1)
		    set_term_oriented(lit->atom);
		}
	    }
	if (flipped) {
	    Gen_ptr_ptr p1 = get_gen_ptr(); 
	    Gen_ptr_ptr p2 = get_gen_ptr();

	    p1->u.i = FLIP_RULE;
	    p2->u.i = n;
	    p1->next = p2;
	    
	    for (p2 = c->justification; p2 && p2->next; p2 = p2->next);
	    if (p2)
		p2->next = p1;
	    else
		c->justification = p1;
	    }
	}
    CLOCK_STOP(ORIENT_EQ_TIME)
}  /* orient_eq_lit */

/*************
 *
 *    renum_vars_recurse(term, varnums) -- called from renumber_vars.
 *
 *************/

void renum_vars_recurse(Term_ptr t, int varnums[])
{
    int i;

    if (COMPLEX(t)) {
	for (i = 0; i < t->arity; i++)
            renum_vars_recurse(t->args[i], varnums);
        }
    else if (VARIABLE(t)) {
        i = 0;
        while (i < MAX_VARS && varnums[i] != -1 && varnums[i] != t->symbol)
	    i++;
        if (i == MAX_VARS)
            abend("in renum_vars_recurse, too many variables.");
        else {
            if (varnums[i] == -1)
                varnums[i] = t->symbol;
	    t->symbol = i;
            }
        }
}  /* renum_vars_recurse */

/*************
 *
 *    void renumber_vars(c)
 *
 *    Renumber the variables of a clause, starting with 0.
 *    Abend if more than MAXVARS distinct variables.
 *
 *************/

void renumber_vars(Clause_ptr c)
{
    Literal_ptr lit;
    int varnums[MAX_VARS];
    int i;

    for (i = 0; i < MAX_VARS; i++)
        varnums[i] = -1;
    for (lit = c->literals; lit; lit = lit->next)
	renum_vars_recurse(lit->atom, varnums);
}  /* renumber_vars */

/*************
 *
 *     add_vars(t, v)
 *
 *     Count the number of occurrences of each variable.
 *
 *************/

void add_vars(Term_ptr t, int v[])
{
    if (VARIABLE(t))
	v[t->symbol]++;
    else {
	int i;
	for (i = 0; i < t->arity; i++)
	    add_vars(t->args[i], v);
	}
}  /* add_vars */

/*************
 *
 *     multi_superset(t1, t2)
 *
 *     Return 0 if any variable has more occurrences in t2.
 *
 *************/

int multi_superset(Term_ptr t1, Term_ptr t2)
{
    int i, ok, v1[MAX_VARS], v2[MAX_VARS];

    for (i=0; i<MAX_VARS; i++)
	v1[i] = v2[i] = 0;

    add_vars(t1, v1);
    add_vars(t2, v2);

    for (i=0, ok=1; i<MAX_VARS && ok; i++) {
	if (v2[i] > v1[i])
	    ok = 0;
	}
    return(ok);
}  /* multi_superset */

/*************
 *
 *     eq_can_be_demod(lit)
 *
 *************/

int eq_can_be_demod(Literal_ptr lit)
{
    if (!pos_eq_lit(lit))
	return(0);
    else if (!term_oriented(lit->atom))
	return(0);
    else if (Flags[LRPO].val)
	return(1);
    else {
	Term_ptr alpha, beta;
	
	alpha = lit->atom->args[0];
	beta = lit->atom->args[1];
	return(term_weight(alpha) > term_weight(beta) &&
	       (Parms[WEIGHT_FUNCTION].val > 0 ||
		multi_superset(alpha, beta)));
	}
}  /* eq_can_be_demod */

/*************
 *
 *    orient_literal_lrpo(lit)
 *   
 *    Flip args if the right side is greater.
 *    set_term_oriented if left side greater (after possible flip).
 *    Return 1 iff flipped.
 *
 *************/

int orient_literal_lrpo(Literal_ptr lit)
{
    Term_ptr alpha, beta, atom;
    int first_bigger, second_bigger, flipped;

    atom = lit->atom;
    flipped = 0;
    if (is_symbol(atom->symbol, "=", 2)) {
        alpha = atom->args[0];
        beta  = atom->args[1];
        
        first_bigger = lrpo_greater(alpha, beta);
        if (!first_bigger) {
            second_bigger = lrpo_greater(beta, alpha);
            if (second_bigger) {
		flipped = 1;
                atom->args[0] = beta;
                atom->args[1] = alpha;
                }
            }
        if (first_bigger || second_bigger)
	    set_term_oriented(atom);
        }
    return(flipped);
}  /* orient_literal_lrpo */

/*************
 *
 *   ordered_alpha_beta_check(alpha, beta, subst)
 *
 *   Return( beta' is not LRPO-greater than alpha' ).
 *
 *************/

static int ordered_alpha_beta_check(Term_ptr alpha, Term_ptr beta, Context_ptr subst)
{
    Term_ptr alpha_p;
    Term_ptr beta_p;
    int rc;

    alpha_p = apply(alpha, subst);
    beta_p  = apply(beta,  subst);
    rc = !lrpo_greater(beta_p, alpha_p);
    zap_term(alpha_p);
    zap_term(beta_p);
    return(rc);
}  /* ordered_alpha_beta_check */

/*************
 *
 *   build_para()
 *
 *************/

static Clause_ptr build_para(Clause_ptr from_cl, Literal_ptr from_lit,
			     Term_ptr beta,
			     Clause_ptr into_cl, Term_ptr into_term,
			     Context_ptr cf, Context_ptr ci)
{
    Clause_ptr para;
    Literal_ptr l1, l2, l3;

    para = get_clause();
    para->justification = para_parents(from_cl, into_cl, 0, 0);

    l3 = NULL;

    for (l1 = from_cl->literals; l1; l1 = l1->next) {
	if (l1 != from_lit) {
	    l2 = get_literal();
	    l2->sign = l1->sign;
	    l2->atom = apply(l1->atom, cf);
	    if (l3)
		l3->next = l2;
	    else
		para->literals = l2;
	    l3 = l2;
	    }
	}

    for (l1 = into_cl->literals; l1; l1 = l1->next) {
	l2 = get_literal();
	l2->sign = l1->sign;
	if (Flags[BASIC_PARAMOD].val)
	    l2->atom = apply_substitute_basic(beta, cf, l1->atom, into_term,ci);
	else
	    l2->atom = apply_substitute(beta, cf, l1->atom, into_term, ci);
	if (l3)
	    l3->next = l2;
	else
	    para->literals = l2;
	l3 = l2;
	}
    return(para);

}  /* build_para */

/*************
 *
 *   para_with_indexing_term()
 *
 *   Paramodulate into "into_term" (which is in clause c) and
 *   its subterms, using indexing to find "from" clauses.
 *
 *************/

static void para_with_indexing_term(Clause_ptr c, Fpa_index_ptr idx,
				    Term_ptr into_term)
{

    if (VARIABLE(into_term))
	return ;  /* do nothing (never paramodulate into variables) */
    else {
	int i;

	for (i = 0; i < into_term->arity; i++)
	    para_with_indexing_term(c, idx, into_term->args[i]);

	if (!basic_para_check(into_term)) {
	    Stats[BASIC_PARAMOD_PRUNES]++;
	    }
	else {
	    Context_ptr cf, ci;
	    Fpa_pos_ptr pos;
	    Clause_ptr para, from_cl;
	    Literal_ptr from_lit;
	    Term_ptr alpha, beta;

	    ci = get_context();
	    cf = get_context();

	    alpha = fpa_retrieve_first(into_term, idx, UNIFY, ci, cf, &pos);
	    while (alpha) {
		from_cl = alpha->containing_clause;

		from_lit = from_cl->literals;
		while (from_lit->atom->args[0] != alpha)
		    from_lit = from_lit->next;

		beta = from_lit->atom->args[1];

		if (!ordered_para_check(from_lit->atom, alpha, beta, cf))
		    Stats[ORDERED_PARAMOD_PRUNES]++;
		else if (!prime_para_check(cf, ci))
		    Stats[PRIME_PARAMOD_PRUNES]++;
		else {
		    para = build_para(from_cl,from_lit,beta,c,into_term,cf,ci);
		    CLOCK_STOP(PARAMOD_TIME)
			process_derived_clause(para);
		    CLOCK_START(PARAMOD_TIME)
		    }
		alpha = fpa_retrieve_next(pos);
		}
	
	    free_context(ci);
	    free_context(cf);
	    }
	}

}  /* para_with_indexing_term */

/*************
 *
 *   para_with_indexing()
 *
 *************/

void para_with_indexing(Clause_ptr c, Fpa_index_ptr idx_alpha,
			Fpa_index_ptr idx_into)
{
    Literal_ptr l;

    CLOCK_START(PARAMOD_TIME)

    /* First paramodulate from c. */

    for (l = c->literals; l; l = l->next) {
	Context_ptr cf, ci;
	Fpa_pos_ptr pos;
	Clause_ptr para;

	cf = get_context();
	ci = get_context();

	if (pos_eq_lit(l)) {
	    Term_ptr alpha, beta, into_term;
	    alpha = l->atom->args[0];
	    beta = l->atom->args[1];
	    into_term = fpa_retrieve_first(alpha, idx_into, UNIFY,
					   cf, ci, &pos);
	    while (into_term) {
		Clause_ptr into_clause = into_term->containing_clause;
		
		if (!ordered_para_check(l->atom, alpha, beta, cf))
		    Stats[ORDERED_PARAMOD_PRUNES]++;
		else if (!basic_para_check(into_term))
		    Stats[BASIC_PARAMOD_PRUNES]++;
		else if (!prime_para_check(cf, ci))
		    Stats[PRIME_PARAMOD_PRUNES]++;
		else if (!semantic_para_parents_check(c, into_clause))
		    Stats[SEMANTIC_PARAMOD_PRUNES]++;
		else {
		    para = build_para(c, l, beta, into_clause,
				      into_term, cf, ci);
		    CLOCK_STOP(PARAMOD_TIME)
		    process_derived_clause(para);
		    CLOCK_START(PARAMOD_TIME)
		    }
		into_term = fpa_retrieve_next(pos);
		}
	    }
	free_context(cf);
	free_context(ci);
	}

    /* Second, paramodulate into c. */

    for (l = c->literals; l; l = l->next) {
	int i;
	if (pos_eq_lit(l)) {
	    /* Don't go into right sides of positive equality literals. */
	    /* Also, don't go into top of left side. */
	    Term_ptr left = l->atom->args[0];
	    for (i = 0; i < left->arity; i++)
		para_with_indexing_term(c, idx_alpha, left->args[i]);
	    }
	else {
	    for (i = 0; i < l->atom->arity; i++)
		para_with_indexing_term(c, idx_alpha, l->atom->args[i]);
	    }
	}

    CLOCK_STOP(PARAMOD_TIME)
}  /* para_with_indexing */

#if 0

/*************
 *
 *   basic_paramod_marks_term()
 *
 *************/

void basic_paramod_marks_term(Term_ptr t)
{
    if (!VARIABLE(t)) {
	int i;
	set_term_nonbasic(t);
	for (i = 0; i < t->arity; i++)
	    basic_paramod_marks_term(t->args[i]);
	}
}  /* basic_paramod_marks_term */

/*************
 *
 *   basic_paramod_marks()
 *
 *   Give all nonvariable terms (including atoms) the term property "basic".
 *
 *************/

void basic_paramod_marks(Clause_ptr c)
{
    Literal_ptr l;
    for (l = c->literals; l; l = l->next)
	basic_paramod_marks_term(l->atom);
}  /* basic_paramod_marks */

#endif

/*************
 *
 *   intersect_nonbasic_marks_term()
 *
 *************/

static void intersect_nonbasic_marks_term(Term_ptr old, Term_ptr new)
{
    if (!VARIABLE(old) && old->symbol == new->symbol) {
	int i;

	if (!term_nonbasic(old))
	    clear_term_nonbasic(new);

	for (i = 0; i < old->arity; i++)
	    intersect_nonbasic_marks_term(old->args[i], new->args[i]);
	}
}  /* intersect_nonbasic_marks_term */

/*************
 *
 *   intersect_nonbasic_marks()
 *
 *   Intersect "nonbasic" properties of old and new, with the result
 *   assigned to new.
 *   Assume literals correspond, in order; that is, one
 *   is an instance of the other.
 *   This is designed for forward subsumption.   If the subsumee
 *   is more basic than the subsumer, some of the nonbasic marks
 *   have to be removed from the subsumer.
 *  
 *
 *************/

void intersect_nonbasic_marks(Clause_ptr old, Clause_ptr new)
{
    Literal_ptr o, n;

    for (o = old->literals, n = new->literals; o && n; o = o->next, n = n->next)
	intersect_nonbasic_marks_term(o->atom, n->atom);
}  /* intersect_nonbasic_marks */

/*************
 *
 *   basic_para_check(into_term)
 *
 *************/

int basic_para_check(Term_ptr into_term)
{
    if (!Flags[BASIC_PARAMOD].val)
	return(1);
    else
	return(!term_nonbasic(into_term));
}  /* basic_para_check */

/*************
 *
 *   ordered_para_check()
 *
 *************/

int ordered_para_check(Term_ptr from_atom, Term_ptr alpha, Term_ptr beta, 
		       Context_ptr from_subst)
{
    if (!Flags[ORDERED_PARAMOD].val)
	return(1);
    else if (term_oriented(from_atom))
	return(1);
    else
	return(ordered_alpha_beta_check(alpha, beta, from_subst));
}  /* ordered_para_check */

/*************
 *
 *   prime_para_check()
 *
 *************/

int prime_para_check(Context_ptr from_subst, Context_ptr into_subst)
{
    if (!Flags[PRIME_PARAMOD].val)
	return(1);
    else
	return(!simplifiable_subst(from_subst) && !simplifiable_subst(into_subst));
}  /* prime_para_check */

/*************
 *
 *   p_term_basic()
 *
 *************/

void p_term_basic(Term_ptr t)
{
    if (term_nonbasic(t))
	printf("#");
    if (VARIABLE(t))
	print_variable(stdout, t);
    else {
	printf("%s", sn_to_str(t->symbol));
        if (t->arity > 0) {
	    int i;
	    printf("(");
	    for (i = 0; i < t->arity; i++) {
		p_term_basic(t->args[i]);
		if (i != t->arity-1)
		    printf(",");
		}
	    printf(")");
	    }
	}
}  /* p_term_basic */

