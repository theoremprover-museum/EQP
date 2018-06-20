#include "Header.h"
#include "Order.h"
#include "List.h"
#include "Symbols.h"

static struct symbol_table *Sym_tab;  /* pointer to symbol table */
static int Sym_ent_count;

/*
 * memory management
 */

static Sym_ent_ptr sym_ent_avail;
static long sym_ent_gets, sym_ent_frees, sym_ent_avails;

/*************
 *
 *    Sym_ent_ptr get_sym_ent()
 *
 *************/

Sym_ent_ptr get_sym_ent(void)
{
    Sym_ent_ptr p;

    sym_ent_gets++;
    if (!sym_ent_avail)
        p = tp_alloc(sizeof(struct sym_ent));
    else {
        sym_ent_avails--;
        p = sym_ent_avail;
        sym_ent_avail = sym_ent_avail->next;
        }
    
    p->name[0] = '\0';
    p->sym_num = 0;
    p->arity = -1;
    p->lex_val = INT_MAX;
    p->lrpo_status = LRPO_LR_STATUS;
    p->skolem = 0;
    p->special_op = 0;
    p->op_type = 0;
    p->op_prec = 0;
    p->assoc_comm = 0;
    p->next = NULL;
    return(p);
}  /* get_sym_ent */

/*************
 *
 *    free_sym_ent()
 *
 *************/

void free_sym_ent(Sym_ent_ptr p)
{
    sym_ent_frees++;
    sym_ent_avails++;
    p->next = sym_ent_avail;
    sym_ent_avail = p;
}  /* free_sym_ent */

/*************
 *
 *   print_symbols_mem()
 *
 *************/

void print_symbols_mem(FILE *fp, int heading)
{
    if (heading)
	fprintf(fp, "  type (bytes each)        gets      frees     in use      avail      bytes\n");

    fprintf(fp, "sym_ent (%4d)      %11ld%11ld%11ld%11ld%9.1f K\n", (int) sizeof(struct sym_ent), sym_ent_gets, sym_ent_frees, sym_ent_gets - sym_ent_frees, sym_ent_avails, (((sym_ent_gets - sym_ent_frees) + sym_ent_avails) * sizeof(struct sym_ent)) / 1024.);
}  /* print_symbols_mem */

/*************
 *
 *   p_symbols_mem()
 *
 *************/

void p_symbols_mem()
{
    print_symbols_mem(stdout, 1);
}  /* p_symbols_mem */

/*
 *  end of memory management
 */

/*************
 *
 *    init_symbol_table()
 *
 *************/

void init_symbol_table(void)
{
    int i;

    Sym_tab = (struct symbol_table *) tp_alloc(sizeof(struct symbol_table));
    for (i = 0; i < SYM_TAB_SIZE; i++)
        Sym_tab->table[i] = NULL;
    Sym_ent_count = 0;
}  /* init_symbol_table */

/*************
 *
 *    empty_sym_tab() -- free all symbols in the symbol table
 *
 *************/

void empty_sym_tab()
{
    Sym_ent_ptr p1, p2;
    int i;

    for (i = 0; i < SYM_TAB_SIZE; i++) {
	p1 = Sym_tab->table[i];
	while (p1) {
	    p2 = p1;
	    p1 = p1->next;
	    free_sym_ent(p2);
	    }
	Sym_tab->table[i] = NULL;
	}
}  /* empty_sym_tab */

/*************
 *
 *   int new_sym_num()
 *
 *   Return the next available symbol number.  It is always negative.
 *
 *   The rightmost 8 bits of the absolute value will not be all zero.
 *   This is so that fpa will not map sym_nums to 0 (the
 *   code for variables).
 *
 *************/

int new_sym_num(void)
{
    Sym_ent_count--;

    if (abs(Sym_ent_count) % 256 == 0)
        Sym_ent_count--;

    return(Sym_ent_count);

}  /* new_sym_num */

/*************
 *
 *   hash_sym(sym_num)
 *
 *************/

static int hash_sym(int sym_num)
{
    return(abs(sym_num) % SYM_TAB_SIZE);
}  /* hash_sym */

/*************
 *
 *    Sym_ent_ptr insert_sym(string, arity)
 *
 *    Insert string/arity into the symbol table and return the symbol
 *    table node.  Do not check if string/arity is already there.
 *
 *************/

Sym_ent_ptr insert_sym(char *s, int arity)
{
    Sym_ent_ptr p;
    int i;

    p = get_sym_ent();
    strcpy(p->name, s);
    p->arity = arity;
    p->sym_num = new_sym_num();
    i = hash_sym(p->sym_num);
    p->next = Sym_tab->table[i];
    Sym_tab->table[i] = p;
    return(p);
}  /* insert_sym */

/*************
 *
 *    int str_to_sn(str, arity) -- Return a symbol number for string/arity.
 *
 *        If the given string/arity is already in the global symbol table,
 *    then return symbol number; else, create a new symbol table entry and
 *    return a new symbol number
 *
 *************/

int str_to_sn(char *str, int arity)
{
    Sym_ent_ptr p, save;
    int i;
    long dummy;

    save = NULL;
    for (i = 0; i < SYM_TAB_SIZE; i++) {
	p = Sym_tab->table[i];
	while (p != NULL) {
	    if (!str_ident(str, p->name))
	        p = p->next;
	    else if (p->arity != arity) {
		save = p;
		p = p->next;
		}
	    else
		return(p->sym_num);
	    }
	}

    if (save && !save->special_op &&
	Flags[CHECK_ARITY].val &&
	!str_ident(str, "$Quantified") &&
	!str_ident(str, "$Hyps") &&
	!str_ident(str, "$Concs")    ) {
		
	fprintf(stderr, "\007\n\nWARNING, multiple arity: %s/%d, %s/%d.\n\n",
		save->name, save->arity, str, arity);
	}

    /* String/arity not in table, so create an entry. */

    p = insert_sym(str, arity);

    if (str[0] == '$' &&
	!initial_str("$Quantified", str) &&
	!initial_str("$ANS", str) &&
	!initial_str("$Ans", str) &&
	!initial_str("$ans", str) &&
	!str_ident(str, "$cons") &&
	!str_ident(str, "$nil") &&
	!str_ident(str, "$Concs") &&
	!str_ident(str, "$Hyps") &&
	!str_long(str+1, &dummy))            /* e.g.,  weight(f($3,a),-2) */

	fprintf(stderr, "\007\n\nWARNING, unrecognized $ symbol: %s.\n\n", str);
	
    return(p->sym_num);
	
}  /* str_to_sn */

/*************
 *
 *    print_syms(file_ptr) -- Display the symbol list.
 *
 *************/

void print_syms(FILE *fp)
{
    Sym_ent_ptr p;
    int i;

    for (i = 0; i < SYM_TAB_SIZE; i++) {
	for (p = Sym_tab->table[i]; p; p = p->next)
	    fprintf(fp, "%d  %s/%d, lex_val=%d\n",
		    p->sym_num, p->name, p->arity, p->lex_val);
	}
}  /* print_syms */

/*************
 *
 *    p_syms()
 *
 *************/

void p_syms(void)
{
    print_syms(stdout);
}  /* p_syms */

/*************
 *
 *    char *sn_to_str(sym_num)  --  given a symbol number, return the name
 *
 *************/

char *sn_to_str(int sym_num)
{
    Sym_ent_ptr p;

    p = Sym_tab->table[hash_sym(sym_num)];
    while (p != NULL && p->sym_num != sym_num)
	p = p->next;
    if (p == NULL)
	return("");
    else
	return(p->name);
}  /* sn_to_str */

/*************
 *
 *    int sn_to_arity(sym_num)  --  given a symbol number, return the arity
 *
 *************/

int sn_to_arity(int sym_num)
{
    Sym_ent_ptr p;

    p = Sym_tab->table[hash_sym(sym_num)];
#ifdef ROO
    p = p->next;  /* skip dummy node */
#endif	
    while (p != NULL && p->sym_num != sym_num)
	p = p->next;
    if (p == NULL)
	return(-1);
    else
	return(p->arity);
}  /* sn_to_arity */

/*************
 *
 *    int sn_to_node(sym_num)
 *
 *    Given a symbol number, return the symbol table node.
 *
 *************/

Sym_ent_ptr sn_to_node(int sym_num)
{
    Sym_ent_ptr p;

    p = Sym_tab->table[hash_sym(sym_num)];
    while (p != NULL && p->sym_num != sym_num)
	p = p->next;
    return(p);  /* possibly NULL */
}  /* sn_to_node */

/*************
 *
 *    sym_tab_member(str, arity)
 *
 *    Similar to str_to_sn, but do not insert if not there,
 *    and return node instead of sn.
 *
 *************/

Sym_ent_ptr sym_tab_member(char *str, int arity)
{
    Sym_ent_ptr p;
    int i;

    for (i = 0; i < SYM_TAB_SIZE; i++) {
        p = Sym_tab->table[i];
        while (p != NULL) {
            if (!str_ident(str, p->name))
                p = p->next;
            else if (p->arity != arity)
                p = p->next;
            else
                return(p);
            }
        }
    return((Sym_ent_ptr ) NULL);

}  /* sym_tab_member */

/*************
 *
 *    int in_sym_tab(s)  --  is s in the symbol table?
 *
 *************/

int in_sym_tab(char *s)
{
    Sym_ent_ptr p;
    int i;

    for (i = 0; i < SYM_TAB_SIZE; i++) {
	p = Sym_tab->table[i];
	while (p != NULL) {
	    if (str_ident(p->name, s))
		return(1);
	    p = p->next;
	    }
	}
    return(0);
}  /* in_sym_tab */

/*************
 *
 *   find_special_nd()
 *
 *************/

Sym_ent_ptr find_special_nd(char *str, int arity)
{
    int i;
    Sym_ent_ptr nd;

    for (i = 0; i < SYM_TAB_SIZE; i++)
	for (nd = Sym_tab->table[i]; nd; nd = nd->next)
	    if (nd->special_op && nd->arity==arity && str_ident(str, nd->name))
		return(nd);
    return(NULL);
}  /* find_special_nd */

/*************
 *
 *    int is_symbol(symbol, str, arity)
 *
 *    Does t have leading function symbol str with arity?
 *
 *************/

int is_symbol(int sym, char *str, int arity)
{
    return(sn_to_arity(sym) == arity && str_ident(sn_to_str(sym), str));
}  /* is_symbol */

/*************
 *
 *    mark_as_skolem(symbol)
 *
 *************/

void mark_as_skolem(int symbol)
{
    Sym_ent_ptr se;

    se = sn_to_node(symbol);

    if (!se) {
        char s[500];
	sprintf(s, "mark_as_skolem, no symbol for %d.", symbol);
	abend(s);
	}
    else
	se->skolem = 1;
}  /* mark_as_skolem */

/*************
 *
 *    int is_skolem(symbol)
 *
 *************/

int is_skolem(int symbol)
{
    Sym_ent_ptr se;

    se = sn_to_node(symbol);

    if (!se) {
        char s[100];
	sprintf(s, "is_skolem, no symbol for %d.", symbol);
	abend(s);
	return(0);  /* to quiet lint */
	}
    else
	return(se->skolem);
}  /* is_skolem */

/*************
 *
 *    int var_name(string) -- Decide if a string represents a variable.
 *
 *        return("string is a variable")
 *
 *************/

int var_name(char *s)
{
    if (Flags[PROLOG_STYLE_VARIABLES].val)
        return((*s >= 'A' && *s <= 'Z') || *s == '_');
    else
        return(*s >= 'u' && *s <= 'z');
}  /* var_name */

/*************
 *
 *    int declare_op(prec, type, str)
 *
 *************/

int declare_op(int prec, int type, char *str)
{
    int arity, sn, save_flag;
    Sym_ent_ptr p;

    if (prec < 1 || prec > 999)
	return(0);

    switch (type) {
      case FX:
      case FY:
      case XF:
      case YF: arity = 1; break;
      case XFX:
      case XFY:
      case YFX: arity = 2; break;
      default: return(0);
	}

    save_flag = Flags[CHECK_ARITY].val;
    Flags[CHECK_ARITY].val = 0;
    sn = str_to_sn(str, arity);
    Flags[CHECK_ARITY].val = save_flag;

    p = sn_to_node(sn);

    /* Don't check if it's already special.  Allow it to change. */

    p->special_op = 1;
    p->op_type = type;
    p->op_prec = prec;
    return(1);

}  /* declare_op */

/*************
 *
 *    init_special_ops()
 *
 *    Declare the built-in special operators.
 *
 *************/

void init_special_ops(void)
{
    int rc;

    rc = declare_op(800,  XFX, "->");
    rc = declare_op(800,  XFX, "<->");
    rc = declare_op(790,  XFY, "|");
    rc = declare_op(780,  XFY, "&");

    rc = declare_op(700,  XFX, "=");
    rc = declare_op(700,  XFX, "!=");

    rc = declare_op(700,  XFX, "<");
    rc = declare_op(700,  XFX, ">");
    rc = declare_op(700,  XFX, "<=");
    rc = declare_op(700,  XFX, ">=");
    rc = declare_op(700,  XFX, "==");
    rc = declare_op(700,  XFX, "=/=");

    rc = declare_op(700,  XFX, "@<");
    rc = declare_op(700,  XFX, "@>");
    rc = declare_op(700,  XFX, "@<=");
    rc = declare_op(700,  XFX, "@>=");

    rc = declare_op(500,  XFY, "+");
    rc = declare_op(500,  XFX, "-");

    rc = declare_op(500,   FX, "+");
    rc = declare_op(500,   FX, "-");

    rc = declare_op(400,  XFY, "*");
    rc = declare_op(400,  XFX, "/");

    rc = declare_op(300,  XFX, "mod");

}  /* init_special_ops */

/*************
 *
 *    int process_op_command(t)
 *
 *************/

int process_op_command(Term_ptr t)
{
    int type, n, rc;
    Term_ptr t1, t2;
    char *s;

    if (t->arity != 3) {
	printf("op command must have arity 3.\n");
	return(0);
	}
    t1 = t->args[0];
    if (!str_int(sn_to_str(t1->symbol), &n) || n < 1 || n > 999) {
	printf("\nERROR: first argument of op command must be 1..999.\n");
	return(0);
	}
    t1 = t->args[1];
    s = sn_to_str(t1->symbol);
    if (str_ident(s, "xfx"))
	type = XFX;
    else if (str_ident(s, "xfy"))
	type = XFY;
    else if (str_ident(s, "yfx"))
	type = YFX;
    else if (str_ident(s, "fx"))
	type = FX;
    else if (str_ident(s, "xf"))
	type = XF;
    else if (str_ident(s, "fy"))
	type = FY;
    else if (str_ident(s, "yf"))
	type = YF;
    else
	type = INT_MAX;

    if (type == INT_MAX || t1->arity != 0) {
	printf("\nERROR: second argument of op command must be xfx, xfy, yfx, xf, yf, fx, or fy.\n");
	return(0);
	}

    t1 = t->args[2];

    if (t1->arity == 0)
	rc = declare_op(n, type, sn_to_str(t1->symbol));
    else if (proper_list(t1)) {
	for ( ; t1->arity == 2; t1 = t1->args[1]) {
	    t2 = t1->args[0];
	    if (t2->arity != 0) {
		printf("\nERROR: list in op command must be all names.\n");
		return(0);
		}
	    rc = declare_op(n, type, sn_to_str(t2->symbol));
	    }
	}
    else {
	printf("\nERROR: third argument of op command must be a name or a list.\n");
	return(0);
	}
    return(1);
}  /* process_op_command */

/*************
 *
 *    set_assoc_comm
 *
 *************/

void set_assoc_comm(char *str)
{
    int sn;
    Sym_ent_ptr p;

    sn = str_to_sn(str, 2);
    p = sn_to_node(sn);
    p->assoc_comm = 1;

}  /* set_assoc_comm */

/*************
 *
 *    is_assoc_comm
 *
 *************/

int is_assoc_comm(int sn)
{
    if (!Internal_flags[AC_PRESENT])
	return(0);
    else {
	Sym_ent_ptr p;
	p = sn_to_node(sn);
	if (!p)
	    return(0);
	else
	    return(p->assoc_comm);
	}

}  /* is_assoc_comm */

/*************
 *
 *    int process_ac_command(t)
 *
 *************/

int process_ac_command(Term_ptr t)
{
    Term_ptr t1, t2;

    if (t->arity != 1) {
	printf("ac command must have arity 1.\n");
	return(0);
	}
    else {
	t1 = t->args[0];
	
	if (t1->arity == 0)
	    set_assoc_comm(sn_to_str(t1->symbol));
	else if (proper_list(t1)) {
	    for ( ; t1->arity == 2; t1 = t1->args[1]) {
		t2 = t1->args[0];
		if (t2->arity != 0) {
		    printf("ERROR: list in assoc_comm command must be all names.\n");
		    return(0);
		    }
		set_assoc_comm(sn_to_str(t2->symbol));
		}
	    }
	else {
	    printf("ERROR: argument of assoc_comm command must be a name or a list.\n");
	    return(0);
	    }
	return(1);
	}
}  /* process_ac_command */

/*************
 *
 *    set_commutative
 *
 *************/

void set_commutative(char *str)
{
    int sn;
    Sym_ent_ptr p;

    sn = str_to_sn(str, 2);
    p = sn_to_node(sn);
    p->commutative = 1;

}  /* set_commutative */

/*************
 *
 *    is_commutative
 *
 *************/

int is_commutative(int sn)
{
    if (!Internal_flags[COMM_PRESENT])
	return(0);
    else {
	Sym_ent_ptr p;
	p = sn_to_node(sn);
	if (!p)
	    return(0);
	else
	    return(p->commutative);
	}
}  /* is_commutative */

/*************
 *
 *    int process_comm_command(t)
 *
 *************/

int process_comm_command(Term_ptr t)
{
    Term_ptr t1, t2;

    if (t->arity != 1) {
	printf("commutative command must have arity 1.\n");
	return(0);
	}
    else {
	t1 = t->args[0];
	
	if (t1->arity == 0)
	    set_commutative(sn_to_str(t1->symbol));
	else if (proper_list(t1)) {
	    for ( ; t1->arity == 2; t1 = t1->args[1]) {
		t2 = t1->args[0];
		if (t2->arity != 0) {
		    printf("ERROR: list in commutative command must be all names.\n");
		    return(0);
		    }
		set_commutative(sn_to_str(t2->symbol));
		}
	    }
	else {
	    printf("ERROR: argument of commutative command must be a name or a list.\n");
	    return(0);
	    }
	return(1);
	}
}  /* process_comm_command */

/*************
 *
 *    int process_multiset_command(t)
 *
 *************/

int process_multiset_command(Term_ptr t)
{
    Term_ptr t1, t2;

    if (t->arity != 1) {
	printf("lrpo_multiset_status command must have arity 1.\n");
	return(0);
	}
    else {
	t1 = t->args[0];
	
	if (proper_list(t1)) {
	    for ( ; t1->arity == 2; t1 = t1->args[1]) {
		t2 = t1->args[0];
		if (t2->arity == 0) {
		    printf("ERROR: list in multiset command must be all complex terms.\n");
		    return(0);
		    }
                sn_to_node(t2->symbol)->lrpo_status = LRPO_MULTISET_STATUS;
		}
	    }
	else {
	    printf("ERROR: argument of multiset command must be a list of complex terms.\n");
	    return(0);
	    }
	return(1);
	}
}  /* process_multiset_command */

/*************
 *
 *    int process_lex_command(t)
 *
 *************/

int process_lex_command(Term_ptr t)
{
    Term_ptr t1, t2;
    int count;

    if (t->arity != 1) {
	printf("lex command must have arity 1.\n");
	return(0);
	}
    else {
	count = 0;
	t1 = t->args[0];
	
	if (proper_list(t1)) {
	    for ( ; t1->arity == 2; t1 = t1->args[1]) {
		t2 = t1->args[0];
		if (VARIABLE(t2)) {
		    printf("ERROR: variable in lex command.\n");
		    return(0);
		    }
		sn_to_node(t2->symbol)->lex_val = ++count;
		}
	    return(1);
	    }
	else {
	    printf("ERROR: argument of lex command must be a list.\n");
	    return(0);
	    }
	}
}  /* process_lex_command */

/*************
 *
 *    int proper_list(t)
 *
 *************/

int proper_list(Term_ptr t)
{
    if (VARIABLE(t))
        return(0);
    else if (CONSTANT(t))
        return(t->symbol == str_to_sn("$nil", 0));
    else if (t->symbol == str_to_sn("$cons", 2))
        return(proper_list(t->args[1]));
    else
        return(0);
}  /* proper_list */

/*************
 *
 *    int list_length(t)
 *
 *************/

int list_length(Term_ptr t)
{
    if (VARIABLE(t))
        return(-1);
    else if (CONSTANT(t)) {
	if (t->symbol == str_to_sn("$nil", 0))
	    return(0);
	else
	    return(-1);
	}
    else if (t->symbol == str_to_sn("$cons", 2))
        return(1 + list_length(t->args[1]));
    else
        return(-1);
}  /* list_length */

/*************
 *
 *   compare_for_auto_lex_order()
 *
 *   First sort on arity:  0 < MAX_INT < ... < 3 < 2 < 1.
 *   Within arity, use strcmp function.
 *
 *************/

int compare_for_auto_lex_order(void *d1, void *d2)
{
    struct sym_ent *p1, *p2;
    int i;

    p1 = (struct sym_ent *) d1;
    p2 = (struct sym_ent *) d2;

    if (p1->arity == p2->arity) {
	i = strcmp(p1->name, p2->name);
	if (i < 0)
	    return(LESS_THAN);
	else if (i > 0)
	    return(GREATER_THAN);
	else {
	    char s[500];
	    sprintf(s, "compare_for_auto_lex_order, strings the same: %s.", p1->name);
	    abend(s);
	    }
	}

    else if (p1->arity == 0)
	return(LESS_THAN);
    else if (p2->arity == 0)
	return(GREATER_THAN);
    else if (p1->arity < p2->arity)
	return(GREATER_THAN);
    else
	return(LESS_THAN);
}  /* compare_for_auto_lex_order */

/*************
 *
 *    auto_lex_order()
 *
 *    Go through the symbol table and find the symbols whose
 *    lex_vals has not been previously set.  Order those symbols
 *    according to compare_for_auto_lex_order, assigning them
 *    lex_vals starting with a number greater than the maximum
 *    previously set lex val.  That is, all of the automatically
 *    assigned lex_vals are greater than any explicitly set.
 *
 *************/

void auto_lex_order(void)
{
    int i, j, n, max;
    struct sym_ent *p;
    struct sym_ent *a[500], **w[500];

    if (abs(Sym_ent_count) > 500)
	abend("auto_lex_order, too many symbols.");

    max = 0;
    for (i = j = 0; i < SYM_TAB_SIZE; i++)
        for (p = Sym_tab->table[i]; p; p = p->next) {
	    if (p->lex_val == INT_MAX)
		a[j++] = p;
	    else
		max = (p->lex_val > max ? p->lex_val : max);
	    }

    /* We find j symbols whose lex_vals have not been set. */

    merge_sort((void **) a, (void **) w, 0, j-1, compare_for_auto_lex_order);

    /* Symbols get lex vals 2, 4, 6, 8, ... . */

    for (i = 0; i < j; i++) {
        a[i]->lex_val = i + 1 + max;
#if 0
        printf("%7s %d %d\n", a[i]->name, a[i]->arity, a[i]->lex_val);
#endif
        }

}  /* auto_lex_order */
