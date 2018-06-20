#include "Header.h"
#include "Order.h"
#include "List.h"
#include "Symbols.h"
#include "Io.h"

/* Default is "x+y"; define OUTPUT_SPACE for "x + y". */

#define OUTPUT_SPACE

/* Token types */

#define OPEN_PAREN  1
#define OTHER_PUNC  2
#define NAME_SYM    6
#define SYM_SYM     7

/* Following structure is to store data on symbol that might be special op. */
 struct sequence_member {
    Term_ptr t;
    short  binary_type;
    short  binary_prec;
    short  unary_type;
    short  unary_prec;
    };

/* The following declarations are for mutually recursive routines. */

 Term_ptr seq_to_term(struct sequence_member *seq,int start,int end,int m);
 Term_ptr str_to_term(char *buf, int *p, int in_list);

/*************
 *
 *    int is_alpha_numeric(c) -- including _ and $
 *
 *************/

static int is_alpha_numeric(char c)
{
    return((c >= '0' && c <= '9') ||
	   (c >= 'a' && c <= 'z') ||
	   (c >= 'A' && c <= 'Z') ||
	   c == '_' ||
	   c == '$');
}  /* is_alpha_numeric */

/*************
 *
 *    int name_sym(s)
 *
 *************/

static int name_sym(char *s)
{
    if (*s == '\'' || *s == '\"')
	return(1);  /* quoted string ok */
    else {
	for ( ; *s; s++)
	    if (!is_alpha_numeric(*s))
		return(0);
	return(1);
	}
}  /* name_sym */

/*************
 *
 *    int is_white(c) -- including start-of-comment '%'.
 *
 *************/

static int is_white(char c)
{
    return(c == ' ' ||
	   c == '\t' ||  /* tab */
	   c == '\n' ||  /* newline */
	   c == '\v' ||  /* vertical tab */
	   c == '\r' ||  /* carriage return */
	   c == '\f' ||  /* form feed */
	   c == '%');
}  /* is_white */

/*************
 *
 *    skip_white(buffer, position)
 *
 *    Advance the pointer to the next nonwhite, noncomment position.
 *
 *************/

static void skip_white(char *buf, int *p)
{
    char c;
    c = buf[*p];
    while (is_white(c)) {
	if (c == '%')  /* skip over comment */
	    while (buf[++(*p)] != '\n') ;
	c = buf[++(*p)];
	}
}  /* skip_white */

/*************
 *
 *    int is_symbol_char(c, in_list)
 *
 *************/

static int is_symbol_char(char c, int in_list)
{
    return(c == '+' ||
	   c == '-' ||
	   c == '*' ||
	   c == '/' ||
	   c == '\\' ||
	   c == '^' ||
	   c == '<' ||
	   c == '>' ||
	   c == '=' ||
	   c == '`' ||
	   c == '~' ||
	   c == ':' ||
	   c == '?' ||
	   c == '@' ||
	   c == '&' ||
	
	   (c == '|' && !in_list) ||
	
	   c == '!' ||
	   c == '#' ||
	   c == ';'    );

}  /* is_symbol_char */

/****************************************

write_term outputs a term in readable format (w.r.t. infix, prefix,
and postfix operators) and without extra parentheses.  It it much
complicated by one feature: deciding where to omit space around
the special operators.  For example, just as we can input a+b+c
instead of a + b + c, we wish to output without spaces were possible.
(I'm sorry the code is so complicated---I couldn't see a simpler way
of doing it.)

There are 2 types of constant/operator/functor:

    NAME_SYM: string of alphanumerics, $, and _.  Also quoted string.
    SYM_SYM:  string of *+-/\^<>=`~:?@&!;# and sometimes | (if not in list)

For completeness, the other characters are
    ,()[]{} and sometimes | (if in list)   puctuation for building terms
    .                                      end of input term
    %                                      start of comment
    "'                                     for quoted strings

For this problem, tokens are of 4 types:
    NAME_SYM
    SYM_SYM
    OPEN_PAREN  '('
    OTHER_PUNC   other punctuation (including space)

Special ops that are NAME_SYMs are always surrounded by spaces.

Here are the space rules for SYM_SYM special ops:

    infix
        omit space before if preceding token is NAME_SYM or OTHER_PUNC
        omit space after if next token is is NAME_SYM or OTHER_PUNC
              (note that space is included if next is '(')

    prefix
        omit space before if preceding token is OTHER_PUNC
        omit space after if next token is is NAME_SYM or OTHER_PUNC

    postfix
        omit space before if preceding token is NAME_SYM or OTHER_PUNC
        always include space after (could omit if next token is OTHER_PUNC,
            but current mechanism won't handle that, and it's not
            that important)

*****************************************/

/*************
 *
 *    int next_token_type(t, n)
 *
 *    Find the next token type that would be output for t.
 *    n is precedence parameter as in write term.
 *
 *************/

static int next_token_type(Term_ptr t, int n)
{
    struct sym_ent *s;
    int na1;
    char *str;


    if (VARIABLE(t))
	return(NAME_SYM);
    else if (t->arity == 0) {
	str = sn_to_str(t->symbol);
        if (str_ident(str, "$nil"))
	    return(OTHER_PUNC);
	else
	    return(name_sym(str) ? NAME_SYM : SYM_SYM);
	}
    else {  /* complex */
	str = sn_to_str(t->symbol);
	if (str_ident(str, "$cons") && t->arity == 2)
	    return(OTHER_PUNC);
	else if (str_ident(str, "$Quantified")) {
	    /* parens if parent is special op */
	    if (n < 1000)
		return(OPEN_PAREN);
	    else
		return(next_token_type(t->args[0], 0));
	    }
	else {
	    s = sn_to_node(t->symbol);
	    if (s->special_op && s->arity == 2) {
		na1 = s->op_prec;
		if (s->op_type == XFX || s->op_type == XFY)
		    na1--;
		if (s->op_prec > n)
		    return(OPEN_PAREN);
		else
		    return(next_token_type(t->args[0], na1));
		}
	    else if (s->special_op && s->arity == 1) {
		na1 = s->op_prec;
		if (s->op_type == FX || s->op_type == XF)
		    na1--;

		if (s->op_prec > n)
		    return(OPEN_PAREN);
		if (s->op_type == FX || s->op_type == FY)
		    return(name_sym(str) ? NAME_SYM : SYM_SYM);
		else
		    return(next_token_type(t->args[0], na1));
		}
	    else
		return(name_sym(str) ? NAME_SYM : SYM_SYM);
	    }
	}
}  /* next_token_type */

/*************
 *
 *    write_term(file_ptr, term, n, prev) -- print in readable form.
 *
 *************/

void write_term(FILE *fp, Term_ptr t, int n, int *prev)
{
    Term_ptr t1;
    struct sym_ent *s;
    int na1, na2, next;
    char *str, *s1, *s2;

    if (!t) {
	fprintf(fp, "<write_term gets NULL pointer>");
	return;
	}

    if (VARIABLE(t)) {
	print_variable(fp, t);
	*prev = NAME_SYM;
	}

    else if (t->arity == 0) {
	str = sn_to_str(t->symbol);
	if (str_ident(str, "$nil")) {
	    fprintf(fp, "[]");
	    *prev = OTHER_PUNC;
	    }
	else {
	    fprintf(fp, "%s", str);
	    *prev = (name_sym(str) ? NAME_SYM : SYM_SYM);
	    }
	}

    else {  /* complex */
	str = sn_to_str(t->symbol);
	
	if (str_ident(str, "$Quantified")) {  /* Quantified Formula */
	    int i;
	    /* parens if parent is special op */
	    if (n < 1000) {
		fprintf(fp, "("); *prev = OPEN_PAREN;
		}
	    for (i = 0; i < t->arity; i++) {
		/* parens if special op in child */
		write_term(fp, t->args[i], 0, prev);
		if (i != t->arity-1) {
		    fprintf(fp, " "); *prev = OTHER_PUNC;
		    }
		}
	    if (n < 1000) {
		fprintf(fp, ")"); *prev = OTHER_PUNC;
		}
	    }   /* end Formula */

	else if (is_symbol(t->symbol, "$cons", 2)) {
	    fprintf(fp, "["); *prev = OTHER_PUNC;
	    write_term(fp, t->args[0], 1000, prev);
	    t1 = t->args[1];
	    while (t1->symbol == str_to_sn("$cons", 2)) {
		fprintf(fp, ","); *prev = OTHER_PUNC;
		write_term(fp, t1->args[0], 1000, prev);
		t1 = t1->args[1];
		}
	    if (t1->symbol == str_to_sn("$nil", 0)) {
		fprintf(fp, "]"); *prev = OTHER_PUNC;
		}
	    else {
		fprintf(fp, "|"); *prev = OTHER_PUNC;
		write_term(fp, t1, 1000, prev);
		fprintf(fp, "]"); *prev = OTHER_PUNC;
		}
	    }   /* end list */

	else {
	    s = sn_to_node(t->symbol);

	    if (s->special_op && s->arity == 2) {  /* infix */
		na1 = na2 = s->op_prec;
		if (s->op_type == XFX || s->op_type == XFY)
		    na1--;
		if (s->op_type == XFX || s->op_type == YFX)
		    na2--;
		if (s->op_prec > n) {
		    fprintf(fp, "("); *prev = OPEN_PAREN;
		    }
		write_term(fp, t->args[0], na1, prev);

		/* Decide on spaces around infix op. */

#ifdef OUTPUT_SPACE
		s1 = s2 = " ";
#else
		if (name_sym(str))
		    s1 = s2 = " ";
		else {
		    if (*prev == OTHER_PUNC || *prev == NAME_SYM)
			s1 = "";
		    else
			s1 = " ";
		    next = next_token_type(t->args[1], na2);
                    if (next == OTHER_PUNC || next == NAME_SYM)
                        s2 = "";
                    else
                        s2 = " ";
		    }
#endif
		
		fprintf(fp, "%s%s%s", s1,str,s2);
		if (str_ident(s2, " "))
		    *prev = OTHER_PUNC;
		else
		    *prev = (name_sym(str) ? NAME_SYM : SYM_SYM);
		write_term(fp, t->args[1], na2, prev);
		if (s->op_prec > n) {
		    fprintf(fp, ")"); *prev = OTHER_PUNC;
		    }
		}

	    else if (s->special_op && s->arity == 1) {  /* prefix,postfix */
		na1 = s->op_prec;
		if (s->op_type == FX || s->op_type == XF)
		    na1--;

		if (s->op_prec > n) {
		    fprintf(fp, "("); *prev = OPEN_PAREN;
		    }

		if (s->op_type == FX || s->op_type == FY) {
		
		    /* Decide on spaces around special prefix op. */

		    if (name_sym(str))
			s1 = s2 = " ";
		    else {
			if (*prev == OTHER_PUNC || *prev == OPEN_PAREN)
			    s1 = "";
			else
			    s1 = " ";
			next = next_token_type(t->args[0], na1);
			if (next == OTHER_PUNC || next == OPEN_PAREN || next == NAME_SYM)
			    s2 = "";
			else
			    s2 = " ";
			}
		
		    fprintf(fp, "%s%s%s", s1,str,s2);
		    if (str_ident(s2, " "))
			*prev = OTHER_PUNC;
		    else
			*prev = (name_sym(str) ? NAME_SYM : SYM_SYM);
		    write_term(fp, t->args[0], na1, prev);
		    }
		else {
		    write_term(fp, t->args[0], na1, prev);

		    /* Decide on spaces around special postfix op. */

		    if (name_sym(str))
			s1 = s2 = " ";
		    else {
			if (*prev == OTHER_PUNC || *prev == NAME_SYM)
			    s1 = "";
			else
			    s1 = " ";
			/* Can't easily tell next token, so just output space. */
			s2 = " ";
			}
		
		    fprintf(fp, "%s%s%s", s1,str,s2);
		    *prev = OTHER_PUNC;
		    }

		if (s->op_prec > n) {
		    fprintf(fp, ")"); *prev = OTHER_PUNC;
		    }
		}

	    else {  /* functor(args) */
		int i;
		fprintf(fp, "%s", str);
		fprintf(fp, "("); *prev = OPEN_PAREN;
		for (i = 0; i < t->arity; i++) {
		    write_term(fp, t->args[i], 1000, prev);
		    if (i != t->arity-1) {
			fprintf(fp, ","); *prev = OTHER_PUNC;
			}
		    }
		fprintf(fp, ")"); *prev = OTHER_PUNC;
		}
	    }
	}
}  /* write_term */

/*************
 *
 *    display_term(file_ptr, term)  --  Display a term in internal form.
 *
 *************/

void display_term(FILE *fp, Term_ptr t)
{
    if (!t)
	fprintf(fp, "<display_term gets NULL pointer>");
    else if (VARIABLE(t))
	print_variable(fp, t);
    else {
	fprintf(fp, "%s", sn_to_str(t->symbol));
        if (t->arity > 0) {
	    int i;
	    fprintf(fp, "(");
	    for (i = 0; i < t->arity; i++) {
		display_term(fp, t->args[i]);
		if (i != t->arity-1)
		    fprintf(fp, ",");
		}
	    fprintf(fp, ")");
	    }
	}
}  /* display_term */

/*************
 *
 *    d_term(term)  --  display_term and \n to the standard output.
 *
 *************/

void d_term(Term_ptr t)
{
    display_term(stdout, t);
    printf("\n");
    fflush(stdout);
}  /* p_term */

/*************
 *
 *    print_term(file_ptr, term)  --  Print a term to a file.
 *
 *    Flag determines write_term vs. display_term.
 *
 *************/

void print_term(FILE *fp, Term_ptr t)
{
    int i;

    if (Flags[DISPLAY_TERMS].val)
	display_term(fp, t);
    else {
	i = OTHER_PUNC;  /* Assume previous token is punctuation. */
	write_term(fp, t, 1000, &i);
	}
}  /* print_term */

/*************
 *
 *    p_term(term)  --  print_term and \n to the standard output.
 *
 *************/

void p_term(Term_ptr t)
{
    print_term(stdout, t);
    printf("\n");
    fflush(stdout);
}  /* p_term */

/*************
 *
 *    print_term_nl(fp, term)  --  print_term followed by period and newline
 *
 *************/

void print_term_nl(FILE *fp, Term_ptr t)
{
    print_term(fp, t);
    fprintf(fp,".\n");
}  /* print_term_nl */

/*************
 *
 *   print_variable(fp, variable)
 *
 *************/

void print_variable(FILE *fp, Term_ptr t)
{
    int i;

    if (Flags[PROLOG_STYLE_VARIABLES].val) {
	fprintf(fp, "%c", (t->symbol % 26) + 'A');
	i = t->symbol / 26;
	if (i > 0)
	    fprintf(fp, "%d", i);
	}
    else {
	if (t->symbol <= 2)
	    fprintf(fp, "%c", 'x'+t->symbol);
	else if (t->symbol <= 5)
	    fprintf(fp, "%c", 'r'+t->symbol);
	else
	    fprintf(fp, "%c%d", 'v', t->symbol);
	}
}  /* print_variable */

/*************
 *
 *    void fill_in_op_data(p, t)
 *
 *************/

static void fill_in_op_data(struct sequence_member *p, Term_ptr t)
{
    struct sym_ent *nd;
    char *str;

    p->t = t;
    p->binary_type = p->unary_type = 0;
    p->binary_prec = p->unary_prec = 0;

    if (t->arity == 0) {
	str = sn_to_str(t->symbol);
	nd = find_special_nd(str, 1);
	if (nd) {
	    p->unary_type = nd->op_type;
	    p->unary_prec = nd->op_prec;
	    }
	nd = find_special_nd(str, 2);
	if (nd) {
	    p->binary_type = nd->op_type;
	    p->binary_prec = nd->op_prec;
	    }
	}
}  /* fill_in_op_data */

/*************
 *
 *    get_name(buffer, position, name, in_list)
 *
 *************/

static void get_name(char *buf, int *p, char *name, int in_list)
{
    int i, ok, okq;
    char c, q;

    i = 0; ok = 1; okq = 1;
    skip_white(buf, p);
    c = buf[*p];
    if (is_alpha_numeric(c)) {
	while ((ok = i < MAX_NAME-1) && is_alpha_numeric(c)) {
	    name[i++] = c;
	    c = buf[++(*p)];
	    }
	}
    else if (is_symbol_char(c, in_list)) {
	while ((ok = i < MAX_NAME-1) && is_symbol_char(c, in_list)) {
	    name[i++] = c;
	    c = buf[++(*p)];
	    }
	}
    else if (c == '\'' || c == '\"') {
	q = c;
	name[i++] = c;
	c = buf[++(*p)];
	while ((ok = i < MAX_NAME-1) && c != q && (okq = c != '\0')) {
	    name[i++] = c;
	    c = buf[++(*p)];
	    }
	if (okq) {
	    name[i++] = c;  /* quote char */
	    ++(*p);
	    }
	}

    if (!ok) {
	fprintf(stdout, "\nERROR, name too big, max is %d; ", MAX_NAME-1);
	name[0] = '\0';
	}
    else if (!okq) {
	fprintf(stdout, "\nERROR, quoted name has no end; ");
	name[0] = '\0';
	}
    else
	name[i] = '\0';
}  /* get_name */

/*************
 *
 *    print_error(fp, buf, pos)
 *
 *************/

static void print_error(FILE *fp, char *buf, int pos)
{
#if 0
    int i;

    fprintf(fp, "%s\n", buf);
    for (i = 0; i < pos; i++) {
	if (buf[i] == '\t')
	    fprintf(fp, "--------");  /* doesn't always work */
	else if (buf[i] == '\n')
	    fprintf(fp, "\n");
	else
	    fprintf(fp, "-");
	}
    fprintf(fp, "^\n");
#else
    int i;
    i = 0;
    if (buf[0] == '\n')
	i = 1;
    while (i < pos) {
	if (buf[i] == '%')  /* skip over comment */
            while (buf[++i] != '\n') ;
 	fprintf(fp, "%c", buf[i++]);
	}
    fprintf(fp, " ***HERE*** ");

    while (buf[i]) {
	if (buf[i] == '%')  /* skip over comment */
            while (buf[++i] != '\n') ;
 	fprintf(fp, "%c", buf[i++]);
	}
    fprintf(fp, "\n");

#endif
}  /* print_error */

/*************
 *
 *    Term_ptr seq_to_quant_term(seq, n)
 *
 *    Take a sequence of terms t1,...,tn and build term $Quantified(t1,...,tn).
 *    t1 is already known to be a quantifier, and n >= 3.
 *    Check that t2,...,tn-1 are all names.
 *    On success, the resulting term is an entirely new copy.
 *
 *************/

static Term_ptr seq_to_quant_term(struct sequence_member *seq, int n)
{
    Term_ptr t;
    int i;

    if (n == 1)
	return(NULL);

    for (i = 1; i < n-1; i++)
	if (seq[i].t->arity != 0)
	    return(NULL);

    /* Special case: negated formula need not be parenthesized.
     * For example, all x -p(x) is OK.  In this case, the sequence is
     * [all, x, -, p(x)], so we must adjust things.
     */

    if (str_ident(sn_to_str(seq[n-2].t->symbol), "-")) {
	if (n == 3)
	    return(NULL);  /* all - p */
	else {
	    Term_ptr t;
	    t = seq_to_term(seq, n-2, n-1, 1000);
	    printf("adjusted term: "); p_term(t);
	    if (t) {
		zap_term(seq[n-2].t);
		zap_term(seq[n-1].t);
		fill_in_op_data(&seq[n-2], t);
		/* caller will still think there are n terms */
		seq[n-1].t = NULL;
		n--;
		}
	    else
		return(NULL);
	    }
	}

    t = get_term(n);
    t->symbol = str_to_sn("$Quantified", n);
    for (i = 0; i < n; i++)
	t->args[i] = copy_term(seq[i].t);
    return(t);
}  /* seq_to_quant_term */

/*************
 *
 *    Term_ptr seq_to_term(seq, start, end, m)
 *
 *    seq is an array of terms/operators, and start and end are
 *    indexes into seq.  This routine attempts to construct a term
 *    starting with start, ending with end, with precedence <= m.
 *    On success, the resulting term is an entirely new copy.
 *
 *************/

 Term_ptr seq_to_term(struct sequence_member *seq, int start, int end, int m)
{
    int i, n, type;
    Term_ptr t1, t2, t3, t;

    if (start == end) {
	t = copy_term(seq[start].t);
	return(t);
	}
    else {

        /* Check if first is prefix op that applies to rest. */

	if (seq[start].t->arity == 0) {
	    type = seq[start].unary_type;
	    n = seq[start].unary_prec;
	    t = seq[start].t;
	
	    if (type == FX && n <= m) {
		t1 = seq_to_term(seq, start+1, end, n-1);
		if (t1) {
		    t3 = get_term(1);
		    t3->symbol = str_to_sn(sn_to_str(t->symbol), 1);
		    t3->args[0] = t1;
		    return(t3);
		    }
		}
	    else if (type == FY && n <= m) {
		t1 = seq_to_term(seq, start+1, end, n);
		if (t1) {
		    t3 = get_term(1);
		    t3->symbol = str_to_sn(sn_to_str(t->symbol), 1);
		    t3->args[0] = t1;
		    return(t3);
		    }
		}
	    }

        /* Check if last is postfix op that applies to all preceding. */

	if (seq[end].t->arity == 0) {
	    type = seq[end].unary_type;
	    n = seq[end].unary_prec;
	    t = seq[end].t;

	    if (type == XF && n <= m) {
		t1 = seq_to_term(seq, start, end-1, n-1);
		if (t1) {
		    t3 = get_term(1);
		    t3->symbol = str_to_sn(sn_to_str(t->symbol), 1);
		    t3->args[0] = t1;
		    return(t3);
		    }
		}
	    else if (type == YF && n <= m) {
		t1 = seq_to_term(seq, start, end-1, n);
		if (t1) {
		    t3 = get_term(1);
		    t3->symbol = str_to_sn(sn_to_str(t->symbol), 1);
		    t3->args[0] = t1;
		    return(t3);
		    }
		}
	    }
		
	/* Look for an infix operator. */

	for (i = start+1; i <= end-1; i++) {
	    if (seq[i].t->arity == 0) {
		type = seq[i].binary_type;
		n = seq[i].binary_prec;
		t = seq[i].t;

		if (type == XFY && n <= m) {
		    t1 = seq_to_term(seq, start, i-1, n-1);
		    if (t1) {
			t2 = seq_to_term(seq, i+1, end, n);
			if (!t2)
			    zap_term(t1);
			}
		    if (t1 && t2) {
			t3 = get_term(2);
			t3->symbol = str_to_sn(sn_to_str(t->symbol), 2);
			t3->args[0] = t1;
			t3->args[1] = t2;
			return(t3);
			}
		    }
		else if (type == YFX && n <= m) {
		    t2 = seq_to_term(seq, i+1, end, n-1);
		    if (t2) {
			t1 = seq_to_term(seq, start, i-1, n);
			if (!t1)
			    zap_term(t2);
			}
		    if (t1 && t2) {
			t3 = get_term(2);
			t3->symbol = str_to_sn(sn_to_str(t->symbol), 2);
			t3->args[0] = t1;
			t3->args[1] = t2;
			return(t3);
			}
		    }
		else if (type == XFX && n <= m) {
		    t1 = seq_to_term(seq, start, i-1, n-1);
		    if (t1) {
			t2 = seq_to_term(seq, i+1, end, n-1);
			if (!t2)
			    zap_term(t1);
			}
		    if (t1 && t2) {
			t3 = get_term(2);
			t3->symbol = str_to_sn(sn_to_str(t->symbol), 2);
			t3->args[0] = t1;
			t3->args[1] = t2;
			return(t3);
			}
		    }
		}  /* name */
	    }  /* loop looking for infix op to apply */
	
	return(NULL);
	}
}  /* seq_to_term */

/*************
 *
 *    struct term_ptr *str_to_args(buffer, position, name)
 *
 *    name -- the functor.
 *
 *    start: functor(  a1 , a2 , a3 )
 *                   ^
 *    end:   functor(  a1 , a2 , a3 )
 *                                  ^
 *************/

static Term_ptr str_to_args(char *buf, int *p, char *name)
{
    Term_ptr t, t_sub;
    Gen_ptr_ptr p0, p1, p2;
    int i, n;

    p0 = NULL;
    n = 0;  /* count subterms to get arity */

    while (buf[*p] != ')') {
	n++;
	t_sub = str_to_term(buf, p, 0);
	if (!t_sub)
	    return(NULL);
	else if (buf[*p] != ',' && buf[*p] != ')') {
	    fprintf(stdout, "\nERROR, comma or ) expected:\n");
	    print_error(stdout, buf, *p);
	    return(NULL);
	    }
	else {
	    p2 = get_gen_ptr();
	    p2->u.t = t_sub;
	    if (p0)
		p1->next = p2;
	    else
		p0 = p2;
	    p1 = p2;
	    if (buf[*p] == ',')
		(*p)++;          /* step past comma */
	    }
	}
    if (n == 0) {
	fprintf(stdout, "\nERROR, functor has no arguments:\n");
	print_error(stdout, buf, *p);
	return(NULL);
	}
    
    t = get_term(n);
    t->symbol = str_to_sn(name, n);

    for (i = 0; i < t->arity; i++) {
	t->args[i] = p0->u.t;
	p2 = p0;
	p0 = p0->next;
	free_gen_ptr(p2);
	}

    return(t);

}  /* str_to_args */

/*************
 *
 *    struct term_ptr *str_to_list(buffer, position)
 *
 *    start: [ a1 , a2 , a3 ]
 *           ^
 *    end:   [ a1 , a2 , a3 ]
 *                           ^
 *************/

static Term_ptr str_to_list(char *buf, int *p)
{
    Term_ptr t_cons, t_head, t_tail, t_return, t_prev;
    int go;

    (*p)++;  /* step past '[' */

    if (buf[*p] == ']') {                        /* [] */
	t_return = get_term(0);
	t_return->symbol = str_to_sn("$nil", 0);
	(*p)++;  /* skip "]" */
	return(t_return);
	}
    else {                           /* [h|t], [t1,...,tn], or [t1,...,tn|t] */
	t_prev = NULL;
	go = 1;
	
	while (go) {
	    t_head = str_to_term(buf, p, 1);
	    if (!t_head)
		return(NULL);  /* error */
	    t_cons = get_term(2);
	    if (t_prev)
		t_prev->args[1] = t_cons;
	    else
		t_return = t_cons;
	    t_cons->symbol = str_to_sn("$cons", 2);
	    t_cons->args[0] = t_head;
	    t_prev = t_cons;
	    go = (buf[*p] == ',');
	    if (go)
		(*p)++;  /* step past ',' */
	    }
	
	if (buf[*p] == ']') {
	    t_tail = get_term(0);
	    t_prev->args[1] = t_tail;
	    t_tail->symbol = str_to_sn("$nil", 0);
	    (*p)++;  /* step past ']' */
	    return(t_return);
	    }
	else if (buf[*p] == '|') {
	    (*p)++;  /* step past '|' */
	    t_tail = str_to_term(buf, p, 1);
	    if (buf[*p] != ']') {
		fprintf(stdout, "\nERROR, ']' expected in list:\n");
		print_error(stdout, buf, *p);
		return(NULL);
		}
	    t_prev->args[1] = t_tail;
	    (*p)++;  /* step past ']' */
	    return(t_return);
	    }
	else {
	    fprintf(stdout, "\nERROR, ], |, or comma expected in list:\n");
	    print_error(stdout, buf, *p);
	    return(NULL);
	    }
	}
}  /* str_to_list */

/*************
 *
 *    int str_to_sequence(buffer, position, seq, in_list)
 *
 *    Read a sequence of operators/terms---It will be parsed into
 *    a term later in str_to_term.
 *    After successful call, position is the delimeter following the term.
 *
 *    Mutually recursive with str_to_term.
 *
 *    If success, return the number of terms read.
 *
 *    If a syntax error is found, print message and return(0).
 *
 *************/

static int str_to_sequence(char *buf, int *p, struct sequence_member *seq, int in_list)
{
    char name[MAX_NAME], c;
    Term_ptr t;
    int done, n, white;;

    done = 0; n = 0;
    while (!done) {
	
	get_name(buf, p, name, in_list);
	white = is_white(buf[*p]);  /* f(a) vs. f (a) */
	skip_white(buf, p);
	
	if (name[0] == '\0' && buf[*p] != '[' && buf[*p] != '(' && buf[*p] != '{') {
	    fprintf(stdout, "\nERROR, name expected:\n");
	    print_error(stdout, buf, *p);
	    return(0);
	    }
	
	else if (name[0] == '\0' && buf[*p] == '(') {         /* (term) */
	    (*p)++;  /* step past '(' */
	    t = str_to_term(buf, p, 0);
	    if (!t)
		return(0);
	    if (buf[*p] != ')') {
		fprintf(stdout, "\nERROR, ')' expected:\n");
		print_error(stdout, buf, *p);
		return(0);
		}
	    (*p)++;  /* step past ')' */
	    }
	
	else if (name[0] == '\0' && buf[*p] == '{') {         /* {term} */
	    (*p)++;  /* step past '{' */
	    t = str_to_term(buf, p, 0);
	    if (!t)
		return(0);
	    if (buf[*p] != '}') {
		fprintf(stdout, "\nERROR, '}' expected:\n");
		print_error(stdout, buf, *p);
		return(0);
		}
	    (*p)++;  /* step past '}' */
	    }
	
	else if (name[0] == '\0' && buf[*p] == '[') {           /* list */
	    t = str_to_list(buf, p);
	    if (!t)
		return(0);
	    }
	
        else if (name[0] != '\0' && !white && buf[*p] == '(')  /* f(args) */
	    {
	    (*p)++;  /* step past '(' */
	    t = str_to_args(buf, p, name);
	    if (!t)
		return(0);
	    (*p)++;  /* step past ')' */
	    }
	
	else if (name[0] != '\0') {                           /* name */
	    t = get_term(0);
	    /* If it's an operator, change arity later. */
	    t->symbol = str_to_sn(name, 0);
	    }
	
	else {
	    fprintf(stdout, "\nERROR, unrecognized error in term:\n");
	    print_error(stdout, buf, *p);
	    return(0);
	    }
	
	/* We have a term t. */
	
	if (n == MAX_COMPLEX) {
	    fprintf(stdout, "\nERROR, term too big:\n");
	    print_error(stdout, buf, *p);
	    return(0);
	    }
	else {
	    fill_in_op_data(&seq[n], t);
	    n++;
	    }
	
	skip_white(buf, p);
	c = buf[*p];
	done = (c == ',' || c == ')' || c == '}' || c == ']' ||
		c == '.' || c == '\0' || (in_list && c == '|'));
	}
    return(n);
}  /* str_to_sequence */

/*************
 *
 *    Term_ptr str_to_term(buffer, position, in_list)
 *
 *    Parse a string and build a term.
 *    Mutually recursive with str_to_sequence.
 *    After successful call, position is the delimeter following the term.
 *
 *    If a syntax error is found, print message and return(NULL).
 *
 *************/

 Term_ptr str_to_term(char *buf, int *p, int in_list)
{
    struct sequence_member seq[MAX_COMPLEX];
    Term_ptr t;
    int n, i, save_pos;

    save_pos = *p;

    n = str_to_sequence(buf, p, seq, in_list);
    if (n == 0)
	return(NULL);

    else if (seq[0].t->arity == 0 && n > 2 &&
	     (str_ident(sn_to_str(seq[0].t->symbol), "all") ||
	      str_ident(sn_to_str(seq[0].t->symbol), "exists"))) {
	t = seq_to_quant_term(seq, n);
	if (!t) {
	    fprintf(stdout, "\nERROR in quantifier prefix starting here:\n");
	    print_error(stdout, buf, save_pos);
	    }
	}

    else {
	t = seq_to_term(seq, 0, n-1, 1000);
	
	if (!t) {
	    fprintf(stdout, "\nERROR, the %d terms/operators in the following sequence are OK, but they\ncould not be combined into a single term with special operators.\n", n);
	    for (i = 0; i < n; i++)
		{ p_term(seq[i].t); printf("  ");}
	    printf("\n");
	    fprintf(stdout, "The context of the bad sequence is:\n");
	    print_error(stdout, buf, save_pos);
	    }
	}

    for (i = 0; i < n; i++)
	if (seq[i].t)
	    zap_term(seq[i].t);
    return(t);
}  /* str_to_term */

/*************
 *
 *     int read_buf(file_ptr, buffer)
 *
 *    Read characters into buffer until one of the following:
 *        1.  '.' is reached ('.' goes into the buffer)
 *        2.  EOF is reached:  buf[0] = '\0' (an error occurs if
 *                 any nonwhite space precedes EOF)
 *        3.  MAX_BUF characters have been read (an error occurs)
 *
 *    If error occurs, return(0), else return(1).
 *
 *************/

static int read_buf(FILE *fp, char *buf)
{
    int c, qc, i, j, ok, eof, eof_q, max, max_q;

    ok = eof = eof_q = max = max_q = 0;  /* stop conditions */
    i = 0;

    while (!ok && !eof && !eof_q && !max && !max_q) {

	c = getc(fp);
	if (c == '%') {  /* comment--discard rest of line */
	    while (c != '\n' && c != EOF)
		c = getc(fp);
	    }
	if (c =='.')
	    ok = 1;
	else if (c == EOF)
	    eof = 1;
	else if (i == MAX_BUF-1)
	    max = 1;
	else {
	    buf[i++] = c;
	    if (c == '\'' || c == '\"') {
		qc = c;
		c = getc(fp);
		while (c != qc && c != EOF && i != MAX_BUF-1) {
		    buf[i++] = c;
		    c = getc(fp);
		    }
		if (i == MAX_BUF-1)
		    max_q = 1;
		else if (c == EOF)
		    eof_q = 1;
		else
		    buf[i++] = c;
		}
	    }
	}

    if (ok) {
	buf[i++] = '.';
	buf[i] = '\0';
	return(1);
	}
    else if (eof) {
	/* white space at end of file is OK */
	j = 0;
	buf[i] = '\0';
	skip_white(buf, &j);
	if (i != j) {
	    fprintf(stdout, "\nERROR, characters after last period: %s\n", buf);
	    buf[0] = '\0';
	    return(0);
	    }
	else {
	    buf[0] = '\0';
	    return(1);
	    }
	}
    else if (eof_q) {
	char s[500];
	buf[i>100 ? 100 : i] = '\0';
	sprintf(s, "read_buf, quoted string has no end:%s", buf);
	abend(s);
	}
    else if (max) {
	char s[500];
	buf[i>100 ? 100 : i] = '\0';
	sprintf(s, "read_buf, input string has more than %d characters, increase MAX_BUF", MAX_BUF);
	abend(s);
	}
    else {  /* max_q */
	char s[500];
	buf[i>100 ? 100 : i] = '\0';
	sprintf(s, "read_buf, input string (which contains quote mark) has more than %d characters, increase MAX_BUF", MAX_BUF);
	abend(s);
	}
    return(0);  /* to quiet lint */
}  /* read_buf */

/*************
 *
 *    Term_ptr term_fixup(t)
 *
 *    change !=(a,b) to -(=(a,b))
 *    change -(3)    to -3              not recursive, -(-(3)) -> -(-3))
 *    change +(3)    to +3              not recursive, +(+(3)) +> +(+3))
 *
 *************/

static Term_ptr term_fixup(Term_ptr t)
{
    Term_ptr t1;
    int neg, i;
    char *s, str[MAX_NAME];
    long l;

    if (is_symbol(t->symbol, "!=", 2)) {
	t1 = get_term(1);
	t1->args[0] = t;
	t1->symbol = str_to_sn("-", 1);
	t->symbol = str_to_sn("=", 2);
	t = t1;
	}
    
    else if ((neg = is_symbol(t->symbol,"-",1)) || is_symbol(t->symbol,"+",1)) {
	t1 = t->args[0];
	s = sn_to_str(t1->symbol);
	if (t1->arity == 0 && str_long(s, &l)) {
	    cat_str((neg ? "-" : "+"), s, str);
	    t1->symbol = str_to_sn(str, 0);
	    free_term(t);
	    t = t1;
	    }
	}
    
    for (i = 0; i < t->arity; i++)
	t->args[i] = term_fixup(t->args[i]);

    return(t);
}  /* term_fixup */

/*************
 *
 *    Term_ptr term_fixup_2(t)
 *
 *    change  -(=(a,b)) to !=(a,b)
 *
 *************/

static Term_ptr term_fixup_2(Term_ptr t)
{
    Term_ptr t1;
    int i;

    if (is_symbol(t->symbol,"-",1) && is_symbol(t->args[0]->symbol,"=",2)) {
	t1 = t->args[0];
	t1->symbol = str_to_sn("!=", 2);
	free_term(t);
	t = t1;
	}
    for (i = 0; i < t->arity; i++)
	t->args[i] = term_fixup_2(t->args[i]);
    return(t);
}  /* term_fixup_2 */

/*************
 *
 *    Term_ptr read_term(file_ptr, retcd_ptr) --
 *
 *    Read and return then next term.
 *    It is assumed that the next term in the file is terminated
 *    with a period.   NULL is returned if EOF is reached first.
 *
 *    If an error is found, return(0); else return(1).
 *
 *************/

Term_ptr read_term(FILE *fp, int *rcp)
{
    char buf[MAX_BUF+1];  /* one extra for \0 at end */
    int p, rc;
    Term_ptr t;

    rc = read_buf(fp, buf);
    if (rc == 0) {  /* error */
	*rcp = 0;
	return(NULL);
	}
    else if (buf[0] == '\0') {  /* ok. EOF */
	*rcp = 1;
	return(NULL);
	}
    else {
	p = 0;
	t = str_to_term(buf, &p, 0);
	if (!t) {
	    *rcp = 0;
	    return(NULL);
	    }
	else {
	    skip_white(buf, &p);
	    if (buf[p] != '.') {
	        fprintf(stdout, "\nERROR, text after term:\n");
	        print_error(stdout, buf, p);
	        *rcp = 0;
	        return(NULL);
	        }
	    else {
		t = term_fixup(t);
		*rcp = 1;
		return(t);
		}
	    }
	}
}  /* read_term */


