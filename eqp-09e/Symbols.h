#ifndef TP_SYMBOLS_H
#define TP_SYMBOLS_H

#define MAX_NAME 51   /* maximum # of chars in any symbol (including '\0') */
#define SYM_TAB_SIZE   50

#define XFX 1  /* special operator types */
#define XFY 2
#define YFX 3
#define FX  4
#define FY  5
#define XF  6
#define YF  7

struct sym_ent {  /* symbol table entry */
    struct sym_ent *next;
    int sym_num;           /* unique identifier */
    int arity;             /* 0 for constants and variables */
    char name[MAX_NAME];   /* the print symbol */

    int lex_val;           /* can be used to assign a lexical value */
    int lrpo_status;       /* status for lexicographic RPO */

    int skolem;

    int special_op;  /* special operator */
    int op_type;     /* operator type */
    int op_prec;     /* operator precedence */

    int assoc_comm;  /* associative-commutative */
    int commutative; /* commutative or symmetric */
    };

typedef struct sym_ent *Sym_ent_ptr;

struct symbol_table {
    struct sym_ent *table[SYM_TAB_SIZE];
    };

/* function prototypes from symbols.c */

Sym_ent_ptr get_sym_ent(void);

void free_sym_ent(Sym_ent_ptr p);

void print_symbols_mem(FILE *fp, int heading);

void p_symbols_mem();

void init_symbol_table(void);

void empty_sym_tab();

int new_sym_num(void);

Sym_ent_ptr insert_sym(char *s, int arity);

int str_to_sn(char *str, int arity);

void print_syms(FILE *fp);

void p_syms(void);

char *sn_to_str(int sym_num);

int sn_to_arity(int sym_num);

Sym_ent_ptr sn_to_node(int sym_num);

Sym_ent_ptr sym_tab_member(char *str, int arity);

int in_sym_tab(char *s);

Sym_ent_ptr find_special_nd(char *str, int arity);

int is_symbol(int sym, char *str, int arity);

void mark_as_skolem(int symbol);

int is_skolem(int symbol);

int var_name(char *s);

int declare_op(int prec, int type, char *str);

void init_special_ops(void);

int process_op_command(Term_ptr t);

void set_assoc_comm(char *str);

int is_assoc_comm(int sn);

int process_ac_command(Term_ptr t);

void set_commutative(char *str);

int is_commutative(int sn);

int process_comm_command(Term_ptr t);

int process_multiset_command(Term_ptr t);

int process_lex_command(Term_ptr t);

int proper_list(Term_ptr t);

int list_length(Term_ptr t);

int compare_for_auto_lex_order(void *d1, void *d2);

void auto_lex_order(void);

#endif  /* ! TP_SYMBOLS_H */
