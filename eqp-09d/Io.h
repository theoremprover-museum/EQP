#ifndef TP_IO_H
#define TP_IO_H

#include "Symbols.h"

#define MAX_COMPLEX  1000  /* number of operators/terms in a sequence */

#define MAX_BUF 5000  /* maximum # of chars in input string (including '\0') */

/* function prototypes from io.c */

void write_term(FILE *fp, Term_ptr t, int n, int *prev);

void display_term(FILE *fp, Term_ptr t);

void print_term(FILE *fp, Term_ptr t);

void p_term(Term_ptr t);

void d_term(Term_ptr t);

void print_term_nl(FILE *fp, Term_ptr t);

void print_variable(FILE *fp, Term_ptr t);

Term_ptr read_term(FILE *fp, int *rcp);

#endif  /* ! TP_IO_H */
