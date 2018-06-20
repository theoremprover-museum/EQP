#ifndef TP_MISC_H
#define TP_MISC_H

/* function prototypes from misc.c */

void abend(char *str);

int initial_str(char *s, char *t);

int str_int(char *s, int *np);

void int_str(int i, char *s);

int str_long(char *s, long int *np);

int bits_ulong(char *s, long unsigned int *np);

void long_str(long int i, char *s);

void cat_str(char *s1, char *s2, char *s3);

int str_ident(char *s, char *t);

void reverse(char *s);

#endif  /* ! TP_MISC_H */
