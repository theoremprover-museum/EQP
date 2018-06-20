#include "Header.h"  /* for ABEND_EXIT */

/*************
 *
 *    abend
 *
 *************/

void abend(char *str)
{
    output_stats(stdout);

    fprintf(stderr, "\n\n\007********** ABNORMAL END **********\n");
    fprintf(stderr, "********** %s\n", str);

    exit(ABEND_EXIT);
    
}  /* abend */

/*************
 *
 *     int initial_str(s, t)  --  Is s an initial substring of t?
 *
 *************/

int initial_str(char *s, char *t)
{
    for ( ; *s == *t; s++, t++)
	if (*s == '\0') return(1);
    return(*s == '\0');
}  /* initial_str */

/*************
 *
 *    int str_int(string, int_ptr) -- Translate a string to an integer.
 *
 *        String has optional '+' or '-' as first character.
 *    Return(1) iff success.
 *
 *************/

int str_int(char *s, int *np)
{
    int i, sign, n;

    i = 0;
    sign = 1;
    if (s[0] == '+' || s[0] == '-') {
	if (s[0] == '-')
	    sign = -1;
	i = 1;
	}
    if (s[i] == '\0')
	return(0);
    else {
	n = 0;
	for( ; s[i] >= '0' && s[i] <= '9'; i++)
	    n = n * 10 + s[i] - '0';
	*np = n * sign;
	return(s[i] == '\0');
	}
}  /* str_int */

/*************
 *
 *    int_str(int, str) -- translate an integer to a string
 *
 *************/

void int_str(int i, char *s)
{
    int j, sign;

    if ((sign = i) < 0)
	i = -i;

    j = 0;
    if (i == 0)
        s[j++] = '0';
    else {
	while (i > 0) {
            s[j++] = i % 10 + '0';
            i = i / 10;
	    }
        }
    if (sign < 0)
	s[j++] = '-';
    s[j] = '\0';
    reverse(s);
}  /* int_str */

/*************
 *
 *    int str_long(string, long_ptr) -- Translate a string to a long.
 *
 *        String has optional '+' or '-' as first character.
 *    Return(1) iff success.
 *
 *************/

int str_long(char *s, long int *np)
{
    int i, sign;
    long n;

    i = 0;
    sign = 1;
    if (s[0] == '+' || s[0] == '-') {
	if (s[0] == '-')
	    sign = -1;
	i = 1;
	}
    if (s[i] == '\0')
	return(0);
    else {
	n = 0;
	for( ; s[i] >= '0' && s[i] <= '9'; i++)
	    n = n * 10 + s[i] - '0';
	*np = n * sign;
	return(s[i] == '\0');
	}
}  /* str_long */

/*************
 *
 *    long_str(int, str) -- translate a long to a string
 *
 *************/

void long_str(long int i, char *s)
{
    int j;
    long sign;

    if ((sign = i) < 0)
	i = -i;

    j = 0;
    if (i == 0)
	s[j++] = '0';
    else {
	while (i > 0) {
	    s[j++] = i % 10 + '0';
	    i = i / 10;
	    }
	}
    if (sign < 0)
	s[j++] = '-';
    s[j] = '\0';
    reverse(s);
}  /* long_str */

/*************
 *
 *    cat_str(s1, s2, s3)
 *
 *************/

void cat_str(char *s1, char *s2, char *s3)
{
    int i, j;

    for (i = 0; s1[i] != '\0'; i++)
        s3[i] = s1[i];
    for (j = 0; s2[j] != '\0'; j++, i++)
	s3[i] = s2[j];
    s3[i] = '\0';
}  /* cat_str */

/*************
 *
 *     int str_ident(s, t) --  Identity of strings
 *
 *************/

int str_ident(char *s, char *t)
{
    return(strcmp(s, t) == 0);
}  /* str_ident */

/*************
 *
 *    reverse(s) -- reverse a string
 *
 *************/

void reverse(char *s)
{
    int i, j;
    char temp;

    for (i = 0, j = strlen(s)-1; i<j; i++, j--) {
	temp = s[i];
	s[i] = s[j];
	s[j] = temp;
	}
}  /* reverse */

