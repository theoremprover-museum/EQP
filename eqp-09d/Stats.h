#ifndef TP_STATS_H
#define TP_STATS_H

/* This file does not depend on header.h */

#include "Clocks.h"

/*************
 *
 *    Statistics.  To install a new statistic, append a new name and
 *    index to this list, then insert the code to output it in the
 *    routine `print_stats'.
 *    Example access:  Stats[INPUT_ERRORS]++;
 *
 *************/

#define MAX_STATS        50  /* increase if necessary */

#define BT_OCCUR_CHECKS   0
#define REWRITES          1
#define FPA_OVERLOADS     2
#define FPA_UNDERLOADS    3
#define AC_INITIATIONS    4
#define AC_CONTINUATIONS  5
#define REWRITE_ATTEMPTS  6

#define INIT_WALL_SECONDS         7
#define INPUT_ERRORS              8
#define K_MALLOCED                9

#define CLAUSES_INPUT                  10
#define CLAUSES_GENERATED_INPUT        11
#define CLAUSES_FORWARD_SUBSUMED_INPUT 12
#define CLAUSES_KEPT_INPUT             13
#define NEW_DEMODULATORS_INPUT         14
#define CLAUSES_BACK_DEMODULATED_INPUT 15

#define GIVEN                    16
#define CLAUSES_GENERATED        17
#define CLAUSES_WT_DELETE        18
#define CLAUSES_FORWARD_SUBSUMED 19
#define CLAUSES_KEPT             20
#define NEW_DEMODULATORS         21
#define CLAUSES_BACK_DEMODULATED 22

#define PROOFS                   23

#define SOS_SIZE                 24
#define USABLE_SIZE              25
#define DEMODULATORS_SIZE        26
#define PASSIVE_SIZE             27
#define DISABLED_SIZE            28

#define USABLE_INPUT             32
#define SOS_INPUT                33
#define DEMODULATORS_INPUT       34
#define PASSIVE_INPUT            35

#define ORDERED_PARAMOD_PRUNES   36
#define PRIME_PARAMOD_PRUNES     37
#define BASIC_PARAMOD_PRUNES     38
#define SEMANTIC_PARAMOD_PRUNES  39
#define CLAUSES_VAR_DELETE       40

/* end of Stats */

#define MAX_INTERNAL_FLAGS  10

#define AC_PRESENT         0
#define COMM_PRESENT       1
#define BT_UNIFY           2
#define INTERP_PRESENT     3

extern long Stats[MAX_STATS];
extern int Internal_flags[MAX_INTERNAL_FLAGS];

/* function prototypes from stats.c */

void init_stats();

void output_stats(FILE *fp);

void print_stats(FILE *fp);

void p_stats(void);

void print_times(FILE *fp);

void p_times(void);

#endif  /* ! TP_STATS_H */
