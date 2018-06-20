#ifndef TP_CLOCKS_H
#define TP_CLOCKS_H

#include <time.h>
#include <sys/time.h>
#include <sys/resource.h>

/*************
 *
 *    Clocks.  To install a new clock, append a new name and
 *    index to this list, then insert the code to output it in the
 *    routine `print_times'.  Example of use: CLOCK_START(INPUT_TIME),
 *    CLOCK_STOP(INPUT_TIME),  micro_sec = clock_val(INPUT_TIME);.
 *
 *************/

#define MAX_CLOCKS          40
 
#define RUN_TIME             1
#define INPUT_TIME           0

#define PARAMOD_TIME         2
#define FOR_SUB_TIME         3
#define DEMOD_TIME           4
#define BD_FIND_TIME         5
#define CONFLICT_TIME        6
#define LRPO_TIME            7
#define WEIGH_TIME           8
#define ORIENT_EQ_TIME       9
#define DISABLE_TIME        10
#define STORE_TIME          11
#define PRIME_PARAMOD_TIME  12
#define SEMANTICS_TIME       13

struct clock {  /* for timing, see cos.h, macros.h, and clocks.c */
    unsigned long accum_msec;   /* accumulated time */
    unsigned long curr_msec;    /* time since clock has been turned on */
    int level;         /* STARTs - STOPs */
    };

extern struct clock Clocks[MAX_CLOCKS];

/*************
 *
 *    CPU_TIME(msec) - It has been sec milliseconds  (UNIX user time)
 *    since the start of this process.
 *
 *************/

#ifdef TP_RUSAGE
#define CPU_TIME(msec)  \
{  \
    struct rusage r;  \
    getrusage(RUSAGE_SELF, &r);  \
    msec = r.ru_utime.tv_sec * 1000 + r.ru_utime.tv_usec / 1000;  \
}  /* CPU_TIME */
#else
#define CPU_TIME(msec) {msec = 0;}
#endif

/*************
 *
 *    CLOCK_START(clock_num) - Start or continue timing.
 *
 *************/

#ifdef NO_CLOCK
#define CLOCK_START(c)   /* empty string */
#else
#define CLOCK_START(c)  \
{  \
    struct clock *cp;  \
    cp = &Clocks[c];  \
    cp->level++; \
    if (cp->level == 1) \
	CPU_TIME(cp->curr_msec) \
}  /* CLOCK_START */
#endif

/*************
 *
 *    CLOCK_STOP(clock_num) - Stop timing and add to accumulated total.
 *
 *    If the clock not running, a warning message is printed.
 *
 *************/

#ifdef NO_CLOCK
#define CLOCK_STOP(c)   /* empty string */
#else
#define CLOCK_STOP(c)  \
{  \
    long msec;  \
    struct clock *cp;  \
    cp = &Clocks[c];  \
    cp->level--; \
    if (cp->level < 0) {  \
	fprintf(stderr, "\007WARNING, CLOCK_STOP: clock %d not running.\n", c);  \
	printf("WARNING, CLOCK_STOP: clock %d not running.\n", c);  \
	cp->level = 0; \
	}  \
    else if (cp->level == 0) {  \
	CPU_TIME(msec)  \
	cp->accum_msec += msec - cp->curr_msec;  \
	}  \
}  /* CLOCK_STOP */
#endif

extern struct clock Clocks[MAX_CLOCKS];

/* function prototypes from clocks.c */

void init_clocks(void);

long clock_val(int c);

void clock_reset(int c);

char *get_time(void);

long system_time(void);

long run_time(void);

long wall_seconds(void);


#endif  /* ! TP_CLOCKS_H */
