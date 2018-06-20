#include "Header.h"
#include "Clocks.h"

 struct clock Clocks[MAX_CLOCKS];

/*************
 *
 *    init_clocks() - Initialize all clocks.
 *
 *************/

void init_clocks(void)
{
    int i;
    for (i = 0; i < MAX_CLOCKS; i++)
	clock_reset(i);
}  /* init_clocks */

/*************
 *
 *    long clock_val(clock_num) - Returns accumulated time in milliseconds.
 *
 *    Clock need not be stopped.
 *
 *************/

long clock_val(int c)
{
    long msec, i, j;

    i = Clocks[c].accum_msec;
    if (Clocks[c].level == 0)
	return(i);
    else {
	CPU_TIME(msec)
	j = msec - Clocks[c].curr_msec;
	return(i+j);
	}
}  /* clock_val */

/*************
 *
 *    clock_reset(clock_num) - Clocks must be reset before being used.
 *
 *************/

void clock_reset(int c)
{
    Clocks[c].accum_msec = 0;
    Clocks[c].level = 0;
}  /* clock_reset */

/*************
 *
 *   char *get_time() - get a string representation of current date and time
 *
 *************/

char *get_time(void)
{
    long i;

    i = time((long *) NULL);
    return(asctime(localtime(&i)));
}  /* get_time */

/*************
 *
 *    long system_time() - Return system time in milliseconds.
 *
 *************/

long system_time(void)
{
#ifdef TP_RUSAGE
    struct rusage r;
    long sec, usec;

    getrusage(RUSAGE_SELF, &r);
    sec = r.ru_stime.tv_sec;
    usec = r.ru_stime.tv_usec;

    return((sec * 1000) + (usec / 1000));
#else
    return(0);
#endif
}  /* system_time */

/*************
 *
 *    long run_time() - Return run time in milliseconds.
 *
 *    This is used instead of the normal clock routines in case
 *    progam is complied with NO_CLOCK.
 *
 *************/

long run_time(void)
{
#ifdef TP_RUSAGE
    struct rusage r;
    long sec, usec;

    getrusage(RUSAGE_SELF, &r);
    sec = r.ru_utime.tv_sec;
    usec = r.ru_utime.tv_usec;

    return((sec * 1000) + (usec / 1000));
#else
    return(0);
#endif
}  /* run_time */

/*************
 *
 *     wall_seconds()
 *
 *************/

long wall_seconds(void)
{
    long i;

    i = time((long *) NULL);
    return(i);
}  /* wall_seconds */

