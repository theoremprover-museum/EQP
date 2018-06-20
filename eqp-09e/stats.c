#include "Header.h"
#include "Stats.h"

/* following are for memory stats */

#include "Term.h"
#include "Symbols.h"
#include "List.h"    
#include "Unify.h"   
#include "Clause.h"  
#include "Discrim.h"
#include "Fpa.h"
#include "Ac.h"      
#include "Paramod.h"

 long Stats[MAX_STATS];
 struct clock Clocks[MAX_CLOCKS];
 int Internal_flags[MAX_INTERNAL_FLAGS];

/*************
 *
 *   init_stats()
 *
 *************/

void init_stats()
{
    int i;

    init_clocks();
    for (i = 0; i < MAX_INTERNAL_FLAGS; i++)
	Internal_flags[i] = 0;
    for (i = 0; i < MAX_STATS; i++)
	Stats[i] = 0;
    Stats[INIT_WALL_SECONDS] = wall_seconds();
}  /* init_stats */

/*************
 *
 *    print_mem()
 *
 *************/

void print_mem(FILE *fp)
{
    fprintf(fp, "\n------------- memory usage ------------\n");

    fprintf(fp, "Memory dynamically allocated (tp_alloc): %d.\n", total_mem());

    print_symbols_mem(fp, 1);
    print_term_mem(fp, 0);
    print_list_mem(fp, 0);
    print_unify_mem(fp, 0);
    print_ac_mem(fp, 0);
    print_discrim_mem(fp, 0);
    print_fpa_mem(fp, 0);
    print_clause_mem(fp, 0);
    print_clause_pair_mem(fp, 0);
}  /* print_mem */

/*************
 *
 *   p_mem
 *
 *************/

void p_mem(void)
{
    print_mem(stdout);
}  /* p_mem */

/*************
 *
 *    print_stats(fp)
 *
 *************/

void print_stats(FILE *fp)
{
    fprintf(fp, "\n-------------- statistics -------------\n");
    
#if 0
    fprintf(fp, "Input errors             %7ld\n", Stats[INPUT_ERRORS]);
    fprintf(fp, "BT occur checks          %7ld\n", Stats[BT_OCCUR_CHECKS]);
    fprintf(fp, "AC initiations           %7ld\n", Stats[AC_INITIATIONS]);
    fprintf(fp, "AC continuations         %7ld\n", Stats[AC_CONTINUATIONS]);
    fprintf(fp, "\n");
#endif

    fprintf(fp, "Clauses input            %7ld\n", Stats[CLAUSES_INPUT]);
    fprintf(fp, "  Usable input             %7ld\n", Stats[USABLE_INPUT]);
    fprintf(fp, "  Sos input                %7ld\n", Stats[SOS_INPUT]);
    fprintf(fp, "  Demodulators input       %7ld\n", Stats[DEMODULATORS_INPUT]);
    fprintf(fp, "  Passive input            %7ld\n", Stats[PASSIVE_INPUT]);
    fprintf(fp, "\n");

    fprintf(fp, "Processed BS (before search)%4ld\n", Stats[CLAUSES_GENERATED_INPUT]);
    fprintf(fp, "Forward subsumed BS      %7ld\n", Stats[CLAUSES_FORWARD_SUBSUMED_INPUT]);
    fprintf(fp, "Kept BS                  %7ld\n", Stats[CLAUSES_KEPT_INPUT]);
    fprintf(fp, "New demodulators BS      %7ld\n", Stats[NEW_DEMODULATORS_INPUT]);
    fprintf(fp, "Back demodulated BS      %7ld\n", Stats[CLAUSES_BACK_DEMODULATED_INPUT]);
    fprintf(fp, "\n");

    fprintf(fp, "Clauses or pairs given   %7ld\n", Stats[GIVEN]);
    fprintf(fp, "Clauses generated        %7ld\n", Stats[CLAUSES_GENERATED]);
    fprintf(fp, "Forward subsumed         %7ld\n", Stats[CLAUSES_FORWARD_SUBSUMED]);
    fprintf(fp, "Deleted by weight        %7ld\n", Stats[CLAUSES_WT_DELETE]);
    fprintf(fp, "Deleted by variable count%7ld\n", Stats[CLAUSES_VAR_DELETE]);
    fprintf(fp, "Kept                     %7ld\n", Stats[CLAUSES_KEPT]);
    fprintf(fp, "New demodulators         %7ld\n", Stats[NEW_DEMODULATORS]);
    fprintf(fp, "Back demodulated         %7ld\n", Stats[CLAUSES_BACK_DEMODULATED]);
    fprintf(fp, "Ordered paramod prunes   %7ld\n", Stats[ORDERED_PARAMOD_PRUNES]);
    fprintf(fp, "Basic paramod prunes     %7ld\n", Stats[BASIC_PARAMOD_PRUNES]);
    fprintf(fp, "Prime paramod prunes     %7ld\n", Stats[PRIME_PARAMOD_PRUNES]);
    fprintf(fp, "Semantic prunes          %7ld\n", Stats[SEMANTIC_PARAMOD_PRUNES]);
    fprintf(fp, "\n");

    fprintf(fp, "Rewrite attmepts         %7ld\n", Stats[REWRITE_ATTEMPTS]);
    fprintf(fp, "Rewrites                 %7ld\n", Stats[REWRITES]);
    fprintf(fp, "\n");

    fprintf(fp, "FPA overloads            %7ld\n", Stats[FPA_OVERLOADS]);
    fprintf(fp, "FPA underloads           %7ld\n", Stats[FPA_UNDERLOADS]);
    fprintf(fp, "\n");

    fprintf(fp, "Usable size              %7ld\n", Stats[USABLE_SIZE]);
    fprintf(fp, "Sos size                 %7ld\n", Stats[SOS_SIZE]);
    fprintf(fp, "Demodulators size        %7ld\n", Stats[DEMODULATORS_SIZE]);
    fprintf(fp, "Passive size             %7ld\n", Stats[PASSIVE_SIZE]);
    fprintf(fp, "Disabled size            %7ld\n", Stats[DISABLED_SIZE]);
    fprintf(fp, "\n");

    fprintf(fp, "Proofs found             %7ld\n", Stats[PROOFS]);

}  /* print_stats */

/*************
 *
 *    p_stats()
 *
 *************/

void p_stats(void)
{
    print_stats(stdout);
}  /* p_stats */

/*************
 *
 *    print_times(fp)
 *
 *************/

void print_times(FILE *fp)
{
    long t, min, hr;

    fprintf(fp, "\n----------- times (seconds) ----------- %s\n", get_time());

    t = run_time();
    fprintf(fp, "user CPU time       %10.2f  ", t / 1000.);
    t = t / 1000; hr = t / 3600; t = t % 3600; min = t / 60; t = t % 60;
    fprintf(fp, " (%ld hr, %ld min, %ld sec)\n", hr, min, t); 

    t = system_time();
    fprintf(fp, "system CPU time     %10.2f  ", t/ 1000.);
    t = t / 1000; hr = t / 3600; t = t % 3600; min = t / 60; t = t % 60;
    fprintf(fp, " (%ld hr, %ld min, %ld sec)\n", hr, min, t); 

    t = wall_seconds() - Stats[INIT_WALL_SECONDS];
    fprintf(fp, "wall-clock time     %7ld     ", t);
    hr = t / 3600; t = t % 3600; min = t / 60; t = t % 60;
    fprintf(fp, " (%ld hr, %ld min, %ld sec)\n", hr, min, t); 

    fprintf(fp, "input time          %10.2f\n", clock_val(INPUT_TIME) / 1000.);
    fprintf(fp, "paramodulation time %10.2f\n", clock_val(PARAMOD_TIME) / 1000.);
    fprintf(fp, "demodulation time   %10.2f\n", clock_val(DEMOD_TIME) / 1000.);
    fprintf(fp, "orient time         %10.2f\n", clock_val(ORIENT_EQ_TIME) / 1000.);
    fprintf(fp, "weigh time          %10.2f\n", clock_val(WEIGH_TIME) / 1000.);
    fprintf(fp, "forward subsume time%10.2f\n", clock_val(FOR_SUB_TIME) / 1000.);
    fprintf(fp, "back demod find time%10.2f\n", clock_val(BD_FIND_TIME) / 1000.);
    fprintf(fp, "conflict time       %10.2f\n", clock_val(CONFLICT_TIME) / 1000.);
    fprintf(fp, "LRPO time           %10.2f\n", clock_val(LRPO_TIME) / 1000.);
    fprintf(fp, "store clause time   %10.2f\n", clock_val(STORE_TIME) / 1000.);
    fprintf(fp, "disable clause time %10.2f\n", clock_val(DISABLE_TIME) / 1000.);
    fprintf(fp, "prime paramod time  %10.2f\n", clock_val(PRIME_PARAMOD_TIME) / 1000.);
    fprintf(fp, "semantics time      %10.2f\n", clock_val(SEMANTICS_TIME) / 1000.);

    fprintf(fp, "\n");
}  /* print_times */

/*************
 *
 *    p_times()
 *
 *************/

void p_times(void)
{
    print_times(stdout);
}  /* p_times */

/*************
 *
 *    output_stats
 *
 *************/

void output_stats(FILE *fp)
{
    print_mem(fp);
    print_stats(fp);
    print_times(fp);
}  /* output_stats */

