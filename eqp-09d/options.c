#include "Options.h"

 struct flag Flags[MAX_FLAGS];
 struct parm Parms[MAX_PARMS];

/*************
 *
 *    init_options()
 *
 *************/

void init_options(void)
{
    int i;
    struct flag *f;
    struct parm *p;

    for (i = 0; i < MAX_FLAGS; i++)
	Flags[i].name = "";
    for (i = 0; i < MAX_PARMS; i++)
	Parms[i].name = "";

    /* Flags are Boolean-valued options */

    f = &(Flags[PROLOG_STYLE_VARIABLES]);
    f->name = "prolog_style_variables";
    f->val = 0;

    f = &(Flags[CHECK_ARITY]);
    f->name = "check_arity";
    f->val = 1;

    f = &(Flags[DISPLAY_TERMS]);
    f->name = "display_terms";
    f->val = 0;

    f = &(Flags[PRINT_GEN]);
    f->name = "print_gen";
    f->val = 0;

    f = &(Flags[DEMOD_HISTORY]);
    f->name = "demod_history";
    f->val = 1;

    f = &(Flags[PRINT_GIVEN]);
    f->name = "print_given";
    f->val = 1;

    f = &(Flags[PRINT_PAIRS]);
    f->name = "print_pairs";
    f->val = 0;

    f = &(Flags[LRPO]);
    f->name = "lrpo";
    f->val = 0;

    f = &(Flags[PARA_PAIRS]);
    f->name = "para_pairs";
    f->val = 0;

    f = &(Flags[PRINT_FORWARD_SUBSUMED]);
    f->name = "print_forward_subsumed";
    f->val = 0;

    f = &(Flags[PRINT_BACK_DEMOD]);
    f->name = "print_back_demod";
    f->val = 1;

    f = &(Flags[PRINT_NEW_DEMOD]);
    f->name = "print_new_demod";
    f->val = 1;

    f = &(Flags[PRINT_KEPT]);
    f->name = "print_kept";
    f->val = 1;

    f = &(Flags[PRINT_LISTS_AT_END]);
    f->name = "print_lists_at_end";
    f->val = 0;

    f = &(Flags[NO_DEMODULATION]);
    f->name = "no_demodulation";
    f->val = 0;

    f = &(Flags[INDEX_BT_DEMOD]);
    f->name = "index_demod";
    f->val = 1;

    f = &(Flags[INDEX_AC_ARGS]);
    f->name = "index_ac_args";
    f->val = 1;

    f = &(Flags[INDEX_PARAMOD]);
    f->name = "index_paramod";
    f->val = 1;

    f = &(Flags[INDEX_BD]);
    f->name = "index_bd";
    f->val = 1;

    f = &(Flags[INDEX_BS]);
    f->name = "index_bs";
    f->val = 1;

    f = &(Flags[INDEX_FS]);
    f->name = "index_fs";
    f->val = 1;

    f = &(Flags[INDEX_CONFLICT]);
    f->name = "index_conflict";
    f->val = 1;

    f = &(Flags[DELAY_BACK_DEMOD]);
    f->name = "delay_back_demod";
    f->val = 0;

    f = &(Flags[DELAY_NEW_DEMOD]);
    f->name = "delay_new_demod";
    f->val = 0;

    f = &(Flags[DEMOD_GIVEN]);
    f->name = "demod_given";
    f->val = 0;

    f = &(Flags[BACK_DEMOD_SOS]);
    f->name = "back_demod_sos";
    f->val = 1;

    f = &(Flags[ORDERED_PARAMOD]);
    f->name = "ordered_paramod";
    f->val = 0;

    f = &(Flags[AC_EXTEND]);
    f->name = "ac_extend";
    f->val = 1;

    f = &(Flags[FUNCTIONAL_SUBSUME]);
    f->name = "functional_subsume";
    f->val = 0;

    f = &(Flags[BASIC_PARAMOD]);
    f->name = "basic_paramod";
    f->val = 0;

    f = &(Flags[PRIME_PARAMOD]);
    f->name = "prime_paramod";
    f->val = 0;

    f = &(Flags[FPA_DELETE]);
    f->name = "fpa_delete";
    f->val = 0;

    /* Parms are integer-valued options */

    p = &(Parms[MAX_MEM]);
    p->name = "max_mem";
    p->min = -1;
    p->max = INT_MAX;
    p->val = 8000;

    p = &(Parms[MAX_WEIGHT]);
    p->name = "max_weight";
    p->min = -INT_MAX;
    p->max = INT_MAX;
    p->val = INT_MAX;

    p = &(Parms[MAX_GIVEN]);
    p->name = "max_given";
    p->min = 0;
    p->max = INT_MAX;
    p->val = INT_MAX;

    p = &(Parms[WEIGHT_FUNCTION]);
    p->name = "weight_function";
    p->min = 0;
    p->max = INT_MAX;
    p->val = 0;

    p = &(Parms[MAX_PROOFS]);
    p->name = "max_proofs";
    p->min = 1;
    p->max = INT_MAX;
    p->val = 1;

    p = &(Parms[REPORT_GIVEN]);
    p->name = "report_given";
    p->min = 1;
    p->max = INT_MAX;
    p->val = INT_MAX;

    p = &(Parms[AC_SUPERSET_LIMIT]);
    p->name = "ac_superset_limit";
    p->min = -1;
    p->max = INT_MAX;
    p->val = -1;

    p = &(Parms[MAX_SECONDS]);
    p->name = "max_seconds";
    p->min = 0;
    p->max = INT_MAX;
    p->val = INT_MAX;

    p = &(Parms[PICK_GIVEN_RATIO]);
    p->name = "pick_given_ratio";
    p->min = -1;
    p->max = INT_MAX;
    p->val = -1;

    p = &(Parms[FPA_DEPTH]);
    p->name = "fpa_depth";
    p->min = 0;
    p->max = INT_MAX;
    p->val = 8;

    p = &(Parms[PAIR_INDEX_SIZE]);
    p->name = "pair_index_size";
    p->min = 1;
    p->max = 100;
    p->val = 50;

    p = &(Parms[MAX_VARIABLES]);
    p->name = "max_variables";
    p->min = 0;
    p->max = INT_MAX;
    p->val = INT_MAX;

    p = &(Parms[PARA_KEY]);
    p->name = "para_key";
    p->min = 0;
    p->max = INT_MAX;
    p->val = 0;

}  /* init_options */

/*************
 *
 *    print_options(fp)
 *
 *************/

void print_options(FILE *fp)
{
    int i, j;

    fprintf(fp, "\n--------------- options ---------------\n");

    j = 0;
    for (i = 0; i < MAX_FLAGS; i++)  /* print set flags */
	if (Flags[i].name[0] != '\0') {
            fprintf(fp, "%s", Flags[i].val ? "set(" : "clear(");
	    fflush(stdout);
	    fprintf(fp, "%s). ", Flags[i].name);
	    fflush(stdout);
	    j++;
	    if (j % 3 == 0)
	        fprintf(fp, "\n");
	    fflush(stdout);
            }

    fprintf(fp, "\n\n");

    j = 0;
    for (i = 0; i < MAX_PARMS; i++)  /* print parms */
	if (Parms[i].name[0] != '\0') {
	    fprintf(fp, "assign(");
	    fprintf(fp, "%s, %d). ", Parms[i].name, Parms[i].val);
	    j++;
	    if (j % 3 == 0)
		fprintf(fp, "\n");
	    }
    fprintf(fp, "\n");

}  /* print_options */

/*************
 *
 *    p_options()
 *
 *************/

void p_options(void)
{
    print_options(stdout);
}  /* p_options */

/*************
 *
 *   auto_change_flag()
 *
 *************/

void auto_change_flag(FILE *fp, int index, int val)
{
    if (Flags[index].val != val) {
	fprintf(fp, "   dependent: %s(%s).\n",
		val ? "set" : "clear", Flags[index].name);
	Flags[index].val = val;
	dependent_flags(fp, index);
	}
}  /* auto_change_flag */

/*************
 *
 *   void dependent_flags(FILE *fp, int index)
 *
 *   Flag[index] has just been changed.  Change any flags or parms that
 *   depend on it.  Write actions to *fp.
 *
 *   Mutually recursive with auto_change_flag and auto_change_parm.
 *
 *************/

void dependent_flags(FILE *fp, int index)
{
    /* This part handles flags that have just been set. */

    if (Flags[index].val) {

	switch (index) {
	    }
	}

    /* This part handles flags that have just been cleared. */

    if (Flags[index].val) {
	switch (index) {
	    }
	}
}  /* dependent_flags */

/*************
 *
 *   auto_change_parm()
 *
 *************/

void auto_change_parm(FILE *fp, int index, int val)
{
    if (Parms[index].val != val) {
	fprintf(fp, "   dependent: assign(%s, %d).\n",
		Parms[index].name, val);
		
	Parms[index].val = val;
	dependent_parms(fp, index);
	}
}  /* auto_change_parm */

/*************
 *
 *   void dependent_parms(FILE *fp, int index)
 *
 *   Parms[index] has just been changed.  Change any flags or parms that
 *   depend on it.  Write actions to *fp.
 *
 *   Mutually recursive with auto_change_flag and auto_change_parm.
 *
 *************/

void dependent_parms(FILE *fp, int index)
{
    switch (index) {
	}
}  /* dependent_parms */

/*************
 *
 *    int change_flag(fp, flag_name, set)
 *
 *    If success, return index of flag, if fail, return -1.
 *    Warning and error messages go to file fp.
 *
 *************/

int change_flag(FILE *fp, char *flag_name, int set)
{
    int index, found;

    found = 0;
    index = 0;
    while (index < MAX_FLAGS && !found)
	if (strcmp(flag_name, Flags[index].name) == 0)
	    found = 1;
	else
	    index++;
    if (!found) {
	fprintf(fp, "ERROR: flag `%s' not found.\n", flag_name);
	return(-1);
	}
    else if (Flags[index].val == set) {
	fprintf(fp, "WARNING: ");
	if (set)
	    fprintf(fp, " flag `%s' already set.\n", flag_name);
	else
	    fprintf(fp, " flag `%s' already clear.\n", flag_name);
	return(index);
	}
    else {
	Flags[index].val = set;
	return(index);
	}
}  /* change_flag */

/*************
 *
 *    int change_parm(fp, parm_name, val)
 *
 *    If success, return index of parm, if fail, return -1.
 *    Warning and error messages go to file fp.
 *
 *************/

int change_parm(FILE *fp, char *parm_name, int val)
{
    int index, found;

    found = 0;
    index = 0;
    while (index < MAX_PARMS && !found)
	if (strcmp(parm_name, Parms[index].name) == 0)
	    found = 1;
	else
	    index++;
    if (!found) {
	fprintf(fp, "ERROR: parameter `%s' not found.\n", parm_name);
	return(-1);
	}
    else if (val < Parms[index].min || val > Parms[index].max) {
	fprintf(fp, "ERROR: assign(%s, %d),", parm_name, val);
	fprintf(fp, " integer must be in range [%d,%d].\n",
		Parms[index].min, Parms[index].max);
	return(-1);
	}
    else if (val == Parms[index].val) {
	fprintf(fp, "WARNING: assign(%s, %d),", parm_name, val);
	fprintf(fp, " already has that value.\n");
	return(index);
	}
    else {
	Parms[index].val = val;
	return(index);
	}
}  /* change_parm */

/*************
 *
 *    check_options(fp)  --  check for inconsistent or odd settings
 *
 *    If a bad combination of settings is found, either a warning
 *    message is printed, or an ABEND occurs.
 *
 *************/

void check_options(FILE *fp)
{
    if (Flags[PARA_PAIRS].val && Flags[DELAY_BACK_DEMOD].val)
	abend("flags para_pairs and delay_back_demod both set");
}  /* check_options */

