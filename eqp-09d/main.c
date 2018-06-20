#include "Header.h"
#include "List.h"
#include "Symbols.h"
#include "Io.h"
#include "Unify.h"
#include "Ac.h"
#include "Discrim.h"
#include "Fpa.h"
#include "Clause.h"
#include "List.h"
#include "Paramod.h"
#include "Demod.h"
#include "Eqp.h"

#include <unistd.h>  /* for gethostname() */

/*************
 *
 *    void print_banner(argc, argv)
 *
 *************/

static void print_banner(int argc, char **argv)
{
    int i;
    char host[64];

    if (gethostname(host, 64) != 0)
	strcpy(host, "???");

    printf("----- EQP 0.9d, April 1998 -----\nThe job began on %s, %s", host, get_time());

    printf("The command was \"");
    for(i = 0; i < argc; i++)
        printf("%s%s", argv[i], (i < argc-1 ? " " : ""));
    printf("\".\n\n");
    
}  /* print_banner */

/*************
 *
 *    main
 *
 *************/

int main(int argc, char **argv)
{
    int rc;

#if 0
    setbuf(stdout, NULL); /* disables output buffering (needed for dwarfs) */
#endif

#if 0
    int i = 0;
    for (i = 1; i < 10; i++)
        {
        printf("This is line %d.\n", i);
        fflush(stdout);
        sleep(1);
        } 
#endif

    print_banner(argc, argv);
    init();

    read_preamble();  /* up to "end_of_commands". */

    if (Stats[INPUT_ERRORS] != 0) {
        fprintf(stderr, "\nInput errors were found.\007\n\n");
        printf("Input errors were found.\n");
        print_options(stdout);
        exit(INPUT_ERROR_EXIT);
        }
    else {
	rc = eq_prover();
        output_stats(stdout);
	exit(rc);
	}
}  /* main */

