#ifndef TP_HEADER_H
#define TP_HEADER_H

/************ BASIC INCLUDES ************/

#if 0
#    include <fpu_control.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

/******** Types of exit ********/

#define PROOF_EXIT           10
#define ABEND_EXIT           11
#define SOS_EMPTY_EXIT       12
#define MAX_GIVEN_EXIT       13
#define INTERRUPT_EXIT       14
#define INPUT_ERROR_EXIT     15
#define MAX_SECONDS_EXIT     16

/************* END OF ALL GLOBAL CONSTANT DEFINITIONS ****************/

#include "Options.h"
#include "Stats.h"
#include "Misc.h"
#include "Avail.h"
#include "Term.h"

/***********************************************************************/

#endif  /* ! TP_HEADER_H */
