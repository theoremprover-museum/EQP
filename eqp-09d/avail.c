#include "Header.h"  /* for Parms[], abend(). */
#include "Avail.h"

#define TP_ALLOC_SIZE 500000  /* Size of blocks allocated by malloc */

static void *Alloc_block;     /* location returned by most recent malloc */
static void *Alloc_pos;       /* current position in block */

static int Malloc_calls;  /* number of calls to malloc */

/*************
 *
 *    void *tp_alloc(n)
 *
 *    Allocate n contiguous bytes, aligned on pointer boundry.
 *
 *************/

void *tp_alloc(size_t n)
{
    char *return_block;
    int scale;
    
    /* if n is not a multiple of sizeof(void *), then round up so that it is */
    
    scale = sizeof(void *);
    if (n % scale != 0)
	n = n + (scale - (n % scale));
    
    if (!Alloc_block || Alloc_block + TP_ALLOC_SIZE - Alloc_pos < n) {
        /* try to malloc a new block */
	if (n > TP_ALLOC_SIZE)
	    abend("in tp_alloc, request too big.");
	else if (Parms[MAX_MEM].val != -1 &&
		 (Malloc_calls+1)*(TP_ALLOC_SIZE/1024.0) > Parms[MAX_MEM].val)
	    abend("in tp_alloc, max_mem parameter exceeded.");
	else {

	    Alloc_pos = Alloc_block = malloc((size_t) TP_ALLOC_SIZE);
	    Malloc_calls++;
	    if (!Alloc_pos)
		abend("in tp_alloc, operating system cannot supply any more memory.");
	    }
        }
    return_block = Alloc_pos;
    Alloc_pos += n;
    return(return_block);
}  /* tp_alloc */

/*************
 *
 *    int total_mem() -- How many K have been dynamically allocated?
 *
 *************/

int total_mem(void)
{
    return( (int) (Malloc_calls * (TP_ALLOC_SIZE / 1024.)));
}  /* total_mem */

