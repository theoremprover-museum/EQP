#include "Header.h"
#include "List.h"
#include "Symbols.h"
#include "Io.h"
#include "Clause.h"
#include "Pindex.h"

/*
 * This file has code for indexing clauses that are to be retrieved in
 * pairs.  When a clause is inserted, its weight is given.  Retrieval
 * is by sum of the weights of the pair -- lowest first.  Say we have
 * clauses with weights 0--4.  Then pairs will be returned in this order:
 *
 * (0,0)
 * (0,1)
 * (1,1)  (0,2)
 * (1,2)  (0,3)
 * (2,2)  (1,3)  (0,4)
 * (2,3)  (1,4)
 * (3,3)  (2,4)
 * (3,4)
 * (4,4)
 *
 * Clauses can be inserted after retrieval has begun; the smallest
 * available pair will always be returned.  When the index is
 * initialized, the caller supplies a parameter N, and the actual
 * weight range for indexing will be 0..N-1.  If an inserted clause has
 * weight outside of this range, the weight will be changed to 0 or N-1.
 *
 * This is intended to be used for binary inference rules such as
 * paramodulation and resolution.
 *
 * We keep a list of clauses with each weight 0..N-1.  When the index
 * is initialized, we allocate two N x N arrays (top and curr)
 * to keep track of the positions in the pairs of lists.
 * (Since the size of the array is not known at compile time,
 * we access elements as a[i*N+j] instead of a[i][j].)
 *
 * For a pair of weights i and j, we use top[i,j] and curr[i,j] to
 * point to positions in list i w.r.t. list j.  And vice versa.
 * Roughly, it goes like this: Lists grow upward, and clauses above
 * "top" have not yet been considered.  Increment top[i,j], then return
 * top[i,j] with each member of list j up to top[j,i].  (Curr[j,i]
 * marks the position between retrievals.)  Now all pairs up to the two
 * tops have been returned.  Now pick the top with the smallest clause
 * ID, increment it, and continue.  If at any point a new clause C is
 * inserted into the index, and C goes with other clasues to make
 * smaller pairs, those smaller pairs are returned before continuing
 * with i and j.
 *
 * Valid states of an index.
 * Case 1:  i == j.
 *   (a) top and curr are both NULL; this occurs if the list is empty.
 *   (b) top and top=curr; done up through top.
 *   (c) top and !curr; done up through top-1.
 *   (d) top and curr and top > curr; done up through <top,curr>.
 * Case 2: i != j.
 *   (a) !t1 and !t2; one or both lists empty.
 *   (b) t1 and t2 and t1=c1 and t2=c2; done up through tops.
 *   (c) t1 and t2 and t1=c1 and c2=NULL; done up through <t1-1, t2>.
 *   (d) t1 and t2 and t1=c1 and c2<t2; done through <t1-1, t2>;
 *       also through <t1,c2>.
 *   (e) same as c,d, but vice versa.
 * 
 * This is similar to the method in "A Theorem-Proving Language
 * for Experimentation" by Henschen, Overbeek, Wos, CACM 17(6), 1974.
 * 
 */

#define INT_LARGE INT_MAX / 2  /* It can be doubled without overflow. */
#define IN_RANGE(i, min, max) (i < min ? min : (i > max ? max : i))

/*************
 *
 *   init_pair_index()
 *
 *   Allocate and initialize a pair-index.
 *   Parameter n specifies the range 0 .. n-1 of weights
 *   that will be used.  If a clause is inserted with weight
 *   outside of that range, the effective weight will be set
 *   to 0 or n-1.
 *
 *************/

Pair_index_ptr init_pair_index(int n)
{
    Pair_index_ptr p;
    int i, j;

    p = get_pair_index();

    p->finished = 1;
    p->n = n;
    p->i = 0;
    p->j = 0;
    p->clause_min = INT_LARGE;
    p->new_clause_min = INT_LARGE;

    p->lists = tp_alloc(n * sizeof(List_ptr));
    p->top   = tp_alloc(n * n * sizeof(List_pos_ptr));
    p->curr  = tp_alloc(n * n * sizeof(List_pos_ptr));

    /* top and curr will be indexed as top[i*n+j]. */

    for (i = 0; i < n; i++)
	p->lists[i] = get_list();
    for (i = 0; i < n; i++)
	for (j = 0; j < n; j++) {
	    p->top[i*p->n+j] = NULL;
	    p->curr[i*p->n+j] = NULL;
	    }

    return(p);
}  /* init_clause_pair_index */

/*************
 *
 *   pairs_available()
 *
 *   Are there any pairs available in the index?
 *
 *************/

int pairs_available(Pair_index_ptr p)
{
    return(!p->finished);
}  /* pairs_available */

/*************
 *
 *   init_pair()
 *
 *   Initialize top and curr for lists i and j.
 *
 *************/

static void init_pair(int i, int j, Pair_index_ptr p)
{
    int n = p->n;
    List_pos_ptr lp_i = p->lists[i]->first;
    List_pos_ptr lp_j = p->lists[j]->first;

    if (lp_i && lp_j) {
	if (i == j) {
	    p->top[i*n+i] = lp_i;
	    p->curr[i*n+i] = NULL;
	    }
	else {
	    p->top[i*n+j] = lp_i;
	    p->top[j*n+i] = lp_j;
	    /* It doesn't matter which curr gets set to NULL. */
	    p->curr[i*n+j] = lp_i;
	    p->curr[j*n+i] = NULL;
	    }
	}
    else {
	p->top[i*n+j] = NULL;
	p->top[j*n+i] = NULL;
	p->curr[i*n+j] = NULL;
	p->curr[j*n+i] = NULL;
	}
}  /* init_pair */

/*************
 *
 *   insert_pair_index()
 *
 *   Insert a clause into a pair index.
 *
 *************/

void insert_pair_index(Clause_ptr c, int wt, Pair_index_ptr p)
{
    /* If the new clause will be the only one in its list, then
     * for each nonempty list, set the top and curr.
     */
    int i, j, id_j, n;

    n = p->n;
    j = IN_RANGE(wt, 0, n-1);
    id_j = c->id;

    if (p->lists[j]->first == NULL) {
	list_append(c, p->lists[j]);
	for (i = 0; i < p->n; i++)
	    init_pair(i, j, p);
	}
    else
	list_append(c, p->lists[j]);

    p->finished = 0;
    if (wt < p->new_clause_min)
	p->new_clause_min = wt;
    if (wt < p->clause_min)
	p->clause_min = wt;
}  /* insert_pair_index */

/*************
 *
 *   delete_pair_index()
 *
 *   Delete a clause from a pair index.  The parameter
 *   wt must be the same as when the clause was inserted.
 *
 *************/

void delete_pair_index(Clause_ptr c, int wt, Pair_index_ptr p)
{
    int i, j;
    int n = p->n;
    List_pos_ptr lp;

    j = IN_RANGE(wt, 0, n-1);

    for (lp = p->lists[j]->first; lp && lp->c != c; lp = lp->next);
    if (!lp) {
	abend("delete_pair_index, clause not found.");
	}

    /* We are deleting a clause from list j.  For each list i, consider the
     * pair [i,j].  Top[i,j] and curr[i,j] (say t1 and c1) point into list i,
     * and top[j,i] and curr[j,i] (say t2 anc c2) point into list j.
     */

    for (i = 0; i < n; i++) {
	List_pos_ptr t1 = p->top[i*n+j];
	List_pos_ptr c1 = p->curr[i*n+j];
	List_pos_ptr t2 = p->top[j*n+i];
	List_pos_ptr c2 = p->curr[j*n+i];

	if (i == j) {
	    if (t2 == lp) {
		/* printf("HERE: i == j\n"); */
		/* This handles t2=c2, c2==NULL, c2 != NULL, singleton list. */
		if (t2->next) {
		    p->top[i*n+i] = t2->next;
		    p->curr[i*n+i] = NULL;
		    }
		else {
		    p->top[i*n+i] = t2->prev;
		    p->curr[i*n+i] = t2->prev;
		    }
		}
	    else if (c2 == lp) {
		p->curr[i*n+i] = c2->prev;
		}
	    }
	else {  /* i != j */

	    if (lp == t2) {
		/* printf("HERE: i != j (B)\n"); */
		if (t2 == c2) {
		    if (t2->next) {
			t2 = t2->next;
			c2 = c2->next;
			c1 = NULL;
			}
		    else if (t2->prev) {
			t2 = t2->prev;
			c2 = c2->prev;
			c1 = t1;
			}
		    else
			t1 = c1 = t2 = c2 = NULL;
		    }
		else if (t2->prev)
		    t2 = t2->prev;
		else if (t2->next) {
		    t2 = t2->next;
		    c2 = NULL;
		    t1 = c1 = p->lists[i]->first;
		    }
		else
		    t1 = c1 = t2 = c2 = NULL;
		}
	    else if (lp == c2) {
		/* printf("HERE: i != j (D)\n"); */
		c2 = c2->prev;
		}

	    p->top[i*n+j] = t1;
	    p->curr[i*n+j] = c1;
	    p->top[j*n+i] = t2;
	    p->curr[j*n+i] = c2;
	    }
	}
    list_remove(c, p->lists[j]);
}  /* delete_pair_index */

/*************
 *
 *   retrieve_pair()
 *
 *************/

void retrieve_pair(Pair_index_ptr p, Clause_ptr *cp1, Clause_ptr *cp2)
{
    int i, j, k, max_k, found, n;

    /* First, find i and j, the smallest pair of weights that
     * have clauses available.  p->i and p->j are from the
     * previous retrieval, and if no clauses have been inserted
     * since then, start with them.  Otherwise, use new_clause_min
     * (the smallest weight inserted since the previous retrieval)
     * and clause_min (the smallest weight in the index) to decide
     * where to start looking.
     */ 
     
    if (p->clause_min + p->new_clause_min < p->i + p->j) {
	i = p->clause_min;
	j = p->new_clause_min;
	}
    else {
	i = p->i;
	j = p->j;
	}

    n = p->n;
    k = i+j;
    max_k = (n + n) - 2;
    found = 0;

    while (k <= max_k && !found) {
	i = k / 2; j = k - i;
	while (i >= 0 && j < n && !found) {
	    /* This test works if (i==j). */
	    found = (p->top[i*n+j] != p->curr[i*n+j] ||
		     p->top[j*n+i] != p->curr[j*n+i] ||
		     (p->top[i*n+j] && p->top[i*n+j]->next) ||
		     (p->top[j*n+i] && p->top[j*n+i]->next));
		     		     
	    if (!found) {
		i--; j++;
		}
	    }
	if (!found)
	    k++;
	}

    if (!found) {
	*cp1 = NULL; *cp2 = NULL;
	p->finished = 1;
	}
    else {
        /* OK, there should be a pair in (i,j). */

        /* Recall that if top[i,j]=curr[i,j] and top[j,i]=top[j,i],
         * then all pairs up to those positions have been returned.
         */

	List_pos_ptr t1 = p->top[i*n+j];
	List_pos_ptr c1 = p->curr[i*n+j];
	List_pos_ptr t2 = p->top[j*n+i];
	List_pos_ptr c2 = p->curr[j*n+i];

	if (i == j) {
	    if (t1 == c1) {
		p->top[i*n+i]  = t1 =  t1->next;
		p->curr[i*n+i] = c1 = NULL;
		}
	    *cp1 = t1->c;
	    p->curr[i*n+i] = c1 = (c1 ? c1->next : p->lists[i]->first);
	    *cp2 = c1->c;
	    }
	else {  /* i != j */
	    if (t1 == c1 && t2 == c2) {
		/* Both tops equal their currs, so pick a top to increment. */
		if (t1->next && (t1->c->id < t2->c->id || !t2->next)) {
		    p->top[i*n+j]  = t1 = t1->next;
		    p->curr[i*n+j] = c1 = c1->next;
		    p->curr[j*n+i] = c2 = NULL;
		    }
		else {
		    p->top[j*n+i]  = t2 = t2->next;
		    p->curr[j*n+i] = c2 = c2->next;
		    p->curr[i*n+j] = c1 = NULL;
		    }
		}
	    if (t1 == c1) {
		*cp1 = t1->c;
		p->curr[j*n+i] = c2 = (c2 ? c2->next : p->lists[j]->first);
		*cp2 = c2->c;
		}
	    else if (t2 == c2) {
		*cp1 = t2->c;
		p->curr[i*n+j] = c1 = (c1 ? c1->next : p->lists[i]->first);
		*cp2 = c1->c;
		}
	    else
		abend("retrieve_pair: bad state.");
	    }
	/* Save the "working pair" for next time. */
	p->i = i;
	p->j = j;
	p->new_clause_min = INT_LARGE;
	}
}  /* retrieve_pair */

 /*************
 *
 *   p_pair_index()
 *
 *   Print a pair index.  It is printed in detail, so it should be
 *   called only for small test indexes.
 *
 *************/

void p_pair_index(Pair_index_ptr p)
{
    int i, j, n;
    List_pos_ptr x;

    n = p->n;

    for (i = 0; i < n; i++) {
	printf("List %d: ", i);
	for (x = p->lists[i]->first; x; x = x->next)
	    printf(" %3d", x->c->id);
	printf(".\n");
	}
    for (i = 0; i < n; i++) {
	for (j = 0; j < n; j++) {
	    printf(" [");
	    if (p->top[i*n+j])
		printf("%2d",p->top[i*n+j]->c->id);
	    else
		printf("--");
	    printf(",");
	    if (p->curr[i*n+j])
		printf("%2d",p->curr[i*n+j]->c->id);
	    else
		printf("--");
	    printf("] ");
	    fflush(stdout);
	    }
	printf("\n");
	}
}  /* p_pair_index */

/* memory management */

static Pair_index_ptr pair_index_avail;
static long pair_index_gets, pair_index_frees, pair_index_avails;

/*************
 *
 *    Pair_index_ptr get_pair_index()
 *
 *************/

Pair_index_ptr get_pair_index(void)
{
    Pair_index_ptr p;

    pair_index_gets++;
    if (!pair_index_avail)
        p = tp_alloc(sizeof(struct pair_index));
    else {
        pair_index_avails--;
        p = pair_index_avail;
        pair_index_avail = pair_index_avail->next;
        }

    p->n = 0;
    p->i = 0;
    p->j = 0;
    p->clause_min = INT_LARGE;
    p->new_clause_min = INT_LARGE;
    p->next = NULL;

    return(p);
}  /* get_pair_index */

/*************
 *
 *    free_pair_index()
 *
 *************/

void free_pair_index(Pair_index_ptr p)
{
    pair_index_frees++;
    pair_index_avails++;
    p->next = pair_index_avail;
    pair_index_avail = p;
}  /* free_pair_index */

/*************
 *
 *   print_pindex_mem()
 *
 *************/

void print_clause_pair_mem(FILE *fp, int heading)
{
    if (heading)
	fprintf(fp, "  type (bytes each)        gets      frees     in use      avail      bytes\n");

    fprintf(fp, "pair_index (%5d)    %11ld%11ld%11ld%11ld%9.1f K\n", sizeof(struct pair_index), pair_index_gets, pair_index_frees, pair_index_gets - pair_index_frees, pair_index_avails, (((pair_index_gets - pair_index_frees) + pair_index_avails) * sizeof(struct pair_index)) / 1024.);
}  /* print_pindex_mem */

/*************
 *
 *   p_clause_pair_mem()
 *
 *************/

void p_clause_pair_mem()
{
    print_clause_pair_mem(stdout, 1);
}  /* p_clause_pair_mem */

/*
 *  end of memory management
 */

/*************
 *
 *   pair_already_used()
 *
 *************/

int pair_already_used(Clause_ptr c1, int weight1,
			   Clause_ptr c2, int weight2,
			   Pair_index_ptr p)
{
    int i, j, id1, id2, rc;
    int n = p->n;
    List_pos_ptr top1, curr1, top2, curr2;

    id1 = c1->id;
    id2 = c2->id;

    i = IN_RANGE(weight1, 0, n-1);
    j = IN_RANGE(weight2, 0, n-1);

    top1 = p->top[i*n+j];
    curr1 = p->curr[i*n+j];

    top2 = p->top[j*n+i];
    curr2 = p->curr[j*n+i];

    if (!top1 || !top2) {
	/* One of the lists is empty.  If this happens, something is
	   probably wrong: why would we be trying to use c1 and c2? 
	*/
	fprintf(stderr, "WARNING, pair_already_used: bad state?\n");
	rc = 0;
	}
    else if (i == j) {
	/* Let id2 be the greater one. */
	if (id1 > id2) {
	    int tmp = id1; id1 = id2; id2 = tmp;
	    }
	rc = ((id2 < top1->c->id) ||
	      (id2 == top1->c->id && curr1 && id1 <= curr1->c->id));
	}
    else {  /* i != j */

	if (top1 == curr1) {
	    rc = ((id1 <  top1->c->id && id2 <= top2->c->id) ||
		  (id1 == top1->c->id && curr2 && id2 <= curr2->c->id));
	    }
	else if (top2 == curr2) {
	    rc = ((id2 <  top2->c->id && id1 <= top1->c->id) ||
		  (id2 == top2->c->id && curr1 && id1 <= curr1->c->id));
	    }
	else
	    abend("pair_already_used, bad state");
	}	

    return(rc);
}  /* pair_already_used */
