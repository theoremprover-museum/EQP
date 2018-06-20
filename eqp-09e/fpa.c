#include "Header.h"
#include "Symbols.h"
#include "Io.h"
#include "List.h"
#include "Unify.h"
#include "Fpa.h"

#define MAX_PATH 110  /* max number of integers in a path, incl. end mark */

#define INTERSECT  1
#define UNION      2
#define LEAF       3

/* We will be deleting Gen_ptr_ptr nodes from FPA lists while outside pointers
 * still point at them, so we'll keep those "deleted" nodes in a valid state
 * until it is safe to really free them.  We maintain a list of pointers to
 * those nodes.
 * This happens, for example, when we are in the midst of generating
 * paramodulants, and we generate one that causes back demodulation of
 * one that the paramodulation structures refer to.
 */

static Gen_ptr_ptr Dangle_list = NULL;

/* for mutually recursive calls */

 Fpa_tree_ptr build_tree_local(Term_ptr t, int u_type, int *path,
			       int j, int bound, Fpa_head_ptr *table);

/*
 * memory management
 */

static Fpa_head_ptr fpa_head_avail;
static Fpa_tree_ptr fpa_tree_avail;
static Fpa_pos_ptr fpa_pos_avail;
static long fpa_head_gets, fpa_head_frees, fpa_head_avails;
static long fpa_tree_gets, fpa_tree_frees, fpa_tree_avails;
static long fpa_pos_gets, fpa_pos_frees, fpa_pos_avails;

/*************
 *
 *    Fpa_head_ptr get_fpa_head()
 *
 *************/

static Fpa_head_ptr get_fpa_head(void)
{
    Fpa_head_ptr p;

    fpa_head_gets++;
    if (!fpa_head_avail)
        p = tp_alloc(sizeof(struct fpa_head));
    else {
        fpa_head_avails--;
        p = fpa_head_avail;
        fpa_head_avail = fpa_head_avail->next;
        }

    p->path = NULL;
    p->list = NULL;
    p->next = NULL;

    return(p);
}  /* get_fpa_head */

/*************
 *
 *    free_fpa_head()
 *
 *************/

static void free_fpa_head(Fpa_head_ptr p)
{
    fpa_head_frees++;
    fpa_head_avails++;
    p->next = fpa_head_avail;
    fpa_head_avail = p;
}  /* free_fpa_head */

/*************
 *
 *    Fpa_tree_ptr get_fpa_tree()
 *
 *************/

static Fpa_tree_ptr get_fpa_tree(void)
{
    Fpa_tree_ptr p;

    fpa_tree_gets++;
    if (!fpa_tree_avail)
        p = tp_alloc(sizeof(struct fpa_tree));
    else {
        fpa_tree_avails--;
        p = fpa_tree_avail;
        fpa_tree_avail = fpa_tree_avail->left;
        }

    p->list = NULL;
    p->left = NULL;
    p->right = NULL;
    p->left_term = NULL;
    p->right_term = NULL;
    p->path = NULL;

    return(p);
}  /* get_fpa_tree */

/*************
 *
 *    free_fpa_tree()
 *
 *************/

static void free_fpa_tree(Fpa_tree_ptr p)
{
    fpa_tree_frees++;
    fpa_tree_avails++;
    p->left = fpa_tree_avail;
    fpa_tree_avail = p;
}  /* free_fpa_tree */

/*************
 *
 *    Fpa_pos_ptr get_fpa_pos()
 *
 *************/

static Fpa_pos_ptr get_fpa_pos(void)
{
    Fpa_pos_ptr p;

    fpa_pos_gets++;
    if (!fpa_pos_avail)
        p = tp_alloc(sizeof(struct fpa_pos));
    else {
        fpa_pos_avails--;
        p = fpa_pos_avail;
        fpa_pos_avail = fpa_pos_avail->next;
        }

    p->next = NULL;

    return(p);
}  /* get_fpa_pos */

/*************
 *
 *    free_fpa_pos()
 *
 *************/

static void free_fpa_pos(Fpa_pos_ptr p)
{
    fpa_pos_frees++;
    fpa_pos_avails++;
    p->next = fpa_pos_avail;
    fpa_pos_avail = p;
}  /* free_fpa_pos */

/*************
 *
 *   print_fpa_mem()
 *
 *************/

void print_fpa_mem(FILE *fp, int heading)
{
    if (heading)
	fprintf(fp, "  type (bytes each)        gets      frees     in use      avail      bytes\n");
    fprintf(fp, "fpa_head (%4d)     %11ld%11ld%11ld%11ld%9.1f K\n", sizeof(struct fpa_head), fpa_head_gets, fpa_head_frees, fpa_head_gets - fpa_head_frees, fpa_head_avails, (((fpa_head_gets - fpa_head_frees) + fpa_head_avails) * sizeof(struct fpa_head)) / 1024.);
    fprintf(fp, "fpa_tree (%4d)     %11ld%11ld%11ld%11ld%9.1f K\n", sizeof(struct fpa_tree), fpa_tree_gets, fpa_tree_frees, fpa_tree_gets - fpa_tree_frees, fpa_tree_avails, (((fpa_tree_gets - fpa_tree_frees) + fpa_tree_avails) * sizeof(struct fpa_tree)) / 1024.);
    fprintf(fp, "fpa_pos (%4d)      %11ld%11ld%11ld%11ld%9.1f K\n", sizeof(struct fpa_pos), fpa_pos_gets, fpa_pos_frees, fpa_pos_gets - fpa_pos_frees, fpa_pos_avails, (((fpa_pos_gets - fpa_pos_frees) + fpa_pos_avails) * sizeof(struct fpa_pos)) / 1024.);

}  /* print_fpa_mem */

/*************
 *
 *   p_fpa_mem()
 *
 *************/

void p_fpa_mem()
{
    print_fpa_mem(stdout, 1);
}  /* p_fpa_mem */

/*
 *  end of memory management
 */

/*************
 *
 *    fpa_init(depth) -- allocate and initilaize an FPA index.
 *
 *************/

Fpa_index_ptr fpa_init(int depth)
{
    int i;
    Fpa_index_ptr p;

    /* Get number of integers needed for largest path, incl. end marker. */
    i = (2*depth + 2*sizeof(int)) / sizeof(int);
    if (i > MAX_PATH)
	abend("fpa_init: depth too big; increase MAX_PATH.");

    p = tp_alloc(sizeof(struct fpa_index));
    p->depth = depth;
    for (i = 0; i < FPA_SIZE; i++)
	p->table[i] = 0;
    return(p);
}  /* fpa_init */

/*************
 *
 *   fpa_dealloc(d)
 *
 *************/

void fpa_dealloc(Fpa_index_ptr p)
{
    int i;

    for (i = 0; i < FPA_SIZE && !p->table[i]; i++);
    if (i < FPA_SIZE)
	abend("in fpa_dealloc, nonempty index.");
    else
	printf("free_fpa_index not implemented.\n");
}  /* fpa_dealloc */

/*
 * MESSY IMPLEMENTATION DETAIL:  Paths have one byte per member, plus
 * a word of 0s to mark the end.  When accessing members of a path,
 * we treat a path as an array of unsigned chars.  When comparing,
 * copying, and hashing paths, we treat them as arrays of ints (for
 * speed).  The form  for argument passing is as an array
 * of ints, because lint complains about possible alignment errors when
 * casting (unsigned char *) to (int *).
 *
 * The current position in the path (usually variable j) counts in bytes.
 */

/*************
 *
 *    static void path_mark_end(path, j)
 *
 *    j (which counts bytes) is one past last entry.
 *
 *************/

static void path_mark_end(int *path, int j)
{
    int i, k, m;
    unsigned char *cpath;

    cpath = (unsigned char *) path;

    /* make sure the rest of the integer, starting with j, and the */
    /* whole next integer (unless j is at beginning) are all 0. */
    
    m = j % sizeof(int);  /* position of j in an int */

    if (m == 0)
	i = sizeof(int);  /* just fill int with 0s */
    else
	i = (2 * sizeof(int)) - m;  /* 0 rest of int and next int */

    for (k = 0; k < i; k++)
	cpath[j++] = 0;
    
}  /* path_mark_end */

/*************
 *
 *    static int hash_path(path)
 *
 *************/

static int hash_path(int *path)
{
    int i, val;

    val = 0;

    for (i = 0; path[i] != 0; i++)
	val += path[i];

    return(abs(val) % FPA_SIZE);
}  /* hash_path */

/*************
 *
 *    static int path_comp(p1, p2)
 *
 *************/

static int path_comp(int *p1, int *p2)
{
    while (*p1 == *p2 && *p1 != 0 && *p2 != 0) {
	p1++;
	p2++;
	}

    if (*p1 < *p2)
	return(-1);
    else if (*p1 > *p2)
	return(1);
    else
	return(0);

}  /* path_comp */

/*************
 *
 *    static int path_size(path) -- in ints, including 0 word at end
 *
 *************/

static int path_size(int *path)
{
    int i;
    int *p1;

    for (i = 1, p1 = path; *p1 != 0; p1++, i++);
    return(i);
}  /* path_size */

/*************
 *
 *    static int *path_copy(path)
 *
 *************/

static int *path_copy(int *path)
{
    int i, j;
    int *p2;

    i = path_size(path);

    p2 = tp_alloc(i * sizeof(int));

    for (j = 0; j < i; j++)
	p2[j] = path[j];

    return(p2);
    
}  /* path_copy */

/*************
 *
 *    static insert_fpa_tab
 *
 *    Insert a term into an FPA indexing list.  Create a new list
 *    if necessary.  The path is something like "p 1 f 2 g 4 h 3 a".
 *
 *************/

static void insert_fpa_tab(Term_ptr obj, Fpa_head_ptr *table, int *path)
{
    int hashval, c;
    Gen_ptr_ptr g1, g2, g3;
    Fpa_head_ptr fp1, fp2, fp3;

    /* Treat path as integers here. */
    
    hashval = hash_path(path);
    fp1 = table[hashval];
    fp2 = NULL;

    while (fp1 && (c = path_comp(fp1->path, path)) == -1) {
	fp2 = fp1;
	fp1 = fp1->next;
	}
    
    if (!fp1 || c != 0) { /* need new fpa_head */
	fp3 = get_fpa_head();
	fp3->path = path_copy(path);
	g1 = get_gen_ptr();
	fp3->list = g1;
	g1->u.t = obj;
    
	if (!fp2) {
	    /* insert at beginning */
	    fp3->next = table[hashval];
	    table[hashval] = fp3;
	    }
	else {  /* insert after fp2 */
	    fp3->next = fp1;
            fp2->next = fp3;
            }
	}

    else { /* we have a matching fpa_head, so insert p in its list */
    
	g1 = fp1->list;
	g2 = NULL;
	/* keep list sorted, decreasing addresses */
	while (g1 && g1->u.t->fpa_id > obj->fpa_id) {
	    g2 = g1;
	    g1 = g1->next;
	    }
	if (g1 && g1->u.t == obj)
	    /* Overloads occur when a term has two or more different
	     * paths that (in this implementation) map to the same path.
             * This can occur because members of paths are unsigned chars,
	     * (range 0--255).  Argument positions can overload if there is
             * a function symbol with arity > 255, and symbols can overload
             * if the symbol number is < -255.  (Symbol numbers are negative
	     * and are made positive for paths.)  Overloaded FPA lists are OK.
	     */
	    Stats[FPA_OVERLOADS]++;  /* object already in list */
	else {
	    g3 = get_gen_ptr();
	    g3->u.t = obj;
	    if (!g2) {  /* insert at beginning */
		g3->next = fp1->list;
		fp1->list = g3;
		}
	    else {  /* insert after g2 */
		g3->next = g1;
		g2->next = g3;
		}
	    }
	}
}  /* insert_fpa_tab */

/*************
 *
 *    static delete_fpa_tab
 *
 *    Delete a term from an FPA indexing list.  It is assumed that
 *    the corresponding `insert_fpa_tab' was previously made.
 *
 *************/

static void delete_fpa_tab(Term_ptr obj, Fpa_head_ptr *table, int *path)
{
    int hashval;
    Gen_ptr_ptr g1, g2;
    Fpa_head_ptr fp1, fp2;
    
    /* Treat path as integers here. */

    hashval = hash_path(path);
    fp1 = table[hashval];
    fp2 = NULL;

    while (fp1 && path_comp(fp1->path, path) != 0) {
	fp2 = fp1;
	fp1 = fp1->next;
	}
    
    if (!fp1)
	Stats[FPA_UNDERLOADS]++;  /* fpa list not found */
    else {  /* we have a matching fpa_head, so look for f in its list */
    
	g1 = fp1->list;
	g2 = NULL;
	/* list is sorted, decreasing IDs */
	while (g1 && g1->u.t->fpa_id > obj->fpa_id) {
	    g2 = g1;
	    g1 = g1->next;
	    }
	if (!g1 || g1->u.t != obj)
	    Stats[FPA_UNDERLOADS]++;  /* term not found in list */
	else {
	    if (!g2) {  /* delete from beginning */
		fp1->list = g1->next;
		if (!fp1->list) {  /* delete fpa_head also */
		    if (!fp2)
		        table[hashval] = fp1->next;
		    else
		        fp2->next = fp1->next;
		    free_fpa_head(fp1);
		    /* don't worry about fp1->path; let it be lost forever */
		    }
		}
	    else  /* delete */
		g2->next = g1->next;

	    /* add g1 to Dangle_list */

  	    g2 = get_gen_ptr();
	    g2->next = Dangle_list;
	    Dangle_list = g2;
	    g2->u.p = g1;

	    }
	}
}  /* delete_fpa_tab */

/*************
 *
 *   void term_fpa_rec
 *
 *   Recursive procedure called by fpa_insert and fpa_delete.
 *
 *************/

static void term_fpa_rec(int insert, Term_ptr t, Term_ptr obj,
     Fpa_head_ptr *table, int bound, int *path, int j)
{
    unsigned char *cpath;

    cpath = (unsigned char *) path;
    
    /* `path' has the path from super_term to t */

    if (VARIABLE(t)) /* variable contributes nothing */
	cpath[j++] = 0;
    else 
	cpath[j++] = abs(t->symbol);
    
    /* insert or delete path */

    path_mark_end(path, j);

    if (insert)
	insert_fpa_tab(obj, table, path);
    else
        delete_fpa_tab(obj, table, path);
    
    if (COMPLEX(t) && bound > 0 && !is_assoc_comm(t->symbol)) {
	int i;
	for (i = 0; i < t->arity; i++) {
	    cpath[j] = i+1;
	    term_fpa_rec(insert, t->args[i], obj,
			 table, bound-1, path, j+1);
	    }
	}
}  /* term_fpa_rec */

/*************
 *
 *    void fpa_insert
 *
 *    Insert a term into an FPA indexing index.  Level == 0
 *    gives indexing on functor only.  With the term f(a,x,g(b)),
 *    Level == 1 gives indexing on f, a, x, and g.
 *
 *************/

void fpa_insert(Term_ptr t, Fpa_index_ptr index)
{
    static int fpa_id_count;
    int path[MAX_PATH];

    if (t->fpa_id == 0)
	t->fpa_id = ++fpa_id_count;
    term_fpa_rec(1, t, t, index->table, index->depth, path, 0);
}  /* fpa_insert */

/*************
 *
 *    void fpa_delete(term, index, object)
 *
 *    Delete a term from an FPA indexing index.   The depth
 *    must be the same as when the term was given to fpa_insert.
 *
 *************/

void fpa_delete(Term_ptr t, Fpa_index_ptr index)
{
    int path[MAX_PATH];

    /* Deletion can be turned off, because it is usually quite slow.
       In some cases with a lot of back demodulation, FPA deletion can
       take half of the run time.  It is slow, because FPA lists are
       kept ordered by decreasing fpa_id (for quick insertion, because
       new terms have high fpa_id).

       If deletion is turned off, terms in disabled clauses remain in
       FPA lists, and the retrieval routine skips them (see
       fpa_retrieve_next).  Terms in disabled clauses are marked with
       a bit "disabled".
     */

    if (Flags[FPA_DELETE].val)
	term_fpa_rec(0, t, t, index->table, index->depth, path, 0);
}  /* fpa_delete */

/*************
 *
 *    static Fpa_tree_ptr get_leaf_node(path, table)
 *
 *    Given a path, if an FPA list exists, then return it in a
 *    leaf node; else return(NULL).
 *
 *************/

static Fpa_tree_ptr get_leaf_node(int *path, Fpa_head_ptr *table)
{
    Fpa_head_ptr fp;
    Fpa_tree_ptr pp;
    int c;
    
    fp = table[hash_path(path)];
    while (fp && (c = path_comp(fp->path,path)) == -1)
	fp = fp->next;
    if (!fp || c != 0)
	return(NULL);
    else {
	pp = get_fpa_tree();
	pp->type = LEAF;
#if 1  /* Path field is for debugging only. */
	pp->path = path_copy(path);
#else	
	pp->path = NULL;
#endif	
	pp->list = fp->list;
	return(pp);
	}
}  /* get_leaf_node */

/*************
 *
 *    static int all_args_vars(t)
 *
 *    Is t complex with variables as all arguments?
 *
 *************/

static int all_args_vars(Term_ptr t)
{
    if (!COMPLEX(t))
	return(0);
    else {
	int i;
	for (i = 0; i < t->arity; i++)
	    if (!VARIABLE(t->args[i]))
		return(0);
	return(1);
	}
}  /* all_args_vars */

/*************
 *
 *    zap_prop_tree(tree) -- Deallocate an FPA indexing tree.
 *
 *    next_term deallocates the tree as it proceeds, so it is not
 *    necessary to call zap_prop_tree if the most recent call to
 *    next_term returned NULL.
 *
 *************/

static void zap_prop_tree(Fpa_tree_ptr n)
{
    if (n) {
	zap_prop_tree(n->left);
	zap_prop_tree(n->right);
	free_fpa_tree(n);
	}
}  /* zap_prop_tree */

/*************
 *
 *   union_with_commutative_branch()
 *
 *************/

static Fpa_tree_ptr union_with_commutative_branch(Fpa_tree_ptr p, Term_ptr t,
           int u_type, int *path, int j, int bound, Fpa_head_ptr *table)
{
    unsigned char *cpath;
    Fpa_tree_ptr p1, p2, p3, p4;

#if 0
    printf("\nENTER union_with_commutative_branch with "); p_term(t);
    p_prop_tree(p);
#endif

    cpath = (unsigned char *) path;
    cpath[j] = 2;
    p1 = build_tree_local(t->args[0], u_type, path, j+1, bound-1, table);
    if (p1) {
	cpath[j] = 1;
	p2 = build_tree_local(t->args[1], u_type, path, j+1, bound-1, table);
	if (p2) {
	    /* build UNION(p,INTERSECT(p1,p2)). */
	    p3 = get_fpa_tree();
	    p3->type = INTERSECT;
	    p3->left = p1;
	    p3->right = p2;

	    p4 = get_fpa_tree();
	    p4->type = UNION;
	    p4->left = p;
	    p4->right = p3;
	    }
	else {
	    zap_prop_tree(p1);
	    p4 = p;
	    }
	}
    else
	p4 = p;

#if 0
    printf("exit union_with_commutative_branch with\n");
    p_prop_tree(p4);
#endif    
    return(p4);
}  /* union_with_commutative_branch */

/*************
 *
 *    Fpa_tree_ptr build_tree_local(term, unif_type, path, bound, table)
 *
 *    Return an FPA indexing tree--to be used with a sequence
 *    of get_next calls.
 *
 *        term:       An error if term is VARIABLE && unif_type != MORE_GEN
 *                    because everything satisfies that query.
 *        unif_type:  UNIFY, INSTANCE, MORE_GEN
 *        path:       must be 0 on initial call
 *        bound:      indexing bound (must be <= fpa_insert bound)
 *        table:   
 *
 *    Note:  If an appropriate fpa list does not exist, then part of
 *    the tree can sometimes be deleted.
 *
 *************/

Fpa_tree_ptr build_tree_local(Term_ptr t, int u_type, int *path,
			      int j, int bound, Fpa_head_ptr *table)
{
    int i, empty;
    Fpa_tree_ptr p1, p2, p3;
    unsigned char *cpath;

    cpath = (unsigned char *) path;
    
    /* `path' has the path to `t' */

    if (VARIABLE(t)) { /* variable */
	if (u_type != MORE_GEN) {  /* error if not "more general" */
	    abend("build_tree_local, var and not more general.");
	    return(NULL);  /* to quiet lint */
	    }
	else {
	    cpath[j++] = 0;
	    path_mark_end(path, j);
	    p1 = get_leaf_node(path, table);
	    return(p1);
	    }
	}
    else {  /* CONSTANT or COMPLEX */
	cpath[j++] = abs(t->symbol);
	if (CONSTANT(t) || bound == 0 || is_assoc_comm(t->symbol) ||
	    (u_type != MORE_GEN && all_args_vars(t))) {
	    path_mark_end(path, j);
	    p2 = get_leaf_node(path, table);
	    }
	else {
	    empty = 0;
	    p2 = NULL;
	    for (i = 0; i < t->arity && !empty; i++) {
		cpath[j] = i+1;
		/* skip this arg if var and "unify" or "instance" */
		if (!VARIABLE(t->args[i]) || u_type == MORE_GEN) {
		    p3 = build_tree_local(t->args[i],u_type,path,j+1,bound-1,table);
		    if (!p3) {
			if (p2) {
			    zap_prop_tree(p2);
			    p2 = NULL;
			    }
			empty = 1;
		        }
		    else if (!p2)
			p2 = p3;
		    else {
		        p1 = get_fpa_tree();
		        p1->type = INTERSECT;
		        p1->left = p2;
		        p1->right = p3;
		        p2 = p1;
		        }
		    }
		}
	    if (is_commutative(t->symbol))
		p2 = union_with_commutative_branch(p2,t,u_type,path,j,bound,table);
	    }
    
	if (u_type != INSTANCE) {  /* unify or generalization, */
	    cpath[j-1] = 0;
	    path_mark_end(path, j);
	    p3 = get_leaf_node(path, table); /* variable */
	    }
	else
	    p3 = NULL;
    
	if (!p2)
	    return(p3);
	else if (!p3)
	    return(p2);
	else {  /* UNION them together */
	    p1 = get_fpa_tree();
	    p1->type = UNION;
	    p1->left = p2;
	    p1->right = p3;
	    return(p1);
	    }
	}
}  /* build_tree_local */

/*************
 *
 *    Fpa_tree_ptr build_for_all(table)
 *
 *    This is called when someone needs terms that unify with or are
 *    instances of a variable.
 *    Every term in an FPA index should be in a list whose path consists
 *    of one symbol.  Build a tree that UNIONs together all such FPA lists.
 *
 *************/

static Fpa_tree_ptr build_for_all(Fpa_head_ptr *table)
{
    Fpa_head_ptr h;
    Fpa_tree_ptr p1, p2, p3;
    int i;
    unsigned char *cpath;

    p1 = NULL;
    for (i = 0; i < FPA_SIZE; i++) {
	for (h = table[i]; h; h = h->next) {
	    cpath = (unsigned char *) h->path;
	    if (cpath[1] == 0) {  /* if path is for first symbol only */
		/* Actually, if a term has more than 255 arguments, this will */
		/* introduce some duplicates, but that's ok. */
		p2 = get_fpa_tree();
		p2->type = LEAF;
		p2->path = h->path;
		p2->list = h->list;
		if (!p1)
		    p1 = p2;
		else {
		    p3 = get_fpa_tree();
		    p3->type = UNION;
		    p3->left = p1;
		    p3->right = p2;
		    p1 = p3;
		    }
		}
	    }
	}
    return(p1);
}  /* build_for_all */

/*************
 *
 *    Fpa_tree_ptr build_tree(t, u_type, bound, table)
 *
 *************/

static Fpa_tree_ptr build_tree(Term_ptr t, int u_type, int bound, Fpa_head_ptr *table)
{
    int path[MAX_PATH];

    if (VARIABLE(t) && u_type != MORE_GEN)
	return(build_for_all(table));
    else
	return(build_tree_local(t, u_type, path, 0, bound, table));
}  /* build_tree */


/*************
 *
 *    next_term
 *
 *    Get the first or next term that satisfies a unification condition.
 *    (Unification conditions are provided by build_tree.)
 *    `maximum' must be 0 on nonresursive calls.  A return of NULL indicates
 *    that there are none or no more terms that satisfy (and the tree has
 *    been deallocated).  If you want to stop getting terms before a NULL
 *    is returned, then please deallocate the tree with zap_prop_tree(tree).
 *
 *    Warning: a return of NULL means that the tree has been deallocated
 *
 *************/

static Term_ptr next_term(Fpa_tree_ptr n, int max)
{
    Gen_ptr_ptr g;
    Term_ptr t1, t2;
    
    if (!n)
	return(NULL);
    else if (n->type == LEAF) {
	g = n->list;  /* list has decreasing IDs */
	while (g && max != 0 && g->u.t->fpa_id > max)
	    g = g->next;
	if (!g) {
	    zap_prop_tree(n);
	    return(NULL);
	    }
	else {
	    n->list = g->next;
	    return(g->u.t);
	    }
	}
    
    else if (n->type == INTERSECT) {
	t1 = next_term(n->left, max);
	if (t1)
	    t2 = next_term(n->right, t1->fpa_id);
	else
	    t2 = (Term_ptr) &t2;  /* anything but NULL */
	while (t1 != t2 && t1 && t2) {
	    if (t1->fpa_id > t2->fpa_id)
		t1 = next_term(n->left, t2->fpa_id);
	    else
		t2 = next_term(n->right, t1->fpa_id);
	    }
	if (!t1 || !t2) {
	    if (!t1)
	       n->left = NULL;
	    if (!t2)
	       n->right = NULL;
	    zap_prop_tree(n);
	    return(NULL); 
	    }
	else
	    return(t1);
	}
    
    else {  /* UNION node */
	/* first get the left term */
	t1 = n->left_term;
	if (!t1) {
	    /* it must be brought up */
	    if (n->left) {
		t1 = next_term(n->left, max);
		if (!t1)
		    n->left = NULL;
		}
	    }
	else  /* it was saved from a previous call */
	    n->left_term = NULL;
	/* at this point, n->left_term == NULL */
    
	/* now do the same for the right side */
	t2 = n->right_term;
	if (!t2) {
	    if (n->right) {
		t2 = next_term(n->right, max);
		if (!t2)
		    n->right = NULL;
		}
	    }
	else
	    n->right_term = NULL;
    
	/* now decide which of of t1 and t2 to return */
	if (!t1) {
	    if (!t2) {
		zap_prop_tree(n);
		return(NULL);
		}
	    else
		return(t2);
	    }
	else if (!t2)
	    return(t1);
	else if (t1 == t2)
	    return(t1);
	else if (t1->fpa_id > t2->fpa_id) {
	    n->right_term = t2;  /* save t2 for next time */
	    return(t1);
	    }
	else {
	    n->left_term = t1;  /* save t1 for next time */
	    return(t2);
	    }
	}
}  /* next_term */

/*************
 *
 *    print_fpa_index(file_ptr, index)
 *
 *************/

void print_fpa_index(FILE *fp, Fpa_index_ptr p)
{
    int i;
    Fpa_head_ptr f;
    Gen_ptr_ptr g;
    
    fprintf(fp, "\nFpa index %x, depth=%d.\n", (unsigned) p, p->depth);
    for (i=0; i<FPA_SIZE; i++) {
	if (p->table[i]) {
	    fprintf(fp, "bucket %d\n", i);
	    f = p->table[i];
	    while (f) {
		print_path(fp, f->path);
		g = f->list;
		while (g) {
		    fprintf(fp, " %lu:", g->u.t->fpa_id);
		    print_term(fp, g->u.t);
		    g = g->next;
		    }
		fprintf(fp, "\n");
		f = f->next;
		}
	    }
	}
}  /* print_fpa_index */

/*************
 *
 *    p_fpa_index(index)
 *
 *************/

void p_fpa_index(Fpa_index_ptr p)
{
    print_fpa_index(stdout, p);
}  /* p_fpa_index */

/*************
 *
 *    print_prop_tree(file_ptr, tree, depth)
 *
 *        Display an FPA lookup tree that has been returned from
 *    build_tree.  Level should be 0 on initial call.
 *
 *************/

void print_prop_tree(FILE *fp, Fpa_tree_ptr n, int depth)
{
    int i;
    
    if (n) {
	for (i=0; i<depth; i++)
	    fprintf(fp, "  ");
	if (n->type == INTERSECT)
	    fprintf(fp, "AND\n");
	else if (n->type == UNION)
	    fprintf(fp, "OR\n");
	else
	    print_path(fp, n->path);
	print_prop_tree(fp, n->left, depth+1);
	print_prop_tree(fp, n->right, depth+1);
	}
}  /* print_prop_tree */

/*************
 *
 *    p_prop_tree(t)
 *
 *************/

void p_prop_tree(Fpa_tree_ptr n)
{
    print_prop_tree(stdout, n, 0);
    printf("\n");
}  /* p_prop_tree */

/*************
 *
 *    print_path(fp, path) -- print an fpa path to a file
 *
 *************/

void print_path(FILE *fp, int *path)
{
    int i;
    char *sym;
    unsigned char *cpath;

    cpath = (unsigned char *) path;

    /* example [f,2,g,1,f,1,h,1,a] */
    
    fprintf(fp, "[");
    for (i = 0; i%2 == 0 || cpath[i] != 0 ; i++) {
	if (i % 2 == 0) {
	    if (cpath[i] == 0)
		sym = "*";
	    else
		sym = sn_to_str(-cpath[i]);  /* positive in cpath */
	    fprintf(fp, "%s", sym);
	    }
	else
	    fprintf(fp, "%d", cpath[i]);

	if (i%2 == 1 || cpath[i+1] != 0)
	    fprintf(fp, ",");
	else
	    fprintf(fp, "]\n");
	}
}  /* print_path */

/*************
 *
 *    p_path(path) -- print an fpa path 
 *
 *************/

void p_path(int *path)
{
    print_path(stdout, path);
}  /* p_path */

/*************
 *
 *    fpa_retrieve_first
 *
 *************/

Term_ptr fpa_retrieve_first(Term_ptr t, Fpa_index_ptr index, int type,
      Context_ptr subst_query, Context_ptr subst_found, Fpa_pos_ptr *ppos)
{
    Fpa_pos_ptr p;

    p = get_fpa_pos();
    p->query_term = t;
    p->type = type;
    p->subst_query = subst_query;
    p->subst_found = subst_found;
    p->tr = NULL;
    p->tree = build_tree(t, type, index->depth, index->table);
#if 0
    printf("whole tree:\n"); p_prop_tree(p->tree);
#endif    
    *ppos = p;
    return(fpa_retrieve_next(p));
}  /* fpa_retrieve_first */

/*************
 *
 *    fpa_retrieve_next
 *
 *************/

Term_ptr fpa_retrieve_next(Fpa_pos_ptr pos)
{
    Term_ptr tq, tf;
    Context_ptr cq, cf;
    Trail_ptr tr;
    int ok;

    tq = pos->query_term;
    cq = pos->subst_query; cf = pos->subst_found;

    clear_subst_1(pos->tr);
    tf = next_term(pos->tree, 0);
    while (tf && term_disabled(tf))
	tf = next_term(pos->tree, 0);
    ok = 0;
    while (tf && !ok) {
#if 0
	printf("potential mate: "); p_term(tf);
#endif	
	tr = NULL;
	switch (pos->type) {
	  case UNIFY:
	    ok = unify(tq, cq, tf, cf, &tr); break;
	  case MORE_GEN:
	    ok = match(tf, cf, tq, &tr); break;
	  case INSTANCE:
	    ok = match(tq, cq, tf, &tr); break;
	  default:
	    ok = 1; break;
	    }
	if (!ok) {
	    tf = next_term(pos->tree, 0);
	    while (tf && term_disabled(tf))
		tf = next_term(pos->tree, 0);
	    }
	}

    if (ok) {
	pos->tr = tr;
	return(tf);
	}
    else {
	free_fpa_pos(pos);
	return(NULL);
	}
}  /* fpa_retrieve_next */

/*************
 *
 *    fpa_cancel
 *
 *************/

void fpa_cancel(Fpa_pos_ptr pos)
{
    clear_subst_1(pos->tr);
    zap_prop_tree(pos->tree);
    free_fpa_pos(pos);
}  /* fpa_cancel */

#if 0

/*************
 *
 *    fpa_bt_retrieve_first
 *
 *************/

Term_ptr fpa_bt_retrieve_first(Term_ptr t, Fpa_index_ptr index, int type,
       Context_ptr subst_query, Context_ptr subst_found, Fpa_pos_ptr *ppos)
{
    Fpa_pos_ptr p;
    p = get_fpa_pos();
    p->query_term = t;
    p->type = type;
    p->subst_query = subst_query;
    p->subst_found = subst_found;
    p->bt_position = NULL;
    p->tree = build_tree(t, type, index->depth, index->table);
#if 0
    printf("whole tree:\n"); p_prop_tree(p->tree);
#endif    
    *ppos = p;
    return(fpa_bt_retrieve_next(p));
}  /* fpa_bt_retrieve_first */

/*************
 *
 *    fpa_bt_retrieve_next
 *
 *************/

Term_ptr fpa_bt_retrieve_next(Fpa_pos_ptr pos)
{
    Term_ptr tq, tf;
    Context_ptr cq, cf;
    Trail_ptr tr;
    int ok;

    tq = pos->query_term;
    cq = pos->subst_query; cf = pos->subst_found;

    if (pos->bt_position) {
	switch (pos->type) {
	  case UNIFY:
	    pos->bt_position = unify_bt_next(pos->bt_position);
	    break;
	  case MORE_GEN:
	  case INSTANCE:
	    pos->bt_position = match_bt_next(pos->bt_position);
	    break;
	    }
	}
    
    if (!pos->bt_position) {
	tf = next_term(pos->tree, 0);
	while (tf && !pos->bt_position) {
#if 0
	    printf("potential mate: "); p_term(tf);
#endif	
	    switch (pos->type) {
	      case UNIFY:
		pos->bt_position = unify_bt_first(tq, cq, tf, cf);
		break;
	      case MORE_GEN:
		pos->bt_position = match_bt_first(tf, cf, tq, 0);
		break;
	      case INSTANCE:
		pos->bt_position = match_bt_first(tq, cq, tf, 0);
		break;
		}
	    if (!pos->bt_position)
		tf = next_term(pos->tree, 0);
	    else
		pos->found_term = tf;
	    }
	}

    if (pos->bt_position)
	return(pos->found_term);
    else {
	free_fpa_pos(pos);
	return(NULL);
	}
}  /* fpa_bt_retrieve_next */

/*************
 *
 *    fpa_bt_cancel
 *
 *************/

void fpa_bt_cancel(Fpa_pos_ptr pos)
{
    zap_prop_tree(pos->tree);
    switch (pos->type) {
      case UNIFY: unify_bt_cancel(pos->bt_position); break;
      case MORE_GEN:
      case INSTANCE: match_bt_cancel(pos->bt_position); break;
	}
    free_fpa_pos(pos);
}  /* fpa_bt_cancel */

#endif

/*************
 *
 *    free_dangle_list
 *
 *************/

void free_dangle_list(void)
{
    Gen_ptr_ptr p2;

    while (Dangle_list) {
	p2 = Dangle_list; 
	Dangle_list = Dangle_list->next;
	free_gen_ptr(p2->u.p);
	free_gen_ptr(p2);
	}
}  /* free_dangle_list */

/*************
 *
 *    print_fpa_index_summary(file_ptr, index)
 *
 *************/

void print_fpa_index_summary(FILE *fp, Fpa_index_ptr p)
{
    int i, n;
    Fpa_head_ptr f;
    Gen_ptr_ptr g;

    if (!p)
	return;
    
    fprintf(fp, "\nFpa index %x, depth=%d.\n", (unsigned) p, p->depth);

    for (i = 0; i < FPA_SIZE; i++) {
	if (p->table[i]) {
	    fprintf(fp, "bucket %d\n", i);
	    for (f = p->table[i]; f; f = f->next) {
		for (g = f->list, n = 0; g; g = g->next, n++);
		fprintf(fp, " %d: ", n);
		print_path(fp, f->path);
		}
	    }
	}
}  /* print_fpa_index_summary */

