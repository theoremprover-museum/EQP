#if SOLO  /* Can be compiled alone for testing, see main() below. */
#include <stdio.h>
#define MAX_COEF    250
#define MAX_BASIS   100
#define MAX_COMBOS  200
#else
#include "Header.h"
#include "Unify.h"
#include "Ac.h"
#endif

/*************
 *
 *   gcd(x,y) - greatest common divisor
 *
 *************/

static int gcd(int x, int y)
{
    int r;
    if (x < y) {r = x; x = y; y = r;}
    r = x % y;
    while (r != 0) {x = y; y = r; r = x % y;}
    return(y);
}  /* gcd */

/*************
 *
 *   lcm(x,y) - least common multiple
 *
 *************/

static int lcm(int x, int y)
{  
    return((x * y) / gcd(x,y));
}  /* lcm */

/*************
 *
 *    p_ac_basis(basis, num_basis, m, n)
 *    Print the basis of solutions
 *
 *************/

void p_ac_basis(int (*basis)[MAX_COEF], int num_basis, int m, int n)
{
    int i, j;
    printf("\nBasis has %d solutions.\n",num_basis);
    for (i = 0; i < num_basis; i++) {
	for (j = 0; j < m+n; j++) printf("%3d",basis[i][j]);
	printf("\n");
	}
    printf("Basis has %d solutions.\n",num_basis);

}  /* p_ac_basis */

/*************
 *
 *   less_vec(a1, a2, length) - true iff each component of a1 is <=
 *   the corresponding component of a2
 *
 *************/

static int less_vec(int *a1, int *a2, int length)
{
    int i;
    for (i = 0; i < length; i++)
        { if (a1[i] > a2[i]) return(0);}
    return(1);
}  /* less_vec */

/*************
 *
 *    var_check_1(constraints, xy, start, stop)
 *
 *    true iff for i=start,...,stop, constraints[i] implies xy[i] <= 1;
 *    For checking basis solutions and combinations of basis solutions.
 *
 *    Otherwise, AC functor would have to unify with another rigid symbol.
 *
 *************/

static int var_check_1(int *constraints, int *xy, int start, int stop)
{
    int i;
    for (i = start; i <= stop; i++)
        { if (constraints[i] && xy[i] > 1) return(0);}
    return(1);
}  /* var_check_1 */

/*************
 *
 *    var_check_2(constraints, xy, start, stop)
 *
 *    TRUE iff for i,j=start,...,stop, 
 *        
 *        constraints[i] && xy[i]  && constraints[j] && xy[j]
 *          implies  constraints[i] = constraints[j] .
 *
 *    Otherwise, variable would have to unify with 2 different functors.
 *
 *    For checking basis solutions only.
 *
 *************/

static int var_check_2(int *constraints, int *xy, int start, int stop)
{
    int i, j;
    
    for (i = start; i <= stop; i++) {
        if (constraints[i] && xy[i]) {
            for (j = i+1; j <= stop; j++) {
		if (constraints[j] && xy[j]) {
		    if (constraints[j] != constraints[i])
			return(0);
		    }
		}
	    }
	}
    return(1);
}  /* var_check_2 */

/*************
 *
 *    add_solution(xy, length, num_basis, basis)
 *    If no solution in basis is less than xy, then append xy to basis
 *    If there is not enough room, return(0).
 *
 *************/

static int add_solution(int *xy, int length, int *num_basis, int (*basis)[MAX_COEF])
{
    int i;
    for (i = 0; i < *num_basis; i++)
        if (less_vec(basis[i], xy, length)) 
            return(1);
    if (*num_basis >= MAX_BASIS) {
	fprintf(stderr, "add_solution: too many base solutions\n");
	return(0);
	}
    for (i = 0; i < length; i++)
        basis[*num_basis][i] = xy[i];
    (*num_basis)++;
    return(1);
}  /* add_solution */

/*************
 *
 *    a_in_bounds(ab,xy,max_y,d,e,m,n,xypos,max_a,max_b,suma,constraints)
 *    Check if a[0],...,a[xypos] is ok:
 *      1. check Huet's (a) condition;
 *      2. check that xy vector is compatible with and constants/functions;
 *      3. set up the max_y vector;
 *      4. check Huet's (b) condition;
 *
 *************/

static int a_in_bounds(int *ab, int *xy, int *max_y, int (*d)[MAX_COEF], int (*e)[MAX_COEF], int m, int n, int xypos, int max_a, int max_b, int suma, int *constraints)
{
    int i, j, f, bsum;
    if (xy[xypos] > max_b)  /* Huet's (a) condition */
        return(0);
    if (var_check_1(constraints, xy, 0, m-1) == 0 ||
	var_check_2(constraints, xy, 0, m-1) == 0)
        return(0);
    for (j = m; j < m+n; j++) /* build max_y vector */
        {
        max_y[j] = max_a;
        for (i = 0; i < m; i++)
            if (xy[i] >= d[i][j])
                {
                f = e[i][j] - 1;
                if (f < max_y[j])
                    max_y[j] = f;
                }
        }
    bsum = 0;
    for (j = m; j < m+n; j++)
        bsum = bsum + ab[j] * max_y[j];

    if (suma <= bsum) /* Huet's (b) condition */
        return(1);
    else
        return(0);
}  /* a_in_bounds */

/*************
 *
 *    b_in_bounds(xy,max_y,constraints,xypos,suma,sumb,m,n,d,e)
 *    Check if b[0],...,b[xypos] is ok:
 *      1. check Huet's (c) and (d) conditions;
 *      2. check that xy vector is compatible with and constants/functions;
 *
 *************/

static int b_in_bounds(int *xy, int *max_y, int *constraints, int xypos, int suma, int sumb, int m, int n)
{
    /* Huet (d) and (c) conditions */
    if (sumb <= suma && xy[xypos] <= max_y[xypos]) 
	/* check constant/function symbol condition */ 
	if (var_check_1(constraints, xy, 0, m+n-1) &&
	    var_check_2(constraints, xy, 0, m+n-1)) 
	    return(1);
        else
            return(0);
    else
        return(0);
}  /* b_in_bounds */

/*************
 *
 *   int dio(ab,m,n,constraints,gound,basis,num_basis)
 *
 *   return: 
 *       0  - no solution within constraints
 *       1  - ok
 *       -1 - too many base solutions
 *
 *   Generate the basis of solutions to the homogeneous linear 
 *   diophantine equation given by ab,m,n.  Huet's algorithm
 *   (Information Processing Letters 7(3) 1978) is used.
 *
 *   The equation has the form a1x1 + ... + amxm  =  b1y1 + ... + bnyn.
 *
 *   ab - the vector of coefficients.  1..m is a1..am,  m+1..m+n is b1..bn.
 *   m, n - the number of a and b coefficients.
 *   constraints - constant/function restrictions.
 *   basis - vector of minimal solutions.
 *   num_basis - number of minimal solutions.
 *
 *   max_a, max_b - the maximums of the a's and b's.
 *   xy - the vector used to construct solutions.
 *   xypos - the current position in the xy vector.
 *   suma, sumb - accumulate the sums as the soutions are constructed.
 *   max_y - used to hold maximums for the y values.
 *   d, e - d[i,j] = lcm(ai,bj) / ai,  e[i,j] = lcm(ai,bj) / bj,  they
 *       are used to construct solutions and for bounds checking.
 *
 *************/


int dio(int *ab, int m, int n, int *constraints, int (*basis)[MAX_COEF], int *num_basis)
{
    int  xy[MAX_COEF], max_y[MAX_COEF];
    int  d[MAX_COEF][MAX_COEF], e[MAX_COEF][MAX_COEF];
    int xypos, max_a, max_b, suma, sumb;
    int i, j, a, b, t, go_a, go_b, backup;

    if (m == 0 || n == 0) {
	*num_basis = 0;
	return(1);
	}

    max_a = 0;
    max_b = 0;
    for (i = 0; i < m; i++)
        for (j = m; j < m+n; j++)
            {
            a = ab[i];
            b = ab[j];
            t = lcm(a,b);
            d[i][j] = t / a;
            e[i][j] = t / b;
            }

    for (i = 0; i < m; i++)
        if (ab[i] > max_a) max_a = ab[i];

    for (i = m; i < m+n; i++)
        if (ab[i] > max_b) max_b = ab[i];

    for (i = 0; i < m+n; i++) xy[i] = 0;

    xypos = m - 1;
    go_a = 1;
    suma = 0;
    *num_basis = 0;

    while(go_a)
        {
        xy[xypos]++;
        suma = suma + ab[xypos];
        if (a_in_bounds(ab,xy,max_y,d,e,m,n,xypos,max_a,max_b,
			suma,constraints))
            {
            sumb = 0;
            xypos = m + n - 1;
            go_b = 1;
            while (go_b)
                {
                xy[xypos]++;
                sumb = sumb + ab[xypos];
                if (b_in_bounds(xy,max_y,constraints,xypos,
				suma,sumb,m,n))
                    {
                    if (suma == sumb) {
			if (add_solution(xy, m+n, num_basis, basis))
			    backup = 1;
			else
			    return(-1);
			}
		    else
			backup = 0;
                    }
                else
		    backup = 1;
		if (backup)
		    {
                    sumb = sumb - xy[xypos] * ab[xypos];
                    xy[xypos] = 0;
                    xypos--;
                    if (xypos < m)
                        go_b = 0;
		    }
		else
                    xypos = m + n - 1;
                }
            xypos = m - 1;
            }
        else
            {
            suma = suma - xy[xypos] * ab[xypos];
            xy[xypos] = 0;
            xypos--;
            if (xypos < 0)
                go_a = 0;
            }
        }

    /* Add the special solutions Sij */
    for (i = 0; i < m+n; i++)
	xy[i] = 0;
    
    for (i = 0; i < m; i++)
	for (j = m; j < m+n; j++)
	    {
	    xy[i] = d[i][j];
	    xy[j] = e[i][j];
            if (var_check_1(constraints,xy,0,m+n-1) &&
		var_check_2(constraints,xy,0,m+n-1))
                if (!add_solution(xy, m+n, num_basis, basis))
		    return(-1);
	    xy[i] = 0;
	    xy[j] = 0;
	    }
    return(1);

}  /* dio */

/* Following macros are for following next_combo routines. */

#define ADD_TO_SUM(sum,basis,i,len) \
    {int j,*p; for (j=0,p=basis[i];j<len;j++) sum[j] += p[j];}

#define SUBTRACT_FROM_SUM(sum,basis,i,len) \
    {int j,*p; for (j=0,p=basis[i];j<len;j++) sum[j] -= p[j];}

/*************
 *
 *    next_combo_a1
 *
 *    Similar to a, but prevent supersets; not really incremental.
 *    If a is not a superset of b, return -1, else return |a-b|.
 *
 *************/

static int superset_degree(int *a, int *b, int n)
{
    int i, c;
    
    for (i=0, c=0; i<n && c>=0; i++) {
	if (b[i] && !a[i])
	    c = -1;
	else if (a[i] && !b[i])
	    c++;
	}
    return(c);
}  /* superset_degree */

int next_combo_a1(int length, int (*basis)[MAX_COEF], int num_basis, int *constraints, int *combo, int *sum, int start_flag, int (*combos)[MAX_BASIS], int *np, int ss_parm)
{
    int i, go, ok;

    if (start_flag) {
	*np = 0;
	go = next_combo_a(length,basis,num_basis,constraints,combo,sum,1);
        while (go) {
	    for (i=0, ok = 1; i < *np && ok; i++)
		/* 0 means basic superset test */
		/* n means allow supersets that have n more elements. */
		if (superset_degree(combo, combos[i], num_basis) > ss_parm)
		    ok = 0;

	    if (ok) {
		if (*np == MAX_COMBOS) {
		    printf("next_combo_a1: MAX_COMBOS.\n");
		    go = 0;
		    }
		else {
		    for (i=0; i<num_basis; i++)
			combos[*np][i] = combo[i];
		    (*np)++;
		    }
		}

	    if (go)
		go = next_combo_a(length,basis,num_basis,constraints,combo,sum,0);
	    }
	}
    
    if (*np > 0) {
	(*np)--;
	for (i=0; i<num_basis; i++)
	    combo[i] = combos[*np][i];
	return(1);
	}
    else
	return(0);

}  /* next_combo_a1 */

/*************
 *
 *    next_combo_a
 *
 *    Find the first or next appropriate subset of the basis.
 *    combo is the current subset, and should be 0,0,...0 for first call.
 *    sum is the solution corresponding to combo, and shoud be
 *    0,...,0 for first call.
 *
 *************/

int next_combo_a(int length, int (*basis)[MAX_COEF], int num_basis, int *constraints, int *combo, int *sum, int start_flag)
{
    int go, backup, pos, i, success;

    if (start_flag) {
	for (i = 0; i < length; i++)
	    sum[i] = 0;
	for (i = 0; i < num_basis; i++)
	    combo[i] = 0;
	}

    success = 0;
    pos = num_basis-1;
    go = (pos >= 0);
    while (go && !success) {
	backup = 1;
	if (!combo[pos]) {
	    combo[pos] = 1;  /* All following positions 0. */
	    ADD_TO_SUM(sum, basis, pos, length);
	    if (var_check_1(constraints, sum, 0, length-1)) {
		/* OK if no component is 0. */
		success = 1;
		for (i = 0; i < length && success; i++)
		    if (sum[i] == 0)
			success = 0;
		backup = 0;
		}
	    }
	if (backup) {
	    combo[pos] = 0;
	    SUBTRACT_FROM_SUM(sum, basis, pos, length);
	    pos--;
	    go = (pos >= 0);
	    }
	else
	    pos = num_basis-1;
	}
    return(success);

}  /* next_combo_a */

/*************
 *
 *    next_combo_b
 *
 *    Find the first or next appropriate subset of the basis.
 *    combo is the current subset, and
 *    sum is the solution corresponding to combo.
 *
 *************/

 int next_combo_b(int length, int (*basis)[MAX_COEF], int num_basis, int *constraints, int *combo, int *sum, int start_flag)
{
    int go, pos, i, success;

    if (start_flag) {
	for (i = 0; i < length; i++)
	    sum[i] = 0;
	for (i = 0; i < num_basis; i++)
	    combo[i] = 0;
	pos = -1;
	}
    else {
	pos = num_basis-1;
	while (pos >= 0 && !combo[pos])
	    pos--;
	combo[pos] = 0;
	SUBTRACT_FROM_SUM(sum, basis, pos, length);
	}

    success = 0; go = 1;

    while (go && !success) {
	if (pos == num_basis-1) {
	    success = 1;
	    for (i = 0; i < length && success; i++)
		if (sum[i] == 0)
		    success = 0;

	    if (!success) {
		while (pos >= 0 && !combo[pos])
		    pos--;
		if (pos < 0)
		    go = 0;
		else {
		    combo[pos] = 0;
		    SUBTRACT_FROM_SUM(sum, basis, pos, length);
		    }
		}
	    }
	else {
	    pos++;
	    combo[pos] = 1;
	    ADD_TO_SUM(sum, basis, pos, length);
	    if (!var_check_1(constraints, sum, 0, length-1)) {
		combo[pos] = 0;
		SUBTRACT_FROM_SUM(sum, basis, pos, length);
		}
	    }
	}
    return(success);
}  /* next_combo_b */

/*************
 *
 *    next_combo_c  -- Hullot's algorithm
 *
 *    Find the first or next appropriate subset of the basis.
 *
 *************/

#define DOWN    1
#define OVER    2
#define BACKUP  3
#define SUCCESS 4
#define FAILURE 5

 int next_combo_c(int length, int (*basis)[MAX_COEF], int n, int *constraints, int *combo, int *sum, int start_flag)
{
    int pos, i, status, ok;

    if (start_flag) {
	for (i = 0; i < length; i++)
	    sum[i] = 0;
	/* set combo[] to root pattern */
	for (i = 0; i < n; i++) {
	    combo[i] = 1;
	    ADD_TO_SUM(sum, basis, i, length);
	    }
	pos = -1;  /* pos, which is index into combo[], also = level-1 */
	/* Fail if a column of basis is all 0 (if not big enough). */
	for (i = 0, ok = 1; i < length && ok; i++)
	    if (sum[i] == 0)
		ok = 0;
	status = (ok ? DOWN : FAILURE);
	}
    else {
	/* use combo[] from previous call */
	pos = n-1;  /* leaf */
	status = BACKUP;
	}

    while (status != SUCCESS && status != FAILURE) {
	if (status == DOWN) {
	    /* go to left child */
	    if (pos == -1 || combo[pos] == 0) {
		/* parent is a left child */
		pos++;
		combo[pos] = 0;
		SUBTRACT_FROM_SUM(sum, basis, pos, length);
		}
	    else {
		pos++;
		for (i = pos+1; i < n; i++) {
		    combo[i] = 1;
		    ADD_TO_SUM(sum, basis, i, length);
		    }
		}
	    /* if big enough */
	    for (i = 0, ok = 1; i < length && ok; i++)
		if (sum[i] == 0)
		    ok = 0;
	    if (ok)
		status = (pos == n-1) ? SUCCESS : DOWN;
	    else
		status = OVER;
	    }
	else if (status == OVER) {
	    /* go to (right) sibling */
	    combo[pos] = 1;
	    ADD_TO_SUM(sum, basis, pos, length);
	    for (i = pos+1; i < n; i++) {
		if (combo[i]) {
		    combo[i] = 0;
		    SUBTRACT_FROM_SUM(sum, basis, i, length);
		    }
		}
	    /* if small enough */
	    if (var_check_1(constraints, sum, 0, length-1))
		status = (pos == n-1) ? SUCCESS : DOWN;
	    else
		status = BACKUP;
	    }
	else if (status == BACKUP) {
	    /* go to nearest ancestor that has a right sibling */
	    while (pos >= 0 && combo[pos])
		pos--;
	    status = (pos < 0 ? FAILURE : OVER);
	    }
	}
    return(status == SUCCESS ? 1 : 0);
    
    }  /* next_combo_c */

/*************
 *
 *    all_combos
 *
 *    For debugging only.  Find all the appropriate subsets and
 *    print a unifier for each.
 *
 *************/

 int all_combos(int m, int n, int (*basis)[MAX_COEF], int num_basis, int *constraints, int (*proc) (/* ??? */))
{
    int combo[MAX_BASIS], sum[MAX_COEF];
    int count, i, j, k, ok;
    
    count = 0;
    ok = (*proc)(m+n, basis, num_basis, constraints, combo, sum, 1);
    while (ok) {
	count++;
#if 1
	for (j = 0; j < num_basis; j++)
	    printf("%d ", combo[j]);
	printf("--------------\n");
#if 0
	for (i = 0; i < m+n; i++) {
	    if (constraints[i])
#if SOLO		
		printf(" %c = ", constraints[i]+'A'-1);
#else
	    printf(" %s = ", sn_to_str(constraints[i]));
#endif	    
	    else
		printf("x%d -> ", i);
	    for (j = 0; j < num_basis; j++)
		if (combo[j])
		    for (k = 0; k < basis[j][i]; k++)
			printf("z%d ", j);
	    printf("\n");
	    }
#endif
#endif
	ok = (*proc)(m+n, basis, num_basis, constraints, combo, sum, 0);
	}
    return(count);
    }  /* all_combos */

#if 0

/*************
 *
 *    all_combos_build
 *
 *    For debugging only.  Find the appropriate subset, and build an
 *    Otter unifier for each.
 *
 *************/

 int all_combos_build(int m, int n, int (*basis)[MAX_COEF], int num_basis, int *constraints, int (*proc) (/* ??? */))
{
    int combo[MAX_BASIS], sum[MAX_COEF];
    struct term *basis_terms[MAX_BASIS][MAX_COEF], *comb_terms[MAX_COEF];
    struct term *t2, *t3, *t4;
    
    int count, i, j, sn, ok;
    
    sn = str_to_sn("f", 2);
    
    set_up_basis_terms(sn, basis, num_basis, m+n, basis_terms);
    
    for (i = 0; i < num_basis; i++) {
	for (j = 0; j < m+n; j++) {
	    if (!basis_terms[i][j])
		printf("--");
	    else
		p_term(basis_terms[i][j]->args[0]);
	    printf("  ");
	    }
	printf("\n");
	}
    fflush(stdout);
    
    for (i = 0; i < m+n; i++)
	sum[i] = 0;
    for (i = 0; i < num_basis; i++)
	combo[i] = 0;
    
    count = 0;
    ok = (*proc)(m+n, basis, num_basis, constraints, combo, sum, 1);
    while (ok) {
	count++;
#if 0
	for (j = 0; j < num_basis; j++)
	    printf("%d ", combo[j]);
	printf("--------------\n");
#endif	
	for (i = 0; i < m+n; i++) {
	    
	    /* build term for comb_terms array */
	    
	    t4 = NULL;
	    for (j = 0; j < num_basis; j++) {
		/* Loop through rows, building t4. */
		t3 = basis_terms[j][i];
		if (combo[j] && t3) {
		    if (!t4)
			t4 = t3->args[0];
		    else {
			t3->args[1] = t4;
			t4 = t3;
			}
		    }
		}
	    
	    comb_terms[i] = t4;
#if 0
	    if (constraints[i])
	        printf(" %s = ", sn_to_str(constraints[i]));
	    else
		printf("x%d -> ", i);
	    p_term(comb_terms[i]); printf("\n");
#endif
	    
	    comb_terms[i] = NULL;
	    }
	
	ok = (*proc)(m+n, basis, num_basis, constraints, combo, sum,0);
	}
    
    for (i = 0; i < num_basis; i++)
	for (j = 0; j < m+n; j++)
	    if (basis_terms[i][j]) {
		t2 = basis_terms[i][j];
		zap_term(t2->args[0]);
		free_term(t2);
		}
    
    return(count);
    }  /* all_combos_build */

#endif

#if SOLO

/*************
 *
 *   main
 *
 *************/

 main()
{
    int m, n, num_basis;
    int ab[MAX_COEF], constraints[MAX_COEF], basis[MAX_BASIS][MAX_COEF];
    int i;
    unsigned long t0, t1, t2, t3;
    
    printf("Enter m and n: ");
    scanf("%d %d", &m, &n);
    
    if (m+n > MAX_COEF) {
	printf("maximum m+n is %d\n", MAX_COEF);
	exit(2);
	}
    
    printf("Enter A and B coefficients:      ");
    for (i=0; i<m+n; i++) scanf("%d", &ab[i]);
    
    printf("Enter corresponding constraints: ");
    for (i=0; i<m+n; i++) scanf("%d", &constraints[i]);
    
    printf("\na = "); for (i=0; i<m; i++) printf("%3d ", ab[i]);
    printf("  b = "); for (i=m; i<m+n; i++) printf("%3d ", ab[i]);
    printf("\n");
    
    printf("    "); for (i=0; i<m; i++) printf("%3d ", constraints[i]);
    printf("      "); for (i=m; i<m+n; i++) printf("%3d ", constraints[i]);
    printf("\n");
    
    num_basis = 0;
    
    t0 = clock();
    dio(ab, m, n, constraints, basis, &num_basis);
    t1 = clock();
    p_ac_basis(basis, num_basis, m, n);
    printf("dio time = %.2f\n", (t1-t0)/1000000.);
    
    if (num_basis > 0) {

#if 1
	t2 = clock();
	i = all_combos(m, n, basis, num_basis, constraints, next_combo_a);
	t3 = clock();
	printf("\nNumber of unifiers is %d\n", i);
	printf("next_combo_a time = %.2f\n", (t3-t2)/1000000.);

	t2 = clock();
	t2 = clock();
	i = all_combos(m, n, basis, num_basis, constraints, next_combo_b);
	t3 = clock();
	printf("\nNumber of unifiers is %d\n", i);
	printf("next_combo_b time = %.2f\n", (t3-t2)/1000000.);
#endif	
	i = all_combos(m, n, basis, num_basis, constraints, next_combo_c);
	t3 = clock();
	printf("\nNumber of unifiers is %d\n", i);
	printf("next_combo_c time = %.2f\n", (t3-t2)/1000000.);
	
	}

}  /* main */

#endif
