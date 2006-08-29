(* Obtain the inverse of an symmetric positive definite matrix
 *			by the modified Cholesky method
 *			    with regularization
 *
 * Synopsis
 *	condn = minvholm(a,n)
 *
 *	float * a;		N-by-N square matrix ptr with elements
 *				arranged in the column-wise manner
 *				Input:  source matrix
 *				Output: inverse matrix
 *	const int n;		Matrix dimension
 *	double condn;		Condition number of a - ratio of
 *				Dmax / Dmin (see below)
 *
 * The program makes use of Cholesky decomposition of a symmetric positive
 * definite matrix A
 *   (1)   A = L D L'
 * D being a diagonal matrix and L is a low triangle matrix, i.e.
 * Lii = 1 and Lij = 0 for i<j.	
 *
 * When matrix A is ill-conditioned, its diagonal elements are increased to	
 * fulfil the conditions
 *   (2)  Dkk > delta,   Lij^2 * Djj <= betta2
 * Expanding (1) in the explicit form yields
 *   (3)  Aij = Lij * Djj + SUM[ Lik * Dkk * Ljk ], k = 1..j-1, j<i
 *        Aii =       Dii + SUM[ Lik * Dkk * Lik ], k = 1..i-1
 *
 * Introducing Cij = Lij * Djj, one obtains the following formulas to
 * compute Lij and Djj
 *   (3a) Dii = Aii - SUM[ Lik^2 * Dkk ], k=1..i-1
 *        Cij = Aij - SUM[ Cik * Ljk ], j<i, k=1..j-1
 *        Lij = Cij/Djj, j<i
 *
 * Inverse of A (1) is the matrix
 *   (4)  Ainv = (L D L')^-1 = B' D^-1 B
 * where D^-1 = diag(1/D11, ... 1/Dnn) and B is L^-1. It is low triangle
 * matrix as well and determined as a solution of the set of linear equations 
 * L B = E (E being unit matrix) with back substitution method
 *   (5)  Bii = 1
 *        Bij = -Lij - SUM[ Lik * Bkj ], j<i, k=j+1..i-1
 *
 * Formula to compute A^-1 is easy to obtain by expanding (4)
 *   (6)  AINVij = SUM[ Bki * Bkj / Dkk ] + Bij / Dii, i=1..n, k=i+1..n
 *
 * Matrices L, B, D, AINV are all arranged at the place of the source
 * matrix A to save memory. Matrix L occupies the low triangle of A. As
 * all diagonal elements of L are equal to one, they are stored to nothing
 * but assumed in calculations. Diagonal elements of D are stored in the
 * diagonals of A; other D elements are zeros. B matrix elements replaces
 * elements of L matrix while they are computed.
 *
 *)


(* Based on my old code, Sep 1990. *)


(*

				/* Local data				*/
static float * A;			/* Work area pointer		*/
static int N;                       	/* Matrix dimension		*/

#define Col_ptr(i)  (A + N*i)		/* Ptr to the i-th column of A	*/
#define Row_ptr(i)  (A + i)		/* Ptr to the i-th row of A	*/
#define Diag_ptr(i) (A + (N+1)*i)	/* Ptr to the i-th diag of A	*/

#define Next_col_ptr(p)	 (p+=N)		/* Shift the col ptr to the next col */
#define Next_row_ptr(p)	 (p++)		/* Shift the row ptr to the next row */
#define Next_diag_ptr(p) (p+=N+1)	/* Shift the diag ptr to the next diag*/

#define D(i)	    A[(N+1)*i]		/* i-th diagonal element	*/  
#define L(i,k)	    A[i+N*k]
#define Ainv(i,k)   A[i+N*k]

	
				/* Obtain a maximum of two real numbers	*/
static double fmax(double a,double b)
{
  if( a > b )
    return a;
  else
    return b;
}

				/* Obtain a minimum of two real numbers	*/
static double fmin(double a,double b)
{
  if( a > b )
    return b;
  else
    return a;
}

				/* Obtain the betta2 threshold, eq. (3)	*/
static double obtain_betta2(A,n)
const float * A;
const int n;
{
  double max_diagonal = 0;		/* max |Aii| is placed here	*/
  double max_notdiagonal = 0;		/* max |Aij| is placed here,j<i	*/
  register float * ap = A;
  register int i,j;

  for(i=0; i<n; i++,ap += n)
  {
    register float *aip = ap;		/* Column i element pointer	*/
    for(j=0; j<i; j++)
      max_notdiagonal = fmax( fabs(*aip++), max_notdiagonal );
    max_diagonal = fmax( fabs(*aip), max_diagonal );
  }

  return fmax( fmax(max_diagonal, EPSILON), max_notdiagonal/sqrt(n*n-1.0) );
}


/*
 *-----------------------------------------------------------------------
 *			Perform the LDL' decomposition
 *			 with regularized formulas (3a)
 */

				/* Return Dmax/Dmin			*/
static double compute_L_D_matrices()
{
  const double betta2 = obtain_betta2(A,N);	/* Regularization	*/
  const double delta  = 1e-5;            	/* criteria		*/
						/* delta accounts for FLOAT el*/
  double Dmax = 0,
	 Dmin = HUGE_VAL;			/* D matrix extremals	*/

  register int i,j;
  register float * lj;	 			/* j-th column of L	*/
  register float * Djj; 			/* j-th diagonal el of D*/

  for(j=0,lj=A,Djj=A; j<N; j++,Next_col_ptr(lj),Next_diag_ptr(Djj))
  {
    double theta = 0;				/* max |Cij|		*/

    for(i=j+1; i<N; i++)			/* Compute the i-th row	*/
    {                                           /* of C in j-th col of L*/
      register float * lik = Row_ptr(i);
      register float * ljk = Row_ptr(j);
      register double cij = lj[i];		/* Initial value - Aij	*/
      register int k;	
      for(k=0; k<j; k++,Next_col_ptr(lik),Next_col_ptr(ljk))
        cij -= *ljk * *lik * D(k);
      theta = fmax( fabs(cij), theta );
      lj[i] = cij;
    }

    *Djj = fmax( fmax( fabs(*Djj), delta), powi(theta,2)/betta2 );
    Dmax = fmax(*Djj,Dmax);
    Dmin = fmin(*Djj,Dmin);
    for(i=j+1; i<N; i++)
    {
      lj[i] /= *Djj;
      D(i) -= powi(lj[i],2) * *Djj;	/* Account for the term [Lik^2 Dkk], */
    }                                	/* with k=j in sum for Dii, (3a.2)   */	
  }
  return Dmax/Dmin;
}


/*
 *-----------------------------------------------------------------------
 *  		Compute the inverse of L, matrix B, with eq.(5)
 * Diagonal elements of B are all ones by definition and aren't computed
 * Bij elements are calculated in the column-wise manner, when Bij element
 * being processed (i>j), the upper elements of the j-th column of A
 * (i.e. Akj, 0<k<i) just contain Bkj, while the rest lower elements
 * (and all the elements to the right, i.e. A.k with k>j) are yet elements of
 * matrix L
 *
 */

static void compute_B_matrix()
{
  register float *bj;	
  register int i,j;

				/* Compute B in col-by-col manner	*/
  for(j=0,bj=Col_ptr(0); j<N; j++,Next_col_ptr(bj))
  {
    for(i=j+1; i<N; i++)
    {
      register double bij = -bj[i];		/* Initial value -Lij	*/
      register int k;				/* and Bik for 0<=k<=j	*/	
      for(k=j+1; k<i; k++)
        bij -= L(i,k) * bj[k];
      bj[i] = bij;
    }
  }
}


/*
 *-----------------------------------------------------------------------
 * 		Compute elements of inverse matrix with eqs.(6)
 * It should be kept in mind that the inverse matrix is symmetric and
 * the lower triangle of A contains B and diagonal of A contain D. They
 * are replaced by the inverse matrix elements during computation.
 */

static void compute_A_inverse()
{
  register float *bi;	
  register int i,j;

  for(i=0,bi=Col_ptr(0); i<N; i++,Next_col_ptr(bi))
  {
    register float *bj;
    register double di = D(i); 
    for(j=0,bj=Col_ptr(0); j<i+1; j++,Next_col_ptr(bj))
    {
      register double aij = (i==j ? 1.0 : bj[i]) / di;
      register int k;			       
      for(k=i+1; k<N; k++)
        aij += bi[k] * bj[k] / D(k);
      Ainv(i,j) = aij;
      Ainv(j,i) = aij;
    }
  }
}



/*
 *-----------------------------------------------------------------------
 *				Root module
 */

double minvhol(a,n)
float * a;				/* Ptr to the A[1,1]. Other elements
					/* follow it in the column-wise manner*/
const int n;				/* Matrix dimension		*/
{
  double condn;
  A = a;
  N = n;
  assure( A[1] == A[N],"Matrix doesn't seem to be a symmetric one" );
  condn = compute_L_D_matrices();
  compute_B_matrix();
  compute_A_inverse();
  return condn;
}

*)
