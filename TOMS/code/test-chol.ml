(*
 *-----------------------------------------------------------------------
 *			Verify MINVHOL routine
 *
 * The following test example may be used to validate MINVHOL routine
 * For Hilbert matrix of the 3 order,
 *	        1	1/2	1/3
 *	H =	1/2	1/3	1/4
 *		1/3	1/4	1/5
 *
 * Cholesky decomposition H = L D L' has the form
 *		1	0	0
 *	L = 	1/2	1	0		D = diag(1	1/12	1/180)
 *		1/3	1	1
 *
 * Matrix B = inv(L) then is
 *		1	0	0
 *	B =    -1/2	1	0
 *		1/6    -1	1
 *
 * Finally, the inverse H matrix is B' D^-1 B, or
 *			9      -36	30
 *	inv(H) =       -36	192    -180
 *			30     -180	180
 *
 *		
 *)

(* In my 1990 code, I computed inv(H)*H and inv(H+E)*(H+E)
   for the matrices of teh order 10. Here are the results.


		Verify MINVHOL routine for matrix inverse


Try to test Hilbert+Unit 10 x 10 square matrix


Condition number obtained 1.93966


Compare two [1:100] vectors - Ainv * A and Unit matrix

Maximal discrepancy    		1.192093e-07
   reached at point no.		56
 Vector 1 at this point    		1
 Vector 2 at this point    		1
 Absolute error v2[i]-v1[i]		1.192093e-07
 Relative error				1.192093e-07

||v1||   			3.16228
||v2||   			3.16228
||v1-v2||				1.521751e-07
||v1-v2||/sqrt(||v1|| ||v2||)		4.812200e-08

Try to test Hilbert 10 x 10 square matrix


Condition number obtained 100000


Compare two [1:100] vectors - Ainv * A and Unit matrix

Maximal discrepancy    		2.27945
   reached at point no.		95
 Vector 1 at this point    		-2.27945
 Vector 2 at this point    		0
 Absolute error v2[i]-v1[i]		2.27945
 Relative error				2

||v1||   			5.60257
||v2||   			3.16228
||v1-v2||				5.28886
||v1-v2||/sqrt(||v1|| ||v2||)		1.25652

*)

