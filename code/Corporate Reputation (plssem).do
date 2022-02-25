/* Example code for the plssem package that accompanies:
   
   Venturini, S. and Mehmetoglu, M. (2022) Software Packages for Partial Least
   Squares Structural Equation Modeling: An Updated Review, in
   Latan, H. and Noonan, R., Partial Least Squares Path Modeling, 2nd edition,
   Springer, 2022
 */

import excel "Corporate Reputation Data.xlsx", sheet("Sheet1") firstrow clear
mvdecode _all, mv(-99)

plssem (LIKE > like_?) (COMP > comp_?) (CUSA > cusa) (CUSL > cusl_?) ///
			 (CSOR < csor_?) (ATTR < attr_?) (PERF < perf_?) (QUAL < qual_?), ///
			 structural(CUSA COMP LIKE, CUSL COMP LIKE CUSA, ///
						LIKE QUAL CSOR PERF ATTR, COMP QUAL CSOR PERF ATTR) ///
			 wscheme(factorial) digits(4) //boot(500)

plssemplot, loadings
plssemplot, cross
plssemplot, innermodel
plssemplot, stats(LIKE)

estat total, plot

plssemplot, outerweights
