/* Example code for the plssem package that accompanies:
   
   Venturini, S., Mehmetoglu, M. and Latan, H. (2023) Software Packages for
   Partial Least Squares Structural Equation Modeling: An Updated Review, in
   Latan, H. and Noonan, R., Partial Least Squares Path Modeling, 2nd edition,
   Springer, 2023
*/

import excel "/Users/Sergio/Documents/Dati_VENTURINI/2_Research/1_Methods/PLS-SEM_software/data/Corporate Reputation Data.xlsx", firstrow clear
mvdecode _all, mv(-99)

plssem (LIKE > like_?) (COMP > comp_?) (CUSA > cusa) (CUSL > cusl_?) ///
       (CSOR < csor_?) (ATTR < attr_?) (PERF < perf_?) (QUAL < qual_?), ///
       structural(CUSA COMP LIKE, CUSL COMP LIKE CUSA, ///
                  LIKE QUAL CSOR PERF ATTR, COMP QUAL CSOR PERF ATTR) ///
       wscheme(factorial) digits(4) boot(500) seed(123)

estat htmt

plssemplot, loadings
plssemplot, cross
plssemplot, innermodel
plssemplot, stats(LIKE)

estat total, digits(4) plot

plssemplot, outerweights

* Multigroup analysis (not reported in the chapter)
plssem (LIKE > like_?) (COMP > comp_?) (CUSA > cusa) (CUSL > cusl_?) ///
       (CSOR < csor_?) (ATTR < attr_?) (PERF < perf_?) (QUAL < qual_?), ///
       structural(CUSA COMP LIKE, CUSL COMP LIKE CUSA, ///
                  LIKE QUAL CSOR PERF ATTR, COMP QUAL CSOR PERF ATTR) ///
       wscheme(factorial) digits(4) ///
	   group(education,
	         method(bootstrap) reps(500) unequal groupseed(123) plot)

* Unobserved heterogeneity analysis (not reported in the chapter)
plssem (LIKE > like_?) (COMP > comp_?) (CUSA > cusa) (CUSL > cusl_?) ///
       (CSOR > csor_?) (ATTR > attr_?) (PERF > perf_?) (QUAL > qual_?), ///
       structural(CUSA COMP LIKE, CUSL COMP LIKE CUSA, ///
				  LIKE QUAL CSOR PERF ATTR, COMP QUAL CSOR PERF ATTR) ///
       wscheme(factorial) digits(4)

estat unobshet, method(rebus) numclass(5) test plot reps(500)
estat unobshet, method(fimix) stop(1e-2) restart(3) groups(1/5) seed(101)
