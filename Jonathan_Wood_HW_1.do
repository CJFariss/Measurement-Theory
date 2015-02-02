import delimited "J:\Measurement Theory\Homework\HW 1\_data_problem_Set_01.csv"
sum unit x1 x2 x3 y   /* Get summary statistics */
cor x1 x2 x3 y   /* Get correlation values for provided data */
glm y x1 x2 x3, family(gaussian) link(identity)   /* Linear Model */

/* Generate new variables: Natural Log of each variable */
g lnX1=ln(x1)   
g lnX2=ln(x2)   
g lnX3=ln(x3)   
g lnY=ln(y)   

/* Assess scatter plots of variables */
twoway (scatter lnY lnX1)   
twoway (scatter lnY lnX2)  
twoway (scatter lnY lnX3)   
twoway (scatter lnY x1)  
twoway (scatter lnY x2)   
twoway (scatter lnY x3)   
twoway (scatter y x1)   
twoway (scatter y x2)  
twoway (scatter y x3)
   
/* Get correlation values for logged variables */  
cor lnY lnX1 lnX2 lnX3    

/* Trying models with different combinations of variables/functional form */  
reg lnY lnX2 lnX3   
reg lnY x1 lnX2 lnX3
reg lnY x1 lnX2 x3
reg lnY x1 x2 lnX3
reg lnY lnX1 lnX2 lnX3  /* this is the final model */

/* The final model is kshown below with an R-Sqaured value of 1.0000.  
This indicates to me that the data were generated with the 
natural log relationships used for the model.  
Thus, the data fit the following:  Y=exp(6.887)x1*x3/(x2^2)  */


/* Final Model:
      Source |       SS       df       MS              Number of obs =      25
-------------+------------------------------           F(  3,    21) =       .
       Model |  41.5546147     3  13.8515382           Prob > F      =  0.0000
    Residual |  .000287725    21  .000013701           R-squared     =  1.0000
-------------+------------------------------           Adj R-squared =  1.0000
       Total |  41.5549024    24  1.73145427           Root MSE      =   .0037

------------------------------------------------------------------------------
         lnY |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
        lnX1 |   1.001242   .0008207  1220.03   0.000     .9995357    1.002949
        lnX2 |  -2.003145   .0014902 -1344.19   0.000    -2.006244   -2.000046
        lnX3 |   1.001948   .0010582   946.82   0.000     .9997474    1.004149
       _cons |   6.887274   .0064361  1070.10   0.000     6.873889    6.900658
------------------------------------------------------------------------------ */

