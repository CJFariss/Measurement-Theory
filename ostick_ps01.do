//Benjamin Ostick
//PLSC 597B, Fariss
//Version 12
//Problem Set 1

summarize

//With a difference in means of approximately 1 (53.28 v. 54.2), standard deviations
//just over 2 apart (25.03 v. 27.26), and similar maximums (99 v. 98), the variables
//x2 and x3 appear quite similar in terms of their distributions. While x1 is slightly
//below these two, it is nonetheless on a similar scale (in terms of minimum, maximum,
//and standard deviation. However, the variable y is vastly different from the rest.
//This is due to its range (23-17728) and its subsequent effect on the mean (1526.8)
//and standard deviation (3460.68).

generate x12 = x1*x2
generate x13 = x1*x3
generate x23 = x2*x3

//Attempting to multiply the three x variables to approximate the vastly different
//distribution of y yielded higher means and both lower standard deviations and maximums
//for all three combinations.

correlate x1 x2
//-0.0402
correlate x1 x3
//-0.2116
correlate x1 y
//0.0483
correlate x2 x3
//0.3538
correlate x2 y
//-0.04359
correlate x3 y
//0.0906

//As evidenced by the descriptive statistics above and borne out in terms of
//correlation, the variables x2 and x3 display the highest correlation out of all the
//pairwise combinations.

regress x1 x2
//Negative coefficient, insignificant
regress x1 x3
//Negative coefficient, insignificant
regress x1 y
//Positive coefficient (minor), insignificant
regress x2 x3
//Positive coefficient, significant at p<0.10
regress x2 y
//Negative coefficient, significant at p<0.05
regress x3 y
//Positive coefficient (minor), insignificant
regress y x1 x2 x3
//x2 = Negative coefficient, significant at p<0.05
regress y x1 x3
//None significant
regress y x2 x3
//x2 = Negative coefficient, significant at p<0.05

//From the above regression combinations, we see that only the variable x2 shows any
//statistically meaningful relationship with the others. Although this does not
//resolve the question of how these variables are related, it does indicate that
//x2 is more greatly linked with y than the other two x variables. Overall, these
//simple explorations of the variables above details some meager information
//concerning their composition and interrelation.
