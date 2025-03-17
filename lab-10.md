Lab 10 - Grading the professor, Pt. 2
================
Yuxin Xie
3.9.2025

## Load packages and data

``` r
library(tidyverse) 
library(tidymodels)
library(openintro)

data(evals)
summary (evals)
```

    ##    course_id        prof_id          score                 rank    
    ##  Min.   :  1.0   Min.   : 1.00   Min.   :2.300   teaching    :102  
    ##  1st Qu.:116.5   1st Qu.:20.00   1st Qu.:3.800   tenure track:108  
    ##  Median :232.0   Median :43.00   Median :4.300   tenured     :253  
    ##  Mean   :232.0   Mean   :45.15   Mean   :4.175                     
    ##  3rd Qu.:347.5   3rd Qu.:70.50   3rd Qu.:4.600                     
    ##  Max.   :463.0   Max.   :94.00   Max.   :5.000                     
    ##         ethnicity      gender           language        age       
    ##  minority    : 64   female:195   english    :435   Min.   :29.00  
    ##  not minority:399   male  :268   non-english: 28   1st Qu.:42.00  
    ##                                                    Median :48.00  
    ##                                                    Mean   :48.37  
    ##                                                    3rd Qu.:57.00  
    ##                                                    Max.   :73.00  
    ##  cls_perc_eval     cls_did_eval     cls_students    cls_level      cls_profs  
    ##  Min.   : 10.42   Min.   :  5.00   Min.   :  8.00   lower:157   multiple:306  
    ##  1st Qu.: 62.70   1st Qu.: 15.00   1st Qu.: 19.00   upper:306   single  :157  
    ##  Median : 76.92   Median : 23.00   Median : 29.00                             
    ##  Mean   : 74.43   Mean   : 36.62   Mean   : 55.18                             
    ##  3rd Qu.: 87.25   3rd Qu.: 40.00   3rd Qu.: 60.00                             
    ##  Max.   :100.00   Max.   :380.00   Max.   :581.00                             
    ##        cls_credits   bty_f1lower     bty_f1upper     bty_f2upper    
    ##  multi credit:436   Min.   :1.000   Min.   :1.000   Min.   : 1.000  
    ##  one credit  : 27   1st Qu.:2.000   1st Qu.:4.000   1st Qu.: 4.000  
    ##                     Median :4.000   Median :5.000   Median : 5.000  
    ##                     Mean   :3.963   Mean   :5.019   Mean   : 5.214  
    ##                     3rd Qu.:5.000   3rd Qu.:7.000   3rd Qu.: 6.000  
    ##                     Max.   :8.000   Max.   :9.000   Max.   :10.000  
    ##   bty_m1lower     bty_m1upper     bty_m2upper       bty_avg     
    ##  Min.   :1.000   Min.   :1.000   Min.   :1.000   Min.   :1.667  
    ##  1st Qu.:2.000   1st Qu.:3.000   1st Qu.:4.000   1st Qu.:3.167  
    ##  Median :3.000   Median :4.000   Median :5.000   Median :4.333  
    ##  Mean   :3.413   Mean   :4.147   Mean   :4.752   Mean   :4.418  
    ##  3rd Qu.:5.000   3rd Qu.:5.000   3rd Qu.:6.000   3rd Qu.:5.500  
    ##  Max.   :7.000   Max.   :9.000   Max.   :9.000   Max.   :8.167  
    ##       pic_outfit        pic_color  
    ##  formal    : 77   black&white: 78  
    ##  not formal:386   color      :385  
    ##                                    
    ##                                    
    ##                                    
    ## 

## Exercise

\##Part 1: Simple linear regression \#1

``` r
m_bty<-lm(score~bty_avg, data=evals)
summary (m_bty)
```

    ## 
    ## Call:
    ## lm(formula = score ~ bty_avg, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.9246 -0.3690  0.1420  0.3977  0.9309 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.88034    0.07614   50.96  < 2e-16 ***
    ## bty_avg      0.06664    0.01629    4.09 5.08e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5348 on 461 degrees of freedom
    ## Multiple R-squared:  0.03502,    Adjusted R-squared:  0.03293 
    ## F-statistic: 16.73 on 1 and 461 DF,  p-value: 5.083e-05

``` r
summary(m_bty)$r.squared
```

    ## [1] 0.03502226

``` r
summary(m_bty)$adj.r.squared
```

    ## [1] 0.03292903

score = 0.07 bty_avg + 3.88 r.squared = 0.03502226. 3.5% of the
variation in price is explained by average beauty rating. adj.r.squared
= 0.03292903

## Part 2: Multiple linear regression

\#2

``` r
m_bty_gen<-lm(score~bty_avg+gender, data=evals)
summary(m_bty_gen)$r.squared
```

    ## [1] 0.05912279

``` r
summary(m_bty_gen)$adj.r.squared
```

    ## [1] 0.05503202

``` r
summary (m_bty_gen)
```

    ## 
    ## Call:
    ## lm(formula = score ~ bty_avg + gender, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8305 -0.3625  0.1055  0.4213  0.9314 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.74734    0.08466  44.266  < 2e-16 ***
    ## bty_avg      0.07416    0.01625   4.563 6.48e-06 ***
    ## gendermale   0.17239    0.05022   3.433 0.000652 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5287 on 460 degrees of freedom
    ## Multiple R-squared:  0.05912,    Adjusted R-squared:  0.05503 
    ## F-statistic: 14.45 on 2 and 460 DF,  p-value: 8.177e-07

score = 0.07 bty_avg + 0.17gendermale + 3.75 r.squared = 0.059. 5.9% of
the variation in score is explained by average beauty rating and gender.
adj.r.squared = 0.055 in m_bty_gen compared to 0.03 in m_bty model,
adding gender improved the model’s explanatory power.

\#3 For each one-unit increase in beauty rating, the evaluation score
increases by 0.07 points, holding gender constant. Male professors
receive an average evaluation score 0.17 points higher than female
professors, holding beauty rating constant.

\#4 r.squared = 0.059. 5.9% of the variation in score is explained by
average beauty rating and gender.

\#5 score = 0.07 bty_avg + 0.17gendermale + 3.75 just male professor,
gendermale=1, scoremale = 0.07 bty_avg + 0.17 + 3.75 scoremale = 0.07
bty_avg + 3.92

\#6 For two professors who received the same beauty rating, Male
professors tend to have the higher course evaluation score. The
difference is +0.17.

\#7 scoremale = 0.07 bty_avg + 3.92 scorefemale = 0.07 bty_avg + 3.75

\#8 adjusted R squared of m_bty_gen = 0.055 adjusted R squared of m_bty
= 0.03 adding gender improved the model’s explanatory power.

\#9 score = 0.06664 bty_avg + 3.88 score = 0.07416 bty_avg +
0.17gendermale + 3.75 adding the gender variable changed the parameter
estimate (slope) for bty_avg. Adding gender slightly increased the
effect of bty_avg on evaluation scores.

\#10

``` r
m_bty_rank<-lm(score~bty_avg+rank, data=evals)
summary (m_bty_rank)
```

    ## 
    ## Call:
    ## lm(formula = score ~ bty_avg + rank, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8713 -0.3642  0.1489  0.4103  0.9525 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       3.98155    0.09078  43.860  < 2e-16 ***
    ## bty_avg           0.06783    0.01655   4.098 4.92e-05 ***
    ## ranktenure track -0.16070    0.07395  -2.173   0.0303 *  
    ## ranktenured      -0.12623    0.06266  -2.014   0.0445 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5328 on 459 degrees of freedom
    ## Multiple R-squared:  0.04652,    Adjusted R-squared:  0.04029 
    ## F-statistic: 7.465 on 3 and 459 DF,  p-value: 6.88e-05

score = 0.06783 bty_avg + (-0.16) RankTenureTrack + (-0.126)
RankTenured + 3.98

For each one-unit increase in beauty rating, the evaluation score
increases by 0.06783 points, holding rank constant. Tenure-track
professors receive scores 0.16 points lower than Teaching-track
professors, holding everything else constant. Tenured professors receive
scores 0.126 points lower than Teaching-track professors, holding
everything else constant. The predicted evaluation score for
Teaching-track professors is 3.98 when bty_avg = 0. which do not have
practical meaning.

\##Part 3: The search for the best model Going forward, only consider
the following variables as potential predictors: rank, ethnicity,
gender, language, age, cls_perc_eval (Percent of students in class who
completed evaluation), cls_did_eval(Number of students in class who
completed evaluation), cls_students(Total number of students in class),
cls_level(Class level: lower, upper), cls_profs(Number of professors
teaching sections in course in sample: single, multiple),
cls_credits(Number of credits of class: one credit (lab, PE, etc.),
multi credit), bty_avg.

\#11 To be honest, I think all of these variables could have some
association with the professor’s evaluation score, either directly or
indirectly. However, if I had to choose one, I would say that
cls_credits (Number of credits of a class) likely has the weakest
association.

\#12

``` r
m_cls_credits<-lm(score~cls_credits, data=evals)
summary (m_cls_credits)
```

    ## 
    ## Call:
    ## lm(formula = score ~ cls_credits, data = evals)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.84702 -0.34702  0.05298  0.35298  0.85298 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            4.14702    0.02552 162.494  < 2e-16 ***
    ## cls_creditsone credit  0.47520    0.10568   4.496 8.75e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5329 on 461 degrees of freedom
    ## Multiple R-squared:  0.04202,    Adjusted R-squared:  0.03994 
    ## F-statistic: 20.22 on 1 and 461 DF,  p-value: 8.751e-06

score = 4.147 + 0.4752 cls_creditsone_credit R-squared: 0.04202. 4.2% of
the variation in score is explained by cls_credits.

\#13 Suppose I wanted to fit a full model with the variables listed
above. If I are already going to include cls_perc_eval (Percent of
students in class who completed evaluation) and cls_students(Total
number of students in class), I should not include cls_did_eval (Number
of students in class who completed evaluation) as an additional
predictor. Because cls_did_eval is closely related to the other two
variables. It can be calcultaed by the other two. cls_did_eval does not
provide any new independent information. Including cls_did_eval as
additional variable can lead to multicollinearity (high correlation
among predictors).

\#14

``` r
m_full<-lm(score~rank+ethnicity+gender+language+age+cls_perc_eval+cls_students+cls_level+cls_profs+cls_credits+bty_avg, data=evals)
summary (m_full)
```

    ## 
    ## Call:
    ## lm(formula = score ~ rank + ethnicity + gender + language + age + 
    ##     cls_perc_eval + cls_students + cls_level + cls_profs + cls_credits + 
    ##     bty_avg, data = evals)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.84482 -0.31367  0.08559  0.35732  1.10105 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            3.5305036  0.2408200  14.660  < 2e-16 ***
    ## ranktenure track      -0.1070121  0.0820250  -1.305 0.192687    
    ## ranktenured           -0.0450371  0.0652185  -0.691 0.490199    
    ## ethnicitynot minority  0.1869649  0.0775329   2.411 0.016290 *  
    ## gendermale             0.1786166  0.0515346   3.466 0.000579 ***
    ## languagenon-english   -0.1268254  0.1080358  -1.174 0.241048    
    ## age                   -0.0066498  0.0030830  -2.157 0.031542 *  
    ## cls_perc_eval          0.0056996  0.0015514   3.674 0.000268 ***
    ## cls_students           0.0004455  0.0003585   1.243 0.214596    
    ## cls_levelupper         0.0187105  0.0555833   0.337 0.736560    
    ## cls_profssingle       -0.0085751  0.0513527  -0.167 0.867458    
    ## cls_creditsone credit  0.5087427  0.1170130   4.348  1.7e-05 ***
    ## bty_avg                0.0612651  0.0166755   3.674 0.000268 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.504 on 450 degrees of freedom
    ## Multiple R-squared:  0.1635, Adjusted R-squared:  0.1412 
    ## F-statistic: 7.331 on 12 and 450 DF,  p-value: 2.406e-12

Adjusted R-squared: 0.1412

\#15 Backward selection is a process that removes the least significant
predictors step by step to find the best model. Adjusted R² accounts for
the number of predictors and only increases if a predictor improves the
model. Adjusted R² focuses on explanatory power. If Adjusted R²
decreases when a variable is removed, it means that variable is
important.

``` r
m_best<-lm(score~ethnicity+gender+language+age+cls_perc_eval+cls_students+cls_credits+bty_avg, data=evals)
summary (m_best)
```

    ## 
    ## Call:
    ## lm(formula = score ~ ethnicity + gender + language + age + cls_perc_eval + 
    ##     cls_students + cls_credits + bty_avg, data = evals)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.89519 -0.31227  0.08596  0.37022  1.09853 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            3.3863086  0.2094164  16.170  < 2e-16 ***
    ## ethnicitynot minority  0.2044482  0.0746764   2.738 0.006428 ** 
    ## gendermale             0.1768250  0.0503142   3.514 0.000485 ***
    ## languagenon-english   -0.1511723  0.1035293  -1.460 0.144930    
    ## age                   -0.0048725  0.0026073  -1.869 0.062298 .  
    ## cls_perc_eval          0.0057538  0.0015405   3.735 0.000212 ***
    ## cls_students           0.0004073  0.0003428   1.188 0.235355    
    ## cls_creditsone credit  0.5230953  0.1050306   4.980 9.03e-07 ***
    ## bty_avg                0.0618985  0.0165267   3.745 0.000203 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5028 on 454 degrees of freedom
    ## Multiple R-squared:  0.1602, Adjusted R-squared:  0.1454 
    ## F-statistic: 10.82 on 8 and 454 DF,  p-value: 5.463e-14

score = 3.3863086 + 0.2044482 ethnicitynot minority + 0.1768250
gendermale + (-0.1511723) languagenon-english + (-0.0048725) age +
0.0057538 cls_perc_eval + 0.0004073 cls_students + 0.5230953
cls_creditsone credit + 0.0618985 bty_avg

\#16 score = 3.3863086 + 0.2044482 ethnicitynot minority + 0.1768250
gendermale + (-0.1511723) languagenon-english + (-0.0048725) age +
0.0057538 cls_perc_eval + 0.0004073 cls_students + 0.5230953
cls_creditsone credit + 0.0618985 bty_avg

Numerical Predictor: cls_perc_eval (Percentage of Students Who Completed
Evaluations) Holding other factors constant, For every 1 percentage
point increase in the number of students who completed the evaluation,
the score increases by 0.0057538 points.

Categorical Predictor: ethnicity (Not Minority vs. Minority) Professors
who are not part of a minority group receive, on average, 0.2044482
points higher on their evaluation scores compared to those in a minority
group, holding all other factors constant.

\#17 score = 3.3863086 + 0.2044482 ethnicitynot minority + 0.1768250
gendermale + (-0.1511723) languagenon-english + (-0.0048725) age +
0.0057538 cls_perc_eval + 0.0004073 cls_students + 0.5230953
cls_creditsone credit + 0.0618985 bty_avg

A professor and course at the University of Texas at Austin that would
be associated with a high evaluation score would have the following
characteristics: Ethnicity: Not Minority (+0.204) Gender: Male (+0.177)
Language: Native English Speaker (+0.151) Younger Age (-0.00487 per
year) Higher Beauty Rating (bty_avg) (+0.0619 per unit) Higher
Percentage of Students Completing Evaluations (cls_perc_eval) (+0.00575
per 1%) Larger Class Size (cls_students) (+0.00041 per student)
One-Credit Courses (cls_credits) (+0.523)

\#18 Would you be comfortable generalizing your conclusions to apply to
professors generally (at any university)? Why or why not? No Different
universities may have different evaluation criteria and student
demographics. For instance, a small private college and a large public
university may have very different evaluation standards and student
expectations. Since this dataset comes from only one university (UT
Austin), it lacks the variability needed to generalize the findings to
other institutions. Without data from multiple universities, we cannot
assume that the same factors influencing evaluation scores at UT Austin
would apply elsewhere.
