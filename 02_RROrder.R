
require(brms)
require(posterior)

load('AllTestScores.rda')

######################
### brms modelling ###
######################

summary.brm1.rn <- summary(brm1.rn <- brm(Score.rn ~
    TestType * TypeOfTraining +
    (1|ParticipantID),
    data=AllTestScores,
    chains=4, iter=8000, cores=4,
    save_pars=save_pars(all=TRUE)))
save(brm1.rn, file='brm1.rn.rda')
save(summary.brm1.rn, file='summary.brm1.rn.rda')
# Group-Level Effects: 
#   ~ParticipantID (Number of levels: 72) 
#               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.72      0.08     0.57     0.89 1.00     5740     9306
# 
# Population-Level Effects: 
#                                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                            0.00      0.16    -0.32     0.33 1.00     7237     9121
# TestTypepost                        -0.40      0.16    -0.71    -0.09 1.00    14123    12639
# TestTypedelayed                     -0.09      0.16    -0.41     0.22 1.00    13526    11622
# TypeOfTrainingHK                     0.22      0.24    -0.24     0.70 1.00     6559     9580
# TestTypepost:TypeOfTrainingHK        0.49      0.23     0.03     0.94 1.00    13361    11815
# TestTypedelayed:TypeOfTrainingHK    -0.08      0.23    -0.55     0.37 1.00    13022    12830
# 
# Family Specific Parameters: 
#       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sigma     0.69      0.04     0.61     0.77 1.00    11023    12204

prior_summary(brm1.rn)
#                    prior     class                             coef         group resp dpar nlpar lb ub       source
#                   (flat)         b                                                                           default
#                   (flat)         b                  TestTypedelayed                                     (vectorized)
#                   (flat)         b TestTypedelayed:TypeOfTrainingHK                                     (vectorized)
#                   (flat)         b                     TestTypepost                                     (vectorized)
#                   (flat)         b    TestTypepost:TypeOfTrainingHK                                     (vectorized)
#                   (flat)         b                 TypeOfTrainingHK                                     (vectorized)
#  student_t(3, -0.1, 2.5) Intercept                                                                           default
#     student_t(3, 0, 2.5)        sd                                                                 0         default
#     student_t(3, 0, 2.5)        sd                                  ParticipantID                  0    (vectorized)
#     student_t(3, 0, 2.5)        sd                        Intercept ParticipantID                  0    (vectorized)
#     student_t(3, 0, 2.5)     sigma                                                                 0         default

post.draws <- as_draws_array(brm1.rn) # posterior_samples
summary.post.draws <- summarize_draws(post.draws)
save(summary.post.draws, file='summary.post.draws.rda')
head(summary.post.draws, 10)
# # A tibble: 10 × 10
#    variable                               mean   median     sd    mad     q5    q95
#    <chr>                                 <num>    <num>  <num>  <num>  <num>  <num>
#  1 b_Intercept                         0.00235  0.00295 0.165  0.162  -0.271  0.276
#  2 b_TestTypepost                     -0.401   -0.402   0.160  0.158  -0.661 -0.137
#  3 b_TestTypedelayed                  -0.0941  -0.0948  0.159  0.157  -0.358  0.169
#  4 b_TypeOfTrainingHK                  0.225    0.225   0.240  0.240  -0.165  0.625
#  5 b_TestTypepost:TypeOfTrainingHK     0.487    0.488   0.232  0.232   0.108  0.868
#  6 b_TestTypedelayed:TypeOfTrainingHK -0.0839  -0.0826  0.231  0.228  -0.472  0.298
#  7 sd_ParticipantID__Intercept         0.721    0.716   0.0824 0.0807  0.593  0.863
#  8 sigma                               0.688    0.686   0.0418 0.0416  0.623  0.760
#  9 Intercept                           0.00698  0.00745 0.0979 0.0963 -0.155  0.165

priors = c(prior(normal(0.0, 0.2), class=Intercept),
    prior(normal(-0.4, 0.2), class=b, coef='TestTypepost'),
    prior(normal(0.0, 0.2), class=b, coef='TestTypedelayed'),
    prior(normal(0.25, 0.25), class=b, coef='TypeOfTrainingHK'),
    prior(normal(0.5, 0.25), class=b, coef='TestTypepost:TypeOfTrainingHK'),
    prior(normal(-0.1, 0.25), class=b, coef='TestTypedelayed:TypeOfTrainingHK'),
    prior(normal(0.7, 0.1), class=sd, group='ParticipantID'),
    prior(cauchy(0, 2.5), class=sd)
)

summary.brm1.rn.withPriors <- summary(brm1.rn.withPriors <- brm(Score.rn ~
    TestType * TypeOfTraining +
    (1|ParticipantID),
    prior=priors,
    data=AllTestScores,
    chains=4, iter=8000, cores=4,
    save_pars=save_pars(all=TRUE)))
save(brm1.rn.withPriors, file='brm1.rn.withPriors.rda')
save(summary.brm1.rn.withPriors, file='summary.brm1.rn.withPriors.rda')
# Group-Level Effects: 
#   ~ParticipantID (Number of levels: 72) 
#               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.71      0.06     0.59     0.84 1.00     8212    10310
# 
# Population-Level Effects: 
#                                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                           -0.02      0.13    -0.26     0.23 1.00    13220    12703
# TestTypepost                        -0.39      0.11    -0.61    -0.18 1.00    24896    13107
# TestTypedelayed                     -0.06      0.11    -0.28     0.16 1.00    26071    13039
# TypeOfTrainingHK                     0.24      0.16    -0.08     0.55 1.00    13907    12113
# TestTypepost:TypeOfTrainingHK        0.48      0.15     0.19     0.78 1.00    23670    12988
# TestTypedelayed:TypeOfTrainingHK    -0.11      0.15    -0.41     0.18 1.00    22239    13287
# 
# Family Specific Parameters: 
#       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sigma     0.68      0.04     0.61     0.77 1.00    11242    12495

prior_summary(brm1.rn.withPriors)
#                 prior     class                             coef         group resp dpar nlpar lb ub       source
#                (flat)         b                                                                           default
#        normal(0, 0.2)         b                  TestTypedelayed                                             user
#    normal(-0.1, 0.25)         b TestTypedelayed:TypeOfTrainingHK                                             user
#     normal(-0.4, 0.2)         b                     TestTypepost                                             user
#     normal(0.5, 0.25)         b    TestTypepost:TypeOfTrainingHK                                             user
#    normal(0.25, 0.25)         b                 TypeOfTrainingHK                                             user
#        normal(0, 0.2) Intercept                                                                              user
#        cauchy(0, 2.5)        sd                                                                 0            user
#      normal(0.7, 0.1)        sd                                  ParticipantID                  0            user
#      normal(0.7, 0.1)        sd                        Intercept ParticipantID                  0    (vectorized)
#  student_t(3, 0, 2.5)     sigma                                                                 0         default

# brm1.rn = add_criterion(brm1.rn, criterion=c('loo'))
# brm1.rn.withPriors = add_criterion(brm1.rn.withPriors, criterion=c('loo'))
# loo_compare(brm1.rn, brm1.rn.withPriors)
# #               elpd_diff se_diff
# # brm1.rn.withPriors  0.0       0.0   
# # brm1.rn            -1.9       0.4   

# bayes_R2(brm1.rn)
# #     Estimate  Est.Error     Q2.5     Q97.5
# # R2 0.5408962 0.04419346 0.445507 0.6168048
# bayes_R2(brm1.rn.withPriors)
# #     Estimate  Est.Error      Q2.5    Q97.5
# # R2 0.5403212 0.04074223 0.4510588 0.611994

# loo(brm1.rn.withPriors)
# #          Estimate   SE
# # elpd_loo   -258.4 11.3
# # p_loo        54.8  4.9
# # looic       516.9 22.7
# # ------
# # Monte Carlo SE of elpd_loo is NA.
# # 
# # Pareto k diagnostic values:
# #                          Count Pct.    Min. n_eff
# # (-Inf, 0.5]   (good)     196   90.7%   2121      
# #  (0.5, 0.7]   (ok)        18    8.3%   1037      
# #    (0.7, 1]   (bad)        2    0.9%   80        
# #    (1, Inf)   (very bad)   0    0.0%   <NA>      

##########################
### Hypotheses testing ###
##########################

load('brm1.rn.withPriors.rda')

### Four hypotheses that we promised in the text! ###

# Is pretest HK better than SR -- NO
hy1 = 'TypeOfTrainingHK = 0'
hypothesis(brm1.rn.withPriors, hy1)
#               Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
# 1 (TypeOfTrainingHK) = 0     0.24      0.16    -0.08     0.56         NA        NA     

# Is posttest HK better than SR -- YES
hy2 = 'TestTypepost:TypeOfTrainingHK > TestTypepost'
hypothesis(brm1.rn.withPriors, hy2, alpha=0.025)
#                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
# 1 (TestTypepost:Typ... > 0     0.88      0.22     0.44     1.32      15999         1    *

# Is pre and post average HK better than SR -- YES
hy3 = '((TypeOfTrainingHK + TestTypepost:TypeOfTrainingHK) / 2) >
	(TestTypepost / 2)'
hypothesis(brm1.rn.withPriors, hy3, alpha=0.025)
#                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
# 1 (((TypeOfTraining... > 0     0.56      0.12     0.32      0.8        Inf         1    *

# Is delayed HK better than SR -- NO
hy4 = 'TestTypedelayed:TypeOfTrainingHK > TestTypedelayed'
hypothesis(brm1.rn.withPriors, hy4, alpha=0.025)
#                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
# 1 (TestTypedelayed:... > 0    -0.05      0.23    -0.49      0.4       0.72      0.42     

# -

# Average across all tests, HK vs. SR -- YES
hy3b = '((TypeOfTrainingHK + TestTypepost:TypeOfTrainingHK + TestTypedelayed:TypeOfTrainingHK) / 3) >
	((TestTypepost + TestTypedelayed) / 3)'
hypothesis(brm1.rn.withPriors, hy3b, alpha=0.025)
#                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
# 1 (((TypeOfTraining... > 0     0.36      0.11     0.13     0.58    1065.67         1    *

# -

# HK, delayed vs. pre -- NO
hy5a = 'TypeOfTrainingHK > TestTypedelayed:TypeOfTrainingHK'
hypothesis(brm1.rn.withPriors, hy5a, alpha=0.025)
#                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
# 1 (TypeOfTrainingHK... > 0     0.35      0.25    -0.13     0.84      11.17      0.92     

# HK, delayed vs. post -- YES
hy5b = 'TestTypepost:TypeOfTrainingHK > TestTypedelayed:TypeOfTrainingHK'
hypothesis(brm1.rn.withPriors, hy5b, alpha=0.025)
#                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
# 1 (TestTypepost:Typ... > 0      0.6      0.18     0.23     0.96    1229.77         1    *

# SR, delayed vs. pre -- NO
hy5c = '0 > TestTypedelayed'
hypothesis(brm1.rn.withPriors, hy5c, alpha=0.025)
#                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
# 1 (0)-(TestTypedela... > 0     0.06      0.11    -0.15     0.28       2.49      0.71     

# SR, delayed vs. post -- YES
hy5d = 'TestTypedelayed > TestTypepost'
hypothesis(brm1.rn.withPriors, hy5d, alpha=0.025)
#                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
# 1 (TestTypedelayed)... > 0     0.33      0.13     0.08     0.58     172.91      0.99    *

# -

# HK, post vs. pre -- NO
hy6 = 'TestTypepost:TypeOfTrainingHK > TypeOfTrainingHK'
hypothesis(brm1.rn.withPriors, hy6, alpha=0.025)
#                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
# 1 (TestTypepost:Typ... > 0     0.25      0.25    -0.23     0.73       5.23      0.84     

# SR, post vs. pre -- YES
hy7 = '0 > TestTypepost'
hypothesis(brm1.rn.withPriors, hy7, alpha=0.025)
#               Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
# 1 (0)-(TestTypepost) > 0     0.39      0.11     0.17     0.61       3999         1    *


