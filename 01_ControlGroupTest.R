
require(car)
require(lme4)
require(brms)

options(show.signif.stars=FALSE)

load('ControlGroupTest.rda')

######################
### lme4 modelling ###
######################

summary(lmm1.rn <- lmer(Score.rn ~
    TestType * TypeOfTraining + 
    (1|ParticipantID), 
    data=ControlGroupTest), cor=FALSE)
# Scaled residuals: 
#      Min       1Q   Median       3Q      Max 
# -1.84988 -0.51361 -0.09164  0.60695  2.38351 
# 
# Random effects:
#  Groups        Name        Variance Std.Dev.
#  ParticipantID (Intercept) 0.4275   0.6538  
#  Residual                  0.5249   0.7245  
# Number of obs: 170, groups:  ParticipantID, 85
# 
# Fixed effects:
#                               Estimate Std. Error t value
# (Intercept)                     0.0192     0.2707   0.071
# TestTypepost                    0.4179     0.2842   1.471
# TypeOfTrainingSR               -0.0707     0.3136  -0.225
# TypeOfTrainingHK                0.1385     0.3182   0.435
# TestTypepost:TypeOfTrainingSR  -0.8136     0.3292  -2.471
# TestTypepost:TypeOfTrainingHK  -0.3182     0.3341  -0.952

# posttest SR vs. HK contrast
linearHypothesis(lmm1.rn, c(0,0,0,0,-1,1), test = 'Chisq' )
# Hypothesis:
# - TestTypepost:TypeOfTrainingSR  + TestTypepost:TypeOfTrainingHK = 0
# 
#   Df  Chisq Pr(>Chisq)
# 1                     
# 2  1 4.1955    0.04053

######################
### brms modelling ###
######################

summary.brm1C.rn <- summary(brm1C.rn <- brm(Score.rn ~
    TestType * TypeOfTraining +
    (1|ParticipantID),
    data=ControlGroupTest,
    chains=4, iter=8000, cores=4,
    save_pars=save_pars(all=TRUE)))
save(brm1C.rn, file='brm1C.rn.rda')
save(summary.brm1C.rn, file='summary.brm1C.rn.rda')
# Multilevel Hyperparameters:
# ~ParticipantID (Number of levels: 85) 
#               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.65      0.10     0.46     0.84 1.00     4155     6390
# 
# Regression Coefficients:
#                               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                         0.02      0.27    -0.52     0.55 1.00     6110     8718
# TestTypepost                      0.41      0.29    -0.14     0.98 1.00     7619    10240
# TypeOfTrainingSR                 -0.08      0.32    -0.70     0.55 1.00     6191     9116
# TypeOfTrainingHK                  0.13      0.32    -0.50     0.78 1.00     6270     9765
# TestTypepost:TypeOfTrainingSR    -0.81      0.34    -1.47    -0.14 1.00     8243    11042
# TestTypepost:TypeOfTrainingHK    -0.31      0.34    -0.98     0.35 1.00     8064    10203
# 
# Further Distributional Parameters:
#       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sigma     0.74      0.06     0.63     0.87 1.00     4780     7014


