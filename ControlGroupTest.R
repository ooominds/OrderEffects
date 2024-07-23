require(openxlsx)
require(car)
require(tidyr)
require(tidyverse)
require(lme4)



ppdat = read.xlsx('PrePostTreatmentControl.xlsx')



#################################################
###     Scores on individual tests            ###
#################################################

## identify correct answers
ppdat$PretestCorrect = ifelse(ppdat$answer == ppdat$correct_answer & ppdat$TestType == 'pre', 1,
                              ifelse(ppdat$answer != ppdat$correct_answer & ppdat$TestType == 'pre', 0, NA))
ppdat$PosttestCorrect = ifelse(ppdat$answer == ppdat$correct_answer & ppdat$TestType == 'post', 1,
                               ifelse(ppdat$answer != ppdat$correct_answer & ppdat$TestType == 'post', 0, NA))



## create individual dfs for each test
pretestonly = ppdat %>% drop_na(PretestCorrect)
posttestonly = ppdat %>% drop_na(PosttestCorrect)


## calculate scores for each test

pretestscore = aggregate(pretestonly$PretestCorrect, 
                         list(pretestonly$ParticipantID, pretestonly$TypeOfTraining, pretestonly$TestType),
                         sum)
posttestscore = aggregate(posttestonly$PosttestCorrect, 
                          list(posttestonly$ParticipantID, posttestonly$TypeOfTraining, posttestonly$TestType),
                          sum)

## add column names to dfs
colnames(pretestscore) = c('ParticipantID', 'TypeOfTraining', 'TestType', 'Correct')
colnames(posttestscore) = c('ParticipantID', 'TypeOfTraining', 'TestType', 'Correct')


## adjust score based on proportion
pretestscore$Score = (pretestscore$Correct/50)*100
posttestscore$Score = (posttestscore$Correct/70)*100


## combine all scores and tests

AllTestScores = rbind(pretestscore,posttestscore)

AllTestScores$TestType = factor(AllTestScores$TestType, levels = c('pre', 'post'))
AllTestScores$TypeOfTraining = factor(AllTestScores$TypeOfTraining, levels= c('SR', 'HK', 'control'))

head(AllTestScores)

# dat$Test = factor(dat$Test, levels=c('pre', 'post', 'delayed'))

## linear mixed effects model

summary(lmm1 <- lmer(Score ~ TestType * TypeOfTraining + 
                       (1|ParticipantID), 
                     data=AllTestScores))

# Linear mixed model fit by REML ['lmerMod']
# Formula: Score ~ TestType * TypeOfTraining + (1 | ParticipantID)
# Data: AllTestScores
# 
# REML criterion at convergence: 963.4
# 
# Scaled residuals: 
#   Min       1Q   Median       3Q      Max 
# -2.33195 -0.44816  0.01779  0.59627  1.55681 
# 
# Random effects:
#   Groups        Name        Variance Std.Dev.
# ParticipantID (Intercept) 10.86    3.296   
# Residual                  10.59    3.254   
# Number of obs: 170, groups:  ParticipantID, 85
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)                         92.2105     0.7514 122.719
# TestTypepost                        -1.7594     0.7466  -2.357
# TypeOfTrainingHK                     0.9659     1.0934   0.883
# TypeOfTrainingcontrol                0.7126     1.4883   0.479
# TestTypepost:TypeOfTrainingHK        2.3644     1.0865   2.176
# TestTypepost:TypeOfTrainingcontrol   3.1220     1.4788   2.111
# 
# Correlation of Fixed Effects:
#   (Intr) TstTyp TyOTHK TypOfT TT:TOTH
# TestTypepst -0.497                             
# TypOfTrnnHK -0.687  0.341                      
# TypOfTrnngc -0.505  0.251  0.347               
# TstTy:TOTHK  0.341 -0.687 -0.497 -0.172        
# TstTypp:TOT  0.251 -0.505 -0.172 -0.497  0.347

linearHypothesis(lmm1, c(0, -1, 0, 0, 1, 0), test = 'Chisq' )

# Linear hypothesis test
# 
# Hypothesis:
#   - TestTypepost  + TestTypepost:TypeOfTrainingHK = 0
# 
# Model 1: restricted model
# Model 2: Score ~ TestType * TypeOfTraining + (1 | ParticipantID)
# 
# Df  Chisq Pr(>Chisq)  
# 1                       
# 2  1 5.9615    0.01462 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

powerTransform(AllTestScores$Score)
# Estimated transformation parameter 
# AllTestScores$Score 
# 7.797953 

TestScores <- densityPlot(AllTestScores$Score^7.798)

source('pm_rn_transform.R')

TestScores_rn <- densityPlot(rn_transform(AllTestScores$Score))

AllTestScores$Score.rn = rn_transform(AllTestScores$Score)

summary(lmm1.rn <- lmer(Score.rn ~ TestType * TypeOfTraining + 
                          (1|ParticipantID), 
                        data=AllTestScores), cor=FALSE)

# Linear mixed model fit by REML ['lmerMod']
# Formula: Score.rn ~ TestType * TypeOfTraining + (1 | ParticipantID)
# Data: AllTestScores
# 
# REML criterion at convergence: 458.4
# 
# Scaled residuals: 
#   Min       1Q   Median       3Q      Max 
# -1.84988 -0.51361 -0.09164  0.60695  2.38351 
# 
# Random effects:
#   Groups        Name        Variance Std.Dev.
# ParticipantID (Intercept) 0.4275   0.6538  
# Residual                  0.5249   0.7245  
# Number of obs: 170, groups:  ParticipantID, 85
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)                        -0.05149    0.15831  -0.325
# TestTypepost                       -0.39571    0.16621  -2.381
# TypeOfTrainingHK                    0.20922    0.23038   0.908
# TypeOfTrainingcontrol               0.07070    0.31356   0.225
# TestTypepost:TypeOfTrainingHK       0.49541    0.24187   2.048
# TestTypepost:TypeOfTrainingcontrol  0.81362    0.32920   2.471

AllTestScores$TypeOfTraining = factor(AllTestScores$TypeOfTraining, levels = c('control', 'SR', 'HK'))

summary(lmm2.rn <- lmer(Score.rn ~ TestType * TypeOfTraining + 
                          (1|ParticipantID), 
                        data=AllTestScores), cor=FALSE)

linearHypothesis(lmm1.rn, c(0, -1, 0, 0, 1, 0), test = 'Chisq' )

# Linear hypothesis test
# 
# Hypothesis:
#   - TestTypepost  + TestTypepost:TypeOfTrainingHK = 0
# 
# Model 1: restricted model
# Model 2: Score.rn ~ TestType * TypeOfTraining + (1 | ParticipantID)
# 
# Df Chisq Pr(>Chisq)  
# 1                      
# 2  1 5.617    0.01779 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

require(brms)

summary(brm1C.rn <- brm(Score.rn ~ TestType * TypeOfTraining + 
                         (1|ParticipantID), 
                       data=AllTestScores))
save(brm1C.rn, file='brm1C.rn.rda')

# Group-Level Effects: 
#   ~ParticipantID (Number of levels: 85) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
# sd(Intercept)     0.65      0.09     0.47     0.84 1.00     1430
# Tail_ESS
# sd(Intercept)     1845
# 
# Population-Level Effects: 
#   Estimate Est.Error l-95% CI
# Intercept                             -0.05      0.16    -0.37
# TestTypepost                          -0.39      0.17    -0.72
# TypeOfTrainingHK                       0.21      0.24    -0.26
# TypeOfTrainingcontrol                  0.08      0.31    -0.51
# TestTypepost:TypeOfTrainingHK          0.49      0.25     0.01
# TestTypepost:TypeOfTrainingcontrol     0.80      0.33     0.14
# u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                              0.25 1.00     3300     3289
# TestTypepost                          -0.06 1.00     3961     3146
# TypeOfTrainingHK                       0.66 1.00     3223     3203
# TypeOfTrainingcontrol                  0.68 1.00     3212     3397
# TestTypepost:TypeOfTrainingHK          0.97 1.00     4267     3372
# TestTypepost:TypeOfTrainingcontrol     1.46 1.00     3326     3605
# 
# Family Specific Parameters: 
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sigma     0.74      0.06     0.64     0.87 1.00     1864     2479
