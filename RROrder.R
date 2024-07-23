
require(openxlsx)
require(car)
require(tidyr)
require(tidyverse)
require(lme4)


ppdat = read.xlsx('PrePostDelayedTestsAnswers.xlsx')

# set type of training
ppdat$TypeOfTraining = substr(ppdat$ParticipantID,
                              nchar(ppdat$ParticipantID), nchar(ppdat$ParticipantID))
ppdat$TypeOfTraining = factor(ppdat$TypeOfTraining,
                              levels=c('H','S'), labels=c('HK','SR'))


#################################################
###     Scores on individual tests            ###
#################################################

## identify correct answers
ppdat$PretestCorrect = ifelse(ppdat$answer == ppdat$correct_answer & ppdat$TestType == 'pre', 1,
                       ifelse(ppdat$answer != ppdat$correct_answer & ppdat$TestType == 'pre', 0, NA))
ppdat$PosttestCorrect = ifelse(ppdat$answer == ppdat$correct_answer & ppdat$TestType == 'post', 1,
                        ifelse(ppdat$answer != ppdat$correct_answer & ppdat$TestType == 'post', 0, NA))
ppdat$DelayedCorrect = ifelse(ppdat$answer == ppdat$correct_answer & ppdat$TestType == 'delayed', 1,
                       ifelse(ppdat$answer != ppdat$correct_answer & ppdat$TestType == 'delayed', 0, NA))


## create individual dfs for each test
pretestonly = ppdat %>% drop_na(PretestCorrect)
posttestonly = ppdat %>% drop_na(PosttestCorrect)
delayedonly = ppdat %>% drop_na(DelayedCorrect)

## calculate scores for each test

pretestscore = aggregate(pretestonly$PretestCorrect, 
                         list(pretestonly$ParticipantID, pretestonly$TypeOfTraining, pretestonly$TestType),
                         sum)
posttestscore = aggregate(posttestonly$PosttestCorrect, 
                          list(posttestonly$ParticipantID, posttestonly$TypeOfTraining, posttestonly$TestType),
                          sum)
delayedscore = aggregate(delayedonly$DelayedCorrect, 
                         list(delayedonly$ParticipantID, delayedonly$TypeOfTraining, delayedonly$TestType), 
                         sum)
## add column names to dfs
colnames(pretestscore) = c('ParticipantID', 'TypeOfTraining', 'TestType', 'Correct')
colnames(posttestscore) = c('ParticipantID', 'TypeOfTraining', 'TestType', 'Correct')
colnames(delayedscore) = c('ParticipantID', 'TypeOfTraining', 'TestType', 'Correct')

## adjust score based on proportion
pretestscore$Score = (pretestscore$Correct/50)*100
posttestscore$Score = (posttestscore$Correct/70)*100
delayedscore$Score = (delayedscore$Correct/70)*100

## combine all scores and tests

AllTestScores = rbind(pretestscore,posttestscore,delayedscore)

AllTestScores$TestType = factor(AllTestScores$TestType, levels = c('pre', 'post', 'delayed'))
AllTestScores$TypeOfTraining = factor(AllTestScores$TypeOfTraining, levels= c('SR', 'HK'))

head(AllTestScores)

# dat$Test = factor(dat$Test, levels=c('pre', 'post', 'delayed'))

## linear mixed effects model

summary(lmm1 <- lmer(Score ~ TestType * TypeOfTraining + 
                       (1|ParticipantID), 
                     data=AllTestScores))

### Linear mixed model relevelled ###
# Linear mixed model fit by REML ['lmerMod']
# Formula: 
#   Score ~ TestType * TypeOfTraining + (1 | ParticipantID)
# Data: AllTestScores
# 
# REML criterion at convergence: 1203.3
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.9007 -0.4607  0.1021  0.5379  1.9821 
# 
# Random effects:
#   Groups        Name        Variance Std.Dev.
# ParticipantID (Intercept) 11.639   3.412   
# Residual                   9.817   3.133   
# Number of obs: 216, groups:  ParticipantID, 72
# 
# Fixed effects:
#   Estimate Std. Error
# (Intercept)                       92.2105     0.7514
# TestTypepost                      -1.7594     0.7188
# TestTypedelayed                   -0.4812     0.7188
# TypeOfTrainingHK                   0.9659     1.0935
# TestTypepost:TypeOfTrainingHK      2.3644     1.0460
# TestTypedelayed:TypeOfTrainingHK   0.3720     1.0460
# t value
# (Intercept)                      122.716
# TestTypepost                      -2.448
# TestTypedelayed                   -0.669
# TypeOfTrainingHK                   0.883
# TestTypepost:TypeOfTrainingHK      2.260
# TestTypedelayed:TypeOfTrainingHK   0.356
# 
# Correlation of Fixed Effects:
#   (Intr) TstTypp TstTypd TyOTHK
# TestTypepst   -0.478                       
# TestTypdlyd   -0.478  0.500                
# TypOfTrnnHK   -0.687  0.329   0.329        
# TstTypp:TOTHK  0.329 -0.687  -0.344  -0.478
# TstTypd:TOTHK  0.329 -0.344  -0.687  -0.478
# TstTypp:TOTHK
# TestTypepst                
# TestTypdlyd                
# TypOfTrnnHK                
# TstTypp:TOTHK              
# TstTypd:TOTHK  0.500  


## S3 method for class 'lme'
# linearHypothesis(model, hypothesis.matrix, rhs=NULL,
#                  vcov.=NULL, singular.ok=FALSE, verbose=FALSE, ...)

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
# 2  1 6.4316    0.01121 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

### Linear mixed model before relevel ###
# Linear mixed model fit by REML ['lmerMod']
# Formula: 
#   Score ~ TestType * TypeOfTraining + (1 | ParticipantID)
# Data: AllTestScores
# 
# REML criterion at convergence: 1203.3
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.9007 -0.4607  0.1021  0.5379  1.9821 
# 
# Random effects:
#   Groups        Name        Variance Std.Dev.
# ParticipantID (Intercept) 11.639   3.412   
# Residual                   9.817   3.133   
# Number of obs: 216, groups:  ParticipantID, 72
# 
# Fixed effects:
#   Estimate Std. Error
# (Intercept)                    93.0672     0.7944
# TestTypepost                    0.7143     0.7599
# TestTypepre                     0.1092     0.7599
# TypeOfTrainingSR               -1.3379     1.0935
# TestTypepost:TypeOfTrainingSR  -1.9925     1.0460
# TestTypepre:TypeOfTrainingSR    0.3720     1.0460
# t value
# (Intercept)                   117.156
# TestTypepost                    0.940
# TestTypepre                     0.144
# TypeOfTrainingSR               -1.224
# TestTypepost:TypeOfTrainingSR  -1.905
# TestTypepre:TypeOfTrainingSR    0.356
# 
# Correlation of Fixed Effects:
#   (Intr) TstTypps TstTyppr TyOTSR
# TestTypepst    -0.478                         
# TestTypepre    -0.478  0.500                  
# TypOfTrnnSR    -0.726  0.347    0.347         
# TstTypps:TOTSR  0.347 -0.726   -0.363   -0.478
# TstTyppr:TOTSR  0.347 -0.363   -0.726   -0.478
# TstTypps:TOTSR
# TestTypepst                  
# TestTypepre                  
# TypOfTrnnSR                  
# TstTypps:TOTSR               
# TstTyppr:TOTSR  0.500  

#### box and cox 

# box.cox(lmm1) box.cox is defunct
# bcPower(AllTestScores$Score, lambda = 0)

powerTransform(AllTestScores$Score)
# Estimated transformation parameter 
# AllTestScores$Score 
# 7.624921 

densityPlot(AllTestScores$Score^7.625)

source('pm_rn_transform.R')

densityPlot(rn_transform(AllTestScores$Score))

AllTestScores$Score.rn = rn_transform(AllTestScores$Score)

summary(lmm1.rn <- lmer(Score.rn ~ TestType * TypeOfTraining + 
                       (1|ParticipantID), 
                     data=AllTestScores), cor=FALSE)

# Linear mixed model fit by REML ['lmerMod']
# Formula: Score.rn ~ TestType * TypeOfTraining + (1 | ParticipantID)
# Data: AllTestScores
# 
# REML criterion at convergence: 557.8
# 
# Scaled residuals: 
#   Min       1Q   Median       3Q      Max 
# -2.30867 -0.55872  0.01728  0.50468  2.63463 
# 
# Random effects:
#   Groups        Name        Variance Std.Dev.
# ParticipantID (Intercept) 0.5057   0.7111  
# Residual                  0.4641   0.6813  
# Number of obs: 216, groups:  ParticipantID, 72
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)                       0.0003689  0.1597517   0.002
# TestTypepost                     -0.3995454  0.1562925  -2.556
# TestTypedelayed                  -0.0919889  0.1562925  -0.589
# TypeOfTrainingHK                  0.2233480  0.2324728   0.961
# TestTypepost:TypeOfTrainingHK     0.4848452  0.2274391   2.132
# TestTypedelayed:TypeOfTrainingHK -0.0863450  0.2274391  -0.380
# 
# Correlation of Fixed Effects:
#   (Intr) TstTypp TstTypd TyOTHK TstTypp:TOTHK
# TestTypepst   -0.489                                     
# TestTypdlyd   -0.489  0.500                              
# TypOfTrnnHK   -0.687  0.336   0.336                      
# TstTypp:TOTHK  0.336 -0.687  -0.344  -0.489              
# TstTypd:TOTHK  0.336 -0.344  -0.687  -0.489  0.500

linearHypothesis(lmm1.rn, c(0, -1, 0, 0, 1, 0), test = 'Chisq' )

# Linear hypothesis test
# 
# Hypothesis:
#   - TestTypepost  + TestTypepost:TypeOfTrainingHK = 0
# 
# Model 1: restricted model
# Model 2: Score.rn ~ TestType * TypeOfTraining + (1 | ParticipantID)
# 
# Df  Chisq Pr(>Chisq)  
# 1                       
# 2  1 6.2566    0.01237 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


summary(lmm2.rn <- lmer(Score.rn ~ TestType * TypeOfTraining + 
                          (1+TypeOfTraining|ParticipantID), 
                        data=AllTestScores), cor=FALSE)
anova(lmm1.rn, lmm2.rn)
# 
# refitting model(s) with ML (instead of REML)
# Data: AllTestScores
# Models:
#   lmm1.rn: Score.rn ~ TestType * TypeOfTraining + (1 | ParticipantID)
# lmm2.rn: Score.rn ~ TestType * TypeOfTraining + (1 + TypeOfTraining | ParticipantID)
# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# lmm1.rn    8 561.58 588.59 -272.79   545.58                     
# lmm2.rn   10 563.19 596.95 -271.60   543.19 2.3896  2     0.3028

require(brms)

load('brm1.rn.rda')
summary(brm1.rn)

# summary(brm1.rn <- brm(Score.rn ~ TestType * TypeOfTraining + 
                        #   (1|ParticipantID), 
                        # data=AllTestScores))

# Family: gaussian 
# Links: mu = identity; sigma = identity 
# Formula: Score.rn ~ TestType * TypeOfTraining + (1 | ParticipantID) 
# Data: AllTestScores (Number of observations: 216) 
# Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
# total post-warmup draws = 4000
# 
# Group-Level Effects: 
#   ~ParticipantID (Number of levels: 72) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.72      0.08     0.57     0.90 1.00     1424     2551
# 
# Population-Level Effects: 
#                                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                            0.00      0.17    -0.33     0.33 1.00     1734     2618
# TestTypepost                        -0.40      0.16    -0.71    -0.10 1.00     3606     3104
# TestTypedelayed                     -0.10      0.16    -0.40     0.23 1.00     2986     3149
# TypeOfTrainingHK                     0.22      0.24    -0.25     0.70 1.00     1755     2593
# TestTypepost:TypeOfTrainingHK        0.49      0.23     0.03     0.92 1.00     3151     3259
# TestTypedelayed:TypeOfTrainingHK    -0.08      0.23    -0.53     0.36 1.00     2790     2764
# 
# Family Specific Parameters: 
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sigma     0.69      0.04     0.61     0.77 1.00     3216     3281

conditional_effects(brm1.rn, 'TestType:TypeOfTraining')
# add line to save plot

hy1 = 'TestTypedelayed:TypeOfTrainingHK < TestTypepost:TypeOfTrainingHK'
hypothesis(brm1.rn, hy1)

# Hypothesis Tests for class b:
#   Hypothesis Estimate Est.Error CI.Lower CI.Upper
# 1 (TestTypedelayed:... < 0    -0.57      0.23    -0.93     -0.2
#    Evid.Ratio Post.Prob Star
#    1     172.91      0.99    *
## delayed is significantly lower than post

# test if preSR is equal to delayedSR (this you can read from the table)
hy2 = 'Intercept = TestTypedelayed'
hypothesis(brm1.rn, hy2)
 
# Hypothesis Tests for class b:
#   Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
# 1 (Intercept)-(Test... = 0      0.1      0.28    -0.45     0.67         NA        NA   

# test if preHK is equal to delayedHK
hy3 = '(Intercept+TypeOfTrainingHK) = (Intercept+TestTypedelayed:TypeOfTrainingHK)'
hypothesis(brm1.rn, hy3)

# Hypothesis Tests for class b:
#   Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
# 1 ((Intercept+TypeO... = 0     0.31      0.41    -0.53      1.1         NA        NA     

# 
summary(brm1.rn <- brm(Score.rn ~ TestType * TypeOfTraining + 
                           (1|ParticipantID),
                         data=AllTestScores))