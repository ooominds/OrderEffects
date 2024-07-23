
require(openxlsx)
require(car)

options(show.signif.stars=FALSE)

ppdat = read.xlsx('OrderHardItems.xlsx')

# set type of training
ppdat$TypeOfTraining = substr(ppdat$ParticipantID,
    nchar(ppdat$ParticipantID), nchar(ppdat$ParticipantID))
ppdat$TypeOfTraining = factor(ppdat$TypeOfTraining,
    levels=c('H','S'), labels=c('HK','SR'))

# is item old (seen) or new
ppdat$NewItem = factor(ifelse((is.na(ppdat$pretest) | is.na(ppdat$posttest)),
    'unseen', 'seen'))

# correct answers
ppdat$PretestCorrect = ifelse(ppdat$pretest == ppdat$correct_answer, 1, 0)
ppdat$PosttestCorrect = ifelse(ppdat$posttest == ppdat$correct_answer, 1, 0)
ppdat$DelayedCorrect = ifelse(ppdat$delayedposttest == ppdat$correct_answer, 1, 0)

# specify factors
ppdat[,1:9] = lapply(ppdat[,1:9], as.factor)

#################################################
### Seen item analysis: TypeOfTraining effect ###
#################################################

seen = droplevels(ppdat[ppdat$NewItem=='seen',])

pre = aggregate(seen$PretestCorrect,
    list(seen$ParticipantID, seen$TypeOfTraining), sum)
post = aggregate(seen$PosttestCorrect,
    list(seen$ParticipantID, seen$TypeOfTraining), sum)
avgseen = merge(pre, post[,c(1,3)], by='Group.1')
colnames(avgseen) = c('ParticipantID', 'TypeOfTraining',
    'CorrectPre', 'CorrectPost')

avgseen$Difference = avgseen$CorrectPost - avgseen$CorrectPre

t.test(avgseen$Difference)
# t = 0.084845, df = 71, p-value = 0.9326

summary(lm1 <- lm(Difference ~ TypeOfTraining, data=avgseen))
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -3.8421 -0.8421  0.1579  0.8851  3.7941 
# 
# Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)
# (Intercept)        0.2059     0.2378   0.866     0.39
# TypeOfTrainingSR  -0.3638     0.3274  -1.111     0.27
# 
# Residual standard error: 1.387 on 70 degrees of freedom
# Multiple R-squared:  0.01734,	Adjusted R-squared:  0.003297 
# F-statistic: 1.235 on 1 and 70 DF,  p-value: 0.2703

################################################################
### Unseen item analysis: TypeOfTraining x LevelOfDifficulty ###
################################################################

unseen = droplevels(ppdat[ppdat$NewItem=='unseen',])

### 1 ### Posttest

unseen.post = droplevels(na.omit(unseen[,c(1,7,8,11)]))
avgunseen.post = aggregate(unseen.post$PosttestCorrect,
    list(unseen.post$ParticipantID, unseen.post$TypeOfTraining,
         unseen.post$difficulty_level), sum)
colnames(avgunseen.post) = c('ParticipantID', 'TypeOfTraining',
    'LevelOfDifficulty', 'SumScore')

avgunseen.post$PercentCorrect =
    ifelse(avgunseen.post$LevelOfDifficulty=='EASY',
        avgunseen.post$SumScore/20,
    ifelse(avgunseen.post$LevelOfDifficulty=='MEDIUM',
        avgunseen.post$SumScore/4,
    ifelse(avgunseen.post$LevelOfDifficulty=='MEDPLUS',
        avgunseen.post$SumScore/7,
    ifelse(avgunseen.post$LevelOfDifficulty=='HARD',
        avgunseen.post$SumScore/10, -99))))

avgunseen.post$LevelOfDifficulty = factor(avgunseen.post$LevelOfDifficulty,
    levels=c('EASY', 'MEDIUM', 'MEDPLUS', 'HARD'))
avgunseen.post$TypeOfTraining = relevel(avgunseen.post$TypeOfTraining,
    ref='SR')

summary(lm2 <- lm(PercentCorrect ~ TypeOfTraining * LevelOfDifficulty,
    data=avgunseen.post))
# Residuals:
#      Min       1Q   Median       3Q      Max 
# -0.53158 -0.05263  0.01471  0.08403  0.16842 
# 
# Coefficients:
#                                            Estimate Std. Error t value Pr(>|t|)
# (Intercept)                                0.903947   0.018356  49.246  < 2e-16
# TypeOfTrainingHK                           0.015170   0.026712   0.568  0.57054
# LevelOfDifficultyMEDIUM                    0.003947   0.025959   0.152  0.87925
# LevelOfDifficultyMEDPLUS                   0.005827   0.025959   0.224  0.82255
# LevelOfDifficultyHARD                     -0.072368   0.025959  -2.788  0.00567
# TypeOfTrainingHK:LevelOfDifficultyMEDIUM   0.062229   0.037776   1.647  0.10061
# TypeOfTrainingHK:LevelOfDifficultyMEDPLUS -0.008978   0.037776  -0.238  0.81231
# TypeOfTrainingHK:LevelOfDifficultyHARD     0.082663   0.037776   2.188  0.02948
# 
# Residual standard error: 0.1132 on 280 degrees of freedom
# Multiple R-squared:  0.1102,	Adjusted R-squared:  0.088 
# F-statistic: 4.956 on 7 and 280 DF,  p-value: 2.653e-05

linearHypothesis(lm2, c(0,0,0,0,-1,0,0,1), test='Chisq')
#   Res.Df    RSS Df Sum of Sq  Chisq Pr(>Chisq)
# 1    281 3.6742                               
# 2    280 3.5850  1  0.089232 6.9693   0.008292

### 2 ### Delayed test

unseen.delay = droplevels(na.omit(unseen[,c(1,7,8,12)]))
avgunseen.delay = aggregate(unseen.delay$DelayedCorrect,
    list(unseen.delay$ParticipantID, unseen.delay$TypeOfTraining,
         unseen.delay$difficulty_level), sum)
colnames(avgunseen.delay) = c('ParticipantID', 'TypeOfTraining',
    'LevelOfDifficulty', 'SumScore')

avgunseen.delay$PercentCorrect =
    ifelse(avgunseen.delay$LevelOfDifficulty=='EASY',
        avgunseen.delay$SumScore/20,
    ifelse(avgunseen.delay$LevelOfDifficulty=='MEDIUM',
        avgunseen.delay$SumScore/4,
    ifelse(avgunseen.delay$LevelOfDifficulty=='MEDPLUS',
        avgunseen.delay$SumScore/7,
    ifelse(avgunseen.delay$LevelOfDifficulty=='HARD',
        avgunseen.delay$SumScore/10, -99))))

avgunseen.delay$LevelOfDifficulty = factor(avgunseen.delay$LevelOfDifficulty,
    levels=c('EASY', 'MEDIUM', 'MEDPLUS', 'HARD'))
avgunseen.delay$TypeOfTraining = relevel(avgunseen.delay$TypeOfTraining,
    ref='SR')

summary(lm3 <- lm(PercentCorrect ~ TypeOfTraining * LevelOfDifficulty,
    data=avgunseen.delay))
# Residuals:
#      Min       1Q   Median       3Q      Max 
# -0.66447 -0.05042  0.04017  0.08553  0.13684 
# 
# Coefficients:
#                                            Estimate Std. Error t value Pr(>|t|)
# (Intercept)                                0.907895   0.020252  44.829   <2e-16
# TypeOfTrainingHK                           0.003870   0.029471   0.131    0.896
# LevelOfDifficultyMEDIUM                    0.006579   0.028641   0.230    0.818
# LevelOfDifficultyMEDPLUS                  -0.001880   0.028641  -0.066    0.948
# LevelOfDifficultyHARD                     -0.044737   0.028641  -1.562    0.119
# TypeOfTrainingHK:LevelOfDifficultyMEDIUM   0.030186   0.041679   0.724    0.470
# TypeOfTrainingHK:LevelOfDifficultyMEDPLUS -0.002322   0.041679  -0.056    0.956
# TypeOfTrainingHK:LevelOfDifficultyHARD     0.056502   0.041679   1.356    0.176
# 
# Residual standard error: 0.1248 on 280 degrees of freedom
# Multiple R-squared:  0.03149,	Adjusted R-squared:  0.007276 
# F-statistic:   1.3 on 7 and 280 DF,  p-value: 0.2499

### 3 ### Method of difference between post and delayed test

avgunseen.post$key = interaction(avgunseen.post$ParticipantID,
    avgunseen.post$LevelOfDifficulty)
avgunseen.delay$key = interaction(avgunseen.delay$ParticipantID,
    avgunseen.delay$LevelOfDifficulty)

avgunseen.diff = merge(avgunseen.post, avgunseen.delay[,c(6,4)], by='key')

avgunseen.diff$Difference = avgunseen.diff$SumScore.x - avgunseen.diff$SumScore.y

t.test(avgunseen.diff$Difference)
# t = -0.057925, df = 287, p-value = 0.9538

# summary(lm4 <- lm(Difference ~ TypeOfTraining * LevelOfDifficulty,
#     data=avgunseen.diff))
# # Residuals:
# #     Min      1Q  Median      3Q     Max 
# # -3.0588 -0.1471 -0.0588  0.3158  3.9412 
# # 
# # Coefficients:
# #                                           Estimate Std. Error t value Pr(>|t|)
# # (Intercept)                               -0.07895    0.16544  -0.477    0.634
# # TypeOfTrainingHK                           0.22601    0.24075   0.939    0.349
# # LevelOfDifficultyMEDIUM                    0.05263    0.23397   0.225    0.822
# # LevelOfDifficultyMEDPLUS                   0.10526    0.23397   0.450    0.653
# # LevelOfDifficultyHARD                     -0.23684    0.23397  -1.012    0.312
# # TypeOfTrainingHK:LevelOfDifficultyMEDIUM  -0.05263    0.34047  -0.155    0.877
# # TypeOfTrainingHK:LevelOfDifficultyMEDPLUS -0.19350    0.34047  -0.568    0.570
# # TypeOfTrainingHK:LevelOfDifficultyHARD     0.14861    0.34047   0.436    0.663
# # 
# # Residual standard error: 1.02 on 280 degrees of freedom
# # Multiple R-squared:  0.01947,	Adjusted R-squared:  -0.005047 
# # F-statistic: 0.7941 on 7 and 280 DF,  p-value: 0.5928



# ######################################################################
# 
# t.test(ppdat.seen.sum$CorrectPre, ppdat.seen.sum$CorrectPost,
#     paired=TRUE, alternative='less')
# # t = 0, df = 71, p-value = 0.5
# 
# delay = aggregate(ppdat.seen$DelayedCorrect,
#     list(ppdat.seen$ParticipantID), sum)
# ppdat.seen.delay.sum = merge(pre, delay, by='Group.1')
# colnames(ppdat.seen.delay.sum) = c('ParticipantID', 'CorrectPre', 'CorrectDelayed')
# 
# t.test(ppdat.seen.delay.sum$CorrectPre, ppdat.seen.delay.sum$CorrectDelayed,
#     paired=TRUE, alternative='less')
# # t = -1.3041, df = 71, p-value = 0.09821
# 
# mean(ppdat.seen.delay.sum$CorrectPre) # 27.23611
# mean(ppdat.seen.delay.sum$CorrectDelayed) # 27.45833
# mean(ppdat.seen.sum$CorrectPre) # 27.23611
# 
# ppdat.seen.sum$Diff = ppdat.seen.sum$CorrectPost - ppdat.seen.sum$CorrectPre
# t.test(ppdat.seen.sum$Diff)
# # t = 0, df = 71, p-value = 1
# 
# ppdat.seen.delay.sum$Diff = ppdat.seen.delay.sum$CorrectDelayed - ppdat.seen.delay.sum$CorrectPre
# t.test(ppdat.seen.delay.sum$Diff)
# # t = 1.3041, df = 71, p-value = 0.1964


