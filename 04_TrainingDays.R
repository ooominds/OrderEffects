
require(mgcv)

options(show.signif.stars=FALSE)

load('AllDayScores.rda')
load('TrainingData.rda')

##############################
### Modelling daily scores ###
##############################

summary.gam2 <- summary(gam2 <- gam(Score ~
    s(TrainingDay, by=TypeOfTraining, k=4) +
    s(ParticipantID, bs='re'),
    data=AllDayScores))
save(gam2, file='gam2.rda')
save(summary.gam2, file='summary.gam2.rda')
# Parametric coefficients:
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept)  90.9253     0.6483   140.2   <2e-16
# 
# Approximate significance of smooth terms:
#                                    edf Ref.df     F p-value
# s(TrainingDay):TypeOfTrainingsr  2.829  2.977 2.625  0.0599
# s(TrainingDay):TypeOfTraininghk  2.647  2.905 0.981  0.3128
# s(ParticipantID)                61.786 71.000 6.705  <2e-16
# 
# R-sq.(adj) =  0.628   Deviance explained = 71.5%
# GCV = 20.592  Scale est. = 15.711    n = 288

# Removing P03H who is very far off...
AllDayScores2 = droplevels(AllDayScores[AllDayScores$ParticipantID!='P03H',])
summary.gam2.2 <- summary(gam2.2 <- gam(Score ~
    s(TrainingDay, by=TypeOfTraining, k=4) +
    s(ParticipantID, bs='re'),
    data=AllDayScores2))
save(gam2.2, file='gam2.2.rda')
save(summary.gam2.2, file='summary.gam2.2.rda')
# Parametric coefficients:
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept)  91.1937     0.6043   150.9   <2e-16
# 
# Approximate significance of smooth terms:
#                                    edf Ref.df     F p-value
# s(TrainingDay):TypeOfTrainingsr  2.828  2.977 2.616  0.0607
# s(TrainingDay):TypeOfTraininghk  2.899  2.992 1.762  0.1386
# s(ParticipantID)                59.362 70.000 5.476  <2e-16
# 
# R-sq.(adj) =   0.58   Deviance explained = 67.7%
# GCV = 20.539  Scale est. = 15.76     n = 284

######################################################
### Modelling individual correct/incorrect answers ###
######################################################

summary.gam3 <- summary(gam3 <- gam(score ~
    OrderOfItem +
    s(training_day, by=group, k=4) +
    s(question, bs='re') +
    s(OrderOfItem, participant_id, bs='fs', m=1),
    family='binomial',
    data=TrainingData))
save(gam3, file='gam3.rda')
save(summary.gam3, file='summary.gam3.rda')
# Parametric coefficients:
#              Estimate Std. Error z value Pr(>|z|)
# (Intercept)  4.228647   3.674321   1.151    0.250
# OrderOfItem -0.005874   0.040137  -0.146    0.884
# 
# Approximate significance of smooth terms:
#                                   edf  Ref.df    Chi.sq p-value
# s(training_day):grouphk         1.163   1.284     0.068   0.940
# s(training_day):groupsr         1.103   1.179     0.013   0.969
# s(question)                   168.724 177.000  1235.299  <2e-16
# s(OrderOfItem,participant_id)  93.648 656.000 16147.925  <2e-16
# 
# R-sq.(adj) =  0.257   Deviance explained = 33.7%
# UBRE = -0.55739  Scale est. = 1         n = 12960


