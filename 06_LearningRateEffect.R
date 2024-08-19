
require(mgcv)

options(show.signif.stars=FALSE)

load('LearningRateData.rda')

#################
### Modelling ###
#################

summary.gam4 <- (summary(gam4 <- gam(Accuracy ~
    TestAndGroup +
    s(LearningRate, by=TestAndGroup, k=4) +
    s(ParticipantID, bs='re'),
    data=LearningRateData,
    method='ML')))
save(gam4, file='gam4.rda')
save(summary.gam4, file='summary.gam4.rda')
# Parametric coefficients:
#                            Estimate Std. Error t value Pr(>|t|)
# (Intercept)                 91.7939     0.8075 113.671   <2e-16
# TestAndGroupposttest.sr     -1.3720     0.7872  -1.743   0.0835
# TestAndGroupdelayedtest.sr  -0.0285     0.7755  -0.037   0.9707
# TestAndGrouppretest.hk       0.6469     1.0921   0.592   0.5546
# TestAndGroupposttest.hk      2.1397     1.0893   1.964   0.0514
# TestAndGroupdelayedtest.hk   2.4114     1.1666   2.067   0.0405
# 
# Approximate significance of smooth terms:
#                                              edf Ref.df      F  p-value
# s(LearningRate):TestAndGrouppretest.sr      1.00      1  3.691  0.05670
# s(LearningRate):TestAndGroupposttest.sr     1.00      1  4.033  0.04651
# s(LearningRate):TestAndGroupdelayedtest.sr  1.00      1  0.224  0.63670
# s(LearningRate):TestAndGrouppretest.hk      1.00      1 25.732 1.32e-06
# s(LearningRate):TestAndGroupposttest.hk     1.00      1  9.232  0.00283
# s(LearningRate):TestAndGroupdelayedtest.hk  1.00      1  7.120  0.00850
# s(ParticipantID)                           54.36     70  3.427  < 2e-16
# 
# R-sq.(adj) =  0.644   Deviance explained = 75.6%
# -ML = 559.59  Scale est. = 7.8038    n = 209

summary.gam4.t <- (summary(gam4.t <- gam(Accuracy ~
    TestAndGroup +
    s(LearningRate, by=TestAndGroup, k=4) +
    s(ParticipantID, bs='re'),
    data=LearningRateData,
    subset=abs(scale(resid(gam4)))<2.5,
    method='ML')))
save(gam4.t, file='gam4.t.rda')
save(summary.gam4.t, file='summary.gam4.t.rda')
# Parametric coefficients:
#                            Estimate Std. Error t value Pr(>|t|)
# (Intercept)                 92.2537     0.7616 121.132  < 2e-16
# TestAndGroupposttest.sr     -1.8434     0.6937  -2.657  0.00883
# TestAndGroupdelayedtest.sr  -0.1401     0.6858  -0.204  0.83843
# TestAndGrouppretest.hk       0.2400     1.0359   0.232  0.81713
# TestAndGroupposttest.hk      1.9303     1.0373   1.861  0.06492
# TestAndGroupdelayedtest.hk   2.2066     1.1018   2.003  0.04720
# 
# Approximate significance of smooth terms:
#                                              edf Ref.df      F  p-value
# s(LearningRate):TestAndGrouppretest.sr      1.00      1  2.567  0.11145
# s(LearningRate):TestAndGroupposttest.sr     1.00      1  7.586  0.00669
# s(LearningRate):TestAndGroupdelayedtest.sr  1.00      1  0.745  0.38956
# s(LearningRate):TestAndGrouppretest.hk      1.00      1 30.198 7.23e-07
# s(LearningRate):TestAndGroupposttest.hk     1.00      1  9.996  0.00194
# s(LearningRate):TestAndGroupdelayedtest.hk  1.00      1  8.269  0.00469
# s(ParticipantID)                           57.65     70  4.543  < 2e-16
# 
# R-sq.(adj) =  0.708   Deviance explained = 80.7%
# -ML = 529.93  Scale est. = 5.9124    n = 205


