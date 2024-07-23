require(openxlsx)
require(car)
require(tidyr)
require(tidyverse)
require(lme4)

trainingdat = read.xlsx('training_4days_data.xlsx')

trainingdat_short = trainingdat[, c(1,2,3,8, 12, 13)]


#################################################
###     Scores on individual tests            ###
#################################################

## identify correct answers
trainingdat_short$Day1Correct = ifelse(trainingdat_short$answer == 
                                         trainingdat_short$correct_answer & 
                                         trainingdat_short$training_day == 'day1', 1,
                              ifelse(trainingdat_short$answer != 
                                       trainingdat_short$correct_answer & 
                                       trainingdat_short$training_day == 'day1', 0, NA))
trainingdat_short$Day2Correct = ifelse(trainingdat_short$answer == 
                                         trainingdat_short$correct_answer & 
                                         trainingdat_short$training_day == 'day2', 1,
                                       ifelse(trainingdat_short$answer != 
                                        trainingdat_short$correct_answer & 
                                        trainingdat_short$training_day == 'day2', 0, NA))
trainingdat_short$Day3Correct = ifelse(trainingdat_short$answer == 
                                         trainingdat_short$correct_answer & 
                                         trainingdat_short$training_day == 'day3', 1,
                                       ifelse(trainingdat_short$answer != 
                                          trainingdat_short$correct_answer & 
                                          trainingdat_short$training_day == 'day3', 0, NA))
trainingdat_short$Day4Correct = ifelse(trainingdat_short$answer == 
                                         trainingdat_short$correct_answer & 
                                         trainingdat_short$training_day == 'day4', 1,
                                       ifelse(trainingdat_short$answer != 
                                          trainingdat_short$correct_answer & 
                                          trainingdat_short$training_day == 'day4', 0, NA))
## create individual dfs for each test
day1only = trainingdat_short %>% drop_na(Day1Correct)
day2only = trainingdat_short %>% drop_na(Day2Correct)
day3only = trainingdat_short %>% drop_na(Day3Correct)
day4only = trainingdat_short %>% drop_na(Day4Correct)

## calculate scores for each test

day1score = aggregate(day1only$Day1Correct, 
                         list(day1only$participant_id, day1only$group, day1only$training_day),
                         sum)
day2score = aggregate(day2only$Day2Correct, 
                      list(day2only$participant_id, day2only$group, day2only$training_day),
                      sum)
day3score = aggregate(day3only$Day3Correct, 
                      list(day3only$participant_id, day3only$group, day3only$training_day),
                      sum)
day4score = aggregate(day4only$Day4Correct, 
                      list(day4only$participant_id, day4only$group, day4only$training_day),
                      sum)

## add column names to dfs
colnames(day1score) = c('ParticipantID', 'TypeOfTraining', 'TrainingDay', 'Correct')
colnames(day2score) = c('ParticipantID', 'TypeOfTraining', 'TrainingDay', 'Correct')
colnames(day3score) = c('ParticipantID', 'TypeOfTraining', 'TrainingDay', 'Correct')
colnames(day4score) = c('ParticipantID', 'TypeOfTraining', 'TrainingDay', 'Correct')

## adjust score based on proportion
day1score$Score = (day1score$Correct/40)*100
day2score$Score = (day2score$Correct/50)*100
day3score$Score = (day3score$Correct/40)*100
day4score$Score = (day4score$Correct/50)*100

## combine all scores and days

AllDayScores = rbind(day1score,day2score,day3score,day4score)

AllDayScores$TrainingDay = factor(AllDayScores$TrainingDay, levels = c('day1', 'day2', 'day3', 'day4'))
AllDayScores$TypeOfTraining = factor(AllDayScores$TypeOfTraining, levels= c('sr', 'hk'))

head(AllDayScores)

## linear mixed effects model

summary(lmm3 <- lmer(Score ~ TrainingDay * TypeOfTraining + 
                       (1|ParticipantID), 
                     data=AllDayScores))

# Linear mixed model fit by REML ['lmerMod']
# Formula: Score ~ TrainingDay * TypeOfTraining + (1 | ParticipantID)
# Data: AllDayScores
# 
# REML criterion at convergence: 1739.3
# 
# Scaled residuals: 
#   Min       1Q   Median       3Q      Max 
# -2.90064 -0.47963  0.06946  0.56123  2.21482 
# 
# Random effects:
#   Groups        Name        Variance Std.Dev.
# ParticipantID (Intercept) 26.29    5.128   
# Residual                  15.79    3.974   
# Number of obs: 288, groups:  ParticipantID, 73
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)                      90.19713    1.04587  86.242
# TrainingDayday2                   1.72368    0.91161   1.891
# TrainingDayday3                  -0.91698    0.91630  -1.001
# TrainingDayday4                   0.04355    0.91630   0.048
# TypeOfTraininghk                  1.56757    1.52696   1.027
# TrainingDayday2:TypeOfTraininghk -1.25310    1.32658  -0.945
# TrainingDayday3:TypeOfTraininghk -0.48008    1.32982  -0.361
# TrainingDayday4:TypeOfTraininghk  0.42704    1.32982   0.321
# 
# Correlation of Fixed Effects:
#   (Intr) TrnnD2 TrnnD3 TrnnD4 TypOfT TD2:TO TD3:TO
# TranngDydy2 -0.436                                          
# TranngDydy3 -0.438  0.497                                   
# TranngDydy4 -0.438  0.497  0.505                            
# TypOfTrnngh -0.685  0.299  0.300  0.300                     
# TrnngD2:TOT  0.299 -0.687 -0.342 -0.342 -0.434              
# TrnngD3:TOT  0.302 -0.343 -0.689 -0.348 -0.435  0.499       
# TrnngD4:TOT  0.302 -0.343 -0.348 -0.689 -0.435  0.499  0.502

powerTransform(AllDayScores$Score)
# Estimated transformation parameter 
# AllDayScores$Score 
# 6.00126   

DayScores <- densityPlot(AllDayScores$Score^6.001)

source('pm_rn_transform.R')

DayScores_rn <- densityPlot(rn_transform(AllDayScores$Score))

AllDayScores$Score.rn = rn_transform(AllDayScores$Score)

summary(lmm1.rn <- lmer(Score.rn ~ TrainingDay * TypeOfTraining + 
                          (1|ParticipantID), 
                        data=AllDayScores), cor=FALSE)

# Linear mixed model fit by REML ['lmerMod']
# Formula: Score.rn ~ TrainingDay * TypeOfTraining + (1 | ParticipantID)
# Data: AllDayScores
# 
# REML criterion at convergence: 778.9
# 
# Scaled residuals: 
#   Min       1Q   Median       3Q      Max 
# -2.02914 -0.58240 -0.06496  0.50674  2.71006 
# 
# Random effects:
#   Groups        Name        Variance Std.Dev.
# ParticipantID (Intercept) 0.7479   0.8648  
# Residual                  0.5302   0.7281  
# Number of obs: 288, groups:  ParticipantID, 73
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)                      -0.07069    0.18238  -0.388
# TrainingDayday2                   0.32029    0.16705   1.917
# TrainingDayday3                  -0.13749    0.16787  -0.819
# TrainingDayday4                   0.18849    0.16787   1.123
# TypeOfTraininghk                  0.28143    0.26618   1.057
# TrainingDayday2:TypeOfTraininghk -0.18528    0.24309  -0.762
# TrainingDayday3:TypeOfTraininghk -0.18284    0.24366  -0.750
# TrainingDayday4:TypeOfTraininghk  0.03001    0.24366   0.123


require(brms)
load('brm1Days.rda')

# summary(brm1Days.rn <- brm(Score.rn ~ TrainingDay * TypeOfTraining + 
#                          (1|ParticipantID), 
#                        data=AllDayScores))
# save(brm1Days.rn, file = 'brm1Days.rda')


# Family: gaussian 
# Links: mu = identity; sigma = identity 
# Formula: Score.rn ~ TrainingDay * TypeOfTraining + (1 | ParticipantID) 
# Data: AllDayScores (Number of observations: 288) 
# Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
# total post-warmup draws = 4000
# 
# Group-Level Effects: 
#   ~ParticipantID (Number of levels: 73) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.88      0.09     0.72     1.07 1.00     1119     2101
# 
# Population-Level Effects: 
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                           -0.07      0.19    -0.44     0.30 1.00     1176     1808
# TrainingDayday2                      0.31      0.17    -0.02     0.65 1.00     2114     2429
# TrainingDayday3                     -0.14      0.17    -0.48     0.19 1.00     2200     2748
# TrainingDayday4                      0.18      0.17    -0.15     0.52 1.00     2271     2691
# TypeOfTraininghk                     0.27      0.27    -0.28     0.80 1.00     1135     1984
# TrainingDayday2:TypeOfTraininghk    -0.18      0.25    -0.66     0.32 1.00     2279     2854
# TrainingDayday3:TypeOfTraininghk    -0.18      0.24    -0.66     0.29 1.00     2155     2915
# TrainingDayday4:TypeOfTraininghk     0.04      0.25    -0.45     0.52 1.00     2137     2768
# 
# Family Specific Parameters: 
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sigma     0.73      0.04     0.67     0.81 1.00     3470     3180
# 
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).


conditional_effects(brm1Days.rn, 'TrainingDay:TypeOfTraining')
png('TrainingDayTypeOfTraining.png', he=5, wi=15, units='in', res=300)
dev.off()


hy1 = 'TrainingDayday2 > TrainingDayday3'
hypothesis(brm1Days.rn, hy1)

# Hypothesis Tests for class b:
#   Hypothesis            Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
# 1 (TrainingDayday2)... > 0     0.46      0.17     0.18     0.73     265.67         1    *


hy2 = 'Intercept > TrainingDayday4' ## If I understand correctly this checks whether there is a significant difference between Day 1 and Day 4 but that would only be for SR, right? 
hypothesis(brm1Days.rn, hy2)

# Hypothesis Tests for class b:
#   Hypothesis                Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
# 1 (Intercept)-(Trai... > 0    -0.26      0.31    -0.77     0.26       0.24      0.19     

#### @Petar pls test this: "The drop from Day 2 to Day 3 at least must be significant …. 
# But I’d also like to report whether what we see on the other days is significant both within group, 
# day by day and between groups day by day. In particular: they seem to be significantly different 
# from each other at the end of Day 1, and then again at the end of Day 4, but is Day 1 different from Day 4?"


