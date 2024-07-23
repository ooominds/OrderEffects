# OrderEffects
This contains the code for the paper Order effects in L2 learning

This is the readme file for the data and code associated with the MS "Order effects in L2"

This folder contains:

R files: 
- OrderHardItems.r: the code for the analysis of the effects of order of exposure (type of training) with a focus on the difficulty level of the items (based on pre- and post-tests results). Goes with OderHardItems.xlsx
- RROrder.r: the code for the overall analysis of pre- and post-test results depending on training and participants. Goes with PrePostDelayedTestsAnswers.xlsx
- ControlGroupTest.r: the code that checks our control group's results against the treatment groups'. Goes with PrePostTreatmentControl.xlsx 
- TrainingDays.r : the code for the analysis of participants' performance on the four days of training. Goes with training_4days_data.xlsx

Data files:
- OderHardItems.xlsx: the data file to be used with OrderHardItems.r
- PrePostDelayedTestsAnswers.xlsx: the data file needed to run  RROrder.r
- PrePostTreatmentControl.xlsx: the data file with the control group's results, to be used with ControlGroupTest.r 
- training_4days_data.xlsx: data file containing participants' answers for each day of training and correct answers for each item, to be used with TrainingDays.r 
- PrePostStimuliForMerging.xlsx: stimuli for pre-, post- and delayed post-tests with level of difficulty for each item
- performance_on_seen_items.csv: participants' scores on previously seen stimuli

RDA files:
- brm1.rn.rda: Bayesian analysis of pre, post, and delayed post-tests results for treatment groups
- brm1C.rn.rda: Bayesian analysis pre and post-tests results for control and treatment groups
- brm1Days.rda: Bayesian analysis of individual training days
