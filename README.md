# OrderEffects

<br>

This contains the code for the analyses reported in

> Romain, L., Milin, P., and Divjak, D. (2024). rder Effects in Second Language Learning. To appear in: *Language Learning*.

- `01_ControlGroupTest.R`: the code that checks our control group's results against the treatment groups'. Runs with `ControlGroupTest` datafile; `brm1C.rn.rda` and `summary.brm1C.rn.rda` contain the Bayesian regression model and its summary, respectively.
- `02_RROrder.R`: the code for the overall analysis of pre- and post-test results depending on training and participants. Runs with `AllTestScores` datafile. All models and their summaries are stored: `brm1.rn.rda` and `summary.brm1.rn.rda`, `summary.post.draws.rda`, `brm1.rn.withPriors.rda` and `summary.brm1.rn.withPriors.rda`.
- `03_HardItems.R`: the code for the analysis of the effects of order of exposure (type of training) with a focus on the difficulty level of the items (based on pre- and post-tests results). Runs with `HardData` datafile.
- `04_TrainingDays.R`: the code for the analysis of participants' performance on the four days of training. Runs with `AllDayScores` and `TrainingData` datafiles.
- `05_LearningRateEffect.jasp`: Estimates the Bayesian nonparametric correlations (Kendall's tau) using the *JASP* statistical program. Data is included in the package.

The data can be downloaded from The University of Birmingham Institutional Research Archive (UBIRA) at [https://edata.bham.ac.uk/1155/](https://edata.bham.ac.uk/1155/). All files are saved in `*.csv` and `*.rda` format.
- `ControlGroupTest`
- `AllTestScores`
- `HardData`
- `AllDayScores`
- `TrainingData`

- - -

For further inquiries contact: ooominds@ooominds.org
