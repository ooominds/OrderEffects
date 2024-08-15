
load('HardData.rda')

# dataset is small and results are very skewed;
# we am going to use log-linear models

########################
### log-linear model ###
########################

freqdat = data.frame(tabdat <- xtabs(~
    Match +
    TestType +
    TypeOfTraining +
    Text,
    data=HardData))
ftable(tabdat)

# no effect; i.e., independence
ll.indep = loglin(tabdat, list(c(1), c(2,3,4)), param=TRUE, fit=TRUE)
1 - pchisq(ll.indep$lrt, ll.indep$df)
# [1] 0

# TestType effect only
ll.TestType = loglin(tabdat, list(c(1,2), c(2,3,4)), param=TRUE, fit=TRUE)
1 - pchisq(ll.TestType$lrt, ll.TestType$df)
# [1] 0

# TypeOfTraining effect only
ll.TypeOfTraining = loglin(tabdat, list(c(1,3), c(2,3,4)), param=TRUE, fit=TRUE)
1 - pchisq(ll.TypeOfTraining$lrt, ll.TypeOfTraining$df)
# [1] 3.907985e-14

# both TestType and TypeOfTraining, but not joint
ll.twoDirect = loglin(tabdat, list(c(1,2), c(1,3), c(2,3,4)), param=TRUE, fit=TRUE)
1 - pchisq(ll.twoDirect$lrt, ll.twoDirect$df)
# [1] 2.542411e-14

# joint effect of TestType and TypeOfTraining
ll.twoJoint = loglin(tabdat, list(c(1,2,3), c(2,3,4)), param=TRUE, fit=TRUE)
1 - pchisq(ll.twoJoint$lrt, ll.twoJoint$df)
# [1] 1.74305e-14

# Text effect only
ll.Text = loglin(tabdat, list(c(1,4), c(2,3,4)), param=TRUE, fit=TRUE)
1 - pchisq(ll.Text$lrt, ll.Text$df)
# [1] 0.002801633

# both TestType and Text, but not joint
ll.twoDirectB = loglin(tabdat, list(c(1,2), c(1,4), c(2,3,4)), param=TRUE, fit=TRUE)
1 - pchisq(ll.twoDirectB$lrt, ll.twoDirectB$df)
# [1] 0.002368087

# both TypeOfTraining and Text, but not joint
ll.twoDirectC = loglin(tabdat, list(c(1,3), c(1,4), c(2,3,4)), param=TRUE, fit=TRUE)
1 - pchisq(ll.twoDirectC$lrt, ll.twoDirectC$df)
# [1] 0.3432132

# all three, direct
ll.threeDirect = loglin(tabdat, list(c(1,2), c(1,3), c(1,4), c(2,3,4)), param=TRUE, fit=TRUE)
1 - pchisq(ll.threeDirect$lrt, ll.threeDirect$df)
# [1] 0.3293712
1 - pchisq(abs(ll.threeDirect$lrt-ll.twoDirectC$lrt),
    abs(ll.threeDirect$df-ll.twoDirectC$df))
# [1] 0.3856872


