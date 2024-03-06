GARCH11_LIKELIHOOD = -2867.938
ARCH1_LIKELIHOOD = -2783.608

LR = 2*abs(ARCH1_LIKELIHOOD - GARCH11_LIKELIHOOD)
cat("Chi-square statistics = ", LR, "\n")
cat("P-Value: ", 1 - pchisq(LR, df = 1), 
    " Therefore, we have enough evidence to accecpt the hypothesis that beta != 0")