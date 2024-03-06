GARCH11_LIKELIHOOD = -2867.938
NGARCH11_LIKELIHOOD = -2887.854

LR = 2*abs(NGARCH11_LIKELIHOOD - GARCH11_LIKELIHOOD)
cat("Chi-square statistics = ", LR, "\n")
cat("P-Value: ", 1 - pchisq(LR, df = 1), 
    " Therefore, we have enough evidence to accecpt the hypothesis that theta != 0")