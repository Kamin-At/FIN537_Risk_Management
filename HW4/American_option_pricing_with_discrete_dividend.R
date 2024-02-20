library(AmericanCallOpt)

r = 0.0485
t = 61/252
div_t = 1/252
steps= 1000

xle = 84.36
xle_sigma = 0.317964
xle_div = 0.85963
xle_div_frac = xle_div/xle
xle_k = 84

xle_amer_call = am_call_bin_propdiv(xle, xle_k, r, xle_sigma, t, steps,
                                            div_t, xle_div_frac)
cat("xle_amer_call:", xle_amer_call)

xlf = 33.70
xlf_sigma = 0.227176
xlf_div = 0.20962
xlf_div_frac = xlf_div/xlf
xlf_k = 34

xlf_amer_call = am_call_bin_propdiv(xlf, xlf_k, r, xlf_sigma, t, steps,
                                    div_t, xlf_div_frac)
cat("xlf_amer_call:", xlf_amer_call)


xly = 133.90
xly_sigma = 0.290478
xly_div = 0.36581
xly_div_frac = xly_div/xly
xly_k = 134
xly_amer_call = am_call_bin_propdiv(xly, xly_k, r, xly_sigma, t, steps,
                                    div_t, xly_div_frac)
cat("xly_amer_call:", xly_amer_call)