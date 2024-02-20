library(AmericanCallOpt)

prices = read.csv("C:/min/coding_project/FIN537_Risk_Management/HW4/ETFprices.csv")
vols = read.csv("C:/min/coding_project/FIN537_Risk_Management/HW4/ETFATMvols.csv")
returns = read.csv("C:/min/coding_project/FIN537_Risk_Management/HW4/ETFreturns.csv")

df = merge(merge(returns, vols, by="Date"), prices, by="Date")[750:1250,]

r = 0.0485
t = 61/252
div_t = 1/252
steps= 100

td_xle = 84.36
td_xle_sigma = 0.317964
xle_div = 0.85963
xle_div_frac = xle_div/td_xle
xle_k = 84

td_xle_amer_call = am_call_bin_propdiv(td_xle, xle_k, r, td_xle_sigma, t, steps,
                                    div_t, xle_div_frac)
cat("today xle_amer_call:", td_xle_amer_call)

td_xlf = 33.70
td_xlf_sigma = 0.227176
xlf_div = 0.20962
xlf_div_frac = xlf_div/td_xlf
xlf_k = 34

td_xlf_amer_call = am_call_bin_propdiv(td_xlf, xlf_k, r, td_xlf_sigma, t, steps,
                                    div_t, xlf_div_frac)
cat("today xlf_amer_call:", td_xlf_amer_call)


td_xly = 133.90
td_xly_sigma = 0.290478
xly_div = 0.36581
xly_div_frac = xly_div/td_xly
xly_k = 134
td_xly_amer_call = am_call_bin_propdiv(td_xly, xly_k, r, td_xly_sigma, t, steps,
                                    div_t, xly_div_frac)
cat("today xly_amer_call:", td_xly_amer_call)

df$log_ret_xle = log(1 + df$XLE.x)
df$log_ret_xlf = log(1 + df$XLF.x)
df$log_ret_xly = log(1 + df$XLY.x)

vol_diff_xle = diff(log(df$XLE.y), lag=1)
vol_diff_xlf = diff(log(df$XLF.y), lag=1)
vol_diff_xly = diff(log(df$XLY.y), lag=1)
df = df[2:501,]
df$log_vol_xle = vol_diff_xle
df$log_vol_xlf = vol_diff_xlf
df$log_vol_xly = vol_diff_xly


new_t = 60/252

td_portfolio_value = td_xle * 30000 + td_xlf * 60000 + td_xly * 10000 - td_xle_amer_call * 30000 - td_xlf_amer_call * 60000 - td_xly_amer_call * 10000
  
PandL_values = rep(-1, nrow(df))

for (i in 1:nrow(df)){
  tmp_new_xle = td_xle * exp(df[i, "log_ret_xle"])
  tmp_new_xlf = td_xlf * exp(df[i, "log_ret_xlf"])
  tmp_new_xly = td_xly * exp(df[i, "log_ret_xly"])
  
  tmp_new_vol_xle = td_xle_sigma * exp(df[i, "log_vol_xle"])
  tmp_new_vol_xlf = td_xlf_sigma * exp(df[i, "log_vol_xlf"])
  tmp_new_vol_xly = td_xly_sigma * exp(df[i, "log_vol_xly"])
  
  new_xle_amer_call = am_call_bin_propdiv(tmp_new_xle, xle_k, r, tmp_new_vol_xle, new_t, steps, NULL, NULL)
  new_xlf_amer_call = am_call_bin_propdiv(tmp_new_xlf, xlf_k, r, tmp_new_vol_xlf, new_t, steps, NULL, NULL)
  new_xly_amer_call = am_call_bin_propdiv(tmp_new_xly, xly_k, r, tmp_new_vol_xly, new_t, steps, NULL, NULL)
  
  new_portfolio_value = tmp_new_xle * 30000 + tmp_new_xlf * 60000 + tmp_new_xly * 10000 - new_xle_amer_call * 30000 - new_xlf_amer_call * 60000 - new_xly_amer_call * 10000
  PandL_values[i] = new_portfolio_value - td_portfolio_value
}

cat("VaR @ 5% =", -quantile(PandL_values, 0.05))