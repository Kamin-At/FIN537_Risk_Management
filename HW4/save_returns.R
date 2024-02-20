library(AmericanCallOpt)

prices = read.csv("C:/min/coding_project/FIN537_Risk_Management/HW4/ETFprices.csv")
vols = read.csv("C:/min/coding_project/FIN537_Risk_Management/HW4/ETFATMvols.csv")
returns = read.csv("C:/min/coding_project/FIN537_Risk_Management/HW4/ETFreturns.csv")

df = merge(merge(returns, vols, by="Date"), prices, by="Date")[750:1250,]

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

write.csv(df, "C:/min/coding_project/FIN537_Risk_Management/HW4/returns.csv")