library("fGarch")
df = read.csv("C:/min/coding_project/FIN537_Risk_Management/HW6/ETFreturns.csv")
log_ret = log(df$XLF + 1)[4788:5787]
garchFit(formula = ~ garch(1, 1), log_ret, include.mean= FALSE)