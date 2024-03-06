library(tseries)
df = read.csv("C:/min/coding_project/FIN537_Risk_Management/HW6/ETFreturns.csv")
log_ret = log(df$XLF + 1)[4788:5787]

initial_guess = c(0.01714471^2, 0.18896015, 0.77782396)

model <- garch(log_ret, order = c(1,1), start=initial_guess)  # Fit GARCH(1,1) 
summary(model)                     # Diagnostic tests