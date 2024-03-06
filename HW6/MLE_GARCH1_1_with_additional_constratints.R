df = read.csv("C:/min/coding_project/FIN537_Risk_Management/HW6/ETFreturns.csv")
log_ret = log(df$XLF + 1)[4788:5787]
cal_GARCH11_log_likelihood <- function(log_ret, alpha, beta, sigma, sigma1){
  if (alpha + beta > 1.){
    return(10000000)
  }
  
  
  T = length(log_ret)
  total = -0.5*log(sigma1^2) - 0.5 * (log_ret[1] / sigma1)^2
  prev_sigma = sigma1
  for (x in 2:T) {
    sigma_t = ((1 - alpha - beta) * sigma^2 + alpha * log_ret[x-1]^2 + beta * prev_sigma ^ 2)^0.5
    total = total -0.5*log(sigma_t^2) - 0.5 * (log_ret[x] / sigma_t)^2
    prev_sigma = sigma_t
    
  }
  total = total - T * 0.5 * log(2*pi)
  
  return(-total)
}

init_alpha = 0.08645
init_beta = 0.8945
init_sigma = sd(log_ret)
init_sigma1 = sd(log_ret)

initital_guess = c(init_alpha, init_beta)
objective_func = function(x) cal_GARCH11_log_likelihood(log_ret, x[1], x[2], init_sigma, init_sigma1)
optim(fn=objective_func, par=initital_guess, method = "L-BFGS-B", lower = c(0.001, 0.001), upper = c(0.9999, 0.9999))
cat("used sigma:", init_sigma, " used sigma1:", init_sigma1)