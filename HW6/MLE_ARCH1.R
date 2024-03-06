df = read.csv("C:/min/coding_project/FIN537_Risk_Management/HW6/ETFreturns.csv")
log_ret = log(df$XLF + 1)[4788:5787]
cal_ARCH1_log_likelihood <- function(log_ret, alpha, sigma, sigma1){
  T = length(log_ret)
  total = -0.5*log(sigma1^2) - 0.5 * (log_ret[1] / sigma1)^2
  prev_sigma = sigma1
  for (x in 2:T) {
    sigma_t = ((1 - alpha) * sigma^2 + alpha * log_ret[x-1]^2)^0.5
    total = total -0.5*log(sigma_t^2) - 0.5 * (log_ret[x] / sigma_t)^2
    prev_sigma = sigma_t
  }
  total = total - T * 0.5 * log(2*pi)
  return(-total)
}

init_alpha = 0.1
init_sigma = 0.01
init_sigma1 = 0.01

initital_guess = c(init_alpha, init_sigma, init_sigma1)
objective_func = function(x) cal_ARCH1_log_likelihood(log_ret, x[1], x[2], x[3])
optim(fn=objective_func, par=initital_guess, method = "L-BFGS-B", 
      lower = c(0., 0.00001, 0.00001), upper = c(0.99999, 1., 1.))