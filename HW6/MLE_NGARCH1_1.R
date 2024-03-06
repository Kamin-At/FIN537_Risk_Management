df = read.csv("C:/min/coding_project/FIN537_Risk_Management/HW6/ETFreturns.csv")
log_ret = log(df$XLF + 1)[4788:5787]
cal_NGARCH11_log_likelihood <- function(log_ret, alpha, beta, sigma, sigma1, theta){
  if (alpha * (1 + theta^2) + beta >= 1.||alpha<0.||beta<0||sigma<0.001||sigma1<0.001){
    return(10000000)
  }
  T = length(log_ret)
  total = -0.5*log(sigma1^2) - 0.5 * (log_ret[1] / sigma1)^2
  prev_sigma = sigma1
  for (x in 2:T) {
    sigma_t = ((1 - alpha * (1 + theta^2) - beta) * sigma ^ 2 + alpha * (log_ret[x-1] - theta * prev_sigma)^2 + beta * prev_sigma ^ 2) ^ 0.5
    total = total -0.5*log(sigma_t^2) - 0.5 * (log_ret[x] / sigma_t)^2
    prev_sigma = sigma_t
  }
  total = total - T * 0.5 * log(2*pi)
  return(-total)
}

init_alpha = 0.1
init_beta = 0.8
init_sigma = 0.03
init_sigma1 = 0.04
init_theta = 0.5

initital_guess = c(init_alpha, init_beta, init_sigma, init_sigma1, init_theta)
objective_func = function(x) cal_NGARCH11_log_likelihood(log_ret, x[1], x[2], x[3], x[4], x[5])
optim(fn=objective_func, par=initital_guess)