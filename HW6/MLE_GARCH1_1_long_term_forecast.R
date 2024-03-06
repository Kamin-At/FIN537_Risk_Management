df = read.csv("C:/min/coding_project/FIN537_Risk_Management/HW6/ETFreturns.csv")
log_ret = log(df$XLF + 1)[4788:5787]
cal_GARCH11_log_likelihood <- function(log_ret, alpha, beta, sigma, sigma1){
  if (alpha + beta > 1.){
    return(10000000)
  }
  t = length(log_ret)
  total = -0.5*log(sigma1^2) - 0.5 * (log_ret[1] / sigma1)^2
  prev_sigma = sigma1
  for (x in 2:t) {
    sigma_t = ((1 - alpha - beta) * sigma^2 + alpha * log_ret[x-1]^2 + beta * prev_sigma ^ 2)^0.5
    total = total -0.5*log(sigma_t^2) - 0.5 * (log_ret[x] / sigma_t)^2
    prev_sigma = sigma_t
  }
  total = total - t * 0.5 * log(2*pi)
  return(-total)
}

init_alpha = 0.08645
init_beta = 0.8945
init_sigma = 0.013
init_sigma1 = 0.013

initital_guess = c(init_alpha, init_beta, init_sigma, init_sigma1)
objective_func = function(x) cal_GARCH11_log_likelihood(log_ret, x[1], x[2], x[3], x[4])
optimal_val = optim(fn=objective_func, par=initital_guess, method = "L-BFGS-B", lower = c(0., 0., 0.0001, 0.0001), upper = c(0.99, 0.99, 1., 1.))
t = length(log_ret)

future_sigmas = rep(0.,t)
optimal_alpha = optimal_val$par[1]
optimal_beta = optimal_val$par[2]
optimal_sigma = optimal_val$par[3]
optimal_sigma1 = optimal_val$par[4]

for (i in 1:t){
  if (i == 1){
    prev_sigma = optimal_sigma1
  }
  else{
    prev_sigma = future_sigmas[i-1]
  }
  future_sigmas[i] = ((1-optimal_alpha-optimal_beta)*optimal_sigma^2 + optimal_alpha * log_ret[i]^2 + optimal_beta * prev_sigma^2)^0.5
}
cat("predicted sigma: ", future_sigmas[t], " => Variance: ", future_sigmas[t]^2)

coeff = 0
n = 21
for (i in 1:n){
  coeff = coeff + (optimal_alpha + optimal_beta)^(i-1)
}
cat("21-day forecast of the Variance = ", 
    n*optimal_sigma^2 + (future_sigmas[t]^2-optimal_sigma^2)*coeff)
cat("The forecast of the Annualized Variance = ", 
    (n*optimal_sigma^2 + (future_sigmas[t]^2-optimal_sigma^2)*coeff)*252/21)