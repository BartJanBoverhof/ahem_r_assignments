library(ztable)

# Extract the results for OS Mito
l_OS_mito <- l_results_all$`OS mito`

# Exponential
lambda <- exp(-l_OS_mito$exponential$intercept)
t <- (16 / 52) * 12
p_S_16_exp <- exp(-lambda * t)

# Weibull
scale <- exp(l_OS_mito$weibull$log_scale)
gamma <- 1 / scale
lambda <- exp(-l_OS_mito$weibull$intercept / scale)
p_S_16_weibull <- exp(-lambda * t ^ gamma)

# Lognormal
lambda <- l_OS_mito$lognormal$intercept
gamma <- exp(l_OS_mito$lognormal$log_scale)
p_S_16_logn <- 1 - 0.07636  # Pre-calculated value from Z-table

# Loglogistic
scale <- exp(l_OS_mito$loglogistic$log_scale)
gamma <- 1 / scale
lambda <- exp(-l_OS_mito$loglogistic$intercept / scale)
p_S_16_logl <- 1 / (1 + lambda * t ^ gamma)

# Combine and display results
v_survival_props <- round(c(p_S_16_exp, p_S_16_weibull, p_S_16_logn, p_S_16_logl), 4)
names(v_survival_props) <- c("exponential", "weibull", "lognormal", "loglogistic")
v_survival_props