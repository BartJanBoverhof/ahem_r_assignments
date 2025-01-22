
# https://www.math.arizona.edu/~rsims/ma464/standardnormaltable.pdf

# Extract the results for OS Mito
l_OS_mito <- l_results_all$`OS mito`

t_weeks <- 15           # number of weeks
t <- (t_weeks/ 52) * 12 # time in months
cat(t_weeks, "weeks is the same as", round(t, 3), "months", "\n")


### Exponential ### 
# Function to calculate survival probability using the Exponential distribution
exponential_survival <- function(t, intercept) {
  # t: time point in months at which the survival probability is estimated
  # intercept: estimate of the intercept for the survival curve (from model fit)
  
  # Calculate lambda (scale parameter) based on the intercept value.
  # Here, lambda is the exponential of the negative of the intercept.
  lambda <- exp(-intercept)  # Scale parameter for the Exponential distribution
  
  # Calculate the survival probability at time t using the Exponential survival function.
  S_t <- exp(-t * lambda)
  
  return(S_t)   # Return the calculated survival probability at time t
}

# Calculation for exponential distribution
intercept_exp <- l_OS_mito$exponential$intercept
p_survival_exp <- exponential_survival(t, intercept_exp)
cat("Survival probability (Exponential) at: ", round(52/t), "weeks :", p_survival_exp, "\n")

### Weibull ### 

# Function to calculate survival probability using the Weibull distribution
weibull_survival <- function(t, intercept, scale) {
  # t: time point at which the survival probability is estimated
  # intercept: estimate of the intercept for the survival curve (from model fit)
  # scale: scale parameter for the Weibull distribution, typically derived from the fitted model.
  #        Note: scale = exp(logscale), where logscale is the log-transformed scale.
  
  # Calculate the lambda (scale parameter) using the intercept and scale value.
  lambda <- exp(-intercept / scale)
  
  # Calculate the gamma (shape parameter) based on the scale.
  gamma <- 1 / scale
  
  # Calculate the survival probability at time t using the Weibull survival function.
  S_t <- exp(-lambda * t ^ gamma)
  
  return(S_t)  # Return the calculated survival probability at time t
}

intercept_weibull <- l_OS_mito$weibull$intercept
scale_weibull     <- exp(l_OS_mito$weibull$log_scale)
p_survival_weibull <- weibull_survival(t, intercept = intercept_weibull, scale = scale_weibull)


cat("Survival probability (Weibull) at time", t, "weeks :", p_survival_weibull, "\n")
# Weibull


### Lognormal ###

# Function to calculate survival probability using the Log-Normal distribution
lognormal_survival <- function(t, intercept, scale) {
  # t: time point at which the survival probability is estimated
  # intercept: estimate of the intercept for the log-normal curve (related to mu)
  # scale: scale parameter for the log-normal curve (related to sigma)
  
  # Calculate mu (mean) and sigma (standard deviation) for the normal distribution
  mu    <- intercept  # Intercept corresponds to the mean (mu) of the underlying normal distribution
  sigma <- scale   # Scale corresponds to the standard deviation (sigma)
  
  # Calculate the survival probability at time t using the log-normal survival function
  # Formula: S(t) = 1 - Phi( (ln(t) - mu) / sigma )
  survival_prob <- 1 - pnorm((log(t) - mu) / sigma)
  
  # Return the survival probability at time t
  return(survival_prob)
}

mu <- intercept_lognormal<- l_OS_mito$lognormal$intercept
sigma <- scale_lognormal    <- exp(l_OS_mito$lognormal$log_scale)


p_survival_lognormal <- lognormal_survival(t, intercept = intercept_lognormal, scale = scale_lognormal) 
  
cat("Survival probability (Lognormal) at time", t, "weeks :", p_survival_lognormal, "\n")



### Loglogistic ### 
# Function to calculate survival probability using the Log-Logistic distribution
loglogistic_survival <- function(t, intercept, scale) {
  # t:         time point at which the survival probability is estimated
  # intercept: estimate of the intercept for the survival curve (from model fit)
  # scale:     scale parameter for the Log-Logistic distribution (typically derived from the fitted model)
  
  # Calculate lambda (scale parameter) using the intercept and scale
  lambda <- exp(-intercept / scale)
  
  # Calculate gamma (shape parameter) as the inverse of the scale
  gamma <- 1 / scale
  
  # Calculate the survival probability at time t using the Log-Logistic survival function
  # The formula is: S(t) = 1 / (1 + lambda * t^gamma)
  S_t <- 1 / (1 + lambda * t^gamma)
  
  # Return the survival probability at time t
  return(S_t)
}

#  for Log-Logistic distribution
intercept_loglogistic <- l_OS_mito$loglogistic$intercept 
scale_loglogistic     <- exp(l_OS_mito$loglogistic$log_scale)

# Calculate survival probability at time_point using Log-Logistic distribution
p_survival_loglogistic <- loglogistic_survival(t, intercept = intercept_loglogistic, scale = scale_loglogistic)
cat("Survival probability (Loglogistic) at time", t, "weeks :", p_survival_loglogistic, "\n")

## Summary ##

# Combine and display results
v_survival_props <- round(c(p_survival_exp, p_survival_weibull, p_survival_lognormal, p_survival_loglogistic), 4)
names(v_survival_props) <- c("exponential", "weibull", "lognormal", "loglogistic")
round(v_survival_props, 3) * 100

