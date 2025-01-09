# R script parameteric formula's
#install.packages("ztable")
library(ztable)

# Restore the object
readRDS(file = "l_results_all.rds")

# The calculations are for OS Mito
l_OS_mito <- l_results_all$`OS mito`

# Exponential
# lambda <- exp(-intercept)
lambda <- exp(-l_OS_mito$exponential$intercept) 
#S(16 weeks) = S((16/52)*12 months) = e^(-lambda*(16/52)*12))= 78.7%
p_S_16_exp <- exp(-lambda*(16/52)*12)
t <- (16/52) * 12
p_S_16_exp <- exp(-lambda * t)
p_S_16_exp # survival probability at week 16 using the exponential distribtuion



#Weibull:
scale <- exp(l_OS_mito$weibull$log_scale)
scale
#Gamma = 1 / scale
gamma <- 1 / scale
gamma # 1.626879
#Lambda= e^(-intercept/scale) = e^(-2.7515/0.614673774)= 0.011374263
lambda <- exp(-l_OS_mito$weibull$intercept/scale) 
# S(16 weeks) = S((16/52)*12 months) = e^(-lambda*((16/52)*12)^gamma)= 90.9%
p_S_16_weibull <- exp(-lambda * t ^ gamma)
p_S_16_weibull
  
#Logn: 
#Lambda = intercept = 2.4421
lambda <- l_OS_mito$lognormal$intercept  
# Gamma=scale=e^(-0.2315)
gamma = exp(l_OS_mito$lognormal$log_scale) # scale comes in log scale, therefore the exponential
# S(16 weeks) = S((16/52)*12 months) =1- ɸ ((ln((16/52)*12)-lambda)/gamma)=1- ɸ(-1.4317)=1-0.07636=%92.4
#p_S_16_logn <- 1 - ɸ (log(t) - lambda) / gamma)
(log(t) - lambda) / gamma
# -1.43173 gives the following value from the Z-table = 0.07636 (column 0.03)
# https://math.arizona.edu/~rsims/ma464/standardnormaltable.pdf
# 1- ɸ(-1.4317)=
p_S_16_logn <- 1 - 0.07636 # =%92.4


#Logl:
#scale=e^(logscale) = 0.444
scale <- exp(l_OS_mito$loglogistic$log_scale)  
scale
#gamma= 1/scale = 2.252
gamma <- 1/scale 
gamma
# Lambda= e^(-intercept/scale)= e^(-2.4736/0.444)= 0.003807
lambda <- exp(-l_OS_mito$loglogistic$intercept/scale)
lambda
#  S(16 weeks) = S((16/52)*12 months) = 1/(1+lambda*t^gamma))=93.3%
p_S_16_logl <- 1/(1 + lambda * t ^ gamma)

  
# show all values
v_survival_props <- round(c(p_S_16_exp, p_S_16_weibull, p_S_16_logn, p_S_16_logl), 4)
names(v_survival_props) <- c("exponential", "weibull", "lognormal", "loglogistic")
v_survival_props
