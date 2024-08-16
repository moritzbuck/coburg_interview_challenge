# Load packages

library(ggplot2)
library(data.table)
library(maps)
library(tidyr)
library(dplyr)
library(ggpubr)
library(farff)
library(pheatmap)

if (!require("farff", quietly = TRUE))
    install.packages("farff")


# Load data

data = as.data.table(readARFF("data/freMTPL2freq.arff"))
var = as.data.table(readARFF("data/freMTPL2sev.arff"))

# joining and summarizing

svar = var[, .(total_claim_amount = sum(ClaimAmount), avg_claim_amount = mean(ClaimAmount), nb_claims = length(ClaimAmount)),  by = IDpol]

fulldata = data %>% left_join(svar, by = c('IDpol' = 'IDpol'))
fulldata[is.na(total_claim_amount) , total_claim_amount := 0]
fulldata[is.na(avg_claim_amount) , avg_claim_amount := 0]
fulldata[is.na(nb_claims) , nb_claims := 0]

#  checking for autocorrelation

cont_columns = c("ClaimNb", "Exposure", "VehPower", "VehAge", "DrivAge", "BonusMalus", "Density", "freq_claim", "total_claim_amount", "avg_claim_amount", "nb_claims")
cat_cols = setdiff(colnames(fulldata) , cont_columns) [2:5]

pheatmap(cor(fulldata[,..columns]))
#
# 



by_region = fulldata[,.(avg_claim_amount = mean(avg_claim_amount), avg_nb_claims = mean(nb_claim_amount), mean_claim_freq = mean(freq_claim), mean_bnonus_malus = mean(BonusMalus), mean_driver_age = mean(DrivAge), mean_veh_age = mean(VehAge), mean_veh_power = mean(VehPower), mean_density = mean(Density)) , by = Region]
by_region[, Region := sub("R","", Region)]

census_data = as.data.table(read.csv('data/census_data.csv', header = TRUE, sep = ','))
by_region = by_region %>% left_join(census_data, by = c('Region' = 'code'))


maps_facts = c("avg_claim_amount", "avg_nb_claims", "mean_claim_freq", "mean_bnonus_malus", "mean_driver_age", "mean_veh_age", "mean_veh_power", "mean_density")

maps = lapply(maps_facts, function(x) map_data('france') %>%
  left_join(by_region, by = c('region' = 'name')) %>%
  ggplot(aes_string(x = "long", y = "lat", group = "group", fill = x)) +
  geom_polygon(color = '#555555', linewidth = .25) +
  coord_map() +
  scale_fill_gradient(low = "white", high = '#00a67d', na.value="#999999")+
  theme_void()+ggtitle(x)
  )

ggarrange(plotlist = maps, ncol=4, nrow=2)

bar_facts = c("Area", "VehBrand", "VehGas")

bars = lapply(bar_facts, function(x)
     ggplot(fulldata[total_claim_amount > 0], aes_string(x=x, y="total_claim_amount"))+geom_boxplot()+scale_y_log10()+ggtitle(x)
     )

ggarrange(plotlist = bars, ncol=3, nrow=1)

xy_facts = c("VehPower", "VehAge", "DrivAge")

xys = lapply(xy_facts, function(x)
     ggplot(fulldata[total_claim_amount > 0], aes_string(x=x, y="total_claim_amount"))+geom_point()+scale_y_log10()+geom_smooth(col="red")+ggtitle(x)
     )

ggarrange(plotlist = xys, ncol=3, nrow=1)

ggplot(fulldata[total_claim_amount >0], aes(x=total_claim_amount/Exposure, y=..density..))+geom_histogram(bins=100, fill="grey", alpha=0.7)+geom_density(col="red")+scale_x_log10()
ggplot(fulldata[total_claim_amount >0], aes(x=avg_claim_amount, y=..density..))+geom_histogram(bins=100, fill="grey", alpha=0.7)+geom_density(col="red")+scale_x_log10()
ggplot(fulldata[total_claim_amount >0], aes(x=freq_claim))+geom_histogram(bins=100, fill="grey", alpha=0.7)+geom_density(col="red")+scale_x_log10()

sub_data = fulldata[Exposure > 0.9]
sub_data[,claims_per_year := nb_claim_amount/Exposure]

modelling_frequency = glm.nb(nb_claim_amount ~ Area+VehPower+VehGas+Region+VehAge+DrivAge+VehBrand,data=sub_data)

# > modelling_frequency = glm.nb(nb_claim_amount/Exposure ~ Density+Area+VehPower+VehGas+Region+VehAge+DrivAge+VehBrand,data=sub_data)
# There were 50 or more warnings (use warnings() to see the first 50)
# > summary(modelling_frequency)
# Call:
# glm.nb(formula = nb_claim_amount/Exposure ~ Density + Area + 
#     VehPower + VehGas + Region + VehAge + DrivAge + VehBrand, 
#     data = sub_data, init.theta = 0.07454252271, link = log)
# Coefficients:
#                 Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   -2.505e+00  5.877e-02 -42.623  < 2e-16 ***
# Density        4.864e-06  5.871e-06   0.828 0.407403    
# AreaB          1.052e-01  3.268e-02   3.218 0.001292 ** 
# AreaC          1.992e-01  2.716e-02   7.333 2.24e-13 ***
# AreaD          4.008e-01  2.937e-02  13.647  < 2e-16 ***
# AreaE          4.994e-01  3.952e-02  12.637  < 2e-16 ***
# AreaF          4.910e-01  1.420e-01   3.457 0.000546 ***
# VehPower       2.426e-02  4.253e-03   5.704 1.17e-08 ***
# VehGasRegular -1.375e-01  1.665e-02  -8.254  < 2e-16 ***
# RegionR21      4.815e-02  1.385e-01   0.348 0.728131    
# RegionR22      1.631e-01  8.122e-02   2.009 0.044590 *  
# RegionR23     -1.427e-01  9.541e-02  -1.495 0.134788    
# RegionR24     -5.118e-02  3.856e-02  -1.327 0.184384    
# RegionR25     -7.627e-02  6.918e-02  -1.102 0.270298    
# RegionR26      9.708e-03  7.666e-02   0.127 0.899226    
# RegionR31     -1.336e-02  5.380e-02  -0.248 0.803927    
# RegionR41     -2.251e-01  6.656e-02  -3.382 0.000720 ***
# RegionR42     -1.528e-01  1.453e-01  -1.052 0.292930    
# RegionR43     -6.787e-02  2.068e-01  -0.328 0.742743    
# RegionR52     -5.956e-02  4.689e-02  -1.270 0.204029    
# RegionR53     -7.319e-02  4.542e-02  -1.611 0.107102    
# RegionR54      6.594e-02  5.726e-02   1.152 0.249508    
# RegionR72      3.809e-02  5.101e-02   0.747 0.455276    
# RegionR73     -2.943e-01  6.882e-02  -4.277 1.90e-05 ***
# RegionR74      2.638e-01  1.008e-01   2.617 0.008879 ** 
# RegionR82      1.476e-01  3.826e-02   3.857 0.000115 ***
# RegionR83     -4.333e-03  1.038e-01  -0.042 0.966701    
# RegionR91      2.568e-02  5.091e-02   0.505 0.613893    
# RegionR93      7.587e-02  3.946e-02   1.923 0.054519 .  
# RegionR94      1.611e-01  1.130e-01   1.425 0.154035    
# VehAge        -1.252e-02  1.641e-03  -7.630 2.35e-14 ***
# DrivAge       -7.322e-03  5.794e-04 -12.639  < 2e-16 ***
# VehBrandB10   -6.508e-02  5.255e-02  -1.239 0.215521    
# VehBrandB11    1.030e-01  5.781e-02   1.781 0.074907 .  
# VehBrandB12   -3.273e-01  2.776e-02 -11.788  < 2e-16 ***
# VehBrandB13    1.331e-02  5.995e-02   0.222 0.824304    
# VehBrandB14   -2.919e-01  1.102e-01  -2.648 0.008091 ** 
# VehBrandB2    -1.280e-02  2.253e-02  -0.568 0.569933    
# VehBrandB3     3.636e-02  3.202e-02   1.136 0.256118    
# VehBrandB4     2.167e-02  4.356e-02   0.497 0.618904    
# VehBrandB5     8.981e-02  3.704e-02   2.425 0.015321 *  
# VehBrandB6     2.385e-02  4.130e-02   0.578 0.563547    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# (Dispersion parameter for Negative Binomial(0.0745) family taken to be 1)
#     Null deviance: 75556  on 428072  degrees of freedom
# Residual deviance: 74259  on 428031  degrees of freedom
# AIC: 207282
# Number of Fisher Scoring iterations: 1
#               Theta:  0.07454 
#           Std. Err.:  0.00101 
#  2 x log-likelihood:  -207196.45700 

m2 <- update(modelling_frequency, . ~ . - Density)

m3 <- update(m2, . ~ . - Area)
m3bis <- update(m2, . ~ . - Region)

mPoisson <- glm(nb_claim_amount/Exposure ~ Area+VehPower+VehGas+Region+VehAge+DrivAge+VehBrand, family = "poisson", data = sub_data)
pchisq(2 * (logLik(m2) - logLik(mPoisson)), df = 1, lower.tail = FALSE)

(est <- cbind(Estimate = coef(m2), confint(m2)))


fitted.results <- predict(modelling_frequency,newdata=sub_data,type='response')
fiti = data.table(fitted = fitted.results, measured = sub_data$nb_claim_amount)
sqrt(sum((fiti$measured-fiti$fitted)**2))

modelling_damageprice = glm(avg_claim_amount ~ Density+Area+BonusMalus+VehPower+VehGas+Region+VehAge+DrivAge+VehBrand,family=gaussian(link = "logit"),data=sub_data[nb_claim_amount >0])
anova(modelling_damageprice)
fitted_cost <- predict(modelling_damageprice,newdata=sub_data,type='response')

#Analysis of Deviance Table
#Model: gaussian, link: identity
#Response: avg_claim_amount
#Terms added sequentially (first to last)
#           Df   Deviance Resid. Df Resid. Dev        F    Pr(>F)    
#NULL                        428072 3.9873e+12                       
#Density     1    7920612    428071 3.9873e+12   0.8507  0.356368    
#Area        5  130096259    428066 3.9872e+12   2.7944  0.015789 *  
#BonusMalus  1 1125982511    428065 3.9860e+12 120.9277 < 2.2e-16 ***
#VehPower    1   17569700    428064 3.9860e+12   1.8869  0.169548    
#VehGas      1   11527313    428063 3.9860e+12   1.2380  0.265857    
#Region     21  235355678    428042 3.9858e+12   1.2036  0.235390    
#VehAge      1    3325630    428041 3.9858e+12   0.3572  0.550086    
#DrivAge     1   75250637    428040 3.9857e+12   8.0817  0.004472 ** 
#VehBrand   10  224145913    428030 3.9855e+12   2.4073  0.007411 ** 
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# removing insignificant values and BonusMalus because unfair

fitted.results <- predict(modelling_frequency,newdata=sub_data,type='response')
fiti = sub_data[, .(expected_claim_per_year = fitted.results, measured_claim_per_year = nb_claim_amount, expected_cost_per_claim = fitted_cost, measured_cost_perclaim = avg_claim_amount)]
fiti[,pricing_per_year := expected_cost_per_claim*expected_claim_per_year]
fiti[,pricing_per_year := expected_cost_per_claim*expected_claim_per_year]

ggplot(fiti, aes(y=pricing_per_year, x=factor(measured_claim_per_year)))+geom_violin()+geom_boxplot(width = 0.2)+geom_smooth(mapping = aes(x=measured_claim_per_year, y=pricing_per_year), method = "lm", col="red", lwd =0.4)

full.fitted.results <- predict(modelling_frequency,newdata=fulldata,type='response')
full.fitted.cost <- predict(modelling_damageprice,newdata=fulldata,type='response')
full.fiti = fulldata[, .(expected_claim_per_year = full.fitted.results, measured_claim_per_year = nb_claim_amount, expected_cost_per_claim = full.fitted.cost, measured_cost_perclaim = avg_claim_amount)]

full.fiti[,pricing_per_year := expected_cost_per_claim*expected_claim_per_year]

