################################################################################
# Step 0 - Prepare the working environment - load packages
################################################################################
# 0i. ggplot2
library(ggplot2,quietly=T)

# 0ii. magrittr
library(magrittr,quietly=T)

################################################################################
# Step 1 - set up the variables to be used in the simulation
################################################################################
# 1i. Set the rate parameter
lambda <- 0.2

# 1ii. Set the number of exponentials to generate
n <- 40

# 1iii. Set the number of simulations
sims <- 1001

################################################################################
# Step 2 - Simulate the data
################################################################################
# 2i. Set a seed for reproducibility
set.seed(8020)
# 2ii. Simulate the data
simulated <- 1:sims %>% lapply(function(i){rexp(n,0.2)})
# 2iii. Calculate the means
means <- simulated %>% sapply(mean)

################################################################################
# Step 3 - Calculate Expected Mean and SD
################################################################################
# 3i. mean
mean_theory <- 1/lambda

# 3ii. SD
sd_theory <- 1/lambda

# 3iii. Variance
var_theory <- sd_expected^2/n

################################################################################
# Step 4 - Calculate actual descriptive statistics of general data
################################################################################
# 4i. mean
mean_actual <- mean(means)

# 4ii. variance
var_actual <- var(means)

# 4iii. confidence intervals for each mean
intervals <- means %>%
             lapply(function(i){i + c(-1,1) * 1.96 * (sd_theory/sqrt(n))})

# 4iv. function to calcualte if true mean is between confidence intervals
is.between <- function(x, a, b) {
  x > a & x < b
}

# 4v. calculate if true mean is between confidence intervals
between <- 1:length(intervals) %>%
           sapply(function(i){is.between(mean_theory, 
                                         intervals[[i]][1], 
                                         intervals[[i]][2])})

# 4vi. calculate confidence coverages
confidence_coverage <- round(100*(sum(between)/length(between)),1)

###############################################################################
# Step 5 - Plot the distribution and show it is normally distributed
################################################################################
ggplot(data.frame("value"=means), aes(x= value))+
  geom_histogram(aes(y=..density..),fill="steelblue", alpha= 0.3,
                 binwidth = 0.1, colour="white")+
  geom_vline(aes(xintercept=mean(means),color="firebrick"))+
  theme_minimal()+
  xlab("Mean")+
  ylab("Density")+
  #ggtitle("Density plot of sample means for random exponentials")+
  theme(panel.grid.minor.y=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank())+
  stat_function(color="darkblue",
                fun= dnorm , args = list(mean = mean(means) , sd = sd(means)))