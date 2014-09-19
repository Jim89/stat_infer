################################################################################
# Assignment 2 - Analysis of ToothGrowth data set
################################################################################
# Load ToothGrowth data and perform basic exploratory analysis
# basic summary of data
# confidence intervals and hypothesis tests to compare growth by supp and dose
# state conclusions and assumptions

################################################################################
# Step 0  - load packages
################################################################################
library(ggplot2,quietly=T)
library(magrittr,quietly=T)
library(dplyr,quietly=T)

################################################################################
# Step 1  - load data and perform some brief summaries
################################################################################
data(ToothGrowth)
data <- ToothGrowth

summary(data$len)
sd_len <- sd(data$len)

################################################################################
# Step 2  - explore based on suppliment
################################################################################
# descriptive stats per suppliment
data_sum_sup <- data %>%
  group_by(supp) %>%
  summarise(mean_growth = mean(len),
            sd_growth = sd(len),
            lower_95 = mean_growth - qt(0.975, 29) * 
              (sd_growth/sqrt(30)),
            upper_95 = mean_growth + qt(0.975, 29) * 
              (sd_growth/sqrt(30)),
            ci = upper_95 - mean_growth)

data_sum_sup$supp <- ifelse(data_sum_sup$supp == "OJ", "Orange Juice",
                            "Ascorbic Acid")

data_sum_sup %>%
ggplot(aes(x = supp, y = mean_growth), colour = supp)+
  geom_bar(stat = "identity", aes(fill = supp))+
  geom_errorbar(aes(ymin=mean_growth-ci, ymax=mean_growth+ci),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  scale_fill_manual(values = c("darkorange4", "darkorange"))+
  theme_minimal()+
  theme(legend.position = "none")+
  ylab("Mean tooth length")+
  ggtitle("Mean tooth length by suppliment administered")+
  xlab("")

################################################################################
# Step 3  - explore based on dose
################################################################################
# average length per dose per suppliment
data_sum_dose <- data %>%
  group_by(dose) %>%
  summarise(mean_growth = mean(len),
            sd_growth = sd(len),
            lower_95 = mean_growth - qt(0.975, 29) * 
              (sd_growth/sqrt(30)),
            upper_95 = mean_growth + qt(0.975, 29) * 
              (sd_growth/sqrt(30)),
            ci = upper_95 - mean_growth)

data_sum_dose %>%
  ggplot(aes(x = as.character(dose), y = mean_growth), colour = dose)+
  geom_bar(stat = "identity", aes(fill = as.character(dose)))+
  geom_errorbar(aes(ymin=mean_growth-ci, ymax=mean_growth+ci),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  scale_fill_manual(values = c("gray34","gray22","gray10"))+
  xlab("Dosage (mg)")+
  ylab("Mean tooth length")+
  theme_minimal()+
  theme(legend.position = "none")

################################################################################
# Step 4  - explore based on suppliment *and* dose
################################################################################
# average length per dose per suppliment
data_sum_both <- data %>%
  group_by(supp, dose) %>%
  summarise(mean_growth = mean(len),
            sd_growth = sd(len),
            lower_95 = mean_growth - qt(0.975, 29) * 
              (sd_growth/sqrt(30)),
            upper_95 = mean_growth + qt(0.975, 29) * 
              (sd_growth/sqrt(30)),
            ci = upper_95 - mean_growth)

data_sum_both$supp <- ifelse(data_sum_both$supp == "OJ", "Orange Juice",
                             "Ascorbic Acid")

data_sum_both %>%
  ggplot(aes(x = as.character(dose), y = mean_growth), colour = dose)+
  geom_bar(stat = "identity", aes(fill = as.character(dose)))+
  facet_grid(.~supp)+
  scale_fill_manual(values = c("gray34","gray22","gray10"))+
  xlab("Dosage (mg)")+
  ylab("Mean tooth length")+
  ggtitle("Mean tooth length by dose (mg) administered")+
  theme_minimal()+
  theme(legend.position = "none")

################################################################################
# Step 5  - hypothesis testing based on suppliment
################################################################################
# lets test that with a t.test:
# hypothesis - orange juice provides a higher growth
oj <- data %>% filter(supp == "OJ") %>% select(len)
vc <- data %>% filter(supp == "VC") %>% select(len)

t.test(oj, vc, alternative = "greater", paired = FALSE, var.equal = TRUE)

################################################################################
# Step 6  - hypothesis testing based on dose
################################################################################
# lets test that with a t.test:
# hypothesis - higher doses provides a higher growth
dose2 <- data %>% filter(dose == 2.0) %>% select(len)
dose1 <- data %>% filter(dose == 1.0) %>% select(len)
dose0.5 <- data %>% filter(dose == 0.5) %>% select(len)

# perform test for 2 being greater than 0.5
t.test(dose2, dose0.5, alternative = "greater", paired = F, var.equal = T)

# perform test for 2 being greater than 1
t.test(dose2, dose1, atlernative = "greater", paired = F, var.equal = T)

# perform test for 1 being greater than 0.5
t.test(dose1, dose0.5, alternative = "greater", paired = F, var.equal = T)

################################################################################
# Step 7  - hypothesis testing based on suppliment and dose
################################################################################
doseoj2 <- data %>% filter(supp == "OJ" & dose == 2.0) %>% select(len)
dosevc2 <- data %>% filter(supp == "VC" & dose == 2.0) %>% select(len)
t.test(doseoj2, dosevc2, paired = F, var.equal = T)

doseoj1 <- data %>% filter(supp == "OJ" & dose == 1.0) %>% select(len)
dosevc1 <- data %>% filter(supp == "VC" & dose == 1.0) %>% select(len)
t.test(doseoj1, dosevc1, paired = F, var.equal = T)

doseoj.5 <- data %>% filter(supp == "OJ" & dose == .5) %>% select(len)
dosevc.5 <- data %>% filter(supp == "VC" & dose == .5) %>% select(len)
t.test(doseoj.5, dosevc.5, paired = F, var.equal = T)

################################################################################
# Step 8  - test assumptions - data normally distributed
################################################################################
data %>%
  ggplot(aes(x = len))+
  geom_density(fill = "gray10", alpha = 0.3, color= "white")+
  theme_minimal()+
  xlab("Tooth Length")+
  ylab("Density")+
  ggtitle("Density Plot of Tooth Size")+
  theme(panel.grid.minor.y=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank())+
  stat_function(color="darkorange",
                fun= dnorm, 
                args = list(mean = mean(data$len), sd = sd(data$len)))



