

######### Q3.

install.packages("modelsummary")
library("modelsummary")

install.packages("mice")
library(mice)

library(tidyverse)


######### Q4.

setwd("C:/Users/ryujy/OneDrive/바탕 화면/2024 spring")
data <- read.csv("wages.csv")




######### Q5. Drop missing value in either 'hgc' or 'tenure' variables.
clean_data <- data %>%
  drop_na(hgc, tenure)




######### Q6. Produce a summary table using modelsummary
install.packages("modelsummary")
library(modelsummary)

clean_data_summary_table <- datasummary_skim(clean_data)
clean_data_summary_table

#Answer 1. Rate: 25%
#Answer 2. logwage variable is most likely to be MAR. 
#          This is because The higher the hgc value, the higher the missing value probability.

clean_data$missing_dummy <- ifelse(is.na(clean_data$logwage), 0, 1)


common_theme <- function() {
  ptcolor <- 'grey20' 
  theme(
    plot.title=element_text(size=14, lineheight=0.8, color=ptcolor, hjust=0.5),
    axis.title.x=element_text(color=ptcolor),
    axis.title.y=element_text(color=ptcolor))
}

ggplot(data=clean_data, aes(x=hgc, y=tenure)) +
  geom_point(aes(colour=missing_dummy), shape=15, size=1.5) +
  labs(x="hgc", y="tenure") +
  common_theme() +
  theme(plot.title=element_text(color="#2255DD")) +
  theme(axis.text.x = element_text(size=5,face='bold'))


ggplot(data=clean_data, aes(x=hgc, y=age)) +
  geom_point(aes(colour=missing_dummy), shape=15, size=1.5) +
  labs(x="hgc", y="age") +
  common_theme() +
  theme(plot.title=element_text(color="#2255DD")) +
  theme(axis.text.x = element_text(size=5,face='bold'))







######### Q7.

#####(1) estimate the regression using only complete cases
clean_data$tenure_sq <- clean_data$tenure^2

lm_1 <- lm(logwage ~ hgc + college + tenure + tenure_sq + age + married, data = clean_data)
summary(lm_1)


clean_data2 <- clean_data %>%
  drop_na(logwage)

lm_2 <- lm(logwage ~ hgc + college + tenure + tenure_sq + age + married, data = clean_data2)

summary(lm_2)
# hgc                      0.0623931


#####(2) perform mean imputation to fill in missing log wages

mean_logwage <- mean(clean_data$logwage, na.rm = TRUE)
view(mean_logwage)
# mean = 1.62519

clean_data3 <- clean_data
clean_data3$logwage[is.na(clean_data3$logwage)] <- mean_logwage


lm_3 <- lm(logwage ~ hgc + college + tenure + tenure_sq + age + married, data = clean_data3)
summary(lm_3)
# hgc                      0.0496877


#####(3) impute missing log wages as their predicted values from the complete cases regression above
#####   (i.e. this would be consistent with the “Missing at Random” assumption)

# Step 1: Fit a regression model using complete dataset
complete <- na.omit(clean_data)  # Remove rows with NA in logwage
lm_4 <- lm(logwage ~ hgc + college + tenure + tenure_sq + age + married, data = complete)
summary(lm_4)


# Step 2: Predict missing logwages
missing_indices <- is.na(clean_data$logwage)  # Identify rows with missing logwage
predicted_logwage <- predict(lm_4, newdata = clean_data[missing_indices, ])
view(predicted_logwage)



# Step 3: Replace missing values with predictions
clean_data4 <- clean_data
clean_data4$logwage[missing_indices] <- predicted_logwage

print(clean_data4)

lm_5 <- lm(logwage ~ hgc + college + tenure + tenure_sq + age + married, data = clean_data4)
summary(lm_5)
# hgc                      0.0623931



#####(4) use the mice package to perform a multiple imputation regression model

install.packages("Amelia")
library(Amelia)

clean_data$married_dummy <- ifelse(clean_data$married == "married", 1, 0)
clean_data$college_dummy <- ifelse(clean_data$college == "college grad", 1, 0)

clean_data5 <- subset(clean_data, select = -college)
clean_data6 <- subset(clean_data5, select = -married)
clean_data7 <- subset(clean_data6, select = -missing_dummy)

 
clean_data_mice <- mice(clean_data7, m = 5, printFlag = FALSE)
clean_data_amelia <- amelia(clean_data7, m = 5, p2s = 0)$imputations


mod <- list()
mod[['Listwise deletion']] <- lm(logwage ~ hgc + tenure + tenure_sq + age + married_dummy + college_dummy, clean_data7)
mod[['Mice']] <- with(clean_data_mice, lm(logwage ~ hgc + tenure + tenure_sq + age + married_dummy + college_dummy)) 
mod[['Amelia']] <- lapply(clean_data_amelia, function(x) lm(logwage ~ hgc + tenure + tenure_sq + age + married_dummy + college_dummy, x))


mod[['Mice']] <- mice::pool(mod[['Mice']])
mod[['Amelia']] <- mice::pool(mod[['Amelia']])

modelsummary(mod)

#refer to https://modelsummary.com/articles/modelsummary.html#multiple-imputation




# Answer: 
# The estimated beta indicates 0.062, but the true value is known to be 0.093. which means underestimae the true effect of education on wagge
# We can think of possible reasons for the underestimation.
# 1) Missing value of wages: Missing values in the dependent variable(wage) lead to a reduced sample size, decreasing the degree of the accurate estimation.
#    Moreover, if the missingness is not completely random(MCAR), it can cause bias into the estimated coefficient.
#    This could be a possible reason about underestimation in complete case analysis with 1669 observations, but Mice and Amelia result are also underestimated.
#    Thus, we can think of other possible reasons.
# 2) Omitted Variable Bias: If there are other relevant variables that affect wages and are correlated with education but are not included in model, 
#    this can lead to biased estimates of the education effect.
# 3) Measurement error: If there is measurement error in the education variable such as inaccurate information of schooling year, 
#    this can reduce the estimated coefficient.
# 4) Sample selection: If the sample unsed in analysis is not representative of the population, this could lead to differences in the estimated effect.










