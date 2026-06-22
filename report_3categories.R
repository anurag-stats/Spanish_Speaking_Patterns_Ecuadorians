#' ---
#' title: "Fixed Effects - 3 categories"
#' author: "Anurag Banerjee"
#' date: "2026-03-25"
#' output: pdf_document
#' ---
#' 
## ----setup, include=FALSE---------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

#' 
#' 
#' In this analysis, we will model relationships for the type of Spanish produced v/s the social and/or demographic variables and sentence type. Sentence type variable, due to the large number of categories and limited data due to 6 speakers, was very sparse and it was difficult to fit any type of model reliably with the current structure. So, I reduced the number of sentence types to sentence groups/ categories based on how similar some statements were, this grouping may not defensible from a linguistic stand point. 
#' 
#' The new grouping for sentence type is:
#' 1. Sentence Type 1-6 : Statements
#' 2. Sentence Type 7-11: yes/ no questions
#' 3. Sentence Type 12-14: wh- questions
#' 4. Sentence type >= 15: commands or others
#' 
#' The response has 3 categories: 
#' - 1: Spanish
#' - 2: Ecuador
#' - 3: Different
#' 
#' The goal of the analysis is to answer the following questions:
#' 1. Did Ecuadorian speakers learn to produce Spanish patterns?
#' 
#' 2. Did Ecuadorian speakers still produce/use Spanish patters in Ecuador?
#' 
#' 3.  In Ecuador, which patterns (Spain or Ecuador) are more produced/used? and determine which factors (age, level of education, gender, time of stay in Spain or return to Ecuador, AND sentence type ) promote the use/production of either or? 
#' 
#' 4. What are the different patterns? intermediate forms between Spain and Ecuador patterns? patterns from other dialects of Spanish? errors?
#' 
#' Since we want to see whether Spanish patterns are learned in the first 2 questions, we will treat Ecuador (2) as our baseline level and then model the relationship to make interpretations easier. So model coefficients can be used to say things like how likely Spanish or "Different Patterns" are over Ecuador patterns. 
#' 
#' In order to answer the first 2 questions we will use predicted probabibilities and construct confidence intervals around them using bootstrapping. We will construct 95% confidence intervals.
#' 
#' Answering the 3rd question is not easy since every variable has a different scale, so we can't just compare coefficients, so we will look at the predicted probabibilities for different values of the variable under study while averaging over the other predictors. For e.g. if we wanna see how important age is we look at predicted probabibility of age = 29 v/s probabibility of age = 44.
#' 
## ---------------------------------------------------------------------------------------------------------------
library(readxl)
df <- read_excel("data_returnees_3categories.xlsx")
head(df)

#' 
#' 
## ---------------------------------------------------------------------------------------------------------------
library(dplyr)

df <- df[complete.cases(df), ]
df_model <- df %>%
  mutate(
    dv = factor(dv,
                levels = c(1, 2, 3),
                labels = c("Ecuador", "Spain", "Different")),
    dv = relevel(dv, ref = "Ecuador"),
    Speaker = factor(Speaker),
    gender = factor(gender),
    edu_level = factor(edu_level),
    sentence_type = as.numeric(as.character(sentence_type)),
    sent_group = case_when(
      sentence_type %in% 1:6 ~ "statement",
      sentence_type %in% 7:11 ~ "yn_question",
      sentence_type %in% 12:14 ~ "wh_question",
      sentence_type %in% 15:17 ~ "command_other",
      TRUE ~ NA_character_
    ),
    sent_group = factor(sent_group,
                        levels = c("statement", "yn_question", "wh_question", "command_other"))
  ) %>%
  filter(!is.na(sent_group))

#' 
#' Assuming independence within speaker observations for now (probably a wrong assumption):
#' 
## ---------------------------------------------------------------------------------------------------------------
library(nnet)

fit_fixed <- multinom(
  dv ~ sent_group + age + gender + stay_spain + return_ecuador + edu_level,
  data = df_model,
  Hess = TRUE,
  trace = FALSE
)

summary(fit_fixed)


#' 
#' Model metrics:
#' 
## ---------------------------------------------------------------------------------------------------------------

pred_class <- predict(fit_fixed, type = "class")
pred_prob  <- predict(fit_fixed, type = "probs")

table(observed = df_model$dv, predicted = pred_class)
summary(pred_prob)
apply(pred_prob, 2, range)


#' For confidence intervals:
## ---------------------------------------------------------------------------------------------------------------
coefs <- summary(fit_fixed)$coefficients
ses   <- summary(fit_fixed)$standard.errors

lower <- coefs - 1.96 * ses
upper <- coefs + 1.96 * ses


#' 
#' 
#' Probabibility of Spanish Patterns overall, based on the model:
## ---------------------------------------------------------------------------------------------------------------
pred_prob <- as.data.frame(predict(fit_fixed, newdata = df_model, type = "probs"))

pred_df <- cbind(df_model, pred_prob)

mean_spain <- mean(pred_df$Spain)

mean_spain

#' 
#' Do they produce Spanish patterns or learn to produce Spanish patterns overall:
#' (Confidence interval computed using bootstrapped samples.)
#' 
## ---------------------------------------------------------------------------------------------------------------
library(boot)

boot_fun <- function(data, indices) {
  d <- data[indices, ]
  
  fit <- multinom(
    dv ~ sent_group + age + gender + stay_spain + return_ecuador + edu_level,
    data = d,
    trace = FALSE
  )
  
  pred <- as.data.frame(predict(fit, newdata = d, type = "probs"))
  mean(pred$Spain)
}

set.seed(123)

boot_out <- boot(
  data = df_model,
  statistic = boot_fun,
  R = 1000
)

boot_out$t0
quantile(boot_out$t, c(0.025, 0.975), na.rm = TRUE)


#' 
#' Based on Stay in Spain and Return to Ecuador, we will compute similar confidence intervals for predicted probabibility
#' 
## ---------------------------------------------------------------------------------------------------------------
library(ggeffects)

pred_stay <- ggpredict(
  fit_fixed,
  terms = "stay_spain [all]"
)

pred_return <- ggpredict(
  fit_fixed,
  terms = "return_ecuador [all]"
)

pred_stay
pred_return


#' 
#' 
## ---------------------------------------------------------------------------------------------------------------
prob_spain_at_boot <- function(data, indices, a, b) {
  d <- data[indices, ]
  
  fit <- multinom(
    dv ~ sent_group + age + gender + stay_spain + return_ecuador + edu_level,
    data = d,
    trace = FALSE
  )
  
  newdata <- d
  newdata$stay_spain <- a
  newdata$return_ecuador <- b
  
  pred <- as.data.frame(predict(fit, newdata = newdata, type = "probs"))
  mean(pred$Spain)
}



#' 
#' 
## ---------------------------------------------------------------------------------------------------------------
get_spain_ci <- function(data, a, b, R = 1000) {
  set.seed(123)
  
  boot_out <- boot(
    data = data,
    statistic = function(data, indices) prob_spain_at_boot(data, indices, a, b),
    R = R
  )
  
  c(
    estimate = boot_out$t0,
    lower = quantile(boot_out$t, 0.025, na.rm = TRUE),
    upper = quantile(boot_out$t, 0.975, na.rm = TRUE)
  )
}
get_spain_ci(df_model, a = 12, b = 0.0493, R = 500)

## ---------------------------------------------------------------------------------------------------------------
grid_df <- df_model %>% distinct(stay_spain, return_ecuador)
out <- t(mapply(
  FUN = function(a, b) {
    get_spain_ci(df_model, a = a, b = b, R = 300)
  },
  a = grid_df$stay_spain,
  b = grid_df$return_ecuador
))

out <- as.data.frame(out)
names(out) <- c("spain_prob", "spain_lower", "spain_upper")

grid_df <- cbind(grid_df, out)
grid_df

#' 
#' 
#' Similar thing for different sent groups:
## ---------------------------------------------------------------------------------------------------------------
prob_spain_group_boot <- function(data, indices, g) {
  d <- data[indices, ]
  
  fit <- multinom(
    dv ~ sent_group + age + gender + stay_spain + return_ecuador + edu_level,
    data = d,
    trace = FALSE
  )
  
  newdata <- d
  newdata$sent_group <- factor(g, levels = levels(d$sent_group))
  
  pred <- as.data.frame(predict(fit, newdata = newdata, type = "probs"))
  mean(pred$Spain)
}

get_spain_group_ci <- function(data, g, R = 1000) {
  set.seed(123)
  
  boot_out <- boot(
    data = data,
    statistic = function(data, indices) {
      prob_spain_group_boot(data, indices, g)
    },
    R = R
  )
  
  c(
    estimate = boot_out$t0,
    lower = quantile(boot_out$t, 0.025, na.rm = TRUE),
    upper = quantile(boot_out$t, 0.975, na.rm = TRUE)
  )
}

group_df <- data.frame(
  sent_group = levels(df_model$sent_group)
)

out <- t(sapply(group_df$sent_group, function(g) {
  get_spain_group_ci(df_model, g = g, R = 500)
}))

out <- as.data.frame(out)
names(out) <- c("spain_prob", "spain_lower", "spain_upper")

group_df <- cbind(group_df, out)
group_df


#' 
#' 
#' Comparing different variables spain probabilities:
#' 
## ---------------------------------------------------------------------------------------------------------------
library(ggeffects)

ggeffect(fit_fixed, terms = "sent_group")



#' 
## ---------------------------------------------------------------------------------------------------------------

pred_sent <- ggeffect(fit_fixed, terms = "sent_group")
pred_sent


#' 
#' 
## ---------------------------------------------------------------------------------------------------------------
pred_age <- ggeffect(fit_fixed, terms = "age")
pred_age

#' 
#' 
## ---------------------------------------------------------------------------------------------------------------
pred_stay_spain <- ggeffect(fit_fixed, terms = "stay_spain")
pred_stay_spain

#' 
#' 
## ---------------------------------------------------------------------------------------------------------------
pred_gender <- ggeffect(fit_fixed, terms = "gender")
pred_gender


#' 
## ---------------------------------------------------------------------------------------------------------------

pred_return_ecuador <- ggeffect(fit_fixed, terms = "return_ecuador")
pred_return_ecuador

#' 
#' 
#' Tried fitting a mixed effects model, but wasn't successful due to lack of unique data, the design matrix is computationally singular, which is a problem and the algorithm doesn't converge. Although, mixed effects should be used, it isn't possible to do so right now since we don't have enough data. A solution could be to use a Bayesian model instead with a weakly informative prior(s), but that requires more rigorous research, so skipping that for now. The most ideal solution would be to talk to more speakers and get a more representative data. Inferences should be made carefully, there is a lot of scope for improvement. 
