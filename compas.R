# Set working directory
setwd('~/Documents/edge_cases')

# Load libraries, register cores
library(ranger)
library(mboost)
library(tidyverse)
library(doMC)
registerDoMC(8)

# Set seed
set.seed(123, kind = "L'Ecuyer-CMRG")

# Import data
df <- read_csv('./compas/compas-scores-two-years.csv')

# From ProPublica GitHub page
df <- df %>%
  filter(days_b_screening_arrest <= 30) %>%
  filter(days_b_screening_arrest >= -30) %>%
  filter(is_recid != -1) %>%
  filter(c_charge_degree != 'O') %>%
  filter(score_text != 'N/A') %>%
  # My riffing 
  mutate(y = factor(two_year_recid)) %>%
  filter(race %in% c('African-American', 'Hispanic', 'Caucasian')) %>%
  mutate(sex = factor(sex), race = factor(race), 
         crim = factor(c_charge_degree)) %>%
  select(sex, age, race, priors_count, crim, y)
n <- nrow(df)

# Monte Carlo function
mc_fn <- function(b) {
  # Draw random subsample
  idx <- sample.int(n, round(n / 2))
  fit_fn <- function(splt) {
    if (splt == 2) {
      idx <- seq_len(n)[-idx]
    }
    # Split into training and test sets 
    trn <- df[-idx, ]
    tst <- df[idx, ]
    # Train models
    lr <- glm(y ~ ., data = trn, family = 'binomial')
    rf <- ranger(y ~ ., data = trn, num.trees = 1000, num.threads = 1)
    gb <- blackboost(y ~ ., data = trn, family = Binomial(), 
                     control = boost_control(mstop = 200, risk = 'oobag'))
    # Test models
    lr_out <- predict(lr, tst, type = 'response')
    rf_out <- rowMeans(predict(rf, tst, predict.all = TRUE)$prediction - 1)
    gb_out <- drop(predict(gb, tst, type = 'response'))
    # Export
    out <- tibble(
      idx, lr_out, rf_out, gb_out, y = tst$y, 
    )
    return(out)
  }
  out <- foreach(splt = c(1, 2), .combine = rbind) %do% fit_fn(splt)
  out$run <- b
  return(out)
}
out <- foreach(b = seq_len(1e4), .combine = rbind) %dopar% mc_fn(b)
saveRDS(out, 'compas_out.rds')


