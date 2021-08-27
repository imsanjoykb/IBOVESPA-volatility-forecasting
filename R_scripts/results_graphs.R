# Graph neural network results

library(dplyr)
library(ggplot2)
library(reshape2)
library(glue)

dset_path <- "../data/input_vol10d.csv"
clean_dset_path <- "../data/input_vol10d_clean.csv"
res_path <- "./python/results_10d_rnn.csv"
res_clean_path <- "./python/results_10d_clean_rnn.csv"


dset <- na.omit(read.csv(dset_path)) %>% mutate(DATE = as.Date(as.character(DATE)))
clean_dset <- na.omit(read.csv(clean_dset_path)) %>% mutate(DATE = as.Date(as.character(DATE)))
res <- read.csv(res_path)
res_clean <- read.csv(res_clean_path)

train_max_idx <- nrow(dset) - 665
valid_max_idx <- nrow(dset) - 60

lab <- c(rep("Series - training set (vol D+21)", train_max_idx),
         rep("Series (vol D+21)", valid_max_idx - train_max_idx),
         rep("Real (vanilla)", nrow(dset) - valid_max_idx))

dset$lab <- lab
dset <- na.omit(dset)

res$lab <- "Predicted (vanilla)"
res$DATE <- dset$DATE[(valid_max_idx + 1):nrow(dset)]

res_clean$lab <- "Predicted (cleaned)"
res_clean$DATE <- dset$DATE[(valid_max_idx + 1):nrow(dset)]

real_clean <- clean_dset[(valid_max_idx + 1):nrow(clean_dset), ]

.df <- data.frame(ref_date = c(dset$DATE, res$DATE, res_clean$DATE),
                  value = c(dset$VOL20D21P, res$pred, res_clean$pred),
                  label = c(dset$lab, res$lab, res_clean$lab)) %>%
  mutate(value = value * sqrt(252) * 100)


# .df <- data.frame(ref_date = c(dset$DATE, res$DATE),
#                   value = c(dset$VOL20D10P, res$pred),
#                   label = c(dset$lab, res$lab)) %>% 
#   mutate(value = value * sqrt(252) * 100)


ggplot(.df, aes(x = ref_date, y = value, colour = label)) + geom_line(size = 1) +
  ggtitle("D+21 IBOVESPA Volatility")


ggplot(.df %>% dplyr::filter(ref_date >= "2017-06-01"), aes(x = ref_date, y = value, colour = label)) + geom_line(size = 1) +
  ggtitle("Volatility forecasting with RNN-1 (zoomed) - 21d") +
  geom_point() + xlab("Date") + ylab("Annualized Volatility (%)") + labs(colour = "Variable") 

real_res <- (dset[(valid_max_idx + 1):nrow(dset), ])$VOL20D21P
dif_df <- data.frame(ref_date = res$DATE, diff = (res$pred*sqrt(252)*100 - real_res*sqrt(252)*100))
ggplot(dif_df, aes(x = ref_date, y = diff)) + geom_line() + geom_point() + 
  geom_hline(yintercept = 0) + ggtitle("RNN-1 prediction vs real delta - 21d") +
  ylab("Difference in % points") + xlab("Date")


real_res_clean <- (clean_dset[(valid_max_idx + 1):nrow(clean_dset), ])$VOL20D10P
dif_df <- data.frame(ref_date = res_clean$DATE, diff = (res_clean$pred*sqrt(252)*100 - real_res_clean*sqrt(252)*100))
ggplot(dif_df, aes(x = ref_date, y = diff)) + geom_line() + geom_point() + 
  geom_hline(yintercept = 0) + ggtitle("RNN-1 prediction vs real delta - 21d, cleaned series") +
  ylab("Difference in % points") + xlab("Date")



van_error <- mean((res$pred*sqrt(252)*100 - real_res*sqrt(252)*100)^2)

van_error_p <- mean(abs((res$pred*sqrt(252)*100 - real_res*sqrt(252)*100)/(real_res*sqrt(252)*100)))

adj_error <- mean((res_clean$pred*sqrt(252)*100 - real_res_clean*sqrt(252)*100)^2)

adj_error_p <- mean(abs((res_clean$pred*sqrt(252)*100 - real_res_clean*sqrt(252)*100)/(real_res_clean*sqrt(252)*100)))
