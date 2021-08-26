# Fitting a GARCH model to compare 
# volatility forecasts

library(fGarch)
library(ggplot2)
library(reshape2)
library(dplyr)



ibov_returns <- data.frame(ref_date = .ibov$ref_date, return = .ibov$return)[-1, ]


tsibov <- ts(zoo(ibov_returns$return, order.by=as.Date(ibov_returns$ref_date)))


outret <- tso(tsibov, maxit = 1, tsmethod = "arima")

adj_ret <- data.frame(ref_date = ibov_returns$ref_date, value = outret$yadj)

days <- ibov_returns$ref_date[2431:2490]
last_day <- ibov_returns$ref_date[2431]
first_day <- ibov_returns$ref_date[1]
zoo_vol <- zoo(ibov_returns$return, order.by=as.Date(ibov_returns$ref_date))
zoo_adj <- zoo(adj_ret$value, order.by = as.Date(adj_ret$ref_date))

forecast_results <- lapply(as.character(days), function(day) {
  day <- as.Date(day)
  v_window <- window(zoo_vol, start = first_day, end = last_day)
  a_window <- window(zoo_adj, start = first_day, end = last_day)
  
  vanilla_garch <- garchFit(formula = ~garch(1,1), ts(v_window), trace = F)
  clean_garch <-  garchFit(formula = ~garch(1,1), ts(a_window), trace = F)
  
  v_fcast <- predict(vanilla_garch, 21)
  a_fcast <- predict(clean_garch, 21)
  
  v_val <- last(as.matrix(v_fcast))
  a_val <- last(as.matrix(a_fcast))
  
  last_day <<- day
  
  data.frame(ref_date = day, v_val = v_val, a_val = a_val)
})

fres <- do.call(rbind, forecast_results)
names(fres) <- c("ref_date", "Predicted (vanilla)", "Predicted (cleaned)")
mfres <- melt(fres, id.vars = "ref_date")
mfres <- mfres[, c("ref_date", "value", "variable")]

vol_10d <- data.frame(ref_date = .in21d$DATE, value = .in21d$VOL20D21P)
vol_10d <- vol_10d[!is.na(vol_10d$value), ]

real_res <- vol_10d[vol_10d$ref_date >= "2018-01-30", ]
real_res$variable <- "Real (vanilla)"

rest_of_series <- vol_10d[vol_10d$ref_date < "2018-01-30" & vol_10d$ref_date >= "2017-06-01", ]
rest_of_series$variable <- "Series (vol D+10)"


.df <- rbind(mfres, real_res, rest_of_series)


ggplot(.df, aes(x = ref_date, y = value*sqrt(252)*100, colour = variable)) + geom_line(size = 1) +
  geom_point() + xlab("Date") + ylab("Annualized Volatility (%)") + labs(colour = "Variable") +
  ggtitle("Volatility forecasting with GARCH (zoomed) - 21d")


van_error <- mean((fres$`Predicted (vanilla)`*sqrt(252)*100 - real_res$value*sqrt(252)*100)^2)
van_error_p <- mean(abs((fres$`Predicted (vanilla)` - real_res$value)/real_res$value))

adj_error <- mean((fres$`Predicted (cleaned)`*sqrt(252)*100 - real_res_clean*sqrt(252)*100)^2)
adj_error_p <- mean(abs((fres$`Predicted (cleaned)` - real_res_clean)/real_res_clean))


dif_df <- data.frame(ref_date = fres$ref_date, diff = (fres$`Predicted (vanilla)`*sqrt(252)*100 - real_res$value*sqrt(252)*100))
ggplot(dif_df, aes(x = ref_date, y = diff)) + geom_line() + geom_point() + 
  geom_hline(yintercept = 0) + ggtitle("GARCH prediction vs real delta (vanilla series) - 10d") +
  ylab("Difference in % points") + xlab("Date")



dif_df <- data.frame(ref_date = fres$ref_date, diff = (fres$`Predicted (cleaned)`*sqrt(252)*100 - real_res_clean*sqrt(252)*100))
ggplot(dif_df, aes(x = ref_date, y = diff)) + geom_line() + geom_point() + 
  geom_hline(yintercept = 0) + ggtitle("GARCH prediction vs real delta (cleaned series) - 10d") +
  ylab("Difference in % points") + xlab("Date")
