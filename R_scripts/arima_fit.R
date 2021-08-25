# Fitting an ARIMA model to the volatility series for 
# comparing forecasts

library(forecast)
library(moments)
library(dplyr)
library(ggplot2)
library(tsoutliers)
library(zoo)
library(tseries)


vol_series <- data.frame(ref_date = .in$DATE, value = .in$VOL20D)
an_vol_series <- data.frame(ref_date = .in$DATE, value = .in$VOL20D * sqrt(252) * 100)

ggplot(an_vol_series, aes(x = ref_date, y = value)) + geom_line() +
  ggtitle("20d IBOVESPA Volatility (Annualized)") +
  xlab("Date") +
  ylab("Value (%)")


xtvol <- zoo(vol_series$value, order.by=as.Date(vol_series$ref_date))
tsvol <- ts(xtvol)

# Outlier detection by tsoutliers package following Chen and Liu method
outl <- tso(tsvol, maxit = 1, tsmethod = "arima", args.tsmethod = list(order = c(1, 0, 0)))

plot(outl)


adj_vol_series <- data.frame(ref_date = .in$DATE, value = outl$yadj)


tsadj <- ts(zoo(adj_vol_series$value, order.by = as.Date(adj_vol_series$ref_date)))


# Create forecasts for the last 60 days in the sample by fitting the model
# until the last day available, simulating "real" day to day usage

days <- adj_vol_series$ref_date[2412:2471]
last_day <- adj_vol_series$ref_date[2412]
first_day <- adj_vol_series$ref_date[1]
zoo_vol <- xtvol 
zoo_adj <- zoo(adj_vol_series$value, order.by = as.Date(adj_vol_series$ref_date))

forecast_results <- lapply(as.character(days), function(day) {
  day <- as.Date(day)
  v_window <- window(zoo_vol, start = first_day, end = last_day)
  a_window <- window(zoo_adj, start = first_day, end = last_day)
  
  vanilla_arima <- auto.arima(ts(v_window))
  clean_arima <- auto.arima(ts(a_window))
  
  v_fcast <- forecast(vanilla_arima, h = 21)
  a_fcast <- forecast(clean_arima, h = 21)
  
  v_val <- last(as.matrix(v_fcast$mean))
  a_val <- last(as.matrix(a_fcast$mean))
  
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

real_clean <- adj_10d[adj_10d$ref_date >= "2018-01-30", ]
real_clean$variable <- "Real (cleaned)"

rest_of_series <- vol_10d[vol_10d$ref_date < "2018-01-30" & vol_10d$ref_date >= "2017-06-01", ]
rest_of_series$variable <- "Series (vol D+21)"


.df <- rbind(mfres, real_res, rest_of_series)


ggplot(.df, aes(x = ref_date, y = value*sqrt(252)*100, colour = variable)) + geom_line(size = 1) +
  geom_point() + xlab("Date") + ylab("Annualized Volatility (%)") + labs(colour = "Variable") + ggtitle("Volatility forecasting with ARIMA (zoomed) - 21d")


van_error <- mean((fres$`Predicted (vanilla)`*sqrt(252)*100 - real_res$value*sqrt(252)*100)^2)

van_error_p <- mean(abs((fres$`Predicted (vanilla)`*sqrt(252)*100  - real_res$value*sqrt(252)*100)/(real_res$value*sqrt(252)*100) ))

adj_error <- mean((fres$`Predicted (cleaned)`*sqrt(252)*100 - real_res_clean*sqrt(252)*100)^2)

adj_error_p <- mean(abs((fres$`Predicted (cleaned)` - real_res_clean)/real_res_clean))




dif_df <- data.frame(ref_date = fres$ref_date, diff = (fres$`Predicted (cleaned)`*sqrt(252)*100 - real_res_clean*sqrt(252)*100))
ggplot(dif_df, aes(x = ref_date, y = diff)) + geom_line() + geom_point() + 
  geom_hline(yintercept = 0) + ggtitle("ARIMA prediction vs real delta (cleaned series) - 21d") +
  ylab("Difference in % points") + xlab("Date")


dif_df <- data.frame(ref_date = fres$ref_date, diff = (fres$`Predicted (vanilla)`*sqrt(252)*100 - real_res$value*sqrt(252)*100))
ggplot(dif_df, aes(x = ref_date, y = diff)) + geom_line() + geom_point() + 
  geom_hline(yintercept = 0) + ggtitle("ARIMA prediction vs real delta (vanilla series) - 21d") +
  ylab("Difference in % points") + xlab("Date")
