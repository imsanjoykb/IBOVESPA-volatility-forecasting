
# build input for the neural network

library(dplyr)
library(ggplot2)
library(reshape2)
library(rugarch)
library(influxdbr)
library(glue)

dbName <- "DadosHistBMFBOVESPA"

con <- influx_connection()

get_series_df <- function(ticker, measurement, key_field) {
  series <- influx_select(con, dbName, field_keys = "*", measurement = measurement, 
                          return_xts = F, simplifyList = T,
                          where = glue("{key_field} = '{ticker}'"))[[1]]
  data.frame(ref_date = as.Date(series$time), return = as.numeric(series$returns), 
             vol = as.numeric(series$vol20d), 
             stringsAsFactors = F)
}




ibov_df <- get_series_df("IBV-PF", "indic", "indic_code")
dol_df <- get_series_df("DOL-D1", "indic", "indic_code")
wti_df <- get_series_df("WTI", "indic", "indic_code")
petr_df <- get_series_df("PETR4", "stocks", "neg_code")
vale_df <- get_series_df("VALE3", "stocks", "neg_code")
# itub_df <- get_series_df("ITUB4", "stocks", "neg_code")
bbdc_df <- get_series_df("BBDC4", "stocks", "neg_code")
pre5y_df <- get_series_df("PRE5Y", "interest_rate", "variable")


common_dates <- base::intersect(ibov_df$ref_date, dol_df$ref_date) %>% 
  base::intersect(petr_df$ref_date) %>% base::intersect(vale_df$ref_date) %>%
  base::intersect(bbdc_df$ref_date)%>%
  base::intersect(wti_df$ref_date) %>% base::intersect(pre5y_df$ref_date)

.ibov <- ibov_df %>% filter(ref_date %in% common_dates)
.dol <- dol_df %>% filter(ref_date %in% common_dates)
.petr <- petr_df %>% filter(ref_date %in% common_dates)
.vale <- vale_df %>% filter(ref_date %in% common_dates)
# .itub <- itub_df %>% filter(ref_date %in% common_dates)
.bbdc <- bbdc_df %>% filter(ref_date %in% common_dates)
.wti <- wti_df %>% filter(ref_date %in% common_dates)
.pre5y <- pre5y_df %>% filter(ref_date %in% common_dates)


.in <- na.omit(data.frame(
  DATE = .ibov$ref_date,
  IBOV = .ibov$return, 
  DOL = .dol$return,
  PETR = .petr$return,
  VALE = .vale$return,
  # ITUB = .itub$return,
  BBDC = .bbdc$return,
  WTI = .wti$return,
  PRE5Y = .pre5y$return,
  VOL20D = .ibov$vol
))

.in1d <- .in %>% mutate(VOL20D1P = c(VOL20D[2:length(VOL20D)], NA))
write.csv(.in1d, file = "input_vol1d.csv", row.names = F)


.in5d <- .in %>% mutate(VOL20D5P = c(VOL20D[6:length(VOL20D)], rep(NA, 5)))
write.csv(.in5d, file = "input_vol5d.csv", row.names = F)

.in10d <- .in %>% mutate(VOL20D10P = c(VOL20D[11:length(VOL20D)], rep(NA, 10)))
write.csv(.in10d, file = "input_vol10d.csv", row.names = F)

.in21d <- .in %>% mutate(VOL20D21P = c(VOL20D[22:length(VOL20D)], rep(NA, 21)))
write.csv(.in21d, file = "input_vol21d.csv", row.names = F)



.in1d <- .in %>% mutate(VOL20D1P = c(VOL20D[2:length(VOL20D)], NA))
write.csv(na.omit(.in1d[,-1]), file = "input_vol1d_other.csv", row.names = F)

.in5d <- .in %>% mutate(VOL20D5P = c(VOL20D[6:length(VOL20D)], rep(NA, 5)))
write.csv(na.omit(.in5d[,-1]), file = "input_vol5d_other.csv", row.names = F)

.in10d <- .in %>% mutate(VOL20D10P = c(VOL20D[11:length(VOL20D)], rep(NA, 10)))
write.csv(na.omit(.in10d[,-1]), file = "input_vol10d_other.csv", row.names = F)

.in21d <- .in %>% mutate(VOL20D21P = c(VOL20D[22:length(VOL20D)], rep(NA, 21)))
write.csv(na.omit(.in21d[,-1]), file = "input_vol21d_other.csv", row.names = F)



# outlier filtering

library(tsoutliers)
library(zoo)

.ibovout <- tso(ts(zoo(.ibov$return, order.by=as.Date(.ibov$ref_date))))
.dolout <- tso(ts(zoo(.dol$return, order.by=as.Date(.dol$ref_date))))
.petrout <- tso(ts(zoo(.petr$return, order.by=as.Date(.petr$ref_date))))
.valeout <- tso(ts(zoo(.vale$return, order.by=as.Date(.vale$ref_date))))
.bbdcout <- tso(ts(zoo(.bbdc$return, order.by=as.Date(.bbdc$ref_date))))
.wtiout <- tso(ts(zoo(.wti$return, order.by=as.Date(.wti$ref_date))))
.preout <- tso(ts(zoo(.pre5y$return, order.by=as.Date(.pre5y$ref_date))))


.in_filt <- na.omit(data.frame(
  DATE = .ibov$ref_date,
  IBOV = .ibovout$yadj, 
  DOL = .dolout$yadj,
  PETR = .petrout$yadj,
  VALE = .valeout$yadj,
  # ITUB = .itub$return,
  BBDC = .bbdcout$yadj,
  WTI = .wtiout$yadj,
  PRE5Y = .preout$yadj,
  VOL20D = .ibov$vol
))

.in1d <- .in_filt %>% mutate(VOL20D1P = c(VOL20D[2:length(VOL20D)], NA))
write.csv(.in1d, file = "input_vol1d_clean.csv", row.names = F)


.in5d <- .in_filt %>% mutate(VOL20D5P = c(VOL20D[6:length(VOL20D)], rep(NA, 5)))
write.csv(.in5d, file = "input_vol5d_clean.csv", row.names = F)

.in10d <- .in_filt %>% mutate(VOL20D10P = c(VOL20D[11:length(VOL20D)], rep(NA, 10)))
write.csv(.in10d, file = "input_vol10d_clean.csv", row.names = F)

.in21d <- .in_filt %>% mutate(VOL20D21P = c(VOL20D[22:length(VOL20D)], rep(NA, 21)))
write.csv(.in21d, file = "input_vol21d_clean.csv", row.names = F)
