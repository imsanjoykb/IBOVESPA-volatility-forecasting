# mutates and inserts data in the influxDB database

library(influxdbr)

dbName <- "DadosHistBMFBOVESPA"

con <- influx_connection()

create_database(con, dbName)


wti_df <- readxl::read_excel("wti_series.xls", sheet = "Data 1", skip = 3)

names(wti_df) <- c("ref_date", "value")

wti_df <- wti_df %>% mutate(ref_date = as.Date(ref_date), indic_group = "RT", indic_code = "WTI") %>% filter(ref_date >= '2018-02-20', ref_date <= '2018-06-01')
rets <- diff(log(wti_df$value))
vol <-  c(rep(NA, 20), zoo::rollapply(rets, 20, sd))
wti_df$returns <- c(NA, rets)
wti_df$vol20d <- vol
wti_df[is.na(wti_df)] <- ""


interest_df <- melt(interest_df, id.vars = "ref_date")


interest_df <- lapply(unique(interest_df$variable), function(cod) {
  subs <- interest_df %>% filter(variable == cod)
  rets <- diff(log(subs$value))
  if(length(rets) < 20) {
    vol <- rep(NA, nrow(subs))
  } else {
    vol <-  c(rep(NA, 20), zoo::rollapply(rets, 20, sd))
  }
  if (length(c(NA, rets)) != nrow(subs) || length(vol) != nrow(subs)) {
    browser()
  }
  subs$returns <- c(NA, rets)
  subs$vol20d <- vol
  subs
})

interest_df <- do.call(rbind, interest_df)

interest_df[is.na(interest_df)] <- ""


indic_df <- lapply(unique(indic_df$indic_code), function(cod) {
  subs <- indic_df %>% filter(indic_code == cod)
  rets <- diff(log(subs$value))
  if(length(rets) < 20) {
    vol <- rep(NA, nrow(subs))
  } else {
    vol <-  c(rep(NA, 20), zoo::rollapply(rets, 20, sd))
  }
  if (length(c(NA, rets)) != nrow(subs) || length(vol) != nrow(subs)) {
    browser()
  }
  subs$returns <- c(NA, rets)
  subs$vol20d <- vol
  subs
})

indic_df <- do.call(rbind, indic_df)

indic_df[is.na(indic_df)] <- ""


historical_stocks <- lapply(unique(historical_stocks$neg_code), function(cod) {
  subs <- historical_stocks %>% filter(neg_code == cod)
  rets <- diff(log(subs$close_prc))
  if(length(rets) < 20) {
    vol <- rep(NA, nrow(subs))
  } else {
    vol <-  c(rep(NA, 20), zoo::rollapply(rets, 20, sd))
  }
  if (length(c(NA, rets)) != nrow(subs) || length(vol) != nrow(subs)) {
    browser()
  }
  subs$returns <- c(NA, rets)
  subs$vol20d <- vol
  subs
})

historical_stocks <- do.call(rbind, historical_stocks)

historical_stocks[is.na(historical_stocks)] <- ""



futs_df <- lapply(unique(futs_df$neg_code), function(cod) {
  subs <- futs_df %>% filter(neg_code == cod)
  rets <- diff(log(subs$close_prc))
  if(length(rets) < 20) {
    vol <- rep(NA, nrow(subs))
  } else {
    vol <-  c(rep(NA, 20), zoo::rollapply(rets, 20, sd))
  }
  if (length(c(NA, rets)) != nrow(subs) || length(vol) != nrow(subs)) {
    browser()
  }
  subs$returns <- c(NA, rets)
  subs$vol20d <- vol
  subs
})

futs_df <- do.call(rbind, futs_df)

futs_df[is.na(futs_df)] <- ""





show_databases(con)

step <- floor(nrow(interest_df)/2)
res <- lapply(seq(1, nrow(interest_df), by = step), function(row) {
  influx_write(x = interest_df[row:(row + step), ] %>% filter(!is.na(ref_date)), con = con, db = dbName, time_col = c("ref_date"), 
               measurement = "interest_rate",
               consistency = "all",
               tag_cols = c("variable"))
})


indic_df$ref_date <- as.POSIXct(indic_df$ref_date) + lubridate::hours(3)
step <- floor(nrow(indic_df)/176)
res <- lapply(seq(1, nrow(indic_df), by = step), function(row) {
  influx_write(x = indic_df[row:(row + step), ] %>% filter(!is.na(ref_date)), con = con, db = dbName, time_col = c("ref_date"), 
               measurement = "indic",
               consistency = "all",
               tag_cols = c("indic_group", "indic_code"))
})
 
wti_df$ref_date <- as.POSIXct(wti_df$ref_date) + lubridate::hours(2)
  
influx_write(x = as.data.frame(wti_df) %>% filter(!is.na(ref_date)), con = con, db = dbName, time_col = c("ref_date"), 
             measurement = "indic", 
             consistency = "all",
             tag_cols = c("indic_group", "indic_code"))


step <- floor(nrow(futs_df)/75)
res <- lapply(seq(1, nrow(futs_df), by = step), function(row) {
influx_write(x = futs_df[row:(row + step), ] %>% filter(!is.na(ref_date)), con = con, db = dbName, time_col = c("ref_date"), 
             measurement = "futures", 
             consistency = "all", 
             max_points = 5000, 
             tag_cols = c("merc", "neg_code", "mat_date"))
})

step <- 3500
historical_stocks$ref_date <- as.POSIXct(historical_stocks$ref_date) + lubridate::hours(3)
res <- lapply(seq(1, nrow(historical_stocks), by = step), function(row) {
influx_write(x = historical_stocks[row:(row + step) , ] %>% filter(!is.na(ref_date)), con = con, db = dbName, time_col = c("ref_date"), 
             measurement = "stocks", 
             consistency = "all", 
             max_points = 5000, 
             tag_cols = c("neg_code"))
})

res <- lapply(seq(1, nrow(historical_options), by = step), function(row) {
influx_write(x = historical_options[row:(row + step), ]  %>% filter(!is.na(ref_date)), con = con, db = dbName, time_col = c("ref_date"), 
             measurement = "options", 
             consistency = "all", 
             max_points = 5000, 
             tag_cols = c("neg_code", "mat_date", "opt_type"))
})

res <- lapply(seq(1, nrow(historical_ibov_options), by = step), function(row) {
influx_write(x = historical_ibov_options[row:(row + step), ] %>% filter(!is.na(ref_date)), con = con, db = dbName, time_col = c("ref_date"), 
             measurement = "options", 
             consistency = "all", 
             max_points = 5000, 
             tag_cols = c("neg_code", "mat_date", "opt_type"))
})

res <- lapply(seq(1, nrow(deriv_opts_df), by = step), function(row) {
influx_write(x = deriv_opts_df[row:(row + step), ] %>% filter(!is.na(ref_date)), con = con, db = dbName, time_col = c("ref_date"), 
             measurement = "options", 
             consistency = "all", 
             max_points = 5000, 
             tag_cols = c("neg_code", "mat_date", "opt_type"))
})




# tests

ans <- influx_select(con, dbName, field_keys = "*", measurement = "indic", return_xts = F, simplifyList = T,
                     limit = 500, order_desc = T)[[1]]

ans <- influx_select(con, dbName, field_keys = "*", measurement = "futures", return_xts = F, simplifyList = T)[[1]]

show_measurements(con, dbName)


ans <- influx_select(con, dbName, field_keys = "*", measurement = "interest_rate", return_xts = F, simplifyList = T)[[1]]
