# A series of graphs used for taking a look at the available series 
# and choosing the inputs for the neural network. 

library(dplyr)
library(ggplot2)
library(reshape2)
library(rugarch)
library(rmgarch)
library(influxdbr)
library(glue)

dbName <- "DadosHistBMFBOVESPA"

con <- influx_connection()

get_series_df <- function(ticker, measurement, key_field) {
  series <- influx_select(con, dbName, field_keys = "*", measurement = measurement, 
                          return_xts = F, simplifyList = T,
                          where = glue("{key_field} = '{ticker}'"))[[1]]
  na.omit(data.frame(ref_date = as.Date(series$time), return = as.numeric(series$returns), 
             vol = as.numeric(series$vol20d), 
             stringsAsFactors = F))
}

gen_graphs <- function(ticker, measurement, key_field, title_ticker) {
  series <- influx_select(con, dbName, field_keys = "*", measurement = measurement, 
                          return_xts = F, simplifyList = T,
                          where = glue("{key_field} = '{ticker}'"))[[1]]
  .df <- data.frame(ref_date = series$time, return = series$returns, vol = series$vol20d, 
                    stringsAsFactors = F)
  retp <- ggplot(.df %>% filter(return != "") %>% mutate(return = as.numeric(return)), 
         aes(x = ref_date, y = return)) + geom_line() + 
    ggtitle(paste('Log-return', title_ticker)) +
    xlab("Date") + ylab("Log-return")
  volp <-  ggplot(.df %>% filter(vol != "") %>% mutate(vol = as.numeric(vol) * sqrt(252) * 100), 
                  aes(x = ref_date, y = vol)) + geom_line() + 
    ggtitle(paste('Annualized moving 20d Volatility', title_ticker)) +
    xlab("Date") + ylab("Vol. 20d (%)")
  list(retp = retp, volp = volp)
}

ibov_graphs <- gen_graphs("IBV-PF", "indic", "indic_code", "IBOVESPA")
petr_graphs <- gen_graphs("PETR4", "stocks", "neg_code", "Petrobrás - PETR4 (PN)")
vale_graphs <- gen_graphs("VALE3", "stocks", "neg_code", "Vale - VALE3 (ON)")
itub_graphs <- gen_graphs("ITUB4", "stocks", "neg_code", "Itaú - ITUB4 (PN)")
bbdc_graphs <- gen_graphs("BBDC4", "stocks", "neg_code", "Bradesco - BBDC4 (PN)")
boi_graphs <- gen_graphs("BOIDI-AV-R$", "indic", "indic_code", "Live Cattle")
dol_graphs <- gen_graphs("DOL-D1", "indic", "indic_code", "Dollar")
eur_graphs <- gen_graphs("EUR", "indic", "indic_code", "Euro")
soy_graphs <- gen_graphs("SOY-PA-US$", "indic", "indic_code", "Soy")
caf_graphs <- gen_graphs("ACF-SA", "indic", "indic_code", "Coffee")



ibov_df <- get_series_df("IBV-PF", "indic", "indic_code")
boi_df <- get_series_df("BOIDI-AV-R$", "indic", "indic_code")
dol_df <- get_series_df("DOL-D1", "indic", "indic_code")
eur_df <- get_series_df("EUR", "indic", "indic_code")
soy_df <- get_series_df("SOY-PA-US$", "indic", "indic_code")
caf_df <- get_series_df("ACF-SA", "indic", "indic_code")
wti_df <- get_series_df("WTI", "indic", "indic_code")
pre1y_df <- get_series_df("PRE1Y", "interest_rate", "variable")
pre3y_df <- get_series_df("PRE3Y", "interest_rate", "variable")
pre5y_df <- get_series_df("PRE5Y", "interest_rate", "variable")
petr_df <- get_series_df("PETR4", "stocks", "neg_code")
vale_df <- get_series_df("VALE3", "stocks", "neg_code")
itub_df <- get_series_df("ITUB4", "stocks", "neg_code")
bbdc_df <- get_series_df("BBDC4", "stocks", "neg_code")

# normalize datasets by date
common_dates <- base::intersect(ibov_df$ref_date, boi_df$ref_date) %>% 
  base::intersect(dol_df$ref_date) %>% base::intersect(eur_df$ref_date) %>%
  base::intersect(caf_df$ref_date) %>% base::intersect(soy_df$ref_date) %>%
  base::intersect(petr_df$ref_date) %>% base::intersect(vale_df$ref_date) %>%
  base::intersect(itub_df$ref_date) %>% base::intersect(bbdc_df$ref_date) %>% 
  base::intersect(wti_df$ref_date) %>% base::intersect(pre1y_df$ref_date) %>% 
  base::intersect(pre3y_df$ref_date) %>% base::intersect(pre5y_df$ref_date)

.ibov <- ibov_df %>% filter(ref_date %in% common_dates)
.boi <- boi_df %>% filter(ref_date %in% common_dates)
.dol <- dol_df %>% filter(ref_date %in% common_dates)
.eur <- eur_df %>% filter(ref_date %in% common_dates)
.soy <- soy_df %>% filter(ref_date %in% common_dates)
.caf <- caf_df %>% filter(ref_date %in% common_dates)
.wti <- wti_df %>% filter(ref_date %in% common_dates)
.pre1y <- pre1y_df %>% filter(ref_date %in% common_dates) 
.pre3y <- pre3y_df %>% filter(ref_date %in% common_dates)
.pre5y <- pre5y_df %>% filter(ref_date %in% common_dates)
.petr <- petr_df %>% filter(ref_date %in% common_dates)
.vale <- vale_df %>% filter(ref_date %in% common_dates)
.itub <- itub_df %>% filter(ref_date %in% common_dates)
.bbdc <- bbdc_df %>% filter(ref_date %in% common_dates)


.rets <- data.frame(
  IBOV = .ibov$return, 
  BOI = .boi$return,
  DOL = .dol$return,
  EUR = .eur$return,
  SOY = .soy$return, 
  ICF = .caf$return,
  WTI = .wti$return,
  PRE1Y = .pre1y$return,
  PRE3Y = .pre3y$return,
  PRE5Y = .pre5y$return,
  PETR = .petr$return,
  VALE = .vale$return,
  ITUB = .itub$return,
  BBDC = .bbdc$return
)

row.names(.rets) <- .ibov$ref_date

gjrg <- ugarchspec(mean.model = list(armaOrder=c(0, 0)), 
                   variance.model = list(model = "gjrGARCH"),
                   distribution.model = "std")
dcc <- dccspec(uspec = multispec(replicate(ncol(.rets), gjrg)), 
               distribution = 'mvt')
  
gfit <- dccfit(dcc, .rets, fit.control = list(scale = T))

dcccor <- rcor(gfit)

xDates <- gfit@model$modeldata$index
series <- names(.rets)

corr_df <- lapply(1:ncol(.rets), function(i) {
  tmp_df <- lapply(1:ncol(.rets), function(j) {
    if (i == j) NULL
    else {
      cor_series <- dcccor[i, j, ]
      data.frame(value = cor_series, ref_date = xDates,
                 src = paste(series[i], 'X', series[j]),
                 stringsAsFactors = F)
    }
  })
  do.call(rbind, tmp_df)
})

corr_df <- do.call(rbind, corr_df)

.df <- corr_df %>% filter(grepl("IBOV X ", src))

ggplot(.df, aes(x = ref_date, y = value, color = src)) +
  geom_line(size = 0.8) +
  ggrepel::geom_label_repel(data = .df %>% filter(ref_date == max(.df$ref_date)), 
            aes(label = src, colour = src, x = max(.df$ref_date), y = value, 
                hjust = -.1)) + 
  ggtitle("Conditional correlation over time") +
  xlab("Date") + ylab("Correlation") + labs(colour = "Series")







ibov_df <- get_series_df("IBV-PF", "indic", "indic_code")
dol_df <- get_series_df("DOL-D1", "indic", "indic_code")
petr_df <- get_series_df("PETR4", "stocks", "neg_code")
vale_df <- get_series_df("VALE3", "stocks", "neg_code")
# itub_df <- get_series_df("ITUB4", "stocks", "neg_code")
bbdc_df <- get_series_df("BBDC4", "stocks", "neg_code")
wti_df <- get_series_df("WTI", "indic", "indic_code")
pre1y_df <- get_series_df("PRE1Y", "interest_rate", "variable")
pre3y_df <- get_series_df("PRE3Y", "interest_rate", "variable")
pre5y_df <- get_series_df("PRE5Y", "interest_rate", "variable")

# normalize datasets by date
common_dates <- base::intersect(ibov_df$ref_date, dol_df$ref_date) %>% 
  base::intersect(petr_df$ref_date) %>% base::intersect(vale_df$ref_date) %>%
  base::intersect(itub_df$ref_date) %>% base::intersect(bbdc_df$ref_date) %>% 
  base::intersect(wti_df$ref_date) %>% base::intersect(pre1y_df$ref_date) %>% 
  base::intersect(pre3y_df$ref_date) %>% base::intersect(pre5y_df$ref_date)

.ibov <- ibov_df %>% filter(ref_date %in% common_dates)
.dol <- dol_df %>% filter(ref_date %in% common_dates)
.petr <- petr_df %>% filter(ref_date %in% common_dates)
.vale <- vale_df %>% filter(ref_date %in% common_dates)
# .itub <- itub_df %>% filter(ref_date %in% common_dates)
.bbdc <- bbdc_df %>% filter(ref_date %in% common_dates)
.wti <- wti_df %>% filter(ref_date %in% common_dates)
.pre1y <- pre1y_df %>% filter(ref_date %in% common_dates) 
.pre3y <- pre3y_df %>% filter(ref_date %in% common_dates)
.pre5y <- pre5y_df %>% filter(ref_date %in% common_dates)

.rets <- na.omit(data.frame(
  IBOV = .ibov$return, 
  DOL = .dol$return,
  PETR = .petr$return,
  VALE = .vale$return,
  # ITUB = .itub$return,
  BBDC = .bbdc$return,
  WTI = .wti$return,
  PRE5Y = .pre5y$return
))

row.names(.rets) <- .ibov$ref_date

gjrg <- ugarchspec(mean.model = list(armaOrder=c(0, 0)), 
                   variance.model = list(model = "gjrGARCH"),
                   distribution.model = "std")
dcc <- dccspec(uspec = multispec(replicate(ncol(.rets), gjrg)), 
               distribution = 'mvt')

gfit <- dccfit(dcc, .rets, fit.control = list(scale = T))

dcccor <- rcor(gfit)

xDates <- gfit@model$modeldata$index
series <- names(.rets)

corr_df <- lapply(1:ncol(.rets), function(i) {
  tmp_df <- lapply(1:ncol(.rets), function(j) {
    if (i == j) NULL
    else {
      cor_series <- dcccor[i, j, ]
      data.frame(value = cor_series, ref_date = xDates,
                 src = paste(series[i], 'X', series[j]),
                 stringsAsFactors = F)
    }
  })
  do.call(rbind, tmp_df)
})

corr_df <- do.call(rbind, corr_df)

.df <- corr_df %>% filter(grepl("IBOV X ", src))

ggplot(.df, aes(x = ref_date, y = value, color = src)) +
  geom_line(size = 0.8) +
  ggrepel::geom_label_repel(data = .df %>% filter(ref_date == max(.df$ref_date)), 
                            aes(label = src, colour = src, x = max(.df$ref_date), y = value, 
                                hjust = -.1)) + 
  ggtitle("Conditional correlation over time") +
  xlab("Date") + ylab("Correlation") + labs(colour = "Series")

