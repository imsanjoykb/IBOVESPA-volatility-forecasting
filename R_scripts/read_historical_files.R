# Unzips anual historical files obtained from 
# http://www.bmfbovespa.com.br/pt_br/servicos/market-data/historico/mercado-a-vista/cotacoes-historicas/
# and reads the contents for updating the influxdb database

ZIPS_DIRECTORY <- "."
KEEP_UNZIPPED_FILE <- FALSE
FIELD_SIZES <- c(reg_type = 2, ref_date = 8, bdi_code = 2, neg_code = 12, 
                 mkt_type = 3, issuer_name = 12, specification = 10, 
                 forward_terms = 3, currency = 4, open_prc = 13, 
                 max_prc = 13, min_prc = 13, avg_prc = 13, close_prc = 13, 
                 best_purchase_offer = 13, best_sale_offer = 13, trades_n = 5, 
                 quantity = 18, volume = 18, strike = 13, opt_indicator = 1, 
                 mat_date = 8, quote_factor = 7, points_strike = 13, 
                 isin = 12, dist_number = 3)
PRICES_DEC <- 2
STRK_PTS_DEC <- 6
RELEVANT_BDI_CODES <- c(ROUND_LOT = 02,
                        INDEX_CALL_OPTS = 74, INDEX_PUT_OPTS = 75,
                        CALL_OPTS = 78, PUT_OPTS = 82)

library(influxdbr)
library(dplyr)

file_list <- list.files(ZIPS_DIRECTORY, recursive = F, full.names = T)

res_df <- lapply(file_list[grepl(pattern = "COTAHIST_A\\d*.ZIP", x = file_list)], function (file) {
  txt_path <- file.path(ZIPS_DIRECTORY, unzip(file, list = T)$Name)
  unzip(file, exdir = ZIPS_DIRECTORY)
  contents <- read.fwf(txt_path, FIELD_SIZES, skip = 1)
  if (!KEEP_UNZIPPED_FILE) unlink(txt_path)
  names(contents) <- names(FIELD_SIZES)
  contents[-nrow(contents), ] %>% filter(bdi_code %in% RELEVANT_BDI_CODES)
})

res_df <- do.call(rbind, res_df)

res_df <- res_df %>% select(ref_date, bdi_code, neg_code, mkt_type, 
                            open_prc, max_prc, min_prc, avg_prc, close_prc, 
                            trades_n, quantity, volume, strike,  
                            opt_indicator, mat_date, 
                            quote_factor, points_strike) %>%
                     mutate(ref_date = as.Date(as.character(ref_date), format = "%Y%m%d"), 
                            neg_code = trimws(as.character(neg_code)),
                            open_prc = open_prc/(10^PRICES_DEC),
                            max_prc = max_prc/(10^PRICES_DEC),
                            min_prc = min_prc/(10^PRICES_DEC),
                            avg_prc = avg_prc/(10^PRICES_DEC),
                            close_prc = close_prc/(10^PRICES_DEC),
                            volume = volume/(10^PRICES_DEC),
                            strike = strike/(10^PRICES_DEC),
                            points_strike = points_strike/(10^STRK_PTS_DEC),
                            mat_date = as.Date(as.character(mat_date), format = "%Y%m%d"))
                

historical_stocks <- res_df %>% filter(bdi_code == 2, !grepl("11", res_df$neg_code)) %>% 
                                mutate(open_prc = open_prc/quote_factor, 
                                       max_prc = max_prc/quote_factor,
                                       min_prc = min_prc/quote_factor,
                                       avg_prc = avg_prc/quote_factor,
                                       close_prc = close_prc/quote_factor) %>% 
                                select(-strike, - quote_factor, -mkt_type, 
                                       -strike, -opt_indicator, -mat_date,
                                       -points_strike, -bdi_code)

historical_options <- res_df %>% filter(bdi_code == 78 | bdi_code == 82) %>% 
                                mutate(trades_n = trades_n * 100,
                                       opt_type = ifelse(mkt_type == 70, 'call', 'put')) %>% 
                                select(-quote_factor, -mkt_type, -opt_indicator, 
                                       -points_strike, - bdi_code)

historical_ibov_options <- res_df %>% filter(bdi_code == 74 | bdi_code == 75) %>% 
                                      mutate(trades_n = trades_n * 100,
                                             opt_type = ifelse(mkt_type == 70, 'call', 'put')) %>% 
                                      select(-quote_factor, -mkt_type, -opt_indicator, 
                                             -points_strike, - bdi_code)