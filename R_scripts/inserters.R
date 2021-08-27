library(utils)
library(lubridate)
library(XML)
library(dplyr)

INDIC_SIZES <- c(trans_idf = 6, comp = 3, tipo_reg = 2, file_date = 8, 
                 indic_group = 2, indic_code = 25, value = 25, dec = 2)

read_indic <- function(dir, ref_date) {
  filep <- file.path(dir, "ind.txt")
  contents <- read.fwf(filep, INDIC_SIZES)
  names(contents) <- names(INDIC_SIZES)
  contents %>% mutate(ref_date = as.Date(ref_date), value = as.numeric(value)/(10^(dec)), 
                      indic_code = trimws(indic_code), file_date = as.Date(as.character(file_date), format="%Y%m%d")) %>%
    filter(ref_date == file_date) %>%
    select(ref_date, indic_group, indic_code, value)
}


CURVE_SIZES <- c(transf_id = 6, comp = 3, tip = 2, date = 8, cod_term = 2, 
                 cod_curv = 5, desc_curv = 15, DC = 5, DU = 5, sig = 1, 
                 value = 14, carac = 1, cod_vert = 5)

read_curve <- function(dir, ref_date) {
  filep <- file.path(dir, "curv.txt")
  contents <- read.fwf(filep, CURVE_SIZES)
  names(contents) <- names(CURVE_SIZES) 
  contents <- contents %>% mutate(DC = as.numeric(DC), value = as.numeric(value)/(10^7), 
                                  cod_curv = trimws(cod_curv)) %>% filter(cod_curv == "PRE")
  interp <- splinefun(x = contents$DC, y = contents$value, method = "monoH.FC")
  
  data.frame(ref_date = ref_date, PRE1Y = interp(365), PRE3Y = interp(365*3), PRE5Y = interp(365*5))
}

BDF_SIZES <- c(trans_id = 6, comp = 3, tipo_reg = 2, file_date = 8,
               tip_neg = 2, cod_merc = 3, cod_m = 1, tipo_serie = 1, 
               serie = 4, time = 6, maturity = 8, strike = 13, 
               contr_size = 13, volume = 13, volumeDol = 13, # contr_size 7 dec
               qtd_aberto = 8, trades_n = 8, quantity = 8, 
               qtd_ult_ofc = 5, sig_ult_ofc = 1, prc_ult_ofc = 8, 
               qtd_ult_ofv = 5, sig_ult_ofv = 1, prc_ult_ofv = 8, 
               sig_open_prc = 1, open_prc = 8, sig_min_prc = 1, 
               min_prc = 8, sig_max_prc = 1, max_prc = 8, 
               sig_avg_prc = 1, avg_prc = 8, qtd_ult_neg = 5,
               sig_last_prc = 1, last_prc = 8, last_time = 6,
               last_date = 8, sig_close_d1 = 1, close_d1 = 8,
               sig_close = 1, close_prc = 8, sig_adj = 1, 
               adjusted_prc = 13, sit_adj = 1, sig_adj_d1 = 1,
               adj_d1 = 13, sit_adj_d1 = 1, adj_by_contr = 13, # adj_by_contr 2 dec
               exerc_volume = 13, exerc_volume_dol = 13, 
               exerc_trade_n = 8, exerc_quantity = 8,
               prices_dec = 1, adj_dec = 1, osc = 8,  # osc 1 dec
               sig_osc = 1, diff_val = 8,  # diff_val se futuro dec = ajuste else dec = prc
               sig_diff = 1, equiv_val = 8, dol_d1 = 13, dol_d0 = 13, # dol 7 dec, equiv 2 dec
               delta_opc = 9, saques = 5, dc = 5, du = 5, ulying_mat = 4,  # delta 7 dec
               margem1 = 13, margem2 = 13, data_entrega = 8, seq_fut = 3, 
               cod_voz = 20, cod_gts = 20, cneg = 1, ref = 4, 
               neg_limit = 8, fin_term = 8, sig_min_lim = 1, 
               min_lim = 13, sig_max_lim = 1, max_lim = 13)


read_bdin <- function(dir, ref_date) {
  filep <- file.path(dir, "bdfinal.txt")
  contents <- read.fwf(filep, BDF_SIZES)
  names(contents) <- names(BDF_SIZES)
  contents <- contents %>% mutate(ref_date = as.Date(ref_date),
                      neg_code = trimws(cod_gts), 
                      merc = cod_merc, 
                      mat_date = as.Date(as.character(maturity), format = "%Y%m%d"),
                      strike = strike/(10^prices_dec), 
                      opt_type = ifelse(tipo_serie == "*", NA, ifelse(tipo_serie == "C", "call", "put")),
                      min_prc = as.numeric(paste0(sig_min_prc, min_prc))/(10^prices_dec),
                      max_prc = as.numeric(paste0(sig_max_prc, max_prc))/(10^prices_dec),
                      avg_prc = as.numeric(paste0(sig_avg_prc, avg_prc))/(10^prices_dec),
                      close_prc = as.numeric(paste0(sig_last_prc, last_prc))/(10^prices_dec),
                      adjusted_prc = as.numeric(paste0(sig_adj, adjusted_prc))/(10^adj_dec),
                      dol_d0 = dol_d0/(10^7)) %>%  filter(quantity != 0) 
  
  opts_df <- contents %>% filter(cod_m == 3 | cod_m == 4) %>%
    select(ref_date, neg_code, open_prc, max_prc, min_prc, avg_prc, close_prc, 
           trades_n, quantity, volume, strike, mat_date, opt_type)
  others_df <- contents %>% filter(cod_m != 3 & cod_m != 4) %>%
    select(ref_date, merc, neg_code, open_prc, max_prc, min_prc, avg_prc, close_prc, 
           adjusted_prc, mat_date, trades_n, quantity, volume)
  
  list(opts = opts_df, futs = others_df)
  
}


MONTH_CODES <- c(`F` = "01", G = "02", H = "03",
                 J = "04", K = "05", M = "06", 
                 N = "07", Q = "08", U = "09", 
                 V = "10", X = "11", Z = "12")

read_bvbg86 <- function(dir, ref_date) {
  filep <- file.path(dir, "BVBG.086.xml")
  

  negDoc <- xmlInternalTreeParse(filep)
  negs <- getNodeSet(negDoc, "//d:PricRpt", c(d="urn:bvmf.217.01.xsd"))
  
  negs_df <- lapply(negs, function(node) {
    ticker <- xmlValue(node[['SctyId']][['TckrSymb']])
    merc <- substr(ticker, 1, 3)
    trd_dt <- xmlValue(node[['TradDt']][['Dt']])
    if(merc %in% RELEVANT_MERCS && ref_date == as.character(trd_dt)) {
      attrib <- node[['FinInstrmAttrbts']]
      volume_reais <- as.numeric(xmlValue(attrib[['NtlFinVol']]))
      neg1 <- as.numeric(xmlValue(attrib[['RglrTxsQty']]))
      neg1 <- if(is.na(neg1)) 0 else neg1
      neg2 <- as.numeric(xmlValue(attrib[['NonRglrTxsQty']]))
      neg2 <- if(is.na(neg2)) 0 else neg2
      n_negocios <- neg1 + neg2
      contratos_negociados <- as.numeric(xmlValue(attrib[['FinInstrmQty']]))
      preco_abertura <- as.numeric(xmlValue(attrib[['FrstPric']]))
      preco_minimo <- as.numeric(xmlValue(attrib[['MinPric']]))
      preco_maximo <- as.numeric(xmlValue(attrib[['MaxPric']]))
      preco_medio <- as.numeric(xmlValue(attrib[['TradAvrgPric']]))
      valor_fechamento <- as.numeric(xmlValue(attrib[['LastPric']]))
      qt <- as.numeric(xmlValue(attrib[['AdjstdQt']]))
      qt_tax <- as.numeric(xmlValue(attrib[['AdjstdQtTax']]))
      valor_ajuste <- if (is.na(qt)) qt_tax else qt
      if (nchar(ticker) > 7) {
        tipo_opc <- ifelse(substr(ticker, 7, 7) == "C", "call", "put")
        strike <- as.numeric(substr(ticker, 8, nchar(ticker))) # some strikes have decimals, some don't...
      } else {
        tipo_opc <- NA
        strike <- NA
      }
      mat_mon <- MONTH_CODES[substr(ticker, 4, 4)]
      mat_yr <- paste0("20", substr(ticker, 5, 6))
      mat_date <- ifelse(is.na(mat_yr), NA, paste0(mat_yr, "-", mat_mon, "-01"))
      # not really, contracts actually differ in day of month 
      # maturity, but this information is on the BVBG 028 file.
      
      
      data.frame(ref_date = ref_date, neg_code = ticker,
                 open_prc = preco_abertura, max_prc = preco_maximo, 
                 min_prc = preco_minimo, merc = merc,
                 avg_prc = preco_medio, close_prc = valor_fechamento, 
                 trades_n = n_negocios, quantity = contratos_negociados,
                 volume = volume_reais, strike = strike, mat_date = mat_date,
                 opt_type = tipo_opc, adjusted_prc = valor_ajuste,
                 stringsAsFactors = FALSE)
    } else NA
  })
  negs_df <- do.call(rbind, negs_df) %>% filter(!is.na(neg_code), trades_n > 0)
  
  opts_df <- negs_df %>% filter(nchar(neg_code) > 7) %>%
    select(ref_date, neg_code, open_prc, max_prc, min_prc, avg_prc, close_prc, 
           trades_n, quantity, volume, strike, mat_date, opt_type)
  others_df <- negs_df %>% filter(nchar(neg_code) < 7) %>%
    select(ref_date, merc, neg_code, open_prc, max_prc, min_prc, avg_prc, close_prc, 
           adjusted_prc, mat_date, trades_n, quantity, volume)
  rm(negs)
  rm(negDoc)
  rm(negs_df)
  gc()
  list(opts = opts_df, futs = others_df)
}


