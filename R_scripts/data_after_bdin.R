# readers for some raw data sources

library(bizdays)
library(utils)
library(httr)
library(lubridate)
library(XML)
library(dplyr)

source("scripts/inserters.R")

bizdays.options$set(default.calendar = "Brazil/ANBIMA")
todos_dias <- list.files("../raw_data", recursive = F)
dias_bdfinal <- todos_dias[todos_dias <= "2017-08-01"]
dias_bvbg <- todos_dias[todos_dias > "2018-02-20" & todos_dias <= "2018-06-01"]
dest_dir <- "../raw_data"

interest_df <- lapply(bizdays::bizseq("2018-02-20", "2018-06-01"), function(rd) {
  d_dir <- file.path(dest_dir, as.character(rd))
  tryCatch ( {read_curve(d_dir, rd)}, 
             error = function(e) { 
               print(paste(rd, "errored."))
              })
})

interest_df <- do.call(rbind, interest_df)

indic_df <- lapply(dias_bvbg, function(rd) {
  d_dir <- file.path(dest_dir, rd)
  read_indic(d_dir, rd)
})

indic_df <- do.call(rbind, indic_df)
  

bdfim_df <- lapply(dias_bdfinal, function(rd) {
  d_dir <- file.path(dest_dir, rd)
  read_bdin(d_dir, rd)
})

deriv_opts_df <- lapply(bdfim_df, function(l) l$opts)
deriv_opts_df <- do.call(rbind, deriv_opts_df)

futs_df <- lapply(bdfim_df, function(l) l$futs) 
futs_df <- do.call(rbind, futs_df)

RELEVANT_MERCS <- c(unique(as.character(futs_df$merc)), "IDI")

bvbg086_df <- lapply(dias_bvbg, function(rd) {
  print(rd)
  d_dir <- file.path(dest_dir, rd)
  read_bvbg86(d_dir, rd)
})

cderiv_opts_df <- lapply(bvbg086_df, function(l) l$opts)
cderiv_opts_df <- do.call(rbind, cderiv_opts_df)

cfuts_df <- lapply(bvbg086_df, function(l) l$futs) 
cfuts_df <- do.call(rbind, cfuts_df)
