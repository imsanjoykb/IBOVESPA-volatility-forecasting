# downloads data from bmf-bovespa website

# Note: Due to website changes these scripts might
# not function correctly

library(bizdays)
library(utils)
library(httr)
library(lubridate)
library(XML)
library(dplyr)

source("scripts/downloaders.R")
source("scripts/main_fun.R")

bizdays.options$set(default.calendar = "Brazil/ANBIMA")
bdays <- bizdays::bizseq("2018-06-20", "2018-06-20")
dest_dir <- "raw_data/"

if (!dir.exists(dest_dir)) dir.create(dest_dir)

for (rd in as.character(bdays)) {
  rd <- as.Date(rd)
  d_dir <- paste0(dest_dir, as.character(rd))
  if (!dir.exists(d_dir)) dir.create(d_dir)
  print(rd)
  # download raw data
  tryCatch(
    {download_data(d_dir, rd)}, 
  error = function(e) {print(paste(rd, "errored."))}
  )
  
}
