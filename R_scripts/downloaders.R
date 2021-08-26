
download_data <- function(dest_dir, ref_date) {
  
  b3_download_handler(c("TS", "ID"), c("curv.txt", "ind.txt"), ref_date, dest_dir)
  
  bvbg_download_handler("PR", "BVBG.086.xml", ref_date, dest_dir)
  
}


b3_download_handler <- function(urlfname, fname, data_ref, dest_dir) {
  mapply(function(url_fname, fname) {
    b3_file_downloader(url_fname, fname, dest_dir, data_ref)
  }, urlfname, fname)
}

b3_file_downloader <- function(url_fname, fname, dest_dir, ref_date) {
  data <- as.Date(ref_date)
  
  data <- format(x=data, format="%y%m%d")
  url <- 'http://www.bmfbovespa.com.br/pesquisapregao/download?filelist='
  url <- paste(url, url_fname, data, '.ex_', sep="")
  dest_file <- paste(dest_dir, '/', fname, '.zip', sep="")
  final_file <- paste(dest_dir, '/', fname, sep="")
  if (url_fname == "BF") {
    url <- paste0('http://www.bmf.com.br/Ftp/ContratosPregaoFinal/BF', data, '.ex_')
    dest_file <- paste(dest_dir, '/', fname, '.ex_', sep="")
    download.file(url, dest_file, "auto", mode="wb", quiet = T)
    txt_files <- unzip(zipfile=dest_file, exdir=dest_dir)
    file.remove(dest_file)
    file.copy(txt_files[length(txt_files)], final_file)
    file.remove(txt_files)
  } else {
    download.file(url, dest_file, "auto", mode="wb", quiet = T)
    if(file.exists(dest_file)) {
      sec_zip <- unzip(zipfile=dest_file, exdir=dest_dir)
      txt_files <- unzip(zipfile=sec_zip, exdir=dest_dir)
      file.remove(dest_file)
      file.remove(sec_zip)
      file.copy(txt_files[length(txt_files)], final_file)
      file.remove(txt_files)
      final_file
    } else {
      print(paste("Error downloading file ", obj$filename))
      return(NULL)
    }
  }
}


bvbg_download_handler <- function(urlfname, fname, data_ref, dest_dir) {
  data <- as.Date(data_ref)
  data <- format(x=data, format="%y%m%d")
  url <- 'http://www.bmfbovespa.com.br/pesquisapregao/download?filelist='
  url <- paste(url, urlfname, data, '.zip', sep="")
  dest_file <- paste(dest_dir, '/', fname, '.zip', sep="")
  final_file <- paste(dest_dir, '/', fname, sep="")
  download.file(url, dest_file, "auto", quiet = T)
  if(file.exists(dest_file)) {
    sec_zip <- unzip(zipfile=dest_file, exdir=dest_dir)
    xml_files <- unzip(zipfile=sec_zip, exdir=dest_dir)
    file.remove(dest_file)
    file.remove(sec_zip)
    file.copy(xml_files[length(xml_files)], final_file)
    file.remove(xml_files)
    final_file
  } else {
    stop(paste("Error downloading file ", fname))
    return(NULL)
  }
}


curva_download_handler <- function(filename, data_ref, dest_dir) {
  zfname <- download_curva_file(filename, data_ref, dest_dir)
  if (!is.null(zfname) && file.exists(zfname)) {
    fname <- unzip(zfname, exdir=dest_dir)
    unlink(zfname)
    f_name <- unzip(fname, exdir = dest_dir)
    unlink(fname)
  } else
    print("Couldn't download file from BM&F BOVESPA - bmf_download_handler")
}


download_curva_file <- function(filename, refDate, destDir, tmpfile=FALSE) {
  if (tmpfile)
    dest <- tempfile(tmpdir=destDir, fileext='.zip')
  else
    dest <- file.path(destDir, filename)
  download_curva_binary_file(dest, refDate)
  dest
}

download_curva_binary_file <- function (dest, date) {
  url <- paste0('http://www.bmfbovespa.com.br/pesquisapregao/download?filelist=TS',
                format(as.Date(date, format = "%d/%m/%Y"), "%y%m%d"),'.ex_')
  req <- GET(url)
  cnt <- content(req)
  if (req$headers[["content-type"]] == "application/octet-stream") {
    writeBin(cnt, dest)
    dest
    Sys.sleep(1)
  } else {
    print("download error, binary data expected! - download_binary_file")
  }
}
