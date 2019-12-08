## read or write shape files
bw_dt <- mclapply(seq(2009, 2018),
                  function(x) {
                      tmp <- list()
                      for (y in c('buurt', 'gem')) {
                          out_file <- paste0(x, '_', y, '.shp')
                          out_path <- paste0('~/calculate_road_distance/data/', out_file)
                          if (!file.exists(out_path)) {
                              tryCatch({
                                  tmp[[y]] <- cbs_shape_read(x, y)
                                  st_write(tmp[[y]], out_path, delete_layer = T)
                              },
                              error = function(e) message(paste0('Error', e))
                              )
                          } else {
                              tmp[[y]] <- st_read(out_path)
                          }  
                      }
                      return(tmp)
                  },
                  mc.cores = n_cores
                  )

## read vierkant statistieken
vierkant_shape_file_paths <- c(
    "2000" = "https://www.cbs.nl/-/media/cbs/dossiers/nederland%20regionaal/vierkanten/2017-cbsvierkant100m.zip",
    "2015" = "https://www.cbs.nl/-/media/cbs/dossiers/nederland%20regionaal/vierkanten/2019-cbs_vk100_2015_v2.zip",
    "2016" = "https://www.cbs.nl/-/media/cbs/dossiers/nederland%20regionaal/vierkanten/2019-cbs_vk100_2016_v2.zip",
    "2017" = "https://www.cbs.nl/-/media/cbs/dossiers/nederland%20regionaal/vierkanten/2019-cbs_vk100_2017_v2.zip",
    "2018" = "https://www.cbs.nl/-/media/cbs/dossiers/nederland%20regionaal/vierkanten/2019-cbs_vk100_2018_v1.zip")

vierkant_dt <- mclapply(seq_along(c(2000, 2015:2018)),
                      function(x) {
                          year <- paste(c(2000, 2015:2018)[x])
                          out_path <- paste0('~/calculate_road_distance/data/', year, '_vierkant.shp')
                          if (!file.exists(out_path)) {
                              in_path <- vierkant_shape_file_paths[[year]]
                              temp_zip <- tempfile()
                              download.file(in_path, temp_zip)
                              temp_dir <- tempdir()
                              out_files <- unzip(temp_zip,
                                                 junkpaths = TRUE,
                                                 exdir = temp_dir)
                              tmp <- st_read(out_files[grepl('.shp', out_files)])
                              st_write(tmp, out_path)
                              unlink(temp_dir, force = T)
                          } else {
                              tmp <- st_read(out_path)
                          }
                          return(tmp)
                      },
                      mc.cores = n_cores
                      )


## end of code
