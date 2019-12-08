## population weighted centroids of municipalities
pop_w_cents <- mclapply(seq(2009, 2018),
                        function(x) {
                            x_ind <- which(seq(2009, 2018) == x)         
                            dt1 <- subset(bw_dt[[x_ind]][[2]],
                                          WATER == 'NEE' & GM_CODE != 'GM9999'
                                          )
                            if (x < 2015) {
                                dt2 <- subset(vierkant_dt[[1]],
                                              get(paste0('INW', x)) >= 0,
                                              select = c('C28992R100',
                                                         paste0('INW', x)
                                                         )
                                              )
                            } else {
                                dt2 <- subset(vierkant_dt[[x_ind - 5]],
                                              INWONER >= 0,
                                              select = c('C28992R100',
                                                         "INWONER"
                                                         )
                                              )
                            }
                            ## find centroids in each square and overlap with municipality
                            vk_cents <- data.table(st_join(st_centroid(dt2),
                                                           dt1)
                                                   )
                            ## clean up a bit
                            vk_cents[,
                                     ':=' (x = as.numeric(geometry[[1]][1]),
                                           y = as.numeric(geometry[[1]][2])),
                                     by = C28992R100
                                     ]
                            setnames(vk_cents, 2, 'INWONER')
                            ## create population weighted centroids
                            pop_w_cents <- vk_cents[!is.na(GM_CODE) & INWONER > 0,
                                                    lapply(.SD, weighted.mean, w = INWONER),
                                                    by = GM_CODE,
                                                    .SDcols = c('x', 'y')
                                                    ][,
                                                      code := as.numeric(gsub('GM', '', GM_CODE))
                                                      ]
                            ## check whether the points are inside municipality limits
                            pop_w_cents_sf <- st_as_sf(pop_w_cents,
                                                       coords = c('x', 'y'),
                                                       crs = st_crs(dt1),
                                                       agr = "constant")
                            ## find centrois outside municipality limits
                            cent_gem_int <- data.table(
                                mun_id = as.numeric(st_intersects(pop_w_cents_sf,
                                                                  dt1
                                                                  )
                                                    )
                            )[,
                              row.id := 1:.N
                              ]
                            missing_cents <- pop_w_cents[which(!cent_gem_int$mun_id %in% seq(1, nrow(pop_w_cents))),
                                                         GM_CODE
                                                         ]
                            ## allocate randomly those that are missing
                            if (length(missing_cents) > 0 ) {
                                set.seed(123)
                                tmp <- as(dt1[as.numeric(gsub('GM', '',
                                                              dt1$GM_CODE)
                                                         ) %in% missing_cents],
                                          'Spatial')
                                missing_cents <- cbind(missing_cents,
                                                       data.frame(spsample(tmp,
                                                                           1,
                                                                           'random')
                                                                  )
                                                       )
                                missing_cents$code <- gsub('GM', '', missing_cents[,1])
                                pop_w_cents <- rbindlist(
                                    list(pop_w_cents[!GM_CODE %in% missing_cents[,1]],
                                         missing_cents)
                                )
                            }                            
                            return(pop_w_cents)
                        },
                        mc.cores = n_cores
                        )

## end of file







