## api query for each od pair
system.time(
    results <- foreach(dest = unique(cent_grid$Var2),
                       .packages=pckgs) %dopar%
        {
            ## find destinations not previously in the od list
            orig_list <- unique(cent_grid$Var1)
            dist_dt <- list()
            ## loop over each destination
            tryCatch({
                for (orig in  orig_list) {                    
                    ## create url with od pairs
                    path <- paste0("/route/v1/driving/",
                                   cent_grid[Var1 == orig & Var2 == dest, o_x],
                                   "_",
                                   cent_grid[Var1 == orig & Var2 == dest, o_y],
                                   "-",
                                   cent_grid[Var1 == orig & Var2 == dest, d_x],
                                   "_",
                                   cent_grid[Var1 == orig & Var2 == dest, d_y],
                                   "?overview=false")
                    path <- gsub('_', ',', gsub('-', ';', path))
                    ## initialisation
                    attempt <- 1
                    raw.result <- NULL
                    ## api query: try 10 times
                    while( is.null(raw.result) && attempt <= 10) {
                        attempt <- attempt + 1
                        try(
                            raw.result <- fromJSON(rawToChar(GET(url = url, path = path)$content))$routes$distance
                        )
                        ## sleep for some times if the request bounces back
                        if (is.null(raw.result)) {Sys.sleep(2)}
                    }
                    ## gather result into a data table
                    dist_dt[[orig]] <- data.table(o = orig, d = dest, dist = ifelse(is.null(raw.result), NA, raw.result))
                }
                return(rbindlist(dist_dt))
            },
            error = function(e) return(paste0("The variable '", orig, "'",
                                              " caused the error: '", e, "'"))
            )
        }    
)
