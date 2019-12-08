## title: calculate road distance using osrm
## author: trond
## date: 8.12.2019

## house keeping

library(raster)
library(data.table)
library(rjson)
library(httr)
library(cbsodataR)
library(cbsshape)
library(parallel)
library(doParallel)
library(sf)
library(sp)

## find number of cores
n_cores <- detectCores() - 1

## read data
source('read_files.R')

## calculate population weighted centroids of municipalities
source('calculate_popw_centroids.R')

## calculate distance over the road between buurt and gem centroids
url <- "http://127.0.0.1:5000" ## url of local backend
pckgs <- c('data.table', 'httr', 'jsonlite')

## initiate cluster
cl <- makeCluster(n_cores, type="FORK")
registerDoParallel(cl)

for (year %in% seq(2009, 2018)) {
    year_ind <- which(seq(2009, 2018) == year)
    dt1 <- bw_dt[[year_ind]][[1]]           
    pts_orig <- data.table(bu_code = as.numeric(gsub('BU', '', dt1$BU_CODE)),
                           gem_code = as.numeric(gsub('GM', '', dt1$GM_CODE)),
                           st_coordinates(st_centroid(dt1))
                           )
    pts_dest <- pop_w_cents[[year_ind]]
    cent_grid <- data.table(expand.grid(pts_orig$bu_code, pts_dest$code))
    setkey(cent_grid, Var1)
    setkey(pts_orig, bu_code)
    cent_grid[pts_orig, ':=' (o_x = i.X, o_y = i.Y, gem_code = i.gem_code)]
    setkey(cent_grid, Var2)
    setkey(pts_dest, code)
    cent_grid[pts_dest, ':=' (d_x = i.x, d_y = i.y)]
    cent_grid <- cent_grid[Var2 != gem_code][, gem_code := NULL][1:1000, ]
    source('calculate_distance.R')
}

stopImplicitCluster()
registerDoSEQ()

## end of script
