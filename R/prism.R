#' Functions for aggregrating PRISM data
#' 
#' @param prismTable A table containing columns that contain at least the following columns:
#' \code{"abs_path"}, contining the path to the daily/weekly/monthy raster layers
#' \code{yearCounter}, a column containing the indexes that you are going to aggregrate _over_
#' (eg, a column of water years if you want to aggregrate by water year).
#' @param yearCounter The name of the column in \code{prismTable} containing your aggregation index.
#' @param areaMask Raster* object or a Spatial* object containing an area mask (if needed).
#' @param fun The aggregration function used to collapse the different raster layers into one
#' summary layer (eg, \code{sum} or \code{mean}).
#' @return A matrix containing statistics run on all of the values within the summary layer.
#' @examples
#' No examples for now
#' 
source("R/utilities.R")

pkgTest("rgdal", "raster")

plainAgg <- function(prismTable, type, yearCounter, areaMask){
  out_mean <- c()
  out_median <- c()
  prismTable <- prismTable[is.na(prismTable[["month"]]) & prismTable[["type"]] == type,]
  # TODO: Apparently using ifelse for non-vectorized operations is a bad practice.
  # It's a style thing; replace with a single `if` (http://stackoverflow.com/a/29784923)
  ifelse(!dir.exists("output/tmp/prism_tmean_yr/"), dir.create("output/tmp/prism_tmean_yr/", recursive = TRUE), FALSE)
  for (year in unique(prismTable[[yearCounter]]))
  {
    print(paste0("Masking raster..."))
    r <- raster(prismTable[["abs_path"]][prismTable[[yearCounter]]==year])
    outFile <- mask(r, areaMask, progress = "text")
  
    print(paste0("Computing cell stats at ", Sys.time()))
    out_mean <- c(out_mean, cellStats(outFile, mean))
    out_median <- c(out_median, cellStats(outFile, median))
    print(paste0("mean ", tail(out_mean, 1), " median ", tail(out_median, 1)))
  }
  returnFrame = cbind(mean = out_mean,median = out_median)
  row.names(returnFrame) = unique(prismTable[[yearCounter]])
  return(returnFrame)
}


month2yrAgg <- function(prismTable, type, yearCounter, areaMask, fun){
  out_mean <- c()
  out_median <- c()
  prismTable <- prismTable[!is.na(prismTable[["month"]]) & prismTable[["type"]] == type,]
  # TODO: Apparently using ifelse for non-vectorized operations is a bad practice.
  # It's a style thing; replace with a single `if` (http://stackoverflow.com/a/29784923)
  ifelse(!dir.exists("output/tmp/prism_tmean_yr/"), dir.create("output/tmp/prism_tmean_yr/", recursive = TRUE), FALSE)
  for (year in unique(prismTable[[yearCounter]]))
  {
    # Set file system path to save output product
    outFileName <- paste("output/tmp/prism_tmean_yr/", year, ".tif", sep="")
    
    # Stack monthly rasters and mask (crop) the entire stack to match areaMask
    print(paste0("Computing raster stack for year ", year, " at ", Sys.time()))
    s <- stack(prismTable[["abs_path"]][prismTable[[yearCounter]]==year])
    print(paste0("Masking raster..."))
    sMasked <- mask(s, areaMask, progress = "text")
    
    # Perform function operation on every layer of stack, cell-wise, producing single output layer
    print(paste0("Computing raster ", as.character(substitute(fun)), " for year ", year, " at ", Sys.time()))
    outFile <- calc(sMasked, fun = fun, filename = outFileName, prj = TRUE, progress = "text", overwrite = TRUE)
    
    print(paste0("Computing cell stats at ", Sys.time()))
    out_mean <- c(out_mean, cellStats(outFile, mean))
    out_median <- c(out_median, cellStats(outFile, median))
    print(paste0("mean ", tail(out_mean, 1), " median ", tail(out_median, 1)))
  }
  returnFrame = cbind(mean = out_mean,median = out_median)
  row.names(returnFrame) = unique(prismTable[[yearCounter]])
  return(returnFrame)
}