pkgTest <- function(...) {
  # -- compiled from many sources by asmtry --
  # Install dependency libraries if not installed and load them
  library(utils) #Seems to break in .Rprofile without this line
  dep.list <- c(...)
  new_pkgs <- dep.list[!(dep.list %in% installed.packages()[,"Package"])]
  if(length(new_pkgs)) install.packages(new_pkgs)
  invisible(lapply(dep.list, library, character.only=T))
}

rtTagParam <- function(rt.table) {
  rt.table[c("xmin", "xmax", "ymin", "ymax")] <- t(sapply(rt.table[["abs_path"]], 
                                                          function(x) as.vector(extent(raster(x)))))
  rt.table["ncell"] <- sapply(rt.table[["abs_path"]], 
                              function(x) ncell(raster(x)))
  rt.table[c("xres", "yres")] <- t(sapply(rt.table[["abs_path"]], 
                                          function(x) res(raster(x))))
  rt.table["nlayers"] <- sapply(rt.table[["abs_path"]], 
                                function(x) ifelse(nlayers(stack(x))>1,nlayers(stack(x)),NA))
  
  return(rt.table)
}

rtTagAnom <- function(input.table, column.var, criteria) {
  output.tag <- rep(FALSE, nrow(input.table))
  # TODO: Vectorize
  for (item in column.var) {
    output.tag[input.table[item] != criteria(input.table[item])] <- TRUE
  }
  return(output.tag)
}

makeAnomtable <- function(parameter.table) {
  return(as.data.frame(sapply(c("xmin", "xmax", "ymin", "ymax", "ncell", "xres", "yres"),  
                              function(x) tagAnomalous(parameter.table, x, modal))))
}

waterYearlt <- function(dates){
  # Accept dates and convert to a water-year date ('lt' = local time) where:
  # Year = water year
  # Month = Month number in water year (eg october = 1, september = 12)
  dates.posix = as.POSIXlt(dates) # If not already
  # Note: POSIXlt, October is month 9
  dates.posix$year <- ifelse(dates.posix$mon > 8, dates.posix$year + 1, dates.posix$year)
  dates.posix$mon <- ifelse(dates.posix$mon > 8, dates.posix$mon - 9, dates.posix$mon + 3)
  
  return(dates.posix)
}

# E:\Users\lbooth\Downloads\RStudioPortable\RStudioPortable.exe
# gdalwarp_wrapper <- function(path.executable, path.gdaldata, argument.string, cut.shapefile, src.file, dst.file) {
# "--config GDAL_DATA", shQuote(path.gdaldata), 
gdalwarp_wrapper <- function(path.executable, argument.string, cut.shapefile, src.file, dst.file) {
  cmd <- paste(shQuote(path.executable), argument.string, 
               "-cutline", cut.shapefile, 
               src.file, dst.file, sep = " ")
  warning(paste("Executing:", cmd, "at", Sys.time(), sep = " "))
  system(cmd, intern = TRUE)
}

# Szudzik's pairing function http://szudzik.com/ElegantPairing.pdf
szudzik_pair <- function(a, b) {
  # Does not test for unequal length inputs, relies on ifelse's usual behavior
  return(ifelse(a >= b, a^2 + a + b, a + b^2))
}

szudzik_unpair <- function(z) {
  x = floor(sqrt(z))
  y = z - x^2
  return(data.frame(
    a = ifelse(y > x, x, y),
    b = ifelse(y > x, y-x, x)
  ))}
