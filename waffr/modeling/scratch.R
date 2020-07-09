cdl.lut <- read.dbf(file = "input/NASS_CA_NAD83/CDL_2007_06.tif.vat.dbf")
cdl.lut <- class.table.2007[,c(2,3)]

importLUT <- function(input.csv){
  cup_kc_cdl <- read_csv("D:/lbooth/wfar/input/TABLES/cup+_kc_cdl.csv", 
                         col_types = cols(`Crop Number` = col_character(), 13 = col_skip()))
  cup_kc_cdl["start.date"] <- as.Date(paste(cup_kc_cdl$`Planting Month`, cup_kc_cdl$`Planting Day`, 2001, 
                                            sep = "-"), format = "%m-%d-%Y")
  cup_kc_cdl["end.date"] <- as.Date(paste(cup_kc_cdl$`Harvest Month`, cup_kc_cdl$`Harvest Day`, 2001, 
                                            sep = "-"), format = "%m-%d-%Y")
}



### Prepare ETo Data ###
# 2012, 2014, and 2015 have extent issues


cimisCRS <- CRS("+init=epsg:3488")
for (year in c(2013:2015))
{
  outFileName <- paste0("output/eto/", year, ".tif")
  cimisPaths <- list.files(path=paste0("D:/lbooth/wfar/input/SPATIALCIMIS/",year), pattern=".(asc)$", full.names=T, recursive=TRUE)
  ETo <- stack(cimisPaths)
  crs(ETo) <- cimisCRS
  calc(ETo, fun = sum, filename = outFileName, prj = TRUE, progress = "text")
}

class.table.2007 <- read.dbf(file = "input/NASS_CA_NAD83/CDL_2007_06.tif.vat.dbf")
class.table.2007 <- class.table.2007[,c(2,3)]

