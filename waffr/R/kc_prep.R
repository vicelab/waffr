#' In this file, we create the Kc rasters using Kc values compiled for the CUP+ model.
#' Out of 70 crops that we have Kc values for,
#'
#' We also import Kc data compiled by Snyder, Orang, Bali, Eching, and Zaccaria 2000 (revised 2014).
#' The data was compiled for the `Basic Irrigation Scheduling` model (BISe), an Excel application that
#' estimates irrigation water requirement and crop ET using the ASCE-PM equation. Kcs are found on the
#' `CropRef` worksheet of the BISe application. Remarks are as follows:
#'
#' 	> Kc data marked in blue were derived in work by T.C. Hsiao and former students at UC Davis.
#'
#' > The Kc for corn was derived by Steduto and Hsiao (1998) maize canopies uhnder two soil water regimes II. Seasonal trends of evapotranspiration, carbon dioxide assimilation and canopy conductance, and as related to leaf area index. Agric. and forest  Meteorol. 89:185-200.
#' The Kc =1.05 for cotton is based on work by Held and Hsiao
#' The Kc = 1.00 for sorghum is based on work by Held and Hsiao.  Millet and
#' For tomato a Kc = 1.10 was selected based on unpublished data from Snyder and Cahn and on expeiments by Held & Hsiao.  The kc values reported by Held and Hsiao were slightly higher, but the tomatoes were full canopy (not in beds, which is the normal practice). The data from Snyder and Cahn were typical for California practices.
#' The data for sunflower were based on data from Hsiao (personal communication)
#'
#'
#' > Kc data marked in green were derived from several sources.  The assumption is that corn has a Kc = 1.00 for ETo calculated using the Pruitt and Doorenbos (1977) hourly ETo equation that is used by the California Irrigation Management Information System CIMIS
#'
#' > Snyder and Pruitt (1992) Evapotranspiration Data Management in California Irrigation & Drainage Session Proceedings/Water Forum '92, EE,HY,IR,WR Div/ASCE, Baltimore, MD/August 2-6, 1992. pp128-133..
#'
#'
#' > Relative Kp values for alfalfa for the crops marked in green were selected from
#'
#' > Wright (1982) New Evapotranspiration Crop Coefficients. Presented at Irrigation and Drainage Specialty Conference, ASCE, July 17-20, Albuquerque, New Mexico. pp 57-74.
#'
#' > The peak Kp values were corn = 0.95, alfalfa = 1.0, beans = 1.0, potatoes = 0.8, sugar beets = 1.0, peas = 0.9, and cereals = 1.0.  Because the equation for ETo was not available at that time, the Kp values cannot be used directly.  However, assuming the Kc = 1.00 is correct for corn, then the approximate peak Kc values for the other crops are found by dividing the Kp by 0.95.  The peak  Kc values for a grass ETo are corn = 1.00, alfalfa = 1.05, beans = 1.00, potatoes = 0.85, sugar beets = 1.05, peas =0.95, and cereals = 1.05.
#'
#' > The rice Kc = 1.1 more current data on typical ETo from CIMIs and the article
#'
#' > Lourence and Pruitt (1971) Energy balance and water use of rice grown in the Central Valley of California. Agron. J. 63:827-832.
#'
#' > Lourence and Pruitt found ET of rice to be about 4-5% higher than lysimeter measured grass in Davis.  Rice ET was measured by Bowen ratio about 25 miles north of Davis.  The postulated that the ETo would be less in the rice growing region because of higher humidity.  As a result, they recommend a Kc = 1.20 to 1.25.  However, CIMIS data indicates that the ETo is only about 5% higher in Davis than at the Nicolas site and in Colusa, which are near the rice growing region. As a result, we would recommend a Kc = 1.05 x 1.05 = 1.10 to estimate ETrice from ETo estimated at a CIMIS station in the rice growing region..

tagAnomalous <- function(input.table, column.var, criteria) {
	output.tag <- rep(FALSE, nrow(input.table))
	# TODO: There's probably a more effecient way of doing this
	for (item in column.var) {
		output.tag[input.table[item] != criteria(input.table[item])] <- TRUE
	}
	return(output.tag)
}

makeAnomtable <- function(parameter.table) {
	return(as.data.frame(sapply(c("xmin", "xmax", "ymin", "ymax", "ncell", "xres", "yres"),
															function(x) tagAnomalous(parameter.table, x, modal))))
}

tagKcDaily <- function(t, crop) {
	# TODO: Refactor into cases
	# TODO: Use interpolation function to allow for different smoothers
	fKc <- ifelse(t < crop[["t_a.1"]], 0,
								ifelse(t >= crop[["t_a.1"]] & t <= crop[["t_e.1"]],  crop[["1st VALUE"]], 0))

	# Only execute next calc if dual crop parameters exist
	if (!is.na(crop[["t_a.2"]])) {
		fKc2 <- ifelse(t < crop[["t_a.2"]], 0,
									 ifelse(t >= crop[["t_a.2"]] & t <= crop[["t_e.2"]],  crop[["2nd VALUE"]], 0))

		# TEST: for overlap between crop 1 and crop 2
		# DRY: This test should occur when CDL.Kc.LUT is imported.
		# TODO: Move to unit tests
		if(any(fKc!=0 & fKc2!=0))
			warning(paste0("oops, you may have double crops with
                     overlapping growing periods for ", crop[["cdl_name"]]))
		# Merge dual-cropping scenarios
		fKc[fKc2!=0] = fKc2[fKc2!=0]
	}
	return(fKc)
}

calculateKcDaily <- function(t, crop) {
	# TODO: Refactor into cases
	# TODO: Use interpolation function to allow for different smoothers
	fKc <- ifelse(t < crop[["t_a.1"]], 0,
								ifelse(t >= crop[["t_a.1"]] & t <= crop[["t_b.1"]],  crop[["Kc AB"]],
											 ifelse(t > crop[["t_b.1"]] & t < crop[["t_c.1"]],
											 			 crop[["Kc AB"]] + round(as.numeric(t - crop[["t_b.1"]], units = "days") *
											 			 													((crop[["Kc CD"]] - crop[["Kc AB"]]) / as.numeric((crop[["t_c.1"]] - crop[["t_b.1"]]), units = "days"))),
											 			 ifelse(t >= crop[["t_c.1"]] & t <= crop[["t_d.1"]],  crop[["Kc CD"]],
											 			 			 ifelse(t > crop[["t_d.1"]] & t < crop[["t_e.1"]],
											 			 			 			 crop[["Kc CD"]] + round(as.numeric(t - crop[["t_d.1"]], units = "days") *
											 			 			 			 													((crop[["Kc E"]] - crop[["Kc CD"]]) / as.numeric((crop[["t_e.1"]] - crop[["t_d.1"]]), units = "days"))),
											 			 			 			 ifelse(t == crop[["t_e.1"]], crop[["Kc E"]], 0))))))

	# Only execute next calc if dual crop parameters exist
	if (!is.na(crop[["t_a.2"]])) {
		fKc2 <- ifelse(t < crop[["t_a.2"]], 0,
									 ifelse(t >= crop[["t_a.2"]] & t <= crop[["t_b.2"]],  crop[["2nd Kc AB"]],
									 			 ifelse(t > crop[["t_b.2"]] & t < crop[["t_c.2"]],
									 			 			 crop[["2nd Kc AB"]] + round(as.numeric(t - crop[["t_b.2"]], units = "days") *
									 			 			 															((crop[["2nd Kc CD"]] - crop[["2nd Kc AB"]]) / as.numeric((crop[["t_c.2"]] - crop[["t_b.2"]]), units = "days"))),
									 			 			 ifelse(t >= crop[["t_c.2"]] & t <= crop[["t_d.2"]], crop[["2nd Kc CD"]],
									 			 			 			 ifelse(t > crop[["t_d.2"]] & t < crop[["t_e.2"]],
									 			 			 			 			 crop[["2nd Kc CD"]] + round(as.numeric(t - crop[["t_d.2"]], units = "days") *
									 			 			 			 			 															((crop[["2nd Kc E"]] - crop[["2nd Kc CD"]]) / as.numeric((crop[["t_e.2"]] - crop[["t_d.2"]]), units = "days"))),
									 			 			 			 			 ifelse(t == crop[["t_e.2"]], crop[["2nd Kc E"]], 0))))))

		# TEST: for overlap between crop 1 and crop 2
		# DRY: This test should occur when CDL.Kc.LUT is imported.
		# TODO: Move to unit tests
		if(any(fKc!=0 & fKc2!=0))
			warning(paste0("oops, you may have double crops with
                     overlapping growing periods for ", crop[["cdl_name"]]))
		# Merge dual-cropping scenarios
		fKc[fKc2!=0] = fKc2[fKc2!=0]
	}
	return(fKc)
}

# Step 1: Prepare lookup table

#' Prepare CDL Kc LUT
#'
#' Given inputs descibing the month and day of planting crops, builds a lookup table for those crops
#'
#' We multiply coefficients by 100 to get integer values between 0 and 100. This will allow us to reclassify the landcover rasters and retain the 8-bit grayscale color depth. If we were to crate rasters with decimal values, the values would be stored as 4-byte floating point and we'd end up with a huge 32-bit image that would be very large on disk, large in memory, and would process at a glacial pace. Of course, it means sacrificing a bit of precision (which we will quantify later).
#'
#' @param input_table: A CSV file with a specific structure - for now, see example in Box under VICE Lab/RESEARCH/PROJECTS/wfar/input/TABLES/cup+_kc_cdl.csv
#' @param start_year: The year that plantings occurred. Harvests could be in the next year - if the month of the harvest day is before the month of the
#'                    planting date, then it is assumed to be the next year. Right now, if you want the input_table to define the year, you'll need
#'                    to extract it and feed it in here. It'd likely be good to embed the years into the input spreadsheets (or having a dedicated processor for
#'                    handling year info from the spreadsheet?)
prepare_lookup_table_data <- function(input_table, start_year){
	CDL.Kc.LUT <- readr::read_csv(input_table)

	CDL.Kc.LUT[["start.date.1"]] <-
		as.Date(paste(CDL.Kc.LUT[["Planting Month"]], CDL.Kc.LUT[["Planting Day"]], start_year, sep = "-"),
						format = "%m-%d-%Y")
	CDL.Kc.LUT[["end.date.1"]] <-
		as.Date(paste(CDL.Kc.LUT[["Harvest Month"]], CDL.Kc.LUT[["Harvest Day"]],
									ifelse(CDL.Kc.LUT[["Harvest Month"]] < CDL.Kc.LUT[["Planting Month"]], start_year+1, start_year),
									sep = "-"),
						format = "%m-%d-%Y")
	CDL.Kc.LUT[["start.date.2"]] <-
		as.Date(paste(CDL.Kc.LUT[["2nd Planting Month"]], CDL.Kc.LUT[["2nd Planting Day"]], start_year, sep = "-"),
						format = "%m-%d-%Y")
	CDL.Kc.LUT[["end.date.2"]] <-
		as.Date(paste(CDL.Kc.LUT[["2nd Harvest Month"]], CDL.Kc.LUT[["2nd Harvest Day"]],
									ifelse(CDL.Kc.LUT[["2nd Harvest Month"]] < CDL.Kc.LUT[["2nd Planting Month"]], start_year+1, start_year),
									sep = "-"),
						format = "%m-%d-%Y")

	CDL.Kc.LUT[["is.doublecrop"]] <- !is.na(CDL.Kc.LUT[["2nd Planting Day"]])

	# TEST: Remove unrepresented crops and print
	# TODO: Move to unit tests
	warning("The following landcover classes do not have matching coefficients, and will be removed: ",
					paste(unique(unlist(CDL.Kc.LUT[!complete.cases(CDL.Kc.LUT[["Kc AB"]]),"cdl_name"])), " "))
	CDL.Kc.LUT <- CDL.Kc.LUT[complete.cases(CDL.Kc.LUT[["Kc AB"]]),]

	# Scale coeff by 100
	CDL.Kc.LUT[,c("Kc AB", "Kc CD", "Kc E", "2nd Kc AB", "2nd Kc CD", "2nd Kc E")] <-
		CDL.Kc.LUT[,c("Kc AB", "Kc CD", "Kc E", "2nd Kc AB", "2nd Kc CD", "2nd Kc E")] * 100

	# Calculate length of growing period
	CDL.Kc.LUT[["lgp.1"]] <- CDL.Kc.LUT$end.date.1 - CDL.Kc.LUT$start.date.1
	CDL.Kc.LUT[["lgp.2"]] <- CDL.Kc.LUT$end.date.2 - CDL.Kc.LUT$start.date.2

	# Calculate dates of Kc inflection points
	# TODO: Clean up with a function; DRY
	CDL.Kc.LUT <-
		cbind(CDL.Kc.LUT, t_a.1 = CDL.Kc.LUT[["start.date.1"]],
					t_b.1 = CDL.Kc.LUT[["start.date.1"]] + (CDL.Kc.LUT[["lgp.1"]] * CDL.Kc.LUT[["% season B"]] * 0.01),
					t_c.1 = CDL.Kc.LUT[["start.date.1"]] + (CDL.Kc.LUT[["lgp.1"]] * CDL.Kc.LUT[["% season C"]] * 0.01),
					t_d.1 = CDL.Kc.LUT[["start.date.1"]] + (CDL.Kc.LUT[["lgp.1"]] * CDL.Kc.LUT[["% season D"]] * 0.01),
					t_e.1 = CDL.Kc.LUT[["end.date.1"]],
					t_a.2 = CDL.Kc.LUT[["start.date.2"]],
					t_b.2 = CDL.Kc.LUT[["start.date.2"]] + (CDL.Kc.LUT[["lgp.2"]] * CDL.Kc.LUT[["% season B"]] * 0.01),
					t_c.2 = CDL.Kc.LUT[["start.date.2"]] + (CDL.Kc.LUT[["lgp.2"]] * CDL.Kc.LUT[["% season C"]] * 0.01),
					t_d.2 = CDL.Kc.LUT[["start.date.2"]] + (CDL.Kc.LUT[["lgp.2"]] * CDL.Kc.LUT[["% season D"]] * 0.01),
					t_e.2 = CDL.Kc.LUT[["end.date.2"]]
		)

	return(CDL.Kc.LUT)

}

#' Prepare CDL Kc Lookup Table
#'
#'
#'
#'
prepare_lookup_table <- function(input_table, start_year, output_folder = NA){
	CDL.Kc.LUT <- prepare_lookup_table_data(input_table, start_year)

	#Next we create the actual look-up table that contains daily Kc values, computed according to the heuristic from CUP+. We're using if-else tests for clarity, but the following could be vectorized for a marginal speedup.
	## TODO: Vectorize with logical array (http://kitchingroup.cheme.cmu.edu/blog/2013/02/23/Vectorized-piecewise-functions/)

	# Plot and inspect
	# plot(seq.Date(as.Date("2001-01-01"), by = "day", length.out = 730), calculateKcDaily(seq.Date(as.Date("2001-01-01"), by = "day", length.out = 730), as.data.frame(CDL.Kc.LUT)[4,]))

	# Create daily Kc values for 2 years.
	CDL.Kc.LUT.daily <- do.call("rbind",
	                            by(CDL.Kc.LUT, 1:nrow(CDL.Kc.LUT),
	                               function(row) rbind(calculateKcDaily(seq.Date(as.Date(paste(start_year,"-01-01", sep="")), by = "day", length.out = 730), row))))

	# TEST: See if we can wrap the coefficients of the second year into the beginning of the first without overlap
	# DRY: This test should occur when CDL.Kc.LUT is imported.
	# TODO: Move to unit tests
	if(any(c(by(CDL.Kc.LUT.daily, 1:nrow(CDL.Kc.LUT.daily), function (row) any(row[1:365]!=0 & row[366:730]!=0)))))
	  warning("oops, you STILL may have double crops with overlapping growing periods for: ",
	          paste(which(c(by(CDL.Kc.LUT.daily, 1:nrow(CDL.Kc.LUT.daily), function (row) any(row[1:365]!=0 & row[366:730]!=0)))), collapse=" "))

	# Wrap into single year and label
	CDL.Kc.LUT.daily <- pmax(CDL.Kc.LUT.daily[ , 1:365], CDL.Kc.LUT.daily[ , 366:730])
	CDL.Kc.LUT.daily <- CDL.Kc.LUT.daily[ , -(366:730)]
	colnames(CDL.Kc.LUT.daily) <- 1:365
	CDL.Kc.LUT.daily <- cbind(value = CDL.Kc.LUT[["VALUE"]], cdl_name = CDL.Kc.LUT[["cdl_name"]], as.data.frame(CDL.Kc.LUT.daily))

	# TODO: Evaluate if it's more readable to set NA earlier in logic
	CDL.Kc.LUT.daily[CDL.Kc.LUT.daily == 0] <- NA

	# Plot and inspect
	#par(mfrow = c(7, 7))
	#for (row in 1:nrow(CDL.Kc.LUT.daily)){
	#  plot(1:365,CDL.Kc.LUT.daily[row,3:367])
	#}

	if(!is.na(output_folder)){
		saveRDS(CDL.Kc.LUT.daily, paste(output_folder, "tables/CDL_Kc_LUT_daily.rds", sep=""))
		write.csv(CDL.Kc.LUT.daily, paste(output_folder, "tables/CDL_Kc_LUT_daily.csv", sep=""))
	}

	return(list("kc_lut" = CDL.Kc.LUT, "kc_lut_daily" = CDL.Kc.LUT.daily))
}


# This final structure is not "tidy", in the sense of variables forming columns and observations forming rows.
# It's a 3-dimensional lookup table project into two.

# Create special LUT for dual-cropped regions for later dis-entanglement


#' Prepare Lookup Table of Dual Crops
#'
#'
#'
prepare_dual_crop_lookup_table <- function(original_lookup_table, output_folder = NA){
	# Subset to only dual-cropped regions
	CDL.LUT.dual <- original_lookup_table[original_lookup_table[["is.doublecrop"]] == TRUE,]
	# Create daily Kc values for 2 years.
	CDL.LUT.dual <- do.call("rbind",
	                            by(CDL.LUT.dual, 1:nrow(CDL.LUT.dual),
	                               function(row) rbind(tagKcDaily(seq.Date(as.Date("2001-01-01"), by = "day", length.out = 730), row))))

	CDL.LUT.dual <- pmax(CDL.LUT.dual[ , 1:365], CDL.LUT.dual[ , 366:730])
	CDL.LUT.dual <- CDL.LUT.dual[ , -(366:730)]
	colnames(CDL.LUT.dual) <- 1:365
	CDL.LUT.dual <- cbind(value = original_lookup_table[original_lookup_table[["is.doublecrop"]] == TRUE,][["VALUE"]], cdl_name = original_lookup_table[original_lookup_table[["is.doublecrop"]] == TRUE,][["cdl_name"]], as.data.frame(CDL.LUT.dual))
	CDL.LUT.dual[CDL.LUT.dual == 0] <- NA

	if(!is.na(output_folder)){
		saveRDS(CDL.LUT.dual, paste(output_folder, "tables/CDL_LUT_dualtagged.rds", sep=""))
		write.csv(CDL.LUT.dual, paste(output_folder, "tables/CDL_LUT_dualtagged.csv", sep=""))
	}

	return(CDL.LUT.dual)
}


# TODO
#* Revise method of modeling dual-cropped regions
#* Refactor math using interpolation function

#* Move tests to unit tests
#* Move 'Plot and inspect' comments to sanity check sheets

#* Refactor to DRY


