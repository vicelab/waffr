#' WAFFR Core Class
#'
#' Coordinates the WAFFR code as a single R6 object so that all data can be managed under
#' the object instead of the caller having to keep track of multiple returns and return
#' types, etc.
#'
#'
WAFFR <- R6::R6Class("WAFFR", list(
	intermediate_output_folder = NA,
	kc_input_table = NULL,
	start_year = 2001,

	kc_lookup_table = NULL,
	kc_lookup_table_daily = NULL,
	kc_dual_lookup_table = NULL, # for dual cropping

	get_kc_lookups = function(){
		lookups = prepare_lookup_table(input_table = self$kc_input_table, start_year = self$start_year, output_folder = self$intermediate_output_folder)
		self$kc_lookup_table <- lookups$kc_lut
		self$kc_lookup_table_daily <- lookups$kc_lut_daily
		self$kc_dual_lookup_table <- prepare_dual_crop_lookup_table(self$kc_lookup_table, output_folder = self$intermediate_output_folder)
	}


))
