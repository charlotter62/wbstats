
#' @noRd
format_wb_tidy_names <- function(x, end_point) {

  global_patterns <- wb_api_name_patterns$global_patterns
  local_patterns <- wb_api_name_patterns[[end_point]]
  all_patterns <- c(global_patterns, local_patterns)


  # this can all be replaces by janitor::clean_names
  x_trim    <- stringr::str_trim(x)
  x_lower   <- stringr::str_to_lower(x_trim)
  x_replace <- stringr::str_replace_all(x_lower, all_patterns)
  x_tidy    <- tibble::tidy_names(x_replace, quiet = TRUE)

  x_tidy
}



#' @noRd
   format_wb_data <- function(x, end_point) {

  x_field_types <- format_wb_get_col_type(x)
  col_index <- which(x_field_types %in% "character")

  # skip date column b/c it has its own parsing function
  col_index <- setdiff(col_index, which(names(x) == "date"))

  x <- format_wb_func(x, readr::parse_guess,
                      col_index = col_index)

  # still need to make sure that blanks are turned to NAs
  if (end_point == "data") {

    x_names <- format_wb_tidy_names(names(x), end_point = end_point)
    names(x) <- x_names

    if("value" %in% x_names) x$value <- as.numeric(x$value)
    if("unit" %in% x_names) x$unit <- as.character(x$unit)
    if("obs_status" %in% x_names) x$obs_status <- as.character(x$obs_status)
    if("footnote" %in% x_names) x$footnote <- as.character(x$footnote)
  }

  if (end_point == "country") x[x$iso3c == "NAM", "iso2c"] <- "NA"

  if (end_point == "source") {

    log_fields <- c("data_available", "metadata_available")
    col_index <- which(names(x) %in% log_fields)

    x <- format_wb_func(x, format_wb_func_as_logical,
                        true_pattern  = "Yes|yes|Y|y",
                        false_pattern = "No|no|N|n",
                        col_index = col_index)
  }

  tibble::as_tibble(x)
}


#' @noRd
format_wb_get_col_type <- function(x, ...) {
  x_type <- sapply(seq(ncol(x)), FUN = function(i) typeof(x[ ,i]))
  names(x_type) <- names(x)
  x_type
}

#' @noRd
format_wb_func_replace_value <- function(x, current_value, replacement, ...) {
  x[x == current_value] <- replacement
  x
}


#' @noRd
format_wb_func_as_logical <- function(x, true_pattern, false_pattern, ...) {
  true_index <- grep(true_pattern, x = x, ...)
  false_index <- grep(false_pattern, x = x, ...)

  index_in_both <- base::intersect(true_index, false_index)

  if(length(index_in_both) != 0)
    warning("Patterns provided match both `TRUE` and `FALSE`.")

  x[true_index]  <- TRUE
  x[false_index] <- FALSE

  as.logical(x, ...)
}



#' @noRd
format_wb_func <- function(df, func, col_index,  ...) {

  if(missing(col_index)) col_index <- seq_len(ncol(df))

  df[, col_index] <- lapply(col_index, FUN = function(i) {
    x<- df[, i]
    func(x, ...)
  })

  df
}


#' @noRd
format_wb_country <- function(x, cache) {

  x_lower <- tolower(x)

  if (missing(cache)) cache <- wbstats::wb_cachelist
  cache_cn <- cache$countries


  if (any(x_lower %in% c("countries", "countries_only", "countries only")))
    # it is actually faster and more reliable to request 'all' and then filter afterwards
    cn_params <- "all"

  else if (any(x_lower %in% c("regions", "regions_only", "regions only")))
    cn_params <- unique_na(cache_cn$region_iso3c)

  else if (any(x_lower %in% c("admin_regions", "admin_regions_only", "admin regions only")))
    cn_params <- unique_na(cache_cn$admin_region_iso3c)

  else if (any(x_lower %in% c("income_levels", "income_levels_only", "income levels only")))
    cn_params <- unique_na(cache_cn$income_level_iso3c)

  else if (any(x_lower %in% c("lending_types", "lending_types_only", "lending types only")))
    cn_params <- unique_na(cache_cn$lending_type_iso3c)

  else if (any(x_lower %in% c("aggregates", "aggregates_only", "aggregates only")))
    cn_params <- unique_na(cache_cn$iso3c[cache_cn$region == "Aggregates"])

  else if (any(x_lower %in% "all"))
    cn_params <- "all"

  else if (length(x_lower) == 1 && x_lower == "the motherland") {
    message("Good choice comrade...")
    cn_params <- "rus"
  }

  else { # any non-special values

    # all of the region, lending, and income names are also listed in these 3
    # columns so a check here checks for all of them
    cn_check <- as.matrix(cache_cn[ , c("iso3c", "iso2c", "country")])

    # don't forget everything is lowercase now
    cn_check <- tolower(cn_check)

    good_cn_index <- x_lower %in% cn_check
    good_cn <- x_lower[good_cn_index]

    if (length(good_cn) == 0)
      stop("No valid values for the country parameter were found.
           Please check the country argument description in wbstats::wb_data() for valid input values.")

    # use x instead of x_lower to KeEp UsEr DeFiNeD cAsInG
    bad_cn <- x[!good_cn_index]

    if (length(bad_cn) > 0)
      warning(paste0("The following country values are not valid and are being excluded from the request: ",
                                           paste(bad_cn, collapse = ",")))

    # the API only accepts IDs and not names, so if a country is listed in x
    # find its iso3c code. This lets the user pass values like "World" or "High Income"
    good_cn_iso3c_index <- lapply(1:ncol(cn_check), function(i) {
      which(cn_check[1:nrow(cn_check), i] %in% x_lower)
      })

    good_cn_iso3c_index <- unique(unlist(good_cn_iso3c_index))
    cn_params <- cn_check[good_cn_iso3c_index, 1]
  }

  paste0(cn_params, collapse = ";")
}

#' @noRd
format_wb_counterpart <- function(x, cache) {

  x_lower <- tolower(x)
  cpt_params <- c()

  if (missing(cache)) cache <- wbstats::wb_cachelist
  cache_cpt <- cache$counterparts

  if(any(x_lower %in% c("mdbs"))){
    cpt_params <- c(cpt_params, unique_na(cache_cpt$id[cache_cpt$mdb=="Y"])) #overwrites blank to mdbs list
  }
  if(any(x_lower %in% c("bilateral"))){
    cpt_params <- c(cpt_params, unique_na(cache_cpt$id[cache_cpt$bilat=="Y"])) #overwrites list to bilateral list
  }

  cpt_check <- as.matrix(cache_cpt[ , c("id", "value")])
  cpt_check <- rbind(cpt_check ,c("MDB", "mdbs"), c("BIL","bilateral")) #include the special cases so they are not flagged as error
  # don't forget everything is lowercase now
  cpt_check <- tolower(cpt_check)

  good_cpt_index <- x_lower %in% cpt_check
  good_cpt <- x_lower[good_cpt_index]

  if (length(good_cpt) == 0)
    stop("No valid values for the counterpart-area parameter were found.
           Please check the counterpart-area argument description in wbstats::wb_data() for valid input values.")

  # use x instead of x_lower to KeEp UsEr DeFiNeD cAsInG
  bad_cpt <- x[!good_cpt_index]
  #bad_cpt <- bad_cpt[!any(x_lower %in% c("mdbs", "bilateral"))] #should not throw error for special cases

  if (length(bad_cpt) > 0)
    warning(paste0("The following counterpart-area values are not valid and are being excluded from the request: ",
                   paste(bad_cpt, collapse = ",")))

  # the API only accepts IDs and not names, so if a counterpart-area is listed in x
  # find its id code. This lets the user pass values like "World Bank-IBRD" or "Austria"
  good_cpt_id_index <- lapply(1:ncol(cpt_check), function(i) {
    which(cpt_check[1:nrow(cpt_check), i] %in% x_lower)
  })

  good_cpt_id_index <- unique(unlist(good_cpt_id_index))
  cpt_params <- c(cpt_params, unique_na(cpt_check[good_cpt_id_index, 1]))

  paste0(cpt_params, collapse = ";")

}

#' @noRd
guess_wb_source <- function(ind, cache){
  if (missing(cache)) cache <- wbstats::wb_cachelist
  source <- sapply(ind, FUN=function(x){
    src <- unique(cache$series$source_id[cache$series$id==x])[1] #pick the first source
    src_name <- cache$sources$source[cache$sources$source_id==src] #get source names
    print(paste0("Source: ", src_name, " used by default for indicator: ", x))
    return(src)
  })
  return(source)
}

#' @noRd
format_wb_source <- function(src, ind, cache){

  if (missing(cache)) cache <- wbstats::wb_cachelist

  #if sources not named by indicator, add names
  if(is.null(names(src))){
    if(length(src)==length(ind)){
      names(src) <- ind
    }
  }

  sapply(ind, FUN=function(i){
    s <- src[i]

    #If an indicator is missing a source
    if(!(i %in% names(src))){
      return(guess_wb_source(i, cache))
    }else{ #The indicator has a source input
      #If the source for this indicator is not in the sources list
      if(!(s %in% cache$series$source_id)){
        writeLines(sprintf("Invalid source provided: %s. See wb_cachelist$sources for valid source ids.\n", s))
        return(guess_wb_source(i, cache))
      }else{
        #If the source exists but does not have that indicator
        inds_by_src <- cache$series$id[cache$series$source_id==s]
        if(!(i %in% inds_by_src)){
          writeLines(sprintf("Source: %s does not have series %s. See wb_cachelist$series for valid source ids.", s, i))
          return(guess_wb_source(i, cache))
        }else{
          #If the source exists and has the indicator (input is correct!)
          return(s)
        }
      }
    }

  })

}
