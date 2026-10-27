#' Define Utah counties
#'
#' `clean_counties()` defines standard naming and coding columns for counties.
#' Renames column names and transforms data type to be compatible with
#' `function()` for defining zip codes, small areas, local health districts and
#' counties.
#'
#' @param df Input data.frame or tibble with with zip code and county data.
#' @param county_col name of county column in `df`.
#' @param zip_col name of zip code column in `df`.
#' @param county_type Type of county data in `df`.
#'  * '"name"': `df` contains names of counties.
#'  * '"seq"': `df` contains sequential county codes (`1:29`).
#'  * '"FIPS"' `df` contains FIPS county codes (odd numbers `1:55`)
#'
#' @returns Original data.frame or tibble with new `zip_code`, `county_name`,
#' `county_code`, and `county_code_FIPS` columns
#'
#'
#' @export

clean_counties <- function(df, county_col, zip_col, county_type){

  # create temporary new county column to transform
  df[["tmp_county"]] <- df[[county_col]]
  # create new zip code column
  df[["zip_code"]] <- df[[zip_col]]

  switch(county_type,
         "name" = {
           tmp <- df |>
             mutate(county_name = stringr::str_to_upper(tmp_county)) |>
             mutate(zip_code = as.numeric(zip_code)) |>
             left_join(by_county |>
                         select(county_name, county) |>
                         mutate(county_name = stringr::str_to_upper(county_name)),
                       by = "county_name") |>
             mutate(county_name = stringr::str_to_title(county_name)) |>
             dplyr::select(-tmp_county)
         },
         "seq" = {
           tmp <- df |>
             mutate(county = as.numeric(tmp_county)) |>
             mutate(zip_code = as.numeric(zip_code)) |>
             left_join(by_county |>
                         select(county_name, county),
                       by = "county") |>
             dplyr::select(-tmp_county)
         },
         "fips" = {
           tmp <- df |>
             mutate(county = (as.numeric(tmp_county)+1)/2) |>
             mutate(zip_code = as.numeric(zip_code)) |>
             left_join(by_county |>
                         select(county_name, county),
                       by = "county") |>
             dplyr::select(-tmp_county)
         }
  )



  county_QA <- tmp |>
    filter_out(zip_code >= 85000 |
                 zip_code < 84000
    ) |>
    filter(is.na(county))

  print(unique(county_QA$county))
  tmp
}
