#' Define Utah Geographic Areas for Health Data
#'
#' `geo_areas()` defines geographic areas for health data. Geographic data is
#' defined at County, Local Health District(LHD) and Small Area levels.
#'
#' @param df Input data.frame with county and zip code columns. Passing input
#' data.frame through `clean_counties` will ensure compatible formating.
#'
#' @returns Original data.frame with new geo area columns for County, LHD and
#' Small Area
#'
#' @export
geo_areas <- function(df){

  tmp <- df |>
    # mutate(County = as.numeric(County)) |>
    # set out-of-state and invalid zip codes to missing
    mutate(
      zipcode = case_when(
        zip_code < 84000 ~ NA,
        zip_code > 85000 ~ NA,
        .default = zip_code)
    ) |>
    # create assigned county variable
    # mutate(
    #   county = county_code
    # ) |>
    # create test small area variable
    # check missing missing zip codes for county designation
    mutate(
      small_area =
        case_when(
          is.na(zipcode) ~ county,
          .default = NULL)
    ) |>
    # Assign the small areas by county if no valid ZIP. This can only happen for
    # Counties that are small areas or are a subset of a small area
    # (Beaver, Carbon, Daggett, Duchesne, Emery, Garfield, Grand, Kane, Morgan,
    # Piute, Rich, San Juan, Uintah, Wasatch, Wayne)
    mutate(
      small_area = if_else(
        is.na(zipcode),
        replace_values(county,
                       from = by_county$county,
                       to = by_county$small_area_by_county
        ),
        small_area)
    ) |>
    # Assign lhd to those with only a valid county no valid zipcode
    mutate(
      lhd = county |>
        replace_values(
          from = by_county$county,
          to = by_county$district)
    ) |>
    # Assign missing zipcodes to 99999
    mutate(
      zipcode = case_when(
        is.na(zipcode) ~ 99999,
        .default = zipcode)
    ) |>
    # Assign small areas with missing zipcode and missing county to 98
    mutate(
      small_area = case_when(
        (zipcode == 99999 & is.na(county)) ~ 98,
        .default = small_area)
    ) |>
    # Assign districts with missing zipcode and missing county to 98
    mutate(
      lhd = case_when(
        (zipcode == 99999 & is.na(county)) ~ 98,
        .default = lhd)
    )|>
    # Assign counties with missing zipcode and missing county to 98
    mutate(
      county = case_when(
        (zipcode == 99999 & is.na(county)) ~ 98,
        .default = county)
    ) |>
    # Assign small area by county and zipcode
    left_join(by_small_area |>
                unnest(cols = zipcode) |>
                unnest(cols = county),
              join_by(county, zipcode),
              keep = FALSE) |>
    mutate(small_area = coalesce(small_area.x, small_area.y)) |>
    select(-small_area.x, -small_area.y) |>
    # Some zip codes cross county lines for these cases we need to assign small
    #   area based on both the zip code and county. However if the county is
    #   missing or inconsistent and the zipcode crosses county lines we will
    #   assign to the small area with the greatest HUD Res Rate.
    #
    # Join table of border crossing zipcodes and counties with HUD res rates.
    left_join(
      zip_county_cross,
      join_by(zipcode == zip_cross),
      keep = TRUE
    ) |>
    mutate(
      small_area = case_when(
        (zipcode == zip_cross & county == county_1) ~ sa_1,
        (zipcode == zip_cross & county == county_2) ~ sa_2,
        (zipcode == zip_cross & county %notin% c(county_1, county_2)) ~ biggest_sa,
        .default = small_area)
    ) |>
    mutate(
      county = case_when(
        (zipcode == zip_cross & county %notin% c(county_1, county_2)) ~ biggest_county,
        .default = county)
    ) |>
    # Remove extra columns from border crossing table
    select(-colnames(zip_county_cross)) |>
    # Assign district based on county when small area is assigned
    mutate(
      lhd = if_else(
        !is.na(small_area),
        replace_values(
          county,
          from = by_county$county,
          to = by_county$district
        ),
        lhd)
    ) |>
    # Assign small areas by zip if zip does not match county and they have not
    #   yet been assigned a small area.
    #
    # Set county to missing when small area is not assigned
    mutate(
      county = case_when(
        is.na(small_area) ~ NA,
        .default = county)
    ) |>
    mutate(small_area = if_else(
      is.na(small_area),
      recode_values(zipcode,
                    from = by_small_area$zipcode,
                    to = by_small_area$small_area
      ),
      small_area)
    ) |>
    # Re-assign county if zipcode recorded or small area has been assigned
    mutate(county = if_else(
      is.na(county),
      recode_values(small_area,
                    from = by_county$small_area,
                    to = by_county$county
      ),
      county)
    ) |>
    mutate(county = if_else(
      is.na(county),
      recode_values(zipcode,
                    from = by_county$zipcode,
                    to = by_county$county
      ),
      county)
    ) |>
    # Assign lhd where there is a valid county and no valid zipcode
    mutate(lhd = if_else(
      is.na(small_area),
      replace_values(lhd,
                     from = by_county$county,
                     to = by_county$district
      ),
      lhd)
    ) |>
    mutate(small_area = case_when(
      (is.na(small_area)) ~ 99,
      .default = small_area)
    ) |>
    mutate(county = case_when(
      small_area == 99 ~ 99,
      small_area == 97 ~ 97,
      .default = county)
    ) |>
    mutate(lhd = case_when(
      small_area == 99 ~ 99,
      small_area == 97 ~ 97,
      .default = lhd)
    ) |>
    mutate(lhd = if_else(
      small_area == 99,
      replace_values(lhd,
                     from = by_county$county,
                     to = by_county$district
      ),
      lhd)
    ) |>
    mutate(county_FIPS = case_when(
      county <= 29 ~ ((county*2)-1),
      .default = county))

}
