clean_counties <- function(df, county_col, zip_col, county_type){

  # create temporary new county column to transform
  df[["tmp_county"]] <- df[[county_col]]
  # create new zip code column
  df[["zip_code"]] <- df[[zip_col]]

  switch(county_type,
         "name" = {
           tmp <- df |>
             mutate(county_name = stringr::str_to_upper(tmp_county)) |>
             left_join(by_county |>
                         select(county_name, county) |>
                         mutate(county_name = str_to_upper(county_name)),
                       by = "county_name") |>
             mutate(county_name = str_to_title(county_name)) |>
             dplyr::select(-tmp_county)
         },
         "seq" = {
           tmp <- df |>
             mutate(county = as.numeric(tmp_county)) |>
             left_join(by_county |>
                         select(county_name, county),
                       by = "county") |>
             dplyr::select(-tmp_county)
         },
         "fips" = {
           tmp <- df |>
             mutate(county = (as.numeric(tmp_county)+1)/2) |>
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
    filter(is.na(county_code))

  print(unique(county_QA$county_name))
  tmp
}
