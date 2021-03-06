## code to prepare `Discrete_Occupations_Sections` dataset
cat('Sourcing Discrete_Occupations_Sections.R', sep = '\n')

library(dplyr)
library(tidyr)
library(readr)
library(usethis)
library(RCurl)

# load data
cat('    reading in biochemical section data', sep = '\n')
con <- url("ftp://ftp.dfo-mpo.gc.ca/AZMP_Maritimes/azmpdata/data/biochemical/ChlNut_MAR_AZMP.RData")
load(con)
close(con)

# load physical data
cat('    reading in physical section data', sep = '\n')
url_name <- "ftp://ftp.dfo-mpo.gc.ca/AZMP_Maritimes/azmpdata/data/physical/sections/"
result <- getURL(url_name,
                 verbose=TRUE,ftp.use.epsv=TRUE, dirlistonly = TRUE)

filenames <- unlist(strsplit(result, "\r\n"))


# create dataframe list
d <- list()
for(i in 1:length(filenames)){
  con <- url(paste0(url_name, filenames[[i]]))
  d[[i]] <- read.csv(con)
}
posections <- do.call('rbind', d)

# rename variables

posections <- posections %>%
  dplyr::rename(depth = pressure)


# target variables to include
target_var <- c("Chlorophyll_A" = "chlorophyll",
                "Nitrate" = "nitrate",
                "Phosphate" = "phosphate",
                "Silicate" = "silicate")

# print order
# section
print_order_section <- c("CSL" = 1,
                         "LL" = 2,
                         "HL" = 3,
                         "BBL" = 4)
# section
print_order_station <- c("CSL1" = 1, "CSL2" = 2, "CSL3" = 3, "CSL4" = 4, "CSL5" = 5, "CSL6" = 6,
                         "LL1" = 1, "LL2" = 2, "LL3" = 3, "LL4" = 4, "LL5" = 5, "LL6" = 6, "LL7" = 7, "LL8" = 8, "LL9" = 9,
                         "HL1" = 1, "HL2" = 2, "HL3" = 3, "HL4" = 4, "HL5" = 5, "HL6" = 6, "HL7" = 7,
                         "BBL1" = 1, "BBL2" = 2, "BBL3" = 3, "BBL4" = 4, "BBL5" = 5, "BBL6" = 6, "BBL7" = 7)
# season
print_order_season <- c("Spring" = 1,
                        "Fall" = 2)

# reformat data
Discrete_Occupations_Sections <- dplyr::left_join(df_data_averaged_l %>%
                                                    dplyr::select(sample_id, parameter_name, data_value) %>%
                                                    dplyr::rename(variable=parameter_name, value=data_value),
                                                  df_sample_filtered %>%
                                                    dplyr::select(sample_id, event_id, year, month, day,
                                                                  time, latitude, longitude, start_depth, standard_depth, station,
                                                                  transect, season) %>%
                                                    dplyr::rename(depth=start_depth, section=transect),
                                                  by=c("sample_id")) %>%
  dplyr::mutate(order_section = unname(print_order_section[section])) %>%
  dplyr::mutate(order_station = unname(print_order_station[station])) %>%
  dplyr::mutate(order_season = unname(print_order_season[season])) %>%
  dplyr::filter(variable %in% names(target_var)) %>%
  dplyr::mutate(variable = unname(target_var[variable])) %>%
  dplyr::group_by(sample_id, variable) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  tidyr::spread(variable, value) %>%
  dplyr::group_by(event_id) %>%
  dplyr::arrange(depth, .by_group=T) %>%
  dplyr::arrange(order_section, year, order_season, order_station) %>%
  dplyr::ungroup(.) %>%
  dplyr::select(section, station, latitude, longitude, year, month, day, event_id,
                sample_id, depth, standard_depth, unname(target_var))


# join physical data
Discrete_Occupations_Sections <- Discrete_Occupations_Sections %>%
  dplyr::bind_rows(., posections)

# save as dataframe not tibble
Discrete_Occupations_Sections <- as.data.frame(Discrete_Occupations_Sections)

# save data to csv
readr::write_csv(Discrete_Occupations_Sections, "inst/extdata/csv/Discrete_Occupations_Sections.csv")

# save data to rda
usethis::use_data(Discrete_Occupations_Sections, overwrite = TRUE)
