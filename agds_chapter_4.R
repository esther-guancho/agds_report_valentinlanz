library(dplyr)
library(lubridate)
library(tidyr)
library(readr)
library(stringr)
library(purrr)


half_hourly_fluxes <- readr::read_csv("./data/FLX_CH-Lae_FLUXNET2015_FULLSET_HH_2004-2006.csv")
half_hourly_fluxes



#select the relevant data
half_hourly_fluxes <- select(
  half_hourly_fluxes,
  starts_with("TIMESTAMP"),
  ends_with("_F"),
  GPP_NT_VUT_REF,
  NEE_VUT_REF_QC,
  starts_with("SWC_F_MDS_"),
  -contains("JSB"),
  NIGHT
)


#convert data to date-time objects
dates <- ymd_hm(half_hourly_fluxes$TIMESTAMP_START)
dates[1]


half_hourly_fluxes <- mutate(
  half_hourly_fluxes,
  TIMESTAMP_START = ymd_hm(TIMESTAMP_START)
)

#mutating both our timestamp variables
half_hourly_fluxes <- half_hourly_fluxes |> 
  mutate(across(starts_with("TIMESTAMP_"), ymd_hm))



#----Exercises----

starwars <- dplyr::starwars
from_tatooine <- dplyr::filter(starwars, homeworld == "Tatooine", species == "Droid")
from_tatooine

?dplyr::arrange
?dplyr::select
?dplyr::pull
?dplyr::slice
?dplyr::desc
?dplyr::arrange

#exercise 1: star wars

#pale characters from Ryloth or Naboo
pale_ryloth_naboo <- dplyr::filter(starwars, skin_color == "pale", (homeworld == "Ryloth")|(homeworld == "Naboo"))
pale_ryloth_naboo

#Who is the oldest among the tallest thirty characters?
chars_by_height <- dplyr::arrange(starwars, desc(height))
tallest_thirty <- chars_by_height[1:30,]
tallest_thirty

tallest_thirty_by_age <- dplyr::arrange(tallest_thirty, birth_year)
oldest <- tallest_thirty_by_age[1,]

#What is the name of the smallest character and their starship in “Return of the Jedi”
dplyr::unnest(starwars, films)



#exercise 2: aggregating




# Aggregating the fluxes into daily fluxes
daily_fluxes <- half_hourly_fluxes |> 
  mutate(date = as_date(TIMESTAMP_START)) |>   # converts time object to a date object
  group_by(date) |> 
  summarise(GPP_NT_VUT_REF = mean(GPP_NT_VUT_REF, na.rm = TRUE),
            n_datapoints = n(), # counts the number of observations per day
            n_measured = sum(NEE_VUT_REF_QC == 0), # counts the number of actually measured data (excluding gap-filled and poor quality data)
            SW_IN_F = mean(SW_IN_F, na.rm = TRUE),  # we will use this later
            .groups = 'drop' # to un-group the resulting data frame
  ) |> 
  mutate(f_measured = n_measured / n_datapoints) # calculate the fraction of measured values over total observations
write_csv(daily_fluxes, file = "C:/Users/valen/Documents/agds_report_valentinlanz/daily_fluxes.csv")

                   