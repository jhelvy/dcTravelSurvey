library(tidyverse)
library(zipcodeR)
options(dplyr.width = Inf)

# Get all zip codes in the Washington metropolitan area, as defined here:
# https://en.wikipedia.org/wiki/Washington_metropolitan_area

md <- c(
    "Calvert County",
    "Charles County",
    "Frederick County",
    "Montgomery County",
    "Prince George's County")
va <- c(
    "Alexandria City",
    "Arlington County",
    "Clarke County",
    "Culpeper County",
    "Fairfax County",
    "Fairfax City",
    "Falls Church City",
    "Fauquier County",
    "Fredericksburg City",
    "Loudoun County",
    "Manassas City",
    "Prince William County",
    "Rappahannock County",
    "Spotsylvania County",
    "Stafford County",
    "Warren County"
)
counties <- c("District of Columbia", md, va, "Jefferson County")
all <- search_state(c("DC", "MD", "VA", "WV"))
keep <- list()
index <- 1
for (i in 1:length(counties)) {
    target <- counties[i]
    matched <- all %>% 
        filter(str_to_lower(county) == str_to_lower(target))
    if (nrow(matched) > 0) {
        keep[[index]] <- matched
        index <- index + 1
    } else {
        print(target)
    }
}
    
zipcodesDf <- do.call(bind_rows, keep) %>% 
    select(
        zipcode, major_city, county, state, population, population_density,
        land_area_in_sqmi, housing_units, occupied_housing_units, 
        median_home_value, median_household_income)
zipcodes <- zipcodesDf %>% 
    select(zipcode)

write_csv(zipcodes, here::here("zipcodes", "zipcodes.csv"))
write_csv(zipcodesDf, here::here("zipcodes", "zipcodesDf.csv"))
