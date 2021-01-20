source('setup.R')

# Define the full set of trips for cars and no cars
trips_yes_car_set <- tribble(
    ~leg1Mode, ~leg2Mode,  ~include, ~note,
    # Only including and taxi as one-leg trips
    car,   none,   1,        "",
    taxi,  none,   1,        "",
    # Only including bus, metro, and walking for two-leg trips
    bus,   none,   1,        "",
    bus,   bus,    1,        "",
    bus,   metro,  1,        "",
    bus,   walk,   0,        "Same as walk-bus",
    metro, none,   1,        "",
    metro, bus,    1,        "",
    metro, metro,  1,        "",
    metro, walk,   0,        "Same as walk-metro",
    walk,  none,   0,        "Not considering walk only",
    walk,  bus,    1,        "",
    walk,  metro,  1,        "",
    walk,  walk,   0,        "Same as 1 leg trip"
) %>%
    filter(include == 1) %>%
    mutate(trip = paste(leg1Mode, leg2Mode, sep='|')) %>% 
    select(-include, -note) %>% 
    mutate(
        numLegs  = ifelse(leg2Mode == none, 1, 2),
        lastLegMode = ifelse(
            numLegs == 1, as.character(leg1Mode), as.character(leg2Mode)),
        carInTrip   = ifelse(str_detect(trip, car), 1, 0),
        taxiInTrip  = ifelse(str_detect(trip, taxi), 1, 0),
        busInTrip   = ifelse(str_detect(trip, bus), 1, 0),
        metroInTrip = ifelse(str_detect(trip, metro), 1, 0),
        walkInTrip  = ifelse(str_detect(trip, walk), 1, 0),
        busOnlyTrip = case_when(
            (leg1Mode == bus) & (leg2Mode == none) ~ 1,
            (leg1Mode == bus) & (leg2Mode == bus) ~ 1,
            (leg1Mode == bus) & (leg2Mode == walk) ~ 1,
            (leg1Mode == walk) & (leg2Mode == bus) ~ 1,
            TRUE ~ 0),
        metroOnlyTrip    = case_when(
            (leg1Mode == metro) & (leg2Mode == none) ~ 1,
            (leg1Mode == metro) & (leg2Mode == metro) ~ 1,
            (leg1Mode == metro) & (leg2Mode == walk) ~ 1,
            (leg1Mode == walk) & (leg2Mode == metro) ~ 1,
            TRUE ~ 0)
        )
trips_no_car_set <- trips_yes_car_set %>%
    filter(carInTrip == FALSE)
