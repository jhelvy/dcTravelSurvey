# Main full factorial DOE construction
# randomized, balanced by number of trips and modes

source('setup.R')
trips <- readRDS(here::here('doe', 'data', 'trips.Rds'))

# Generate full factorial for all attributes except for trips
ff <- as_tibble(expand.grid(
    price        = c(2, 5, 10, 15),       # Full trip, USD $
    carExpress   = c(0, 1),               # Dummy for car express lane
    expressFee   = c(5, 10),              # USD $
    startTime    = c(0, 5, 10),           # Minutes waiting at start
    leg1Time     = c(10, 20, 30, 45),     # Minutes
    transferTime = c(0, 5, 10),           # Minutes
    leg2Time     = c(10, 15, 20),         # Minutes
    tripTimeUnc  = c(0.05, 0.10, 0.20)))  # Plus/minus % of total trip time

# With cars -------------------------------------------------------------------

# Preview balanced trips
getDiffs(trips$yes)
getModeCount(trips$yes)
getTripCount(trips$yes)
count(trips$yes, numLegs)

doe_bal_yes <- ff %>%
    mutate(ffId = row_number()) %>%
    makeBalancedFF(trips$yes)

# Without cars ----------------------------------------------------------------

# Preview balanced trips
getDiffs(trips$no)
getModeCount(trips$no)
getTripCount(trips$no)
count(trips$no, numLegs)

doe_bal_no <- ff %>%
    mutate(expressFee = 0) %>%
    distinct() %>%
    mutate(ffId = row_number()) %>%
    makeBalancedFF(trips$no)

# Save result ----------------------------------------------------------------

saveRDS(list(yes = doe_bal_yes, no = doe_bal_no),
        here::here('doe', 'data', 'doe.Rds'))
