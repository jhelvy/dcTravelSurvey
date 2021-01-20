source('setup.R')
source(here::here('doe', '1defineTrips.R'))

# # Compute and save the scores from different combinations of trips
# # This is an exhaustive search up through 2 of each row in the tripSet
# # Don't want to go beyond 2 of any one trip, otherwise we'll
# # have less information about specific trips
# diffs_yes <- getAllDiffs(trips_yes_car_set)
# diffs_no <- getAllDiffs(trips_no_car_set)
# diffs <- list(yes = diffs_yes, no = diffs_no)
# saveRDS(diffs, here::here('doe', 'data', 'diffs.Rds'))

# Get a balanced set of trips based on mode and numLegs
diffs <- readRDS(here::here('doe', 'data', 'diffs.Rds'))

# With Cars
tripResults <- getTripResults(diffs$yes)
min(tripResults$mode)
min(tripResults$numLegs)
tripResults %>% arrange(score, mean) %>% head()
tripResults %>% arrange(mean, score) %>% head()
tripResults %>% arrange(mode, numLegs) %>% head()
tripResults %>% arrange(numLegs, mode) %>% head()
trips_yes_car <- trips_yes_car_set[diffs$yes[[11]]$rows,]
getDiffs(trips_yes_car)
getModeCount(trips_yes_car)
getLegsCount(trips_yes_car)
getTripCount(trips_yes_car)

# Without Cars
tripResults <- getTripResults(diffs$no)
min(tripResults$mode)
min(tripResults$numLegs)
tripResults %>% arrange(score, mean) %>% head()
tripResults %>% arrange(mean, score) %>% head()
tripResults %>% arrange(mode, numLegs) %>% head()
tripResults %>% arrange(numLegs, mode) %>% head()
trips_no_car <- trips_no_car_set[diffs$no[[48]]$rows,]
getDiffs(trips_no_car)
getModeCount(trips_no_car)
getLegsCount(trips_no_car)
getTripCount(trips_no_car)

# Save balanced trips
trips_yes_car <- trips_yes_car %>% mutate(tripId = row_number())
trips_no_car <- trips_no_car %>% mutate(tripId = row_number())
trips <- list(yes = trips_yes_car, no = trips_no_car)
saveRDS(trips, here::here('doe', 'data', 'trips.Rds'))


# # Compare different solutions:
# t1 <- readRDS(here::here('doe', 'data', 'trips.Rds'))
# t2 <- readRDS(here::here('doe', 'data', 'trips0.Rds'))
# 
# rbind(
#     unlist(getDiffs(t1$yes)),
#     unlist(getDiffs(t2$yes)))
# 
# rbind(
#     unlist(getModeCount(t1$yes)),
#     unlist(getModeCount(t2$yes)))
# 
# cbind(
#     getTripCount(t1$yes),
#     getTripCount(t2$yes))
