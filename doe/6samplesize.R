source('setup.R')

# Load survey design
survey_no <- readRDS(here::here('doe', 'data', 'survey_no.Rds'))
survey_yes <- readRDS(here::here('doe', 'data', 'survey_yes.Rds'))
survey <- bind_rows(survey_no, survey_yes) %>%
    dummyCode(vars = c(
        'trip', 'numLegs', 'leg1Mode', 'leg2Mode',
        'tripTimeUnc')) %>%
    clean_names('lower_camel') %>% 
    rename(
        obsID = obsId, 
        tripID = tripId, 
        respID = respId, 
        qID = qId, 
        altID = altId)

# Define model parameters ----------------------------------------------------

# Define models
pars_basic <- c(
    "price",
    "carExpress",
    "expressFee",
    "startTime",
    "leg1Time",
    "transferTime",
    "leg2Time",
    "tripTimeUnc0_1",
    "tripTimeUnc0_2")

pars_mode <- c(
    pars_basic,
    # Leg 1 dummy is leg1ModeWalk
    "leg1ModeCar",
    "leg1ModeUberTaxi",
    "leg1ModeBus",
    "leg1ModeMetro",
    # Leg 2 dummy is leg2ModeNone
    "leg2ModeBus",
    "leg2ModeMetro")

pars_trip <- c(
    pars_basic,
    # Trip dummy is tripCarNone
    "tripUberTaxiNone",
    "tripMetroNone",
    "tripBusNone",
    "tripBusBus",
    "tripBusMetro",
    "tripMetroMetro",
    "tripMetroBus",
    "tripWalkBus",
    "tripWalkMetro")

# Run MNL models --------------------------------------------------------------

basic <- sampleSizer(
    survey = survey,
    parNames = pars_basic)

mode <- sampleSizer(
    survey = survey,
    parNames = pars_mode)

trip <- sampleSizer(
    survey = survey,
    parNames = pars_trip)

mode_int <- sampleSizer(
    survey = survey,
    parNames = pars_mode,
    interactions = TRUE)

# Save results
saveRDS(basic, here::here('doe', 'samplesize', 'basic.Rds'))
saveRDS(mode, here::here('doe', 'samplesize', 'mode.Rds'))
saveRDS(trip, here::here('doe', 'samplesize', 'trip.Rds'))

# Make plots -----------------------------------------------------------------

basic <- readRDS(here::here('doe', 'samplesize', 'basic.Rds'))
mode <- readRDS(here::here('doe', 'samplesize', 'mode.Rds'))
trip <- readRDS(here::here('doe', 'samplesize', 'trip.Rds'))

# View results
df <- trip
summary(df)
ggplot(getErrorTable(df)) + 
    geom_point(aes(x = sampleSize, y = se))

