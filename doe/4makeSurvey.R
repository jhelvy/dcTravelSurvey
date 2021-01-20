source('setup.R')

# Read in balanced full factorial
doe <- readRDS(here::here('doe', 'data', 'doe.Rds'))
doe_yes <- doe$yes
doe_no <- doe$no

# Set number of respondents and respondent question
nResp     <- 3000
nAltsPerQ <- 3 # Number of alternatives per question
nQPerResp <- 8 # Number of questions per respondent

# Make the doe
survey_yes <- makeSurvey(doe_yes, nResp, nAltsPerQ, nQPerResp)
survey_no  <- makeSurvey(doe_no, nResp, nAltsPerQ, nQPerResp)

# Save doe
saveRDS(survey_yes, here::here('doe', 'data', 'survey_yes.Rds'))
saveRDS(survey_no, here::here('doe', 'data', 'survey_no.Rds'))

# Make small survey for survey to match with trip images
survey_yes_metadata <- survey_yes %>%
    select(respID, qID, altID, uniqueTripID)
survey_no_metadata <- survey_no %>%
    select(respID, qID, altID, uniqueTripID)
write_csv(survey_yes_metadata, here::here('doe', 'data', 'survey_yes_metadata.csv'))
write_csv(survey_no_metadata, here::here('doe', 'data', 'survey_no_metadata.csv'))
