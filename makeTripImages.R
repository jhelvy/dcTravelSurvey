source('setup.R')

# Read in DOEs
survey_no <- readRDS(here::here('doe', 'data', 'survey_no.Rds')) %>%
    mutate(hasCar = 'no')
survey_yes <- readRDS(here::here('doe', 'data', 'survey_yes.Rds')) %>%
    mutate(hasCar = 'yes')

# Create unique sets of trips
tripsYes <- distinct(survey_yes, uniqueTripID, .keep_all = TRUE)
tripsNo <- distinct(survey_no, uniqueTripID, .keep_all = TRUE)

# Create save folders
saveRoot <- here::here("images")

i <- 1
lang <- 'en'

# Create all trip images
s <- data.frame(w = 2.5, h = 5.5, d = 100) # image save setting
for (lang in c('en', 'sp')) {
    saveRootLang <- paste(saveRoot, lang, sep = '/')
    saveRootYes <- paste(saveRootLang, 'imagesCarYes', sep = '/')
    saveRootNo <- paste(saveRootLang, 'imagesCarNo', sep = '/')
    if (!dir.exists(saveRootLang)) { dir.create(saveRootLang) }
    if (!dir.exists(saveRootYes)) { dir.create(saveRootYes) }
    if (!dir.exists(saveRootNo)) { dir.create(saveRootNo) }
    for (i in seq(nrow(tripsYes))) {
    # for (i in seq(10)) {
        savePath <- paste0(saveRootYes, '/', i, '.png')
        tripDf <- getTripDf(filter(tripsYes, uniqueTripID == i))
        trip <- makePlot(tripDf, lang = lang)
        ggsave(savePath, trip, width = s$w, height = s$h, dpi = s$d)
    }
    for (i in seq(nrow(tripsNo))) {
    # for (i in seq(10)) {
        savePath <- paste0(saveRootNo, '/', i, '.png')
        tripDf <- getTripDf(filter(tripsNo, uniqueTripID == i))
        trip <- makePlot(tripDf, lang = lang)
        ggsave(savePath, trip, width = s$w, height = s$h, dpi = s$d)
    }
}
