source('setup.R')
survey_no <- readRDS(here::here('doe', 'data', 'survey_no.Rds')) %>%
    mutate(hasCar = 'no')
survey_yes <- readRDS(here::here('doe', 'data', 'survey_yes.Rds')) %>%
    mutate(hasCar = 'yes')
survey <- bind_rows(survey_no, survey_yes)

survey %>%
    group_by(hasCar) %>%
    getModeCount() %>%
    gather(mode, count, car:walk) %>%
    group_by(hasCar) %>%
    arrange(hasCar) %>%
    mutate(p = count / sum(count),
           p = round(p, 2))

survey %>%
    group_by(hasCar) %>%
    count(numLegs) %>%
    mutate(p = n / sum(n),
           p = round(p, 2))

survey %>%
    group_by(hasCar) %>%
    getTripCount() %>%
    arrange(hasCar) %>%
    as.data.frame()
