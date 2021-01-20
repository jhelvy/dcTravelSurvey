# Functions for balancing the full factorial ----------------------------------

getAllDiffs <- function(tripSet) {
    n <- nrow(tripSet)
    combs <- getCombinations(n)
    result <- list()
    for (i in 1:length(combs)) {
        rows <- c(combs[[i]], seq(n))
        diffs <- unlist(getDiffs(tripSet[rows,]))
        result[[i]] <- list(rows = rows, diffs = diffs)
    }
    return(result)
}

getCombinations <- function(n) {
    result <- list()
    index <- 1
    for (i in 1:n) {
        combs <- t(combn(seq(n), i))
        for (j in 1:nrow(combs)) {
            result[[index]] <- combs[j,]
            index <- index + 1
        }
    }
    return(result)
}

getDiffs <- function(trips) {
    return(list(
        mode    = getModeDiff(trips),
        numLegs = getLegsDiff(trips)))
}

getModeDiff <- function(df) {
    temp <- getModeCount(df)
    temp <- temp[which(temp != 0)]
    return(max(temp) / min(temp))
}

getModeCount <- function(df) {
    return(df %>%
        summarise(
            car  = sum(carInTrip),
            taxi = sum(taxiInTrip),
            bus  = sum(busInTrip),
            metro= sum(metroInTrip),
            walk = sum(walkInTrip)))
}

getTripCount <- function(df) {
    return(df %>%
        count(trip) %>% 
        arrange(desc(n)))
}

getLegsDiff <- function(df) {
    temp <- getLegsCount(df)
    return(max(temp$n) / min(temp$n))
}

getLegsCount <- function(df) {
    return(count(df, numLegs))    
}

getTripResults <- function(diffs) {
    temp <- list()
    for (i in 1:length(diffs)) {
        temp[[i]] <- diffs[[i]]$diffs
    }
    result <- as.data.frame(do.call(rbind, temp)) %>%
        mutate(
            index = row_number(),
            score = mode + numLegs) %>% 
        rowwise() %>% 
        mutate(mean  = mean(c(mode, numLegs)))
    return(result)
}

# Functions for making the full factorial -------------------------------------

makeBalancedFF <- function(ff, trips) {
    # Add trips to the full factorial
    ff_bal <- expand.grid(
        ffId   = ff$ffId,
        tripId = trips$tripId) %>%
        left_join(trips) %>%
        left_join(ff) %>%
        select(-ffId) %>%
        # Filter out nonsensical alternatives and add some helpful variables
        fixTimeCases() %>%
        carSpecificCleaning() %>%
        filterCases() %>%
        addTimeSummary() %>%
        # Add repeated rows to balance the FF based on the tripId
        getBalancedFF()
    return(ff_bal)
}

fixTimeCases <- function(df) {
    result <- df %>%
        mutate(
            # For 1 leg trips, leg2 times and transfer times are 0
            # If leg 2 is Walk, then the transfer time is 0  
            leg2Time = ifelse(numLegs == 1, 0, leg2Time),
            transferTime = ifelse(
                (numLegs == 1) | (leg2Mode == walk), 0, transferTime),
            # If first leg is walk, then no wait time at the start
            startTime = ifelse(leg1Mode == walk, 0, startTime)) %>% 
    # Remove duplicates that may now be remaining
    distinct()
    return(result)
}

carSpecificCleaning <- function(df) {
    result <- df %>%
    mutate(
        # You can only have an express fee for the express mode
        expressFee = ifelse(carExpress == 1, expressFee, 0),
        # If first leg is car, then no wait time at the start
        startTime = ifelse(leg1Mode == car, 0, startTime),
        # Can't have carExpress if car isn't in trip
        carExpress = ifelse(carInTrip == 0, 0, carExpress),
        # If trip contains car, minimum price is $5
        price = ifelse(carInTrip & (price < 5), 5, price)
    ) %>% 
    # Remove duplicates that may now be remaining
    distinct()
    return(result)
}

filterCases <- function(df) {
    # Filter out unrealistic or illogical cases
    result <- df %>%
    mutate(
        # If trip is bus only or metro only, maximum price is $10
        price = ifelse((busOnlyTrip == 1) & (price > 10), 10, price),
        price = ifelse((metroOnlyTrip == 1) & (price > 10), 10, price),
        # If trip contains taxi, minimum price is $5
        price = ifelse(taxiInTrip & (price < 5), 5, price),
        # Max walking time in any leg is 30 minutes
        leg1Time = ifelse((leg1Mode == walk) & (leg1Time > 30), 30, leg1Time),
        leg2Time = ifelse((leg2Mode == walk) & (leg2Time > 30), 30, leg2Time)
    ) %>% 
    # Remove duplicates that may now be remaining
    distinct()
    return(result)
}

addTimeSummary <- function(df) {
    # Generate some useful summary variables
    result <- df %>% mutate(
        totalLegTime  = leg1Time + leg2Time,
        totalWaitTime = startTime + transferTime,
        totalTripTime = totalLegTime + totalWaitTime,
        tripTimeMax   = ceiling(totalTripTime*(1 + tripTimeUnc)),
        tripTimeRange = paste(totalTripTime, '-', tripTimeMax, 'minutes',
                        sep=' ')) %>%
    # Remove duplicates that may now be remaining
    distinct()
    return(result)
}

getBalancedFF <- function (ff) {
    proportions <- ff %>%
        count(tripId) %>%
        mutate(diff = max(n) - n)
    newRows <- list()
    for (i in 1:nrow(proportions)) {
        rowIds <- which(ff$tripId == proportions$tripId[i])
        sampleIds <- sample(x = rowIds, size = proportions$diff[i], replace = T)
        newRows[[i]] <- ff[sampleIds,]
    }
    newRows <- do.call(rbind, newRows)
    ff_bal <- rbind(ff, newRows)
    return(ff_bal)
}

# Functions for making the DOE ------------------------------------------------

makeSurvey <- function(df, nResp, nAltsPerQ, nQPerResp) {
    nRows  <- nResp*nAltsPerQ*nQPerResp
    doe    <- repDf(df, n = ceiling(nRows / nrow(df)))
    doe    <- doe[1:nRows,]
    doe    <- doe[sample(nrow(doe)),]
    survey <- addMetaData(doe, nAltsPerQ, nQPerResp)
    survey <- addUniqueTripIDs(survey)
    survey <- removeDoubleAlts(survey, nAltsPerQ, nQPerResp)
    return(survey)
}

repDf <- function(df, n) {
    return(df[rep(seq_len(nrow(df)), n), ])
}

addMetaData <- function(doe, nAltsPerQ, nQPerResp) {
    nRowsPerResp   <- nAltsPerQ*nQPerResp
    nResp          <- nrow(doe) / nRowsPerResp
    doe$respID     <- rep(seq(nResp), each=nRowsPerResp)
    doe$qID        <- rep(rep(seq(nQPerResp), each=nAltsPerQ), nResp)
    doe$altID      <- rep(seq(nAltsPerQ), nResp*nQPerResp)
    doe$obsID      <- rep(seq(nResp * nQPerResp), each=nAltsPerQ)
    row.names(doe) <- NULL
    return(doe)
}

addUniqueTripIDs <- function(doe) {
    temp <- as.data.table(doe)
    temp[,distinctTrip := paste(
        price, expressFee, tripTimeRange, trip, startTime, leg1Time,
        transferTime, leg2Time, sep = '|'),]
    uniqueTrips <- data.frame(
        distinctTrip = unique(temp$distinctTrip),
        uniqueTripID = seq(length(unique(temp$distinctTrip))))
    return(as.data.frame(temp) %>%
               left_join(uniqueTrips) %>%
               select(-distinctTrip))
}

removeDoubleAlts <- function(doe, nAltsPerQ, nQPerResp) {
    doe <- addUniqueTripCounts(doe)
    doubleRows <- which(doe$numUnique != nAltsPerQ)
    while (length(doubleRows) != 0) {
        cat('Number repeated: ', length(doubleRows), '\n')
        newRows <- sample(x=seq(nrow(doe)), size=length(doubleRows), replace=F)
        doe[doubleRows,] <- doe[newRows,]
        doe <- addMetaData(doe, nAltsPerQ, nQPerResp)
        doe <- addUniqueTripCounts(doe)
        doubleRows <- which(doe$numUnique != nAltsPerQ)
    }
    return(select(doe, -numUnique))
}

addUniqueTripCounts <- function(doe) {
    temp <- as.data.table(doe)
    temp[,numUnique := length(unique(uniqueTripID)),by = obsID]
    return(as.data.frame(temp))
}

# Functions for the samplesize experiments ------------------------------------

dummyCode = function(df, vars) {
    df = as.data.frame(df)
    nonVars = colnames(df)[which(! colnames(df) %in% vars)]
    # Keep the original variables and the order to restore later after merging
    df$order = seq(nrow(df))
    for (i in 1:length(vars)) {
        var      = vars[i]
        colIndex = which(colnames(df) == var)
        levels   = sort(unique(df[,colIndex]))
        mergeMat = as.data.frame(diag(length(levels)))
        mergeMat = cbind(levels, mergeMat)
        colnames(mergeMat) = c(var, paste(var, levels, sep='_'))
        df = merge(df, mergeMat)
    }
    # Restore the original column order
    new = colnames(df)[which(! colnames(df) %in% c(vars, nonVars))]
    df = df[c(nonVars, vars, new)]
    # Restore the original row order
    df = df[order(df$order),]
    row.names(df) = df$order
    df$order <- NULL
    return(df)
}

formatSE <- function(df) {
    df <- df %>%
        mutate(category = case_when(
            coef %in% c('price', 'expressFee') ~ 'Price',
            str_detect(coef, 'int')            ~ 'Interaction',
            str_detect(coef, 'Mode')           ~ 'Mode',
            str_detect(coef, 'trip')           ~ 'Mode',
            str_detect(coef, 'Time')           ~ 'Time',
            TRUE                               ~ 'Other',
        ))
    return(df)
}

# Functions for making the tripDfs -------------------------------------------

getTripDf <- function(row) {
    trip   <- makeTripVector(row)
    tripDf <- tibble(
        x     = 0,
        y     =  getYSpacing(row),
        label = trip) %>%
        addPlotLabels() %>%
        changeCarLabels(row) %>%
        mutate(
            price     = row$price,
            timeRange = row$tripTimeRange, 
            express   = row$carExpress, 
            fee       = row$expressFee)
    return(tripDf)
}

makeTripVector <- function(row) {
    start <- makeTripStart(row)
    legs <- makeTripLegs(row)
    transfer <- makeTripTransfer(row)
    trip <- c(start, legs[1], transfer, legs[2])
    return(c(trip[!is.na(trip)], 'End'))
}

makeTripStart <- function(row) {
    start <- 'Start'
    if (row$startTime != 0) {
        start <- paste('Start\n(', row$startTime, ' min wait)', sep='')
    }
    return(start)
}

makeTripLegs <- function(row) {
    if (row$numLegs == 1) {
        legs <- paste(row$leg1Mode, '\n(', row$leg1Time, ' mins)', sep='')
    } else {
        legs <- row[c('leg1Mode', 'leg2Mode')]
        legTimes <- row[c('leg1Time', 'leg2Time')]
        legs <- paste(legs, '\n(', legTimes, ' mins)', sep='')
    }
    return(legs)
}

makeTripTransfer <- function(row) {
    if (row$numLegs == 1) { return(NULL) }
    transfer <- 'Transfer'
    if (row$transferTime != 0) {
        transfer <- paste('Transfer\n(', row$transferTime, ' min wait)',
                          sep='')
    }
    return(transfer)
}

getYSpacing <- function(row) {
    legTimes <- row[c('leg1Time', 'leg2Time')]
    legTimes <- legTimes[1:row$numLegs]
    breaks  = cumsum(c(0, -1 * legTimes / sum(legTimes)))
    if (abs(breaks[2]) < 0.12) {
        breaks[2] = -0.12
    }
    if (abs(breaks[length(breaks)-1]) > 0.88) {
        breaks[length(breaks)-1] = -0.88
    }
    spacing = (breaks[2:length(breaks)] - breaks[1:(length(breaks) - 1)]) / 2
    y       = c(0)
    for (i in 1:(length(breaks) - 1)) {
        y = c(y, breaks[i] + spacing[i], breaks[i+1])
    }
    return(y)
}

addPlotLabels <- function(tripDf) {
    # Compute where to put nodes
    node = rep(0, nrow(tripDf))
    node[which(str_detect(tripDf$label, 'Transfer'))] <- 1
    # Compute which type of label to print
    labelType = rep('Transit', nrow(tripDf))
    labelType[which(str_detect(tripDf$label, 'Transfer'))] <- 'Node'
    labelType[c(1, nrow(tripDf))] <- 'Terminal'
    # Set line points
    lineNodes <- getPlotLineNodes(tripDf)
    # Add variables to data frame
    tripDf$node      <- node
    tripDf$labelType <- labelType
    tripDf$lineNodes <- lineNodes
    return(tripDf)
}

getPlotLineNodes <- function(tripDf) {
    lineNodes <- rep(-1, nrow(tripDf))
    lineNodes[which(str_detect(tripDf$label, 'Walk'))] <- 0
    index = 1
    for (i in 1:length(lineNodes)) {
        if (lineNodes[i] == 0) {
            index = index + 1
            next
        }
        lineNodes[i] = index
    }
    # Any legs < 3 nodes don't need a solid lineNodes
    walkNodes <- as.integer(names(which(table(lineNodes) < 3)))
    lineNodes[which(lineNodes %in% walkNodes)] <- 0
    return(lineNodes)
}

changeCarLabels <- function(tripDf, row) {
    carRows <- which(str_detect(tripDf$label, 'Car'))
    if (length(carRows) == 0) { return(tripDf) }
    # Change "Car" to "Car: Express" if row$carExpress == 1
    if (row$carExpress == 1) {
        tripDf[carRows,]$label <- str_replace(
            tripDf[carRows,]$label, 'Car', 'Car: Express'
        )        
    }
    # Change "Transfer" to "Park" for car transfers
    tripDf[(carRows + 1),]$label <- str_replace(
        tripDf[(carRows + 1),]$label, 'Transfer', 'Park'
    )
    return(tripDf)
}

# Functions for making the trip plots -----------------------------------------

makePlot <- function(trip, lang = 'en') {
    labels <- getLabels(trip, lang)
    trip <- translateTrip(trip, lang)
    p <-
        ggplot(data = trip, aes(x = x, y = y)) +
        # Draw lines
        geom_line(data = trip, size = 1, linetype = 'dotted') +
        geom_line(data = filter(trip, lineNodes == 1), size = 1) +
        geom_line(data = filter(trip, lineNodes == 2), size = 1) +
        geom_line(data = filter(trip, lineNodes == 3), size = 1) +
        # Draw nodes
        geom_point(data = filter(trip, node == 1), size = 4, pch = 21,
                   fill = 'white', colour = 'black') +
        geom_text_repel(data = filter(trip, labelType == 'Transit'),
                        aes(label = label),
                        size           = 4,
                        force          = 3,
                        nudge_x        = 1,
                        fontface       = "bold",
                        family         = labels$font,
                        box.padding    = unit(0.35, "lines"),
                        point.padding  = unit(0.75, "lines"),
                        color          = "black",
                        segment.colour = "black") +
        geom_label_repel(data = filter(trip, labelType == 'Node'),
                         aes(label=label),
                         size           = 4,
                         force          = 3,
                         nudge_x        = -1,
                         fontface       = "bold",
                         family         = labels$font,
                         box.padding    = unit(0.35, "lines"),
                         point.padding  = unit(0.75, "lines"),
                         color          = "black",
                         fill           = "white",
                         segment.colour = "black") +
        geom_label(data = filter(trip, labelType == 'Terminal'),
                   aes(label=label),
                   label.size = 1,
                   fontface   = "bold",
                   family     = labels$font,
                   fill       = "white",
                   color      = "black") +
        scale_x_continuous(limits=c(-1, 0.6)) +
        # Add option label, and price and time totals at the top
        annotate("text", x = -0.7, y = 0.15,
                 fontface = "bold", family = labels$font,
                 label = labels$price) +
        annotate("text", x = -0.35, y = 0.15, hjust = 0,
                 family = labels$font,
                 label = paste0(labels$priceVal, "\n", 
                                labels$timeRange)) +
        theme_void()
    return(p)
}

getLabels <- function(trip, lang = 'en') {
    option <- "Option "
    price <- "Total Price:\nTotal Time:"
    font <- "Roboto Condensed"
    timeRange <- unique(trip$timeRange)
    priceVal <- paste0("$", unique(trip$price))
    fee <- unique(trip$fee)
    if (lang == 'sp') {
        option <- "Opción "
        price <- "Precio total:\nTiempo total:"
        timeRange <- str_replace(timeRange, 'minutes', 'minutos')
    }
    if (lang == 'ko') {
        option <- "선택 "
        price <- "총 비용:\n총 시간:"
        font <- "NanumMyeongjo"
        timeRange <- str_replace(timeRange, 'minutes', '분')
    }
    if (unique(trip$express == 1) & (fee > 0)) {
        priceVal <- paste0(priceVal, " + $", fee)
        if (lang == 'sp') {
            priceVal <- paste0(priceVal, " tarifa")
        } else if (lang == 'ko') {
            priceVal <- paste0(priceVal, " 요금")
        } else {
            priceVal <- paste0(priceVal, " fee")
        }
    }
    return(list(option = option, price = price, font = font, 
                priceVal = priceVal, timeRange = timeRange))
}

translateTrip <- function(trip, lang = 'en') {
    if (lang == 'en') { return(trip) }
    if (lang == 'sp') {
        trip <- trip %>% mutate(
            label = str_replace(label, 'Start', 'Inicio'),
            label = str_replace(label, 'min wait', 'minutos\nde espera'),
            label = str_replace(label, 'mins', 'minutos'),
            label = str_replace(label, 'Park', 'Aparcar'),
            label = str_replace(label, 'Transfer', 'Traslado'),
            label = str_replace(label, 'Walk', 'Caminar'),
            label = str_replace(label, 'Car', 'Carro'),
            label = str_replace(label, 'Express', 'vía express'),
            label = str_replace(label, 'Bus', 'Autobús'),
            label = str_replace(label, 'Autonomous\nTaxi', 'Taxi\nAutónomo'),
            label = str_replace(label, 'End', 'Fin')
        )
    } else if (lang == 'ko') {
        trip <- trip %>% mutate(
            label = str_replace(label, 'Start', '출발'),
            label = str_replace(label, 'min wait', '분 기다림'),
            label = str_replace(label, 'mins', '분'),
            label = str_replace(label, 'Park', '주차'),
            label = str_replace(label, 'Transfer', '갈아탐'),
            label = str_replace(label, 'Walk', '도보'),
            label = str_replace(label, 'Car\\: Express', '승용차: 고속 차선'),
            label = str_replace(label, 'Car', '승용차'),
            label = str_replace(label, 'Bus', '버스'),
            label = str_replace(label, 'Uber/Taxi', '택시/우버'),
            label = str_replace(label, 'Autonomous\nTaxi', '자율 주행 택시'),
            label = str_replace(label, 'End', '도착')
        )
    }
    return(trip)
}
