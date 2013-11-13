options(warn = -1)

best <- function(state, outcome) {
    # Read outcome data
    stats <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    stats[, 11] <- as.numeric(stats[, 11])
    stats[, 17] <- as.numeric(stats[, 17])
    stats[, 23] <- as.numeric(stats[, 23])
    state_list <- unique(stats$State)

    # Check that state and outcome are valid
    if (is.element(state, state_list) == FALSE) {
        stop("invalid state") 
    } else if (outcome %in% c("heart attack", "heart failure", "pneumonia") == FALSE) {
        stop("invalid outcome") 
    }

    # Return hospital name in that state with lowest 30-day death rate
    keep <- complete.cases(stats[stats$State == state, ])
    state_stats <- stats[stats$State == state, ][keep, ]
    if (outcome == "heart attack") {
        provider <- state_stats[order(state_stats[, 11], state_stats$Hospital.Name), ][1, 1]
        return(state_stats$Hospital.Name[state_stats$Provider.Number == provider])
    } else if (outcome == "heart failure") {
        provider <- state_stats[order(state_stats[, 17], state_stats$Hospital.Name), ][1, 1]
        return(state_stats$Hospital.Name[state_stats$Provider.Number == provider])
    } else {
        provider <- state_stats[order(state_stats[, 23], state_stats$Hospital.Name), ][1, 1]
        return(state_stats$Hospital.Name[state_stats$Provider.Number == provider])
    }


}



