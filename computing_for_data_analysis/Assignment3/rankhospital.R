rankhospital <- function(state, outcome, num = "best") {
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


    # Return hospital name in that state with the given rank 30-day death rate
    keep <- complete.cases(stats[stats$State == state, ])
    state_outcome <- stats[stats$State == state, ][keep, ]

    # deal with num
    if (num == "best") num = 1
    else if (num == "worst") num = nrow(state_outcome)
    
    if (num > nrow(state_outcome)) return(NA)

    if (outcome == "heart attack") {
        rank_state <- state_outcome[order(state_outcome[, 11], state_outcome$Hospital.Name), ]
        return(rank_state[num, ]$Hospital.Name)
    } else if (outcome == "heart failure") {
        rank_state <- state_outcome[order(state_outcome[, 17], state_outcome$Hospital.Name), ]
        return(rank_state[num, ]$Hospital.Name)
    } else {
        rank_state <- state_outcome[order(state_outcome[, 23], state_outcome$Hospital.Name), ]
        return(rank_state[num, ]$Hospital.Name)
    }

}
