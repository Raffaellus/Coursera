rankall <- function(outcome, num = "best") {
    # Read outcome data 
    stats <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    stats[, 11] <- as.numeric(stats[, 11])
    stats[, 17] <- as.numeric(stats[, 17])
    stats[, 23] <- as.numeric(stats[, 23])
    state_list <- unique(stats$State)

    # Check that state and outcome are valid
    if (outcome %in% c("heart attack", "heart failure", "pneumonia") == FALSE) stop("invalid outcome") 


    # map outcome to index
    if (outcome == "heart attack") index <- 11
    else if (outcome == "heart failure") index <- 17
    else index <- 23

    df_target <- data.frame(cbind(state = stats$State, outcome = stats[, index], hospital = stats$Hospital.Name), 
                            stringsAsFactors = FALSE)
    target <- transform(df_target, outcome = as.numeric(outcome))

    # For each state, find the hospital of the given rank 
    all_hospital <- rep("", length(state_list))
    count <- 1
    num_num <- 0
    for (st in state_list) {
        keep <- complete.cases(target[target$state == st, ])
        state_outcome <- target[target$state == st, ][keep, ]

        # deal with num
        if (num == "best") new_num <- 1
        else if (num == "worst") new_num <- nrow(state_outcome)
        else new_num <- num

        if (new_num > nrow(state_outcome)) {
            all_hospital[count] <- NA
        } else {
            rank_state <- state_outcome[order(state_outcome$outcome, state_outcome$hospital), ]
            all_hospital[count] <- rank_state[new_num, ]$hospital
        }
        count <- count + 1
    }

    # Return a data frame with the hospital names and the state name
    output <- data.frame(cbind(hospital = all_hospital, state = state_list))
    row.names(output) <- state_list 
    order_output <- output[order(output$state), ]
    return(order_output)
}

