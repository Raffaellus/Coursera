getmonitor <- function(id, directory, summarize = FALSE) {
    ## 'id' is a vector of length 1 indicating the monitor ID number.
    ## The user can specify 'id' as either and integer, a character, 
    ## or a numeric.

    ## 'directory' is a character vector of length 1 
    ## indicating the location of the CSV files.

    ## 'summarize' is a logical indicating whether a summary of 
    ## the data should be printed to the console.

    if (as.numeric(id) < 10) {
        new_id <- paste(c("00", as.character(id)), collapse="")
    } else if (as.numeric(id) >= 10 & as.numeric(id) < 100) {
        new_id <- paste(c("0", as.character(id)), collapse="")
    } else {
        new_id <- as.character(id)
    }

    file_name <- paste(c(directory, "/", new_id, ".csv"), collapse="")
    data <- read.csv(file_name, header=TRUE)
    if (summarize == TRUE) {
        print(summary(data))
    }
    return(data)
}

