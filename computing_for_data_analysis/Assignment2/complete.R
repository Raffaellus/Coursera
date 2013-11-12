complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used

    obs <- rep(0, length(id))
    count <- 0
    for (index in id) {
        count <- count + 1
        if (index < 10) {
            new_id <- paste(c("00", as.character(index)), collapse="")
        } else if (index >= 10 & index < 100) {
            new_id <- paste(c("0", as.character(index)), collapse="")
        } else {
            new_id <- as.character(index)
        }

        file_name <- paste(c(directory, "/", new_id, ".csv"), collapse="")
        data <- read.csv(file_name, header=TRUE)
        obs[count] <- sum(complete.cases(data))
    }
    output <- data.frame(id = id, nobs=obs)
    return(output)
}
