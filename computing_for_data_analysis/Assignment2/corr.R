corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0

    allObs <- complete(directory, 1:332)
    index <- allObs["id"][allObs["nobs"] > threshold]
    all_corr <- rep(0, length(index))
    if (length(index) > 0) {
        count <- 0
        for (i in index) {
            count <- count + 1
            data <- getmonitor(i, directory)
            complete_data <- data[complete.cases(data),]
            all_corr[count] <- cor(complete_data["sulfate"], complete_data["nitrate"])
        }
        return(all_corr)
    } else {
        return(vector("numeric"))
    }
}
