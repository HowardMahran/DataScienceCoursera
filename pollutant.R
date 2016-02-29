readPollutants <- function (directory, id=1:332){
    ## 'directory' is the location of the CSV files
    ## id is the monitor ID numbers to be used
    pData <- c() ## empty vector for pollutant only data
    for (i in id){
        
        ## Set the filename prefix
        if(i < 10) {filepre <- "00"}
        else if (i >=10 && i < 100) { filepre <-"0"}
        else { filepre <-""}
        
        ## make the file name 
        fname <- paste(getwd(),directory,filepre, i,".csv", sep="")
        
        ## load the file data
        data<- read.csv(fname)
        
        ## Add the Pollutant data to the last set loaded after removing the NA's
        pData <- c(pData,na.omit(data[]))
        
    }
    ## Calculate the mean
    return (pData)
}


pollutantmean <- function (directory, pollutant, id=1:332){
    ## 'directory' is the location of the CSV files
    ## 'pollutant' is either "sulfate" or "nitrate"
    ## id is the monitor ID numbers to be used
   pData <- c() ## empty vector for pollutant only data
    for (i in id){
       
        ## Set the filename prefix
        if(i < 10) {filepre <- "00"}
        else if (i >=10 && i < 100) { filepre <-"0"}
        else { filepre <-""}
        
        ## make the file name 
        fname <- paste(getwd(),directory,filepre, i,".csv", sep="")
        
        ## load the file data
         data<- read.csv(fname)
         
        ## Add the Pollutant data to the last set loaded after removing the NA's
         pData <- c(pData,na.omit(data[[pollutant]]))
         
    }
    ## Calculate the mean
    return(mean(pData))
}

complete <- function (directory, id=1:332){
    ## 'directory' is the location of the CSV files
       ## id is the monitor ID numbers to be used
    dMatrix <- NULL ## empty matrix to hold the answers from each file
   
     for (i in id){
        
        ## Set the filename prefix
        if(i < 10) {filepre <- "00"}
        else if (i >=10 && i < 100) { filepre <-"0"}
        else { filepre <-""}
        
        ## make the file name 
        fname <- paste(getwd(),directory,filepre, i,".csv", sep="")
        
        ## load the file data and omit the NA values all in one step
       countComplete<- nrow(na.omit(read.csv(fname)))
       
       dMatrix <- rbind(dMatrix,c(i,countComplete)) ## adds new rows to the matrix
      
    }
    
  dFrame <- data.frame(dMatrix) ## converts the matrix into a data frame
  names(dFrame) <- c('id','nobs') ## gives the data frame row names
  return(dFrame) #returns the data frame

}


corr <- function(directory, threshold = 0) {
    cr <- NULL ## the matrix to hold the data for correlation
    cD <-  complete(directory) ## Get the completed data set ID's and number of Observations ('nobs') 
    for (i in 1:nrow(cD)){ ## loop throught the dataset

        if (cD[i,]$nobs > threshold)   { ## check if 'nobs' is greater than 'threshold' if yes go get the data
        
            pollutants <- readPollutants(directory, i) ## Use custom function to get data
            nitrate <- pollutants$nitrate # For clairity
            sulfate <- pollutants$sulfate
            cr <- c(cr, cor(nitrate, sulfate)) # Build the correlation recordset
        }
       
    }
   
    return(cr)
    
}

