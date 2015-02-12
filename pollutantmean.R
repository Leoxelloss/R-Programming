pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
    
  resultFrame <- data.frame("ID" = character(), "Pollutants" = numeric(),stringsAsFactors=FALSE)
  
  for(num in id){
    num
    if(num < 10)
    {
      num<-paste(0,num,sep="")
    }
    if(num < 100)
    {
      num<-paste(0,num,sep="")
    }
    num<-as.character(num)
    fileName <- paste(directory,paste(num,".csv",sep=""),sep="/")
    dataCSV<-read.csv(fileName,stringsAsFactors=FALSE)  #read in all the data
    pollutantData <- dataCSV[!is.na(dataCSV[[pollutant]]),]  # get the non NA data
    resultFrame <- rbind(resultFrame,pollutantData)  # add all the result rows into resultframe
  }
  mean(as.numeric(resultFrame[[pollutant]]))
}