complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  resultFrame <- data.frame("id" = character(), "nobs" = character(),stringsAsFactors=FALSE)
  for(num in id){
    filenum<-num
    if(num < 10)
   {
       filenum<-paste(0,num,sep="")
   }
   if(num < 100)
   {
     filenum<-paste(0,filenum,sep="")
   }
   filenum<-as.character(filenum)
    fileName <- paste(directory,paste(filenum,".csv",sep=""),sep="/")
    dataCSV<-read.csv(fileName,stringsAsFactors=FALSE)
    completeData <- dataCSV[complete.cases(dataCSV),]
    newrow <- c(num,nrow(completeData))
    resultFrame[nrow(resultFrame)+1,]<-newrow
  }
  resultFrame
}