corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  id<-1:332
  completeInfo <- complete(directory,id)
  resultVector <- vector()
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
    nobs<-completeInfo[num,2]
    
    if(as.numeric(nobs)>threshold){
      correlatedData <-cor(completeData[["nitrate"]],completeData[["sulfate"]])
      resultVector <- c(resultVector,correlatedData)
    }
  }
  #colnames(resultFrame) <- c("ID", "NOBS")
  resultVector
}