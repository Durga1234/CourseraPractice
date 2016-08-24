
setwd("F:/R PRACTICE/specdata")

##part-1
pollutantmean <- function(directory,pollutant, id=1:332){
  
  
  if(grep("specdata",directory)==1){
    
    directory <- ("./specdata/")
    
  }
  
  mean_vector <- c()
  all_files <- as.character(list.files(directory))
  file_paths <- paste(directory, all_files,sep="")
  for(i in id){
    current_file <- read.csv(file_paths[i],header = T, sep = ",")
    head(current_file)
    pollutant
    na_removed <- current_file[!is.na(current_file[,pollutant]),pollutant]
    mean_vector <- c(mean_vector,na_removed)
  }
  result <- mean(mean_vector)
  return(round(result,4))
}




pollutantmean("specdata","nitrate")

?formatC

## Part-2

cc <- complete("specdata", c(6,10,20,34,100,200,310))

complete <- function(directory, id =1:332){
  
  nobs=numeric()
  
  for(i in id){
    
    newRead <- read.csv(paste(directory,"/", formatC(i,width = 3, flag="0"),".csv", sep=""))
    nobs= c(nobs,sum(complete.cases(newRead)))
  }
  return(data.frame(id,nobs))
}
cc <- complete("specdata", 54)
print(cc$nobs)

set.seed(42)
cc <- complete("specdata",332:1)
use <- sample(332,10)
print(cc[use,"nobs"])


corr <- function(directory,threshold=0){
  
  
  df=complete(directory)
  ids= df[df["nobs"]>threshold, ]$id
  corrr=numeric()
  for(i in ids){
    newRead=read.csv(paste(directory, "/", formatC(i,width=3,flag="0"),".csv", sep = ""))
    dff=newRead[complete.cases(newRead),]
    corrr <- c(corrr,cor(dff$sulfate, dff$nitrate))
    
  }
  return(corrr)
}

cr <- corr("specdata",150)


cr <- corr("specdata")
cr <- sort(cr)
set.seed(868)
out <- round(cr[sample(length(cr),5)],4)
print(out)



cr <- corr("specdata",129)
cr <- sort(cr)
n <- length(cr)
set.seed(197)
out <- c(n,round(cr[sample(n,5)],4))
print(out)



cr <- corr("specdata",2000)
n <- length(cr)
cr <- corr("specdata",1000)
cr <- sort(cr)
print(c(n,round(cr,4)))








