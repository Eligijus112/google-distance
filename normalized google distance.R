getGoogleCount <- function(searchTerms=NULL, language="en", ...){
  require(RCurl)
  entry    <- paste(searchTerms, collapse="+")
  siteHTML <- getForm("http://www.google.com/search",
                      hl=language, lr="", q=entry,
                      btnG="Search")
  
  write.table(siteHTML, file="tmp google.txt")  
  indicatorWord <- "resultStats"        
  posExtractStart <- gregexpr(indicatorWord, siteHTML,
                              fixed = TRUE)[[1]]
  stringExtract <- as.character(substring(siteHTML, first=posExtractStart[2]-30,
                                          last = posExtractStart[2] +50 ))
  count <- strsplit(stringExtract, 'resultStats')[[1]][2] 
  count <- strsplit(count, split='results')[[1]][1]
  count <- strsplit(count, split='>')[[1]][2]
  if(length(strsplit(count, split=" ")[[1]])==2){
    count <- strsplit(count, split=" ")[[1]][2] 
  }
  count <- as.numeric(gsub(",", "", count))
  return(count)
}

getDistance <- function(x,y){
  xy <- getGoogleCount(c(x, y)) 
  x  <- getGoogleCount(c(x))
  y  <- getGoogleCount(c(y))
  
  xy <- as.numeric(gsub(",", "", xy))
  x  <- as.numeric(gsub(",", "", x ))
  y  <- as.numeric(gsub(",", "", y ))
  M <- 859000000 
  dist <- (max(log(x), log(y)) - log(xy))/(log(M)-min(log(x), log(y)))   
  return(dist)
}