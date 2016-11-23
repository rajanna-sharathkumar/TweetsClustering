args = commandArgs(trailingOnly=TRUE)

if (length(args) == 4) {
  
  # rm(list = ls())
  if (!require("jsonlite")) {
    install.packages("jsonlite", repos="http://cran.rstudio.com/") 
    library("jsonlite")
  }
  
  if (!require("stringr")) {
    install.packages("stringr", repos="http://cran.rstudio.com/") 
    library("stringr")
  }
  
  options(warn=-1)
  
  # setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  
  # preprocess twitter data
  Clean_String <- function(string){
    temp <- tolower(string)
    temp <- stringr::str_replace_all(temp,"[^a-zA-Z0-9\\s]", " ")
    temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
    temp <- stringr::str_split(temp, " ")[[1]]
    indexes <- which(temp == "")
    if(length(indexes) > 0){
      temp <- temp[-indexes]
    } 
    return(temp)
  }
  
  # returns jaccard distance
  jaccard_dist <- function(str1,str2){
    seta <- Clean_String(str1)
    setb <- Clean_String(str2)
    # print(seta)
    # print(setb)
    # print(seta[[1]][1])
    if(length(seta) == 0 || length(setb)==0){
      x <- 0
      y <- if(length(seta)==0) length(setb) else length(seta)
    }
    else{
      x <- length(intersect(seta,setb))
      y <- length(union(seta,setb))
    }
    
    res <- round(1-(x/y),3)
    # if(res < 0) print(res)
    return(res)
  }
  
  # kmeans clustering based on Jaccards distance
  kmeans <- function(){
    n_iter <- 0
    repeat{
      n_iter <- n_iter+1
      
      #Assign centroids
      for(i in 1:nrow(tweets.data)){
        str1 <- tweets.data[i,"text"]
        jacd <- c()
        # print(i)
        for(j in 1:k){
          str2 <- tweets.data[which(tweets.data[,"id_str"] %in% cent[j,1]),"text"]
          jacd[j] <- jaccard_dist(str1,str2)
        }
        tweets.data[i,"cid"] <- match(min(jacd),jacd)
        
      }
      
      #Calculate new Centroids
      ncent <- c()
      for(i in 1:k){
        jacd <- c()
        points <- which(tweets.data[,"cid"] %in% i)
        if(length(points)!=0){
          for(t in 1:length(points)){
            str1 <- tweets.data[points[t],"text"]
            rest_points <- points[-t]
            dist <- 0
            for(point in rest_points){
              str2 <- tweets.data[point,"text"]
              dist <- dist + jaccard_dist(str1,str2)
            }
            jacd[t] <- dist
            
          }
          ncent[i] <- tweets.data[points[match(min(jacd),jacd)], "id_str"]
        }
        else{
          ncent[i] <- cent[i,1]
        }
      }
      
      for(i in 1:k){
        if(ncent[i] != cent[i,1]){
          cent[i,1] <- ncent[i]
          cent[i,2] <- 1
        }
      }
      if(length(which(cent[i,2] %in% 1)) == 0) break
      cent[i,2] <- 0
    }
    
    return(tweets.data)
  }
  
  sse <- function(){
    dist <- 0
    for(i in 1:k){
      str1 <- tweets.data[which(tweets.data[,"id_str"] %in% cent[i,1]),"text"]
      points <- which(tweets.data[,"cid"] %in% i)
      if(length(points)!=0){
        for(point in points){
          str2 <- tweets.data[point,"text"]
          jacd <- jaccard_dist(str1,str2)
          
          dist <- dist+(jacd ^ 2)
        }
      }
    }
    # print(dist)
    return(dist)
  }
  
  # read twitter data
  tweets.data <- stream_in(file(args[3]))
  tweets.data["cid"] <- 0
  
  # read Initial seeds
  centroids <- read.table(args[2], header = FALSE)
  cent <- c()
  for(i in 1:length(centroids$V1)){
    cent <- rbind(cent,str_replace_all(centroids$V1[i],",",""))
  }
  cent <- cbind(cent,0)
  
  # NUmber of clusters
  k <- 25
  k <- as.integer(args[1])
  
  #call kmeans
  tweets.data <- kmeans()
  
  # validate clustering
  dist <- sse()
  
  # write result to a file
  final.res <- c()
  
  for(i in 1:k){
    final.res <- rbind(final.res,c(i,
                                   paste(which(tweets.data[,"cid"] %in% i), collapse = ",")))
  }
  
  final.res <- rbind(final.res,c("SSE", dist))
  res.df<-  data.frame(Centroids = final.res[,1], Centroid_points = final.res[,2])
  # print(final.res)
  write.table(res.df,args[4],row.names = FALSE, col.names = FALSE, sep = " ")
  
}else{
  stop("tweets-k-means <numberOfClusters> <initialSeedsFile> <TweetsDataFile> <outputFile>", call.=FALSE)
}
