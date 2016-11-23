# TweetsClustering

k-means clustering algorithm on tweet analysis using Jaccard distance

Programming language used: R

Files included:

1. InitialSeeds.txt-contains the initial centroids of the k-means
2. Output.txt - sample output
3. tweets-k-means.R - k-means clustering implemented in R
4. Tweets.json - the boston bombing tweets dataset 


Steps to run the code:

1.On the command line go the directory containing the files

2.Type or copy and paste the below command to run the python program on the command line
       
Rscript --vanilla tweets-k-means.R 25 ./InitialSeeds.txt ./Tweets.json ./output.txt
       
Note:You can change the name of the output file if the output file with the name already exists.

SSE value="13.537636"
