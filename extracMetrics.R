library(vegan)


#function to get the diffrent metrics from a dataframe
#input: dataframe representing sitesXspecies matrix 
#output: named vector
getMetric <- function(df){
  
}

richness <- specnumber(test[[1]])
mean_richness <- mean(richness)
var_richness <- var(richness)

#hill 0-2
renyi(test[[1]],c(0,1,2),hill = T)[1:3,]
renyi(test[[1]],c(0,1,2),hill = T)[1:3,]

