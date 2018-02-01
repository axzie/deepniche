


#function to get the diffrent metrics from a dataframe
#input: dataframe representing sitesXspecies matrix 
#output: named vector
getMetric <- function(df){
  library(vegan)
  result <- NULL
  #get the hill 0,1,2
  hills <- renyi(df,c(0,1,2),hill = T)
  
  hill_means <- apply(hills,MARGIN = 2,mean)
  names(hill_means) <- c("hill0_mean","hill1_mean","hill2_mean")
  
  hill_vars <- apply(hills,MARGIN = 2,var)
  names(hill_vars) <- c("hill0_var","hill1_var","hill2_var"
                        )
  #get beta-div
  betas <- hills[,3]/hills[,2]
  beta_mean <- c("beta_mean"=mean(betas))
  beta_var <- c("beta_var"=var(betas))
  
  result <- c(hill_means,hill_vars,beta_mean,beta_var)
  return(result)
  
}

#wrapper function to extrac the metrics from a list of sitesXspecies mat's 
#input: list of dataframe representing sitesXspecies matrix 
#output: dataframe of metrics

give_metric_list <- function(the_list){
  result <- sapply(the_list, "getMetric")
  result <- as.data.frame(t(result))
  return(result)
}


neutral_m <- cbind(give_metric_list(neutral),"model"="neutral")
niche_m <- cbind(give_metric_list(niche),"model"="niche")

















test3 <- give_metric_list(test)
test3 <- as.data.frame(t(test3)) 
getMetric(test[[1]])

richness <- specnumber(test[[1]])
mean_richness <- mean(richness)
var_richness <- var(richness)

#hill 0-2
renyi(test[[1]],c(0,1,2),hill = T)[1:3,]

hills <- renyi(test[[1]],c(0,1,2),hill = T)[1:3,]
apply(hills,MARGIN = 2,mean)
colMeans(hills)

betas <- hills[,3]/hills[,2]
mean(betas)
