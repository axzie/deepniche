#TODO: should we crop?



#creat sitesXspecies - matrix
#input: vector: species, plot_ids
#output: dataframe containing the matrix

create_sites_species_m <- function(data)
{
  library("dplyr")
  library("tidyr")
  result <- data %>% group_by(plot.id)  %>% count(spr1) %>% spread(spr1,n)
  result[is.na(result)] <- 0
  result$plot.id<-NULL
  result$"<NA>" <- NULL
  return(as.data.frame(result))
}

#listwrapper for creating some or all sites x species matrices
#input: the list and optionaly a index vector
#output: list of sitesXspecies matrices

give_sites_species_matrix_list <- function(the_list,index=NULL)
{
  if(is.null(index)==F)
  {
    if(is.vector(index, mode = "numeric"))
    {
      selection <- the_list[index]
      result <- lapply(selection,function(x)
      {
        data <- x[,c(7,4)]
        return(create_sites_species_m(data))
      })
      return(result)
    } 
    else { stop("sorry index is not a nummeric vector") }
  } 
  else
  {
    result <- lapply(the_list,function(x)
    {
      data <- x[,c(7,4)]
      return(create_sites_species_m(data))
    })
    return(result)
  }
}

test <- give_sites_species_matrix_list(sim.neutral,c(1))
test2 <- give_sites_species_matrix_list(sim.niche,c(1))

install.packages("")
