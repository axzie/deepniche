#TODO: should we crop?

library("dplyr")
library("tidyr")

#creat sitesXspecies - matrix
#input: vector: species, plot_ids
#output: dataframe containing the matrix

create_sites_species_m <- function(species,plot_ids)
{
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
        species <- x[,7]
        plot_ids <- x[,4]
        create_sites_species_m(species,plot_ids)
      })
      return(result)
    } 
    else { stop("sorry index is not a nummeric vector") }
  } 
  else
  {
    result <- lapply(the_list,function(x)
    {
      species <- x[,7]
      plot_ids <- x[,4]
      create_sites_species_m(species,plot_ids)
    })
    return(result)
  }
}

test <- give_sites_species_matrix_list(sim.neutral,1:100)

install.packages("")
