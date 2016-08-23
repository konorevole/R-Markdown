
#Function for setting the variables to factors and ordering them in desired order
#Input is the data frame in question; output is same data frame with variables set as factors
set.factors <- function(x){
  x$Season <- factor(x$Season, levels = c("Summer", "Fall", "Winter", "Spring", "Summer15"))
  x$Site <- factor(x$Site, levels = c("IMS", "Carrot", "NOAA", "Army"))
  x$CoreName <- factor(x$CoreName, levels = c("LO", "HO", "LM", "MM", "HM", "Ref", "LoRef", "HiRef"))
  return(x)
  }

#Function to add a column for habitat.
#Input is the data frame, output is the same data frame with an additional column for habitat.
#Need to set the factors explicitly in script
add.habitat <- function(x){
  mutate(x, Habitat = ifelse(CoreName == "LO" | CoreName == "HO", "oyster",
                                  ifelse(CoreName == "LM" | CoreName == "MM" | CoreName == "HM", "marsh",
                                         as.character(CoreName))))
  }

#Function to add a column for restored age
#Input is the data frame, output is the same data frame with an additional column for age
#Need to set the factors explicitly in script
add.age <- function(x){
   mutate(x, Age = ifelse(Site == "IMS" , "0",
                         ifelse(Site == "Carrot", "2",
                                 ifelse(Site == "NOAA", "7", "20"))))
}

#Function to calculate the parameters needed when representing summarized data
#Input is data frame, output is the same data frame with additional columns:
#Sorting by the core type (ie LO), calculates average N2 flux, # of cores, stdev, and std error
#Remember to create a new data frame when using this function to avoid permanently changing original
full.summary <- function(x){
  ddply(x, c("Site", "Season", "CoreName"), summarise,
        avg_N2Flux = mean(N2Flux),
        N = length(N2Flux),
        stdev = sd(N2Flux),
        se = stdev / sqrt(N))
}
