
#Function for setting the variables to factors and ordering them in desired order
#Input is the data frame in question; output is same data frame with variables set as factors
set.factors <- function(x){
  x$Season <- factor(x$Season, levels = c("Summer", "Fall", "Winter", "Spring", "Summer15"))
  x$Site <- factor(x$Site, levels = c("IMS", "Carrot", "NOAA", "Army"))
  x$CoreName <- factor(x$CoreName, levels = c("LO", "HO", "LM", "MM", "HM", "Ref", "LoRef", "HiRef"))
  return(x)
}

#Modified version of the above function to reflect only the basic levels of interest
#ie eliminating Summer 2015 and Lo/Hi Ref cores
set.factors.basic <- function(x){
  x$Season <- factor(x$Season, levels = c("Summer", "Fall", "Winter", "Spring"))
  x$Site <- factor(x$Site, levels = c("IMS", "Carrot", "NOAA", "Army"))
  x$CoreName <- factor(x$CoreName, levels = c("LO", "HO", "LM", "MM", "HM", "Ref"))
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

#Function to add a column for habitat with Sandflat instead of Ref/LoRef/HiRef
#Note that this works best if Lo/Hi Ref eliminated before running, b/c add.habitat function
#will copy over the Lo/Hi Ref label
#Input is the data frame, output is the same data frame with an additional column for habitat + sandflat
#Need to set the factors explicitly in script after
add.sandflat <- function(x){
  mutate(x, Habitat = ifelse(CoreName == "LO" | CoreName == "HO", "oyster",
                             ifelse(CoreName == "LM" | CoreName == "MM" | CoreName == "HM", "marsh",
                                    "sandflat")))
}

#Function to add a column for restored age
#Input is the data frame, output is the same data frame with an additional column for age
#Need to set the factors explicitly in script
add.age <- function(x){
   mutate(x, Age = ifelse(Site == "IMS" , "0",
                         ifelse(Site == "Carrot", "2",
                                 ifelse(Site == "NOAA", "7", "20"))))
}

#Function to add a column with #s for Season instead of words
#Will probably be most useful for regression tree analysis
#Input is the data frame, output is the same data frame with an additional column for season as #s
season.numbers <- function(x){
  mutate(x, SeasonNumber = ifelse(Season == "Summer" , 1,
                             ifelse(Season == "Fall", 2,
                                ifelse(Season == "Winter", 3, 4))))
}

#Function to add a column with #s for Habitat instead of words
#Will probably be most useful for regression tree analysis
#Input is the data frame, output is the same data frame with an additional column for habitat as #s
habitat.numbers <- function(x){
  mutate(x, HabitatNumber = ifelse(Habitat == "oyster" , 1,
                                  ifelse(Habitat == "marsh", 2, 3)))
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

#Function written by someone online to add r (coefficient of correlation) and p values to scatterplot
#matrix. I edited it to instead show R^2 (coefficient of correlation) by simply squaring r.
#Used to specify the upper panel when calling the pairs function (ie upper.panel = panel.cor).
#Do not need to specify inputs when using as part of pairs command.
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  R2 <- r^2
  txt <- format(c(R2, 0.123456789), digits = digits)[1]
  txt <- paste("R2= ", txt, sep = "")
  text(0.5, 0.6, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.4, txt2)
}



