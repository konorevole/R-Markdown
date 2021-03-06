
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

#Modified version of set.factors.basic that addresses the factor levels more commonly used in the thesis
#Habitat includes SANDFLAT not reference!!
#Note that this does not include core names, which were generally dropped for thesis
set.factors.habitat <- function(x){
  x$Season <- factor(x$Season, levels = c("Summer", "Fall", "Winter", "Spring"))
  x$Site <- factor(x$Site, levels = c("IMS", "Carrot", "NOAA", "Army"))
  x$Habitat <- factor(x$Habitat, levels = c("oyster", "marsh", "sandflat"))
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

#Function to add a column for restored age as a CHARACTER
#Input is the data frame, output is the same data frame with an additional column for age
#Need to set the factors explicitly in script
add.age <- function(x){
   mutate(x, Age = ifelse(Site == "IMS" , "0",
                         ifelse(Site == "Carrot", "2",
                                 ifelse(Site == "NOAA", "7", "20"))))
}

#Function to add a column for restored age as a NUMBER
#Notice slight but important difference from above!! This function is good for regression graphs
#or anything requiring age to be treated as a number rather than category.
#Input is the data frame, output is the same data frame with an additional column for age
#Need to set the factors explicitly in script, although the above function should probably be used
#if factors are the ultimate goal.
add.num.age <- function(x){
  mutate(x, NumAge = ifelse(Site == "IMS", 0,
                            ifelse(Site == "Carrot", 2,
                                   ifelse(Site == "NOAA", 7, 20))))
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


#Function to add restored site age descriptors for the x axis based on the site name. Includes the
#intermediate value "changed" to set the factor levels within the function.

#I think this is the #correct way to do this! I had not been able to figure this out when writing the 
#above functions,#so at some point it may be prudent to adjust those if it won't mess up the other 
#code too much.
age.axis <- function(x){
  changed <- mutate(x, Years = ifelse(Site == "IMS" , "0 years",
                           ifelse(Site == "Carrot", "2 years",
                                  ifelse(Site == "NOAA", "7 years", "20 years"))))
  changed$Years <- factor(changed$Years, levels = c("0 years", "2 years", "7 years", "20 years"))
  return(changed)
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


#Function found on Stack Overflow (http://stackoverflow.com/questions/34533472/insert-blanks-into-a-vector-for-e-g-minor-tick-labels-in-r)
#to add minor tick marks. Not entirely sure how it works, but you generally want inverse = T. Input
#is the regular breaks (which should be defined beforehand as custom_breaks <- seq(x, x, x)), the
#inverse of the tick division (ie 2 for tick marks halfway between the breaks), and inverse = T.
#ex labels = every_nth(custom_breaks, 2, inverse = T)
#Note that custom_breaks will need to be set as if creating breaks for MINOR ticks, not major ones

every_nth <- function(x, nth, empty = TRUE, inverse = FALSE) 
{
  if (!inverse) {
    if(empty) {
      x[1:nth == 1] <- ""
      x
    } else {
      x[1:nth != 1]
    }
  } else {
    if(empty) {
      x[1:nth != 1] <- ""
      x
    } else {
      x[1:nth == 1]
    }
  }
}



