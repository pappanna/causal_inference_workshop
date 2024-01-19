#############################################################################################
# Causal Inference Workshop 
# Anna Papp (ap3907@columbia.edu)
# Week 1 - Modeling assumptions, create data 
# last modified: 01/18/24
#############################################################################################

# Setup -------------------------------------------------------------------------------------

## clear variables in environment and script 
rm(list=ls(all=TRUE)); cat("\014") 

## load packages
if(!require(pacman)) install.packages('pacman') 
pacman::p_load(data.table, dplyr, tidyr, 
               raster, gstat, lattice, ncf)    

## directory 
if(Sys.info()["user"] == "annapapp") {
  setwd('/Users/annapapp/Library/CloudStorage/GoogleDrive-ap3907@columbia.edu/My Drive/PhD/02_teaching/04_causalinference/code/') # anna WD
} else {
  setwd('/[OTHER USER]') 
}

## set seed for reproducibility
set.seed(123)

# Create folders -----------------------------------------------------------------------------

# create directory for saving data and output
dir.create(file.path("week1/", "data"))

# Functions ----------------------------------------------------------------------------------

# function for creating spatially correlated error 
# from: https://rpubs.com/jguelat/autocorr
rmvn <- function(n, mu = 0, V = matrix(1)) {
  p <- length(mu)
  if (any(is.na(match(dim(V), p)))) 
    stop("Dimension problem!")
  D <- chol(V)
  t(matrix(rnorm(n * p), ncol = p) %*% D + rep(mu, rep(n, p)))
}

# Create fake data ---------------------------------------------------------------------------

# define constants: number of observations, counties, coefficient, phi for spatially correlated error
n <- 100
counties <- 150
beta <- 0.54 
gamma <- 2.1
phi <- 0.05
errorScale <- 25
sample <- 2000

# simulate spatially correlated error term 
grid <- expand.grid(1:n, 1:n) 

# set up distance matrix
distance <- as.matrix(dist(grid))

# generate random variable
e <- rmvn(1, rep(0, n*n), exp(-phi * distance))
rm(distance)
eR <- rnorm(n*n, mean = 0, sd = 2)

# create X 
X <- rnorm(n*n, mean = 0, sd = 2)

# covariate 
C <- rnorm(n*n, mean = 0.05, sd = 2)

# save to dataframe 
data <- data.frame(cbind(grid[, 1:2] - 0.5, e = e * errorScale + eR, X, C))

# change latitude and longitude 
data <- data %>% mutate(lat = 30 + Var1 / 5, 
                        lon = -100 + Var2 / 5)

# create y 
data <- data %>% mutate(y = beta * X + gamma * C + e)

# create counties 
kMeans <- kmeans(data[,1:2], centers = counties)
countyId <- kMeans$cluster
data <- data.frame(cbind(data, countyId))

# keep final dataframe
data <- data %>% dplyr::select(lat, lon, countyId, y, X, C, e)

# sample from final dataframe 
dataSample <- data[sample(nrow(data), sample), ]

# save data 
write.csv(dataSample, "week1/data/01a_data_sample.csv")
