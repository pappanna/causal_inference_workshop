#############################################################################################
# Causal Inference Workshop 
# Anna Papp (ap3907@columbia.edu)
# Week 1 - Modeling assumptions, data visualization and regressions
# last modified: 01/18/24
#############################################################################################

# Setup -------------------------------------------------------------------------------------

## clear variables in environment and script 
rm(list=ls(all=TRUE)); cat("\014") 

## load packages
if(!require(pacman)) install.packages('pacman') 
pacman::p_load(data.table, ggplot2, dplyr, tidyr, 
               lfe, conleyreg, estimatr,
               stargazer, dotwhisker, ggpubr,
               PNWColors, viridis)    

## directory 
if(Sys.info()["user"] == "annapapp") {
  setwd('/Users/annapapp/Library/CloudStorage/GoogleDrive-ap3907@columbia.edu/My Drive/PhD/02_teaching/04_causalinference/code/') # anna WD
} else {
  setwd('/[OTHER USER]') 
}

# Create folders -----------------------------------------------------------------------------

# create directory for saving data and output
dir.create(file.path("week1/", "output"))

# Load Data --------------------------------------------------------------------------------

dataSample <- read.csv("week1/data/01a_data_sample.csv")

# Visualize Data ----------------------------------------------------------------------------

pal <- pnw_palette("Shuksan2",100,type="continuous")

# visualize counties 
countyPlot <- ggplot(dataSample) + 
  geom_point(aes(x = lon, y = lat, color=factor(countyId))) + 
  scale_color_viridis_d()+
  theme_bw() + xlab(" ") + ylab(" ") + ggtitle("counties") + 
  theme(legend.position="none")

# visualize error
errorPlot <- ggplot(dataSample) + 
  geom_point(aes(x = lon, y = lat, color=e)) + 
  scale_color_gradientn(colours = pal) +
  theme_bw() + xlab(" ") + ylab(" ") + ggtitle("error") + 
  theme(legend.position="none")

# visualize x
xPlot <- ggplot(dataSample) + 
  geom_point(aes(x = lon, y = lat, color=X)) + 
  theme_bw() + xlab(" ") + ylab(" ") + ggtitle("X") + 
  scale_color_gradientn(colours = pal) +
  theme(legend.position="none")

# visualize y
yPlot <- ggplot(dataSample) + 
  geom_point(aes(x = lon, y = lat, color=y)) + 
  scale_color_gradientn(colours = pal) +
  theme_bw() + xlab(" ") + ylab(" ") + ggtitle("y") + 
  theme(legend.position="none")

ggarrange(countyPlot, errorPlot, xPlot, yPlot, ncol=2, nrow=2, common.legend = F) +  bgcolor("white") + border("white")
ggsave(file="week1/output/01b_plots.png", height = 10, width=10)

# Regressions -------------------------------------------------------------------------------

## Regressions ---- 

# simple linear regression without clustered standard errors
model1 <- felm(data = dataSample, y~X|0|0|0)
coefModel1 <- tidy(model1, conf.int=TRUE) %>% filter(term %in% c("X")) %>% mutate(model = "Linear Regression\n")

# same as: 
#model1b <- lm(data = dataSample, y~X)
#summary(model1b)

# linear regression with clustered standard errors
model2 <- felm(data = dataSample, y~X|0|0|countyId)
coefModel2 <- tidy(model2, conf.int=TRUE) %>% filter(term %in% c("X")) %>% mutate(model = "Linear Regression\nClustered SEs")

# or another way of calculating clustered standard errors in R 
#model2b <- lm_robust(y ~ X, clusters = countyId, data = dataSample)
#summary(model2b)

# linear regression with conley standard errors 
model3 <- conleyreg(y~X, dataSample, 100, lat="lat", lon="lon")
coefModel3 <- tidy(model3, conf.int=TRUE) %>% filter(term %in% c("X")) %>% mutate(model = "Linear Regression\nConley SEs")

## Table ---- 
stargazer(model1, model2, model3,
          type="text", df=F, report=("vc*sp"), model.names = FALSE,
          omit="Constant",  
          title = "Clustering Standard Errors", 
          covariate.labels = c("X"),
          dep.var.labels = c("y"))

## Coefficient Plot ---- 

# combine 
coef <- rbind(coefModel1, coefModel2, coefModel3)
rm(coefModel1, coefModel2, coefModel3)

# color palette 
pal <- pnw_palette("Starfish", 3 , type = "discrete")

# plot 
dwplot(coef, vars_order = c("X"), dot_args = list(size = 3),
       model_order = rev(c("Linear Regression\n", "Linear Regression\nClustered SEs", "Linear Regression\nConley SEs"))) + 
  theme_bw() + coord_flip() + xlab("beta") + ylab(" ") + 
  geom_vline(xintercept = 0,colour = "grey60",linetype = 2) + 
  scale_color_manual(name = "Model",values=pal, breaks=rev(c("Linear Regression\n", "Linear Regression\nClustered SEs", "Linear Regression\nConley SEs"))) +
  theme(legend.position = "bottom", 
        axis.text.x=element_text(size = 14), 
        axis.text.y=element_text(size = 16), 
        axis.title.x=element_text(size = 14), 
        axis.title.y=element_text(size = 16),
        legend.text=element_text(size=12), 
        legend.title=element_text(size=14))
ggsave(file="week1/output/01b_coefplot.png")




