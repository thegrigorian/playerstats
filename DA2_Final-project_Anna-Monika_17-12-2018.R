## CLEAR MEMORY
rm(list=ls())

######################################## SET UP ###########################################
###########################################################################################

## Load required packages
library(haven)
library(lspline)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(plyr)
library(latticeExtra)
library(countrycode)
library(sjmisc)
library(stargazer)
library(arm)
library(readr)
library(dplyr)
library(lmtest)
library(sandwich)
library(segmented)
library(data.table)
library(dplyr)
library(tidyr)

## Set working directory
dir <-  "/Users/user/Desktop/DA2/"

## Location folders
data_in <- paste0(dir,"cases_studies_public/final-project/")
data_out <- paste0(dir,"textbook_work/final-project/")
func <- paste0(dir, "textbook_work/ch00_tech_prep/")
output <- paste0(dir,"textbook_work/final-project/output/")

source(paste0(func, "theme_bg.R"))
source(paste0(func, "da_helper_functions.R")) 


#################################### DATA PREPARATION #####################################
###########################################################################################

## Load data, then join datasets. My year of choice for this excercise is 2014 - 2015 season.
data <- read.csv(paste0(data_in,"players_stats.csv"), stringsAsFactors = T)
str(data)
data<-data[!duplicated(data),]
nrow(data)
data$lnHeight <- log(data$Height)
data$lnEFF <- log(data$EFF)

## Removing all the missing values
data<- subset(data, !is.na(data$EFF) & !is.na(data$Height) & 
                !is.na(data$PTS) & !is.na(data$BMI) &!is.na(data$Team) & !is.na(data$lnEFF) & !is.na(data$lnHeight))
nrow(data)

#################################### DATA EXPLORATION #####################################
###########################################################################################

# BMI density plot
ggplot(data, aes(x=BMI)) + 
  geom_histogram(binwidth = 1, center=12.25, closed="left", fill = "skyblue2", color = "black", alpha = 0.8) +
  geom_density(fill='skyblue2')
  ggsave(paste0(output, "BMIdensity.png"), width=12, height=7.5)

# Height density plot
ggplot(data, aes(x=Height)) + 
  geom_histogram(binwidth = 2, center=12.25, closed="left", fill = "skyblue2", color = "black", alpha = 0.8) +
  geom_density(fill='skyblue2')
  ggsave(paste0(output, "Heightdensity.png"), width=12, height=7.5)

# EFF density plot
ggplot(data, aes(x=EFF)) + 
  geom_histogram(binwidth = 100, center=12.25, closed="left", fill = "skyblue2", color = "black", alpha = 0.8) 
  ggsave(paste0(output, "EFFdensity.png"), width=12, height=7.5)

# Age desnity plot
ggplot(data, aes(x=Age)) + 
  geom_histogram(binwidth = 2, center=12.25, closed="left", fill = "skyblue2", color = "black", alpha = 0.8) 
  ggsave(paste0(output, "Agedensity.png"), width=12, height=7.5)

# Bar chart for position
ggplot(data, aes(x=Pos)) + 
  geom_bar(fill = "skyblue2", color = "black") 
  ggsave(paste0(output, "Posdensity.png"), width=12, height=7.5)

# Filtering for 22<BMI<28
data <- data[data$BMI>22,]
data <- data[data$BMI<28,]
nrow(data)


#################################### DATA ANALYTICS #####################################
#########################################################################################




######################################### LOESS ######################################### 

# ESTIMATE A LOESS REGRESSION OF LIFE EXPECTANCY ON LN GDP PER CAPITA
reg1 <- loess(lnEFF ~ Height, data, control = loess.control(surface = "direct"))
summary(reg1)
data$pred_loess <- predict(reg1, data)

ggplot(data = data, aes(x = Height, y = EFF)) +
  geom_point(size = 3, fill = "blue", color = "darkorange2", shape = 1, stroke = 2) +
  geom_smooth(method="loess", colour="black", se=F, size = 1.5)+
  labs(x = "Height in cm",y = "Efficiency score")+
  theme_classic() +
  background_grid(major = "y", minor = "y", size.major = 0.2) +
  ggtitle("Graph 1: Loess Regression of Efficiency score on height in cms")
ggsave(paste0(output, "plot_loess.png"), width=12, height=7.5)



####################################### QUADRATIC ####################################### 

# ESTIMATE A LINEAR REGRESSION OF LIFE EXPECTANCY WITH POLYNOMIAL
data$Heightsq<-data$Height^2


reg2 <- lm(EFF ~ Height + Heightsq , data=data)
summary(reg2)


data$pred_quad<-predict(lm(EFF ~ Height + Heightsq , data=data))
z<-predict(reg2,data,se.fit=TRUE)
data$pred_quadSE<- z[[2]]
data$pred_quadCIUP<-data$pred_quad + 2*data$pred_quadSE
data$pred_quadCILO<-data$pred_quad - 2*data$pred_quadSE

ggplot(data = data, aes(x = Height, y = EFF)) +
  geom_smooth(method="loess", colour="skyblue2", se=F, size = 3)+
  geom_point(size = 3, fill = "darkorange2", color = "darkorange2", shape = 1, stroke = 1) +
  geom_line(data = data, aes(x = Height, y = pred_quad), colour = "black", size = 1.5)+
  geom_line(data=data,aes(x = Height, y = pred_quadCIUP), linetype = 2,size=1)+
  geom_line(data=data,aes(x = Height, y = pred_quadCILO), linetype = 2,size=1)+
  labs(x = "Height in cm",y = "Efficiency rating (NBA)") +
  scale_color_manual(name = "", values=c("black"), 
                     labels = c( "Quadric polynomial regression")) +
  background_grid(major = "y", minor = "y", size.major = 0.2) +
  theme(legend.position="bottom",
        legend.text = element_text(size=16),
        axis.text = element_text(size = 16, color = "black")) +
  ggtitle("Graph 2: Linear Regression of efficiency on height  with quadratic polynomial")
ggsave(paste0(output, "plot_quad.png"), width=12, height=7.5)



# EXTENDED MODEL BY ADDING AGE DUMMY VARIABLE
data$young<- data$Age<=25
reg3 <- lm(EFF ~ young + Height + Heightsq , data=data)
summary(reg3)


reg1 <- lm(EFF ~ Height, data=data)

stargazer_r(list(reg1, reg2, reg3), 
            se = 'robust',  digits=3, 
            out=paste(output,"results1.html",sep=""))



# ANOVA TEST FOR DIFFERENCE IN AVERAGE HEIGHT IN DIFFERENT POSITIONS 
position <- data %>%
  group_by(Pos) %>%
  summarize(mean_height = mean(Height, na.rm = TRUE), total_points = sum(PTS, na.rm = TRUE), mean_EFF=mean(EFF, na.rm=TRUE))

ggplot(data = position, aes(x = mean_height, y = mean_EFF, label=Pos)) +
  geom_point(size=3)+
  geom_text(size=6)

ggtitle("Graph3: Efficiency on height with by positions")
ggsave(paste0(output, "positioneff.png"), width=12, height=7.5)

res.aov <- aov(Height ~ Pos, data = data)
summary(res.aov)



# EXTENDED MODEL WITH DUMMY VARIABLES FOR POSITIONS

#generating dummy variables
data$c<- data$Pos=='C'
data$pf<- data$Pos=='PF'
data$pg<- data$Pos=='PG'
data$sf<- data$Pos=='SF'

reg4 <- lm(EFF ~ young + Height + Heightsq + c + pf + pg + sf, data=data)
reg5 <- lm(EFF ~ c + pf + pg + sf, data=data)
reg6 <- lm(Height ~ c + pf + pg + sf, data=data)

stargazer_r(list(reg4, reg5, reg6), 
            se = 'robust',  digits=3, 
            out=paste(output,"results2.html",sep=""))


# CONTROL FOR POSITION

reg7 <- lm(EFF ~ Height, data=data[data$Pos=='C',])
reg8 <- lm(EFF ~ Height, data=data[data$Pos=='PF',])
reg9 <- lm(EFF ~ Height, data=data[data$Pos=='PG',])
reg10 <- lm(EFF ~ Height, data=data[data$Pos=='SF',])
reg11 <- lm(EFF ~ Height, data=data[data$Pos=='SG',])


stargazer_r(list(reg7, reg8, reg9, reg10, reg11), 
            se = 'robust',  digits=3, 
            out=paste(output,"results3.html",sep=""))
