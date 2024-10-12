library(readr)
library(ggplot2) 
library(dslabs)
library(dplyr)

ndvi1<- read_csv("NDVI_kiwi_guzzon_csv.csv")
ndvi2<- read_csv("NDVI_kiwi_le_ferriere_csv.csv")

ndvi1$Ndvi <- gsub(",", ".", ndvi1$Ndvi ) # so that R recognizes number in NDVI $ ndvi
ndvi1$Ndvi  <- as.numeric(ndvi1$Ndvi )
stat_ndvi1<- summary(ndvi1$Ndvi) # basic stats ndvi1

ndvi1 %>% ggplot(aes(x=Ndvi)) + 
  geom_histogram(binwidth=0.05) + 
  xlab("Values_ndvi") + 
  ggtitle("NDVI_histogram") # we need a small binwidth in order to see the values

ndvi2$Ndvi <- gsub(",", ".", ndvi2$Ndvi ) 
ndvi2$Ndvi  <- as.numeric(ndvi2$Ndvi )
stat_ndvi2<- summary(ndvi2$Ndvi) # basic stats ndvi2

ndvi2 %>% ggplot(aes(x=Ndvi)) + 
  geom_histogram(binwidth=0.05) + 
  xlab("Values_ndvi") + 
  ggtitle("NDVI_2_histogram")

Shapiro_ndvi1<- shapiro.test(ndvi1$Ndvi)
Shapiro_ndvi2<- shapiro.test(ndvi2$Ndvi)

ifelse(Shapiro_ndvi1$p.value< 0.05, print("not normal distribution"), print("normal distribution"))

ifelse(Shapiro_ndvi2$p.value< 0.05, print("not normal distribution"), print("normal distribution"))

