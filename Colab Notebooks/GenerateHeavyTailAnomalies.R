library(mvtnorm) # to generate heavy-tailed data
library(fitHeavyTail)
library(stringr)
library(dplyr)
library(tidyverse)

generateAnomalyFile<-function(cc_outliers, mn_scale, cov_scale, pathRoot) {
  myList<-fit_mvt(cc_outliers, initial=NULL, max_iter=100, ptol=0.001, ftol=Inf, return_iterates = FALSE, verbose=FALSE)
  cc_synthetic_heavytail_outliers<-rmvt(nrow(cc_outliers), sigma=myList$cov*cov_scale, df=3, delta=myList$mu*mn_scale)
  full_path<-paste(pathRoot, "cc_heavytail_mn_scale_",as.character(mn_scale),"_cov_scale_",as.character(cov_scale),"_outliers.csv", sep="" )
  print(full_path)
  write.csv(cc_synthetic_heavytail_outliers, full_path, row.names=FALSE)
  
}

creditcardfraud_original_outliers <- read_csv("C:/Users/Keith Willard/CSCI5523/Group_Project/dataset/creditcardfraud_outliers.csv")
#drop the class column before fitting mu, and cov
cc<-subset(creditcardfraud_original_outliers, select = -class)

path_root="C:/Users/Keith Willard/CSCI5523/Group_Project/dataset/R-produced-HeavyTail-Outlier-Files/"

mn_scales<-c(.5, 1, 1.1, 1.5)
cov_scales<-c(.5, 1, 1.1, 1.5)
#mn_scales<-c(1)
#cov_scales<-c(1)
for (mn_scale in mn_scales) {
  for (cov_scale in cov_scales) {
    generateAnomalyFile(cc, mn_scale, cov_scale, path_root) 
    
  }
}