rm(list = ls())
set.seed(212)
options(digits = 10)

library(tidyverse)

wd <- dirname(rstudioapi::getSourceEditorContext()$path) %>%
  setwd()

##reading the original mass lists
setwd(paste0(wd, "/files"))
rawfiles <- list.files(pattern = ".ascii")

raX <- list()
for(i in 1:length(rawfiles)) {
  raX[[i]] <- read.table(rawfiles[i], header = TRUE)
}

##reading the filtered isotopes 
setwd(paste0(wd, "/files/wiggles_bk-filtered/iso_filter-removed"))
isofiles <- list.files(pattern = ".ascii")

isoX <- list()
for(i in 1:length(isofiles)) {
  isoX[[i]] <- read.table(isofiles[i], header = TRUE)
}

##reading the completely filtered files 
setwd(paste0(wd, "/files/wiggles_bk-filtered/iso_filter-filtered/md_filter-filtered"))
finalfiles <- list.files(pattern = ".ascii")

finalX <- list()
for(i in 1:length(finalfiles)) {
  finalX[[i]] <- read.table(finalfiles[i], header = TRUE)
}



##getting information together
descS <- data.frame(nSignals = integer(), nFeatures = integer(),
                n13C = integer(), n34S = integer(), n37Cl = integer())

for(i in 1:length(raX)) {
  descS[i,"nSignals"] <- nrow(raX[[i]])
  descS[i,"nFeatures"] <- nrow(finalX[[i]])
  descS[i,"n13C"] <- nrow(subset(isoX[[i]], Isotope == "13C"))
  descS[i,"n34S"] <- nrow(subset(isoX[[i]], Isotope == "34S"))
  descS[i,"n37Cl"] <- nrow(subset(isoX[[i]], Isotope == "37Cl"))
}
rownames(descS) <- rawfiles

par(mfrow = c(1,2))
plot(descS$nFeatures, descS$nSignals)
plot(descS$nFeatures, descS$n13C)

par(mfrow = c(1,3))
plot(descS$n13C)
abline(h = median(descS$n13C))
plot(descS$n34S)
abline(h = median(descS$n34S))
plot(descS$n37Cl)
abline(h = median(descS$n37Cl))
par(mfrow = c(1,1))