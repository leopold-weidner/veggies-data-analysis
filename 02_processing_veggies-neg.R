# -*- coding: utf-8 -*-
#"""
#Created on Mon Feb 20 12:10:17 2023
#
#@author: weidner.leopold
#"""

##--------------document setup--------------
rm(list = ls())
options(digits = 6)
options(warn = 1)
set.seed(212)
Sys.setenv(LANGUAGE="en")
memory.limit(size = 8e10)
options(warnPartialMatchDollar = TRUE)

library(tidyverse)
library(magrittr)
library(customLW)

"%notin%" <- base::Negate("%in%")
"% %" <- function(a, b) paste(a, b)
"%+%" <- function(a, b) paste0(a, b)
"%||%" <- function(lhs, rhs) {if (!is.null(lhs)) {lhs} else {rhs}}
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
cal_z_score <- function(x){(x - mean(x)) / sd(x)}

ColorPalette <- c("CHO" = "darkblue", "CHNO" = "darkorange1", "CHN" = "cyan", 
                  "CHNOS" = "red3", "CHOS" = "forestgreen", "CH" = "grey90")

wd <- dirname(rstudioapi::getSourceEditorContext()$path) %>%
  setwd()

##--------------loading data--------------
setwd(paste0(wd, "/netcalced"))

consensus <- netcalc_consensusIDs(fileheader = c("mean", "ID", "H", "C", "O", "N", "S", 
                                                 "ThMass.Neutral", "changes", "iter", "Error.ppm"))

ann <- read.table("annotation1.txt", header = FALSE)

colnames(ann) <- c("mean", "ID", "H", "C", "O", "N", "S",
                   "ThMass.Neutral", "changes", "iter", "Error.ppm")
setwd(wd)

mtrx <- openxlsx::read.xlsx("2023-02-20-Matrix-Veggies.xlsx")
ann$mean <- NULL
ftdata <- merge(ann, mtrx, by = "ID")
ftdata <- ftdata[ftdata$ID %in% consensus$ID,]
##--------------clean-up--------------
vdata <- ftdata

vdata %$% plot(mean, Error.ppm)

par(mfrow = c(2, 2))
hist(vdata$Error.ppm)
boxplot(vdata[ ,2:6])
plot(vdata$C, vdata$Error.ppm)
plot(vdata$H, vdata$Error.ppm)
par(mfrow = c(1, 1))

##---

vdata <- subset(vdata, Error.ppm >= -0.2 & Error.ppm <= 0.2)      ##adapt to dataset!
vdata <- subset(vdata, mean <= 262.9 | mean >= 263.1)             ##deleting electronical noise


par(mfrow = c(1, 2))
hist(vdata$changes)
hist(vdata$iter)
par(mfrow = c(1, 1))

vdata <- vdata[vdata$iter < 2, ]
vdata <- vdata[vdata$changes < 2, ]

vdata <- subset(vdata, C <= 66)                                           
vdata <- subset(vdata, H <= 126)                                          
vdata <- subset(vdata, O <= 27)                                             
vdata <- subset(vdata, N <= 6)                                            

vdata$HtoC_rate <- vdata$H / vdata$C                                           
vdata$OtoC_rate <- vdata$O / vdata$C
vdata$NtoC_rate <- vdata$N / vdata$C

par(mfrow = c(1, 2))
hist(vdata$HtoC_rate, xlim = c(-0.1, 3), breaks = 1E3)
vdata %$% plot(mean, HtoC_rate)
hist(vdata$OtoC_rate, xlim = c(-0.1, 2), breaks = 500)
vdata %$% plot(OtoC_rate, HtoC_rate)
hist(vdata$NtoC_rate, xlim = c(-0.1, 1), breaks = 100)
vdata %$% plot(NtoC_rate, HtoC_rate)
par(mfrow = c(1, 1))

vdata <- subset(vdata, HtoC_rate >= 0.1 & HtoC_rate <= 2.4)
vdata <- subset(vdata, OtoC_rate >= 0 & OtoC_rate <= 1.5)
vdata <- subset(vdata, NtoC_rate >= 0 & NtoC_rate <= 0.8)

##calculating mass defect, double bond equivalents and aromaticity index
vdata$mass_floored <- floor(vdata$ThMass.Neutral)                         
vdata$massdefect <- vdata$ThMass.Neutral - vdata$mass_floored      
vdata$DBE <- 1 + 0.5 * ((2 * vdata$C) - vdata$H + vdata$N)
vdata$DBE_C <- vdata$DBE / vdata$C
vdata$AI <- NA
for (i in 1:nrow(vdata)) {
  DBE_ai <- (1 + vdata[i,"C"] - vdata[i, "O"] - (0.5 * vdata[i, "H"]))
  C_ai <- (vdata[i, "C"] - vdata[i, "O"]  - vdata[i, "N"])
  
  if(DBE_ai > 0 && C_ai > 0) {vdata[i, "AI"] <-  DBE_ai / C_ai} else {vdata[i, "AI"] <- 0
  }
}


plot(vdata[,c("mean", "Error.ppm", "HtoC_rate", "OtoC_rate", "NtoC_rate", "DBE", "AI")])

par(mfrow = c(1, 2))
vdata %$% plot(mean, DBE)
vdata %$% plot(mean, AI)
par(mfrow = c(1, 1))

vdata <- subset(vdata, mean <= 800)

vdata <- subset(vdata, DBE <= 18 & DBE >= 0)

table(is.wholenumber(vdata$DBE))
vdata <- vdata[is.wholenumber(vdata$DBE), ]

vdata <- subset(vdata, AI <= 1)

table(duplicated(vdata$ID))
vdata <- vdata[!duplicated(vdata$ID),]

##--------------------Writing molecular formulas and assigning element spaces--------------------
vdata <- generate_mol_formula(vdata)
vdata <- generate_chem_space(vdata)

vdata$chem_space <- as.character(vdata$chem_space)

for(i in 1:nrow(vdata)) {
  if(vdata[i, "O"] == 0 && vdata[i, "N"] > 0) {
    vdata[i, "chem_space"] <- "CHN"
  }
  if(vdata[i, "O"] == 0 && vdata[i, "N"] == 0) {
    vdata[i, "chem_space"] <- "CH"
  }
}

vdata$chem_space <- factor(vdata$chem_space)

table(vdata$chem_space)

vdata <- vdata[vdata$chem_space != "CH", ]
vdata <- vdata[vdata$chem_space != "CHN", ]

##ex <- vdata
##ex <- ex[ex$ThMass.Neutral > 200 & ex$Error.ppm < -0.1,]
##vdata <- vdata[vdata$ID %notin% ex$ID,]

vdata$intensity_mean <- rowMeans(vdata[,14:68], na.rm = TRUE)

##--------------------diagnstic plots--------------------
plot(error_bySpace(vdata))

dp_errors <- ggplot(vdata, aes(x = mean, y = Error.ppm, color = chem_space, alpha = 0.95)) +
  geom_point() +
  scale_color_manual(values = ColorPalette) +
  theme_bw()
dp_DBE <- ggplot(vdata, aes(x = mean, y = DBE, color = chem_space, alpha = 0.95)) +
  geom_point() +
  scale_color_manual(values = ColorPalette) +
  theme_bw()
dp_AI <- ggplot(vdata, aes(x = mean, y = AI, color = chem_space, alpha = 0.95)) +
  geom_point() +
  scale_color_manual(values = ColorPalette) +
  theme_bw()
dp_MD <- ggplot(vdata, aes(x = mean, y = massdefect, color = chem_space, alpha = 0.95)) +
  geom_point() +
  scale_color_manual(values = ColorPalette) +
  theme_bw()
dp_IM <- ggplot(vdata, aes(x = chem_space, y = log2(intensity_mean), fill = chem_space)) + 
  geom_boxplot(aes(alpha = 0.9)) +
  geom_hline(yintercept = log2(2000000)) +
  scale_fill_manual(values = ColorPalette) + 
  theme_minimal()
dp_EP <- ggplot(vdata, aes(x = chem_space, y = Error.ppm, fill = chem_space)) + 
  geom_boxplot(aes(alpha = 0.9)) +
  scale_fill_manual(values = ColorPalette) + 
  theme_minimal()
dp_MZ <- ggplot(vdata, aes(x = chem_space, y = mean, fill = chem_space)) + 
  geom_boxplot(aes(alpha = 0.9)) +
  scale_fill_manual(values = ColorPalette) + 
  theme_minimal()
dp_DBE2 <- ggplot(vdata, aes(x = chem_space, y = DBE, fill = chem_space)) + 
  geom_boxplot(aes(alpha = 0.9)) +
  scale_fill_manual(values = ColorPalette) + 
  theme_minimal()

ggpubr::ggarrange(dp_errors, dp_MD, dp_DBE, dp_AI, ncol = 2, nrow = 2)
ggpubr::ggarrange(dp_IM, dp_EP, dp_DBE2, dp_MZ, ncol = 2, nrow = 2)



##------------extracting special experiments----------------------
#--no Panade--

panEx <- vdata[, c("ID", "mean", "ThMass.Neutral", "Error.ppm", "mol_formula", "chem_space", 
                   "C", "N", "O", "S", "HtoC_rate", "OtoC_rate",
                   "VegKnuspI", "VegKnuspII", "VegKnuspNoPan",
                   "NoChickI", "NoChickII", "NoChickNoPan")]

panEx$nacount <- apply(panEx, 1, \(x) sum(is.na(x))) 
panEx <- panEx[panEx$nacount != 6, ]
panEx <- panEx[panEx$chem_space != "CHNOS", ]


#--MIX experiments--

mixEx <- vdata[, c("ID", "mean", "ThMass.Neutral", "Error.ppm", "mol_formula", "chem_space", 
                   "C", "N", "O", "S", "HtoC_rate", "OtoC_rate",
                   "MixIPaC", "MixISemml", "MixIges",
                   "MixIIChick", "MixIISal", "MixIIges")]

mixEx$nacount <- apply(mixEx, 1, \(x) sum(is.na(x))) 
mixEx <- mixEx[mixEx$nacount != 6, ]
mixEx <- mixEx[mixEx$chem_space != "CHNOS", ]


##------------ftlist----------------------

samplecats <- openxlsx::read.xlsx("2023-02-20-SampleCats.xlsx")
samplecats <- samplecats[!duplicated(samplecats$cat), ]
samplecats$sample <- str_remove(samplecats$sample, "I")

##todo: delete CHNOS in others than Zwiebel/KOHL

iC <- c("ID", "mean", "ThMass.Neutral", "Error.ppm", "mol_formula", "chem_space", 
        "C", "N", "O", "S", "HtoC_rate", "OtoC_rate")

ftlist = list()


for (i in 1:nrow(samplecats)) {
  
    cols.temp = colnames(vdata)[str_detect(colnames(vdata), samplecats[i, "sample"])]
    ftlist[[(length(ftlist)+1)]] = data.frame(vdata[, c(iC, cols.temp)])
    names(ftlist)[length(ftlist)] = samplecats[i, "cat"]
  
}

ftlist <- lapply(ftlist, \(x) {
  x$na_count <- apply(x, 1, \(y) sum(is.na(y)))
  x <- subset(x, x$na_count == 0)
  x$intensity_mean <- rowMeans(x[ ,13:14], na.rm = TRUE)
  rownames(x) <- 1:nrow(x)
  return(x)
})


ftlist <- lapply(ftlist, \(x) x[complete.cases(x),])


save(vdata, ftlist, mixEx, panEx, file = "2023-02-20-veggies.RData")


##---------general VK---------
(p1a = ggplot(mixEx, aes(x = OtoC_rate, y = HtoC_rate, fill = chem_space, size = intensity_mean)) +
    geom_point(shape = 21, alpha = 0.5, color = "black") +
    scale_fill_manual(values = ColorPalette) +
    scale_size_continuous(range = c(1, 7)) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(from = 0, to = 1.5, by = 0.2)) +
    scale_y_continuous(limits = c(0.5, 2.25), breaks = seq(from = 0, to = 2.5, by = 0.5)) +
    labs(title = "CHO - space stratified by Ion source",
         fill = "chemical space",
         size = "average intensity (continuous)",
         x = "\nO/C",
         y = "H/C\n") +
    theme_bw() +                                                                        
    theme(panel.grid = element_blank(),                                                      
          axis.title = element_text(size = 14, colour = "black"),                              
          axis.text = element_text(size = 11, colour = "black"),                                 
          axis.ticks = element_line(colour = "black"),                                            
          legend.text = element_text(color = "black", size = 11),                                 
          legend.title = element_text(size = 14),
          legend.position = "bottom") +
    guides(fill = guide_legend(order = 1, override.aes = list(size = 4)),
           size = guide_legend(order = 2)) +
    annotate(geom = "text", x = 0, y = 0, label = "", color = 0, size = 11))

(p1b = ggplot(sal, aes(x = mean, y = HtoC_rate, fill = chem_space, size = intensity_mean)) +
    geom_point(shape = 21, alpha = 0.5, color = "black") +
    scale_fill_manual(values = ColorPalette) +
    scale_size_continuous(range = c(1, 7)) +
    scale_x_continuous(limits = c(120, 700), breaks = seq(from = 100, to = 700, by = 100)) +
    scale_y_continuous(limits = c(0.5, 2.25), breaks = seq(from = 0, to = 2.5, by = 0.5), position = "right") +
    labs(title = "",
         fill = "chemical space",
         size = "average intensity (continuous)",
         x = "\nm/z",
         y = "H/C\n") +
    theme_bw() +                                                                        
    theme(panel.grid = element_blank(),                                                      
          axis.title = element_text(size = 14, colour = "black"),                              
          axis.text = element_text(size = 11, colour = "black"),                                 
          axis.ticks = element_line(colour = "black"),                                            
          legend.text = element_text(color = "black", size = 11),                                 
          legend.title = element_text(size = 14),
          legend.position = "bottom") +
    guides(fill = guide_legend(order = 1, override.aes = list(size = 4)),
           size = guide_legend(order = 2)) +
    annotate(geom = "text", x = 0, y = 0, label = "", color = 0, size = 11))

ggpubr::ggarrange(p1a, p1b, ncol = 2, nrow = 1)
