# -*- coding: utf-8 -*-
#"""
#Created on Wed Feb 22 12:57:17 2023
#
#@author: weidner.leopold
#"""

##1280 x 400 px for VK
##862 x 540 px for pies

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
library(ggforce)

"%notin%" <- base::Negate("%in%")
"% %" <- function(a, b) paste(a, b)
"%+%" <- function(a, b) paste0(a, b)
"%||%" <- function(lhs, rhs) {if (!is.null(lhs)) {lhs} else {rhs}}

ColorPalette <- c("CHO" = "darkblue", "CHNO" = "darkorange1", "CHN" = "cyan", 
                  "CHNOS" = "red3", "CHOS" = "forestgreen", "CH" = "grey90")

wd <- dirname(rstudioapi::getSourceEditorContext()$path) %>%
  setwd()

##--------------loading data--------------
load(file = "2023-02-20-veggies.RData")
load(file = "/Users/lw/Nextcloud/Atlas-PYR/matrices/2022-09-06-fisch.RData")

##--------------having a look at fish with Panade--------------
panfish <- vdata[,c("mol_formula", "AlaskPanI", "AlaskPanI",
                    "FStabI", "FStabII")]
panfish$nacount <- apply(panfish, 1, \(x) sum(is.na(x)))
panfish <- panfish[panfish$nacount <= 1, ]
panfish$nacount <- NULL

panfish <- vdata[vdata$mol_formula %in% panfish$mol_formula, ]
panfish <- panfish[panfish$mol_formula %notin% fdata$mol_formula, ]

(p1a = ggplot(panfish, aes(x = OtoC_rate, y = HtoC_rate, fill = chem_space, size = intensity_mean)) +
    geom_point(shape = 21, alpha = 0.5, color = "black") +
    scale_fill_manual(values = ColorPalette) +
    scale_size_continuous(range = c(1, 7)) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(from = 0, to = 1.5, by = 0.2)) +
    scale_y_continuous(limits = c(0.5, 2.25), breaks = seq(from = 0, to = 2.5, by = 0.5)) +
    labs(title = "new features on fish with panade",
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

(p1b = ggplot(panfish, aes(x = mean, y = HtoC_rate, fill = chem_space, size = intensity_mean)) +
    geom_point(shape = 21, alpha = 0.5, color = "black") +
    scale_fill_manual(values = ColorPalette) +
    scale_size_continuous(range = c(1, 7)) +
    scale_x_continuous(limits = c(120, 620), breaks = seq(from = 100, to = 600, by = 100)) +
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

(PanadClustVK <- ggpubr::ggarrange(p1a, p1b, ncol = 2, nrow = 1))

##--------------PanEx I: chciken nuggets--------------
nuggPan <- panEx[,c("ID", "NoChickI", "NoChickII")]  ##getting features in panade run
nuggPan <- nuggPan[complete.cases(nuggPan), ]

nuggNoPan <- panEx[,c("ID", "NoChickNoPan")]         ##getting features in NO panade run
nuggNoPan <- nuggNoPan[complete.cases(nuggNoPan), ]

ggvenn::ggvenn(list("NuggetsPanade" = nuggPan$ID, 
                    "NuggetsNoPanade" = nuggNoPan$ID), 
               c("NuggetsPanade", "NuggetsNoPanade"))


##  ---   A only in NO! Panada    ----
nuggNoPanFeat <- nuggNoPan[nuggNoPan$ID %notin% nuggPan$ID, ]   ##features
nuggNOPanFeat <- panEx[panEx$ID %in% nuggNOPanFeat$ID, ]

(p1a = ggplot(nuggNOPanFeat, aes(x = OtoC_rate, y = HtoC_rate, fill = chem_space, size = NoChickNoPan)) +
    geom_point(shape = 21, alpha = 0.5, color = "black") +
    scale_fill_manual(values = ColorPalette) +
    scale_size_continuous(range = c(1, 7)) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(from = 0, to = 1.5, by = 0.2)) +
    scale_y_continuous(limits = c(0.5, 2.25), breaks = seq(from = 0, to = 2.5, by = 0.5)) +
    labs(title = "in NO Panade, n = 435",
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

(p1b = ggplot(nuggNOPanFeat, aes(x = mean, y = HtoC_rate, fill = chem_space, size = NoChickNoPan)) +
    geom_point(shape = 21, alpha = 0.5, color = "black") +
    scale_fill_manual(values = ColorPalette) +
    scale_size_continuous(range = c(1, 7)) +
    scale_x_continuous(limits = c(120, 620), breaks = seq(from = 100, to = 600, by = 100)) +
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

(nochicknopanadd <- ggpubr::ggarrange(p1a, p1b, ncol = 2, nrow = 1))

##  ---   ABonly in WITH! Panada    ----
nuggPanFeat <- nuggPan[nuggPan$ID %notin% nuggNoPan$ID, ]   ##features
nuggPanFeat <- panEx[panEx$ID %in% nuggPanFeat$ID, ]

(p1a = ggplot(nuggPanFeat, aes(x = OtoC_rate, y = HtoC_rate, fill = chem_space, size = NoChickI)) +
    geom_point(shape = 21, alpha = 0.5, color = "black") +
    scale_fill_manual(values = ColorPalette) +
    scale_size_continuous(range = c(1, 7)) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(from = 0, to = 1.5, by = 0.2)) +
    scale_y_continuous(limits = c(0.5, 2.25), breaks = seq(from = 0, to = 2.5, by = 0.5)) +
    labs(title = "in WITH Panade, n = 243",
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

(p1b = ggplot(nuggPanFeat, aes(x = mean, y = HtoC_rate, fill = chem_space, size = NoChickI)) +
    geom_point(shape = 21, alpha = 0.5, color = "black") +
    scale_fill_manual(values = ColorPalette) +
    scale_size_continuous(range = c(1, 7)) +
    scale_x_continuous(limits = c(120, 620), breaks = seq(from = 100, to = 600, by = 100)) +
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

(nochickpanadd <- ggpubr::ggarrange(p1a, p1b, ncol = 2, nrow = 1))

##intensity ratio analysis

ratioNuggets <- panEx[,-c(13:15)]
ratioNuggets <- ratioNuggets[complete.cases(ratioNuggets), ]
ratioNuggets$NoChick <- (ratioNuggets$NoChickI + ratioNuggets$NoChickII) / 2
ratioNuggets$ratio <- ratioNuggets$NoChickNoPan / ratioNuggets$NoChick

ggplot(ratioNuggets, aes(x = log2(NoChick), y = log2(NoChickNoPan),
                         fill = chem_space, size = ratio)) +
    geom_point(shape = 21, alpha = 0.5, color = "black") +
    geom_abline(slope = 1, intercept = 0) +
    scale_fill_manual(values = ColorPalette) +
    scale_size_continuous(range = c(1, 7)) +
    labs(title = "ratio analysis",
         fill = "chemical space",
         size = "intensity ratio (keine panade) / (panade)") +
    theme_bw() +                                                                        
    theme(panel.grid = element_blank(),                                                      
          axis.title = element_text(size = 14, colour = "black"),                              
          axis.text = element_text(size = 11, colour = "black"),                                 
          axis.ticks = element_line(colour = "black"),                                            
          legend.text = element_text(color = "black", size = 11),                                 
          legend.title = element_text(size = 14),
          legend.position = "bottom") +
    guides(fill = guide_legend(order = 1, override.aes = list(size = 4)),
           size = guide_legend(order = 2)) 


##--------------PanEx II: veggie knusper--------------
backPan <- panEx[,c("ID", "VegKnuspI", "VegKnuspII")]  ##getting features in panade run
backPan <- backPan[complete.cases(backPan), ]

backNoPan <- panEx[,c("ID", "VegKnuspNoPan")]         ##getting features in NO panade run
backNoPan <- backNoPan[complete.cases(backNoPan), ]

ggvenn::ggvenn(list("FischPanade" = backPan$ID, 
                    "FischNoPanade" = backNoPan$ID), 
               c("FischPanade", "FischNoPanade"))


##  ---   A only in NO! Panada    ----
backNoPanFeat <- backNoPan[backNoPan$ID %notin% backPan$ID, ]   ##features
backNoPanFeat <- panEx[panEx$ID %in% backNoPanFeat$ID, ]

(p1a = ggplot(backNoPanFeat, aes(x = OtoC_rate, y = HtoC_rate, fill = chem_space, size = VegKnuspNoPan)) +
    geom_point(shape = 21, alpha = 0.5, color = "black") +
    scale_fill_manual(values = ColorPalette) +
    scale_size_continuous(range = c(1, 7)) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(from = 0, to = 1.5, by = 0.2)) +
    scale_y_continuous(limits = c(0.5, 2.25), breaks = seq(from = 0, to = 2.5, by = 0.5)) +
    labs(title = "in NO Panade, n = 520",
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

(p1b = ggplot(backNoPanFeat, aes(x = mean, y = HtoC_rate, fill = chem_space, size = VegKnuspNoPan)) +
    geom_point(shape = 21, alpha = 0.5, color = "black") +
    scale_fill_manual(values = ColorPalette) +
    scale_size_continuous(range = c(1, 7)) +
    scale_x_continuous(limits = c(120, 620), breaks = seq(from = 100, to = 600, by = 100)) +
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

(backnopanVK <- ggpubr::ggarrange(p1a, p1b, ncol = 2, nrow = 1))

##  ---   ABonly in WITH! Panada    ----
backPanFeat <- backPan[backPan$ID %notin% backNoPan$ID, ]   ##features
backPanFeat <- panEx[panEx$ID %in% backPanFeat$ID, ]

(p1a = ggplot(backPanFeat, aes(x = OtoC_rate, y = HtoC_rate, fill = chem_space, size = VegKnuspI)) +
    geom_point(shape = 21, alpha = 0.5, color = "black") +
    scale_fill_manual(values = ColorPalette) +
    scale_size_continuous(range = c(1, 7)) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(from = 0, to = 1.5, by = 0.2)) +
    scale_y_continuous(limits = c(0.5, 2.25), breaks = seq(from = 0, to = 2.5, by = 0.5)) +
    labs(title = "in WITH Panade, n = 627",
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

(p1b = ggplot(backPanFeat, aes(x = mean, y = HtoC_rate, fill = chem_space, size = VegKnuspI)) +
    geom_point(shape = 21, alpha = 0.5, color = "black") +
    scale_fill_manual(values = ColorPalette) +
    scale_size_continuous(range = c(1, 7)) +
    scale_x_continuous(limits = c(120, 620), breaks = seq(from = 100, to = 600, by = 100)) +
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

(backpanfeat <- ggpubr::ggarrange(p1a, p1b, ncol = 2, nrow = 1))

##intensity ratio analysis

ratioFisch <- panEx[,-c(16:18)]
ratioFisch <- ratioFisch[complete.cases(ratioFisch), ]
ratioFisch$VegKnusp <- (ratioFisch$VegKnuspI + ratioFisch$VegKnuspII) / 2
ratioFisch$ratio <- ratioFisch$VegKnuspNoPan / ratioFisch$VegKnusp

ggplot(ratioFisch, aes(x = log2(VegKnusp), y = log2(VegKnuspNoPan),
                       fill = chem_space, size = ratio)) +
  geom_point(shape = 21, alpha = 0.5, color = "black") +
  geom_abline(slope = 1, intercept = 0) +
  scale_fill_manual(values = ColorPalette) +
  scale_size_continuous(range = c(1, 7)) +
  labs(title = "ratio analysis",
       fill = "chemical space",
       size = "intensity ratio (keine panade) / (panade)") +
  theme_bw() +                                                                        
  theme(panel.grid = element_blank(),                                                      
        axis.title = element_text(size = 14, colour = "black"),                              
        axis.text = element_text(size = 11, colour = "black"),                                 
        axis.ticks = element_line(colour = "black"),                                            
        legend.text = element_text(color = "black", size = 11),                                 
        legend.title = element_text(size = 14),
        legend.position = "bottom") +
  guides(fill = guide_legend(order = 1, override.aes = list(size = 4)),
         size = guide_legend(order = 2)) 





