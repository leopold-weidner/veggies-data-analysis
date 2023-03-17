# -*- coding: utf-8 -*-
#"""
#Created on Mon Feb 20 15:34:17 2023
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
library(ComplexHeatmap)

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


#--Mix I Breakfast Buffet ----
mixIa <- mixEx[,c("ID", "MixISemml")]
mixIb <- mixEx[,c("ID", "MixIPaC")]
mixIges <- mixEx[,c("ID", "MixIges")]

mixIa <- mixIa[complete.cases(mixIa),]
mixIb <- mixIb[complete.cases(mixIb),]
mixIges <- mixIges[complete.cases(mixIges),]

mixI <- list(Semml = paste0("ID", mixIa$ID), 
             PaC = paste0("ID", mixIb$ID), 
             ges = paste0("ID", mixIges$ID))

m2 <- make_comb_mat(mixI)
m2
set_size(m2)
comb_size(m2)
(u1 <- UpSet(m2, comb_order = rev(order(comb_size(m2)))))
view(as.data.frame(comb_size(m2)))

##plotting the "new features"

featSemml <- mixEx[, c("ID", "MixISemml")]
featSemml <- featSemml[complete.cases(featSemml), ]
featPac <- mixEx[, c("ID", "MixIPaC")]
featPac <- featPac[complete.cases(featPac), ]
featMixI <- mixEx[mixEx$ID %notin% featSemml$ID & mixEx$ID %notin% featPac$ID, ]

(p4a = ggplot(featMixI, aes(x = OtoC_rate, y = HtoC_rate, fill = chem_space, size = MixIges)) +
    geom_point(shape = 21, alpha = 0.5, color = "black") +
    scale_fill_manual(values = ColorPalette) +
    scale_size_continuous(range = c(1, 7)) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(from = 0, to = 1.5, by = 0.2)) +
    scale_y_continuous(limits = c(0.5, 2.25), breaks = seq(from = 0, to = 2.5, by = 0.5)) +
    labs(title = "Mix Experiments I: Breakfast Buffet - features unique in mix",
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

(p4b = ggplot(featMixI, aes(x = mean, y = HtoC_rate, fill = chem_space, size = MixIges)) +
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

(mixIVK <- ggpubr::ggarrange(p4a, p4b, ncol = 2, nrow = 1))



#--Mix II Surf and Turf ----
mixIIa <- mixEx[,c("ID", "MixIIChick")]
mixIIb <- mixEx[,c("ID", "MixIISal")]
mixIIges <- mixEx[,c("ID", "MixIIges")]

mixIIa <- mixIIa[complete.cases(mixIIa),]
mixIIb <- mixIIb[complete.cases(mixIIb),]
mixIIges <- mixIIges[complete.cases(mixIIges),]

mixII <- list(Chicken = paste0("ID", mixIIa$ID), 
              Salmon = paste0("ID", mixIIb$ID), 
              ges = paste0("ID", mixIIges$ID))

m2 <- make_comb_mat(mixII)
m2
set_size(m2)
comb_size(m2)
(u2 <- UpSet(m2, comb_order = rev(order(comb_size(m2)))))
view(as.data.frame(comb_size(m2)))

##plotting the "new features"
mixIIunique <- mixIIges[mixIIges$ID %notin% mixIIa$ID,]
mixIIunique <- mixIIunique[mixIIunique$ID %notin% mixIIb$ID, ]
mixIIunique <- mixEx[mixEx$ID %in% mixIIunique$ID, ]
mixIIunique <- mixIIunique[mixIIunique$chem_space != "CHOS",]

(p4a = ggplot(mixIIunique, aes(x = OtoC_rate, y = HtoC_rate, fill = chem_space, size = MixIIges)) +
    geom_point(shape = 21, alpha = 0.5, color = "black") +
    scale_fill_manual(values = ColorPalette) +
    scale_size_continuous(range = c(1, 7)) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(from = 0, to = 1.5, by = 0.2)) +
    scale_y_continuous(limits = c(0.5, 2.25), breaks = seq(from = 0, to = 2.5, by = 0.5)) +
    labs(title = "Mix Experiments II: Surf and Turf - features unique in mix",
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

(p4b = ggplot(mixIIunique, aes(x = mean, y = HtoC_rate, fill = chem_space, size = MixIIges)) +
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

(mixIIVK <- ggpubr::ggarrange(p4a, p4b, ncol = 2, nrow = 1))

##1620x335
ggpubr::ggarrange(mixIVK, mixIIVK, ncol = 2)
