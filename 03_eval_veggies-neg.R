# -*- coding: utf-8 -*-
#"""
#Created on Mon Feb 20 12:09:17 2023
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
featurenumbers <- data.frame("cat" = names(ftlist), "nfeat" = unlist(lapply(ftlist, nrow)))

##----------introduction: Kohl and Karotte-------------------
(p1a = ggplot(ftlist$Karotte, aes(x = OtoC_rate, y = HtoC_rate, fill = chem_space, size = intensity_mean)) +
   geom_point(shape = 21, alpha = 0.5, color = "black") +
   scale_fill_manual(values = ColorPalette) +
   scale_size_continuous(range = c(1, 7)) +
   scale_x_continuous(limits = c(0, 1), breaks = seq(from = 0, to = 1.5, by = 0.2)) +
   scale_y_continuous(limits = c(0.5, 2.25), breaks = seq(from = 0, to = 2.5, by = 0.5)) +
   labs(title = "Karotte",
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

(p1b = ggplot(ftlist$Karotte, aes(x = mean, y = HtoC_rate, fill = chem_space, size = intensity_mean)) +
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

(p2a = ggplot(ftlist$Kohl, aes(x = OtoC_rate, y = HtoC_rate, fill = chem_space, size = intensity_mean)) +
    geom_point(shape = 21, alpha = 0.5, color = "black") +
    scale_fill_manual(values = ColorPalette) +
    scale_size_continuous(range = c(1, 7)) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(from = 0, to = 1.5, by = 0.2)) +
    scale_y_continuous(limits = c(0.5, 2.25), breaks = seq(from = 0, to = 2.5, by = 0.5)) +
    labs(title = "Kohl",
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

(p2b = ggplot(ftlist$Karotte, aes(x = mean, y = HtoC_rate, fill = chem_space, size = intensity_mean)) +
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

(introVegs <- ggpubr::ggarrange(p1a, p1b, p2a, p2b, ncol = 2, nrow = 2))


dftable <- as.data.frame(table(ftlist$Karotte$chem_space))
colnames(dftable) <- c("category", "count")
dftable$fraction <- dftable$count / sum(dftable$count)
dftable$ymax <- cumsum(dftable$fraction)
dftable$ymin <- c(0, head(dftable$ymax, n = -1))
dftable$labelPosition <- (dftable$ymax + dftable$ymin) / 2
dftable$label <- paste0(dftable$category, ": ", round(dftable$fraction *100), "%")

(karP <- ggplot(dftable, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = category)) +
    geom_rect() +
    geom_text(x = 1, aes(y = labelPosition, label = label, color = category), size = 6) + # x here controls label position (inner / outer)
    scale_fill_manual(values = ColorPalette) +
    scale_color_manual(values = ColorPalette) +
    coord_polar(theta = "y") +
    xlim(c(-1, 4)) +
    labs(title = "Karotte") +
    theme_void() +
    theme(legend.position = "none"))

dftable <- as.data.frame(table(ftlist$Kohl$chem_space))
colnames(dftable) <- c("category", "count")
dftable$fraction <- dftable$count / sum(dftable$count)
dftable$ymax <- cumsum(dftable$fraction)
dftable$ymin <- c(0, head(dftable$ymax, n = -1))
dftable$labelPosition <- (dftable$ymax + dftable$ymin) / 2
dftable$label <- paste0(dftable$category, ": ", round(dftable$fraction *100), "%")

(kohP <- ggplot(dftable, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = category)) +
    geom_rect() +
    geom_text(x = 1, aes(y = labelPosition, label = label, color = category), size = 6) + # x here controls label position (inner / outer)
    scale_fill_manual(values = ColorPalette) +
    scale_color_manual(values = ColorPalette) +
    coord_polar(theta = "y") +
    xlim(c(-1, 4)) +
    labs(title = "Kohl") +
    theme_void() +
    theme(legend.position = "none"))


(introVegs <- ggpubr::ggarrange(karP, kohP, nrow = 2))


##------------features in all vegetables----------------------
gemueseIDs <- ftlist$Karotte$ID
gemueseIDs <- gemueseIDs[gemueseIDs %in% ftlist$Kartoffel$ID]
gemueseIDs <- gemueseIDs[gemueseIDs %in% ftlist$Rosenkohl$ID]
gemueseIDs <- gemueseIDs[gemueseIDs %in% ftlist$Kohl$ID]
gemueseIDs <- gemueseIDs[gemueseIDs %in% ftlist$Zwiebel$ID]
gemueseIDs <- gemueseIDs[gemueseIDs %in% ftlist$Zucchini$ID]

gemueseIDs <- vdata[vdata$ID %in% gemueseIDs, ]
  
  
(p1a = ggplot(gemueseIDs, aes(x = OtoC_rate, y = HtoC_rate, fill = chem_space, size = intensity_mean)) +
    geom_point(shape = 21, alpha = 0.5, color = "black") +
    scale_fill_manual(values = ColorPalette) +
    scale_size_continuous(range = c(1, 7)) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(from = 0, to = 1.5, by = 0.2)) +
    scale_y_continuous(limits = c(0.5, 2.25), breaks = seq(from = 0, to = 2.5, by = 0.5)) +
    labs(title = "Features in all Vegetables",
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

(p1b = ggplot(gemueseIDs, aes(x = mean, y = HtoC_rate, fill = chem_space, size = intensity_mean)) +
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

(allVegs <- ggpubr::ggarrange(p1a, p1b, ncol = 2, nrow = 1))


dftable <- as.data.frame(table(gemueseIDs$chem_space))
colnames(dftable) <- c("category", "count")
dftable$fraction <- dftable$count / sum(dftable$count)
dftable$ymax <- cumsum(dftable$fraction)
dftable$ymin <- c(0, head(dftable$ymax, n = -1))
dftable$labelPosition <- (dftable$ymax + dftable$ymin) / 2
dftable$label <- paste0(dftable$category, ": ", round(dftable$fraction *100), "%")

(kabDNT <- ggplot(dftable, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = category)) +
    geom_rect() +
    geom_text(x = 1, aes(y = labelPosition, label = label, color = category), size = 6) + # x here controls label position (inner / outer)
    scale_fill_manual(values = ColorPalette) +
    scale_color_manual(values = ColorPalette) +
    coord_polar(theta = "y") +
    xlim(c(-1, 4)) +
    labs(title = "Gemuese commons") +
    theme_void() +
    theme(legend.position = "none"))


##----------unique in  Kohl and Karotte-------------------
karottenIDs <- ftlist$Karotte$ID
karottenIDs <- karottenIDs[karottenIDs %notin% ftlist$Kartoffel$ID]
karottenIDs <- karottenIDs[karottenIDs %notin% ftlist$Rosenkohl$ID]
karottenIDs <- karottenIDs[karottenIDs %notin% ftlist$Kohl$ID]
karottenIDs <- karottenIDs[karottenIDs %notin% ftlist$Zwiebel$ID]
karottenIDs <- karottenIDs[karottenIDs %notin% ftlist$Zucchini$ID]
karottenIDs <- vdata[vdata$ID %in% karottenIDs, ]

kohlIDs <- ftlist$Kohl$ID
kohlIDs <- kohlIDs[kohlIDs %notin% ftlist$Kartoffel$ID]
kohlIDs <- kohlIDs[kohlIDs %notin% ftlist$Karotte$ID]
kohlIDs <- kohlIDs[kohlIDs %notin% ftlist$Zwiebel$ID]
kohlIDs <- kohlIDs[kohlIDs %notin% ftlist$Zucchini$ID]
##zero!


(p1a = ggplot(karottenIDs, aes(x = OtoC_rate, y = HtoC_rate, fill = chem_space, size = intensity_mean)) +
    geom_point(shape = 21, alpha = 0.5, color = "black") +
    scale_fill_manual(values = ColorPalette) +
    scale_size_continuous(range = c(1, 7)) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(from = 0, to = 1.5, by = 0.2)) +
    scale_y_continuous(limits = c(0.5, 2.25), breaks = seq(from = 0, to = 2.5, by = 0.5)) +
    labs(title = "Karotte unique",
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

(p1b = ggplot(karottenIDs, aes(x = mean, y = HtoC_rate, fill = chem_space, size = intensity_mean)) +
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

(karottenVK <- ggpubr::ggarrange(p1a, p1b, ncol = 2, nrow = 1))

zwiebelIDs <- ftlist$Zwiebel$ID
zwiebelIDs <- zwiebelIDs[zwiebelIDs %notin% ftlist$Kartoffel$ID]
zwiebelIDs <- zwiebelIDs[zwiebelIDs %notin% ftlist$Karotte$ID]
zwiebelIDs <- zwiebelIDs[zwiebelIDs %notin% ftlist$Rosenkohl$ID]
zwiebelIDs <- zwiebelIDs[zwiebelIDs %notin% ftlist$Kohl$ID]
zwiebelIDs <- zwiebelIDs[zwiebelIDs %notin% ftlist$Zucchini$ID]
zwiebelIDs <- vdata[vdata$ID %in% zwiebelIDs, ]

(p1a = ggplot(zwiebelIDs, aes(x = OtoC_rate, y = HtoC_rate, fill = chem_space, size = intensity_mean)) +
    geom_point(shape = 21, alpha = 0.5, color = "black") +
    scale_fill_manual(values = ColorPalette) +
    scale_size_continuous(range = c(1, 7)) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(from = 0, to = 1.5, by = 0.2)) +
    scale_y_continuous(limits = c(0.5, 2.25), breaks = seq(from = 0, to = 2.5, by = 0.5)) +
    labs(title = "Zwiebel unique",
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

(p1b = ggplot(zwiebelIDs, aes(x = mean, y = HtoC_rate, fill = chem_space, size = intensity_mean)) +
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

(zwiebelVK <- ggpubr::ggarrange(p1a, p1b, ncol = 2, nrow = 1))

(uniqueVeggs <- ggpubr::ggarrange(karottenVK, zwiebelVK, nrow = 2))

dftable <- as.data.frame(table(karottenIDs$chem_space))
colnames(dftable) <- c("category", "count")
dftable$fraction <- dftable$count / sum(dftable$count)
dftable$ymax <- cumsum(dftable$fraction)
dftable$ymin <- c(0, head(dftable$ymax, n = -1))
dftable$labelPosition <- (dftable$ymax + dftable$ymin) / 2
dftable$label <- paste0(dftable$category, ": ", round(dftable$fraction *100), "%")

(karP <- ggplot(dftable, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = category)) +
    geom_rect() +
    geom_text(x = 1, aes(y = labelPosition, label = label, color = category), size = 6) + # x here controls label position (inner / outer)
    scale_fill_manual(values = ColorPalette) +
    scale_color_manual(values = ColorPalette) +
    coord_polar(theta = "y") +
    xlim(c(-1, 4)) +
    labs(title = "Karotte unique") +
    theme_void() +
    theme(legend.position = "none"))

dftable <- as.data.frame(table(zwiebelIDs$chem_space))
colnames(dftable) <- c("category", "count")
dftable$fraction <- dftable$count / sum(dftable$count)
dftable$ymax <- cumsum(dftable$fraction)
dftable$ymin <- c(0, head(dftable$ymax, n = -1))
dftable$labelPosition <- (dftable$ymax + dftable$ymin) / 2
dftable$label <- paste0(dftable$category, ": ", round(dftable$fraction *100), "%")

(zwP <- ggplot(dftable, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = category)) +
    geom_rect() +
    geom_text(x = 1, aes(y = labelPosition, label = label, color = category), size = 6) + # x here controls label position (inner / outer)
    scale_fill_manual(values = ColorPalette) +
    scale_color_manual(values = ColorPalette) +
    coord_polar(theta = "y") +
    xlim(c(-1, 4)) +
    labs(title = "Zwiebel unique") +
    theme_void() +
    theme(legend.position = "none"))

(introVegs <- ggpubr::ggarrange(karP, zwP, nrow = 2))


##----------cheese-------------------

cheesies <- ftlist$Raclette
cheesies <- cheesies[cheesies$ID %in% ftlist$Rougette$ID, ]

(p1a = ggplot(cheesies, aes(x = OtoC_rate, y = HtoC_rate, fill = chem_space, size = intensity_mean)) +
    geom_point(shape = 21, alpha = 0.5, color = "black") +
    scale_fill_manual(values = ColorPalette) +
    scale_size_continuous(range = c(1, 7)) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(from = 0, to = 1.5, by = 0.2)) +
    scale_y_continuous(limits = c(0.5, 2.25), breaks = seq(from = 0, to = 2.5, by = 0.5)) +
    labs(title = "Cheese common",
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

(p1b = ggplot(cheesies, aes(x = mean, y = HtoC_rate, fill = chem_space, size = intensity_mean)) +
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

(cheeseVK <- ggpubr::ggarrange(p1a, p1b, ncol = 2, nrow = 1))


dftable <- as.data.frame(table(karottenIDs$chem_space))
colnames(dftable) <- c("category", "count")
dftable$fraction <- dftable$count / sum(dftable$count)
dftable$ymax <- cumsum(dftable$fraction)
dftable$ymin <- c(0, head(dftable$ymax, n = -1))
dftable$labelPosition <- (dftable$ymax + dftable$ymin) / 2
dftable$label <- paste0(dftable$category, ": ", round(dftable$fraction *100), "%")

(karP <- ggplot(dftable, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = category)) +
    geom_rect() +
    geom_text(x = 1, aes(y = labelPosition, label = label, color = category), size = 6) + # x here controls label position (inner / outer)
    scale_fill_manual(values = ColorPalette) +
    scale_color_manual(values = ColorPalette) +
    coord_polar(theta = "y") +
    xlim(c(-1, 4)) +
    labs(title = "Karotte unique") +
    theme_void() +
    theme(legend.position = "none"))

dftable <- as.data.frame(table(zwiebelIDs$chem_space))
colnames(dftable) <- c("category", "count")
dftable$fraction <- dftable$count / sum(dftable$count)
dftable$ymax <- cumsum(dftable$fraction)
dftable$ymin <- c(0, head(dftable$ymax, n = -1))
dftable$labelPosition <- (dftable$ymax + dftable$ymin) / 2
dftable$label <- paste0(dftable$category, ": ", round(dftable$fraction *100), "%")


##----------common in meat alternatives-------------------
ma <- ftlist$BeyBu$ID 
ma <- ma[ma %in% ftlist$MuehlFrick$ID]
ma <- ma[ma %in% ftlist$MuehlSchnitzel$ID]
ma <- ma[ma %in% ftlist$NoBeefBu$ID]
ma <- ma[ma %in% ftlist$NoChick$ID]
ma <- ma[ma %in% ftlist$NoHD$ID]
ma <- ma[ma %in% ftlist$NoMince$ID]
ma <- ma[ma %in% ftlist$VegBack$ID]
ma <- ma[ma %in% ftlist$VegKnusp$ID]
ma <- vdata[vdata$ID %in% ma, ]

(p2a = ggplot(ma, aes(x = OtoC_rate, y = HtoC_rate, fill = chem_space, size = intensity_mean)) +
    geom_point(shape = 21, alpha = 0.5, color = "black") +
    scale_fill_manual(values = ColorPalette) +
    scale_size_continuous(range = c(1, 7)) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(from = 0, to = 1.5, by = 0.2)) +
    scale_y_continuous(limits = c(0.5, 2.25), breaks = seq(from = 0, to = 2.5, by = 0.5)) +
    labs(title = "Common in all meat alternatives",
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

(p2b = ggplot(ma, aes(x = mean, y = HtoC_rate, fill = chem_space, size = intensity_mean)) +
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

(maVK <- ggpubr::ggarrange(p2a, p2b, ncol = 2, nrow = 1))


dftable <- as.data.frame(table(ma$chem_space))
colnames(dftable) <- c("category", "count")
dftable$fraction <- dftable$count / sum(dftable$count)
dftable$ymax <- cumsum(dftable$fraction)
dftable$ymin <- c(0, head(dftable$ymax, n = -1))
dftable$labelPosition <- (dftable$ymax + dftable$ymin) / 2
dftable$label <- paste0(dftable$category, ": ", round(dftable$fraction *100), "%")

(maP <- ggplot(dftable, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = category)) +
    geom_rect() +
    geom_text(x = 1, aes(y = labelPosition, label = label, color = category), size = 6) + # x here controls label position (inner / outer)
    scale_fill_manual(values = ColorPalette) +
    scale_color_manual(values = ColorPalette) +
    coord_polar(theta = "y") +
    xlim(c(-1, 4)) +
    labs(title = "Common in all meat alternatives") +
    theme_void() +
    theme(legend.position = "none"))

##----------comparing steak with BeyBu-------------------
load(file = "/Users/lw/Nextcloud/Atlas-PYR/matrices/2022-05-30-cldata-SteakH.RData")

(p2a = ggplot(ftlist$BeyBu, aes(x = OtoC_rate, y = HtoC_rate, fill = chem_space, size = intensity_mean)) +
    geom_point(shape = 21, alpha = 0.5, color = "black") +
    scale_fill_manual(values = ColorPalette) +
    scale_size_continuous(range = c(1, 7)) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(from = 0, to = 1.5, by = 0.2)) +
    scale_y_continuous(limits = c(0.5, 2.25), breaks = seq(from = 0, to = 2.5, by = 0.5)) +
    labs(title = "Common in all meat alternatives",
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

(p2b = ggplot(ftlist$BeyBu, aes(x = mean, y = HtoC_rate, fill = chem_space, size = intensity_mean)) +
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

(vegBu <- ggpubr::ggarrange(p2a, p2b, ncol = 2, nrow = 1))

##pie chart
dftable <- as.data.frame(table(ftlist$BeyBu$chem_space))
colnames(dftable) <- c("category", "count")
dftable$fraction <- dftable$count / sum(dftable$count)
dftable$ymax <- cumsum(dftable$fraction)
dftable$ymin <- c(0, head(dftable$ymax, n = -1))
dftable$labelPosition <- (dftable$ymax + dftable$ymin) / 2
dftable$label <- paste0(dftable$category, ": ", round(dftable$fraction *100), "%")

(vegBuP <- ggplot(dftable, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = category)) +
    geom_rect() +
    geom_text(x = 1, aes(y = labelPosition, label = label, color = category), size = 6) + # x here controls label position (inner / outer)
    scale_fill_manual(values = ColorPalette) +
    scale_color_manual(values = ColorPalette) +
    coord_polar(theta = "y") +
    xlim(c(-1, 4)) +
    labs(title = "Common in all meat alternatives") +
    theme_void() +
    theme(legend.position = "none"))


##venn diagramm
ggvenn::ggvenn(list("steak" = cldata_steak$mol_formula, "veg" = ma$mol_formula), 
               c("steak", "veg"))

##intersection with bovine steak
intersectSteak <- ma$mol_formula[ma$mol_formula %in% cldata_steak$mol_formula]
intersectSteak <- vdata[vdata$mol_formula %in% intersectSteak, ]

##are the commons also in e.g. bread
load(file = "/Users/lw/Nextcloud/Atlas-PYR/matrices/2020-12-02-ftlist-DoEII.RData")
table(intersectSteak$mol_formula %in% ftlist$AC_temp242time12.0$sum_formular)
intersectSteak$classsmarker <- "meats"

intersectSteak$classsmarker[intersectSteak$mol_formula %in% 
                              ftlist$AC_temp242time12.0$sum_formular] <- "bakery"
intersectSteak$classsmarker <- factor(intersectSteak$classsmarker)
load(file = "2023-02-20-veggies.RData") ##reload veggie ftlist

(p3a = ggplot(intersectSteak, aes(x = OtoC_rate, y = HtoC_rate, fill = chem_space, size = intensity_mean)) +
    geom_point(shape = 21, alpha = 0.5, color = "black") +
    facet_wrap("classsmarker") +
    scale_fill_manual(values = ColorPalette) +
    scale_size_continuous(range = c(1, 7)) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(from = 0, to = 1.5, by = 0.2)) +
    scale_y_continuous(limits = c(0.5, 2.25), breaks = seq(from = 0, to = 2.5, by = 0.5)) +
    labs(title = "Common in all meat alternatives and Steak",
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

prop.table(table(intersectSteak$classsmarker))

##----------PCA of meat alternatives-------------------
library(FactoMineR)
library(factoextra)

##extracting the meat alternative samples
surrnames <- colnames(vdata)
pcadata <- vdata[,c(16,17,22,23,30,31,38:43,45:48,59:62)]
rownames(pcadata) <- vdata$mol_formula

##missingness filter
pcadata$nacount <- apply(pcadata, 1, \(x) sum(is.na(x)))
pcadata <- pcadata[pcadata$nacount < 15, ]
pcadata$nacount <- NULL

##imputation
pcadata_imp <- customLW::impute_noise(pcadata)

##transformation and reshaping
pcadata_tr <- as.data.frame(t(log2(pcadata_imp)))

##metadata 
meatSurrMeta <- openxlsx::read.xlsx("2023-02-22-metadata-veggies.xlsx")
meatSurrMeta$producer <- factor(meatSurrMeta$producer)
meatSurrMeta$surrogate <- factor(meatSurrMeta$surrogate)
meatSurrMeta$panade <- factor(meatSurrMeta$panade)
pcadata_tr$cat <- meatSurrMeta$panade

rownames(pcadata_tr) <- meatSurrMeta$sample

##calculate PC
res.pca <- PCA(pcadata_tr, scale.unit = T, ncp = 10, quali.sup = ncol(pcadata_tr))

##visualizing individuals
(p1 <- fviz_pca_ind(res.pca, axes = c(1,2), habillage = ncol(pcadata_tr),
                    addEllipses = F, palette = "npg",
                    ggtheme = theme_bw() +
                      theme(panel.grid = element_blank(),                                                      
                            axis.title = element_text(size = 14, colour = "black"),                              
                            axis.text = element_text(size = 11, colour = "black"),                                 
                            axis.ticks = element_line(colour = "black"),                                            
                            legend.text = element_text(color = "black", size = 9),                                 
                            legend.title = element_text(size = 14))))

(p2 <- fviz_pca_ind(res.pca, axes = c(1,3), habillage = ncol(pcadata_tr),
                    addEllipses = F, palette = "npg",
                    ggtheme = theme_bw() +
                      theme(panel.grid = element_blank(),                                                      
                            axis.title = element_text(size = 14, colour = "black"),                              
                            axis.text = element_text(size = 11, colour = "black"),                                 
                            axis.ticks = element_line(colour = "black"),                                            
                            legend.text = element_text(color = "black", size = 9),                                 
                            legend.title = element_text(size = 14))))
ggpubr::ggarrange(p1,p2,nrow = 1)

##----------Panade cluster of PCA-------------------
(p <- fviz_pca_ind(res.pca, axes = c(3,4), habillage = ncol(pcadata_tr),
                    addEllipses = F, palette = "npg",
                    ggtheme = theme_bw() +
                      theme(panel.grid = element_blank(),                                                      
                            axis.title = element_text(size = 14, colour = "black"),                              
                            axis.text = element_text(size = 11, colour = "black"),                                 
                            axis.ticks = element_line(colour = "black"),                                            
                            legend.text = element_text(color = "black", size = 9),                                 
                            legend.title = element_text(size = 14))))


coordspca <- as.data.frame(res.pca$var$coord) 
panadcoords <- coordspca[coordspca$Dim.3 < 0 & coordspca$Dim.4 > 0, ]
panadcoords <- vdata[vdata$mol_formula %in% rownames(panadcoords), ]
panadcoords <- panadcoords[panadcoords$chem_space == "CHO" | 
                             panadcoords$chem_space == "CHNO", ]

(p1a = ggplot(panadcoords, aes(x = OtoC_rate, y = HtoC_rate, fill = chem_space, size = intensity_mean)) +
    geom_point(shape = 21, alpha = 0.5, color = "black") +
    scale_fill_manual(values = ColorPalette) +
    scale_size_continuous(range = c(1, 7)) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(from = 0, to = 1.5, by = 0.2)) +
    scale_y_continuous(limits = c(0.5, 2.25), breaks = seq(from = 0, to = 2.5, by = 0.5)) +
    labs(title = "PCA Dim. 3 + 4 Vars for Panade",
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

(p1b = ggplot(panadcoords, aes(x = mean, y = HtoC_rate, fill = chem_space, size = intensity_mean)) +
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

##are these features related to wheat bread?
load(file = "/Users/lw/Nextcloud/Atlas-PYR/matrices/2020-12-02-ftlist-DoEII.RData")
table(panadcoords$mol_formula %in% ftlist$AC_temp242time12.0$sum_formular)
intersectSteak$classsmarker <- "meats"

intersectSteak$classsmarker[intersectSteak$mol_formula %in% 
                              ftlist$AC_temp242time12.0$sum_formular] <- "bakery"
intersectSteak$classsmarker <- factor(intersectSteak$classsmarker)
load(file = "2023-02-20-veggies.RData") ##reload veggie ftlist

##----------Comparing Hot Air and Steam---------------
(p1a = ggplot(ftlist$Zucchini, aes(x = OtoC_rate, y = HtoC_rate, fill = chem_space, size = intensity_mean)) +
   geom_point(shape = 21, alpha = 0.5, color = "black") +
   scale_fill_manual(values = ColorPalette) +
   scale_size_continuous(range = c(1, 7)) +
   scale_x_continuous(limits = c(0, 1), breaks = seq(from = 0, to = 1.5, by = 0.2)) +
   scale_y_continuous(limits = c(0.5, 2.25), breaks = seq(from = 0, to = 2.5, by = 0.5)) +
   labs(title = "Zucchini Hot AIr",
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

(p1b = ggplot(ftlist$Zucchini, aes(x = mean, y = HtoC_rate, fill = chem_space, size = intensity_mean)) +
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

(p2a = ggplot(vdata, aes(x = OtoC_rate, y = HtoC_rate, fill = chem_space, size = Zucchini_Steam)) +
    geom_point(shape = 21, alpha = 0.5, color = "black") +
    scale_fill_manual(values = ColorPalette) +
    scale_size_continuous(range = c(1, 7)) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(from = 0, to = 1.5, by = 0.2)) +
    scale_y_continuous(limits = c(0.5, 2.25), breaks = seq(from = 0, to = 2.5, by = 0.5)) +
    labs(title = "Zucchini steamed",
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

(p2b = ggplot(vdata, aes(x = mean, y = HtoC_rate, fill = chem_space, size = Zucchini_Steam)) +
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

(steamHAVK <- ggpubr::ggarrange(p1a, p1b, p2a, p2b, ncol = 2, nrow = 2))
