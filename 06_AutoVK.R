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
library(customLW)

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

##--------------VK for every Sample Type--------------

dir.create("VK_out")
setwd(paste0(wd, "/VK_out"))

for(i in 1:length(ftlist)) {
  
  
  png(file = paste0(names(ftlist[i]), "-VK.png"), width = 1280, height = 400)
  print({
    a = ggplot(ftlist[[i]], aes(x = OtoC_rate, y = HtoC_rate, fill = chem_space, size = intensity_mean)) +
       geom_point(shape = 21, alpha = 0.5, color = "black") +
       scale_fill_manual(values = ColorPalette) +
       scale_size_continuous(range = c(1, 7)) +
       scale_x_continuous(limits = c(0, 1), breaks = seq(from = 0, to = 1.5, by = 0.2)) +
       scale_y_continuous(limits = c(0.5, 2.25), breaks = seq(from = 0, to = 2.5, by = 0.5)) +
       labs(title = names(ftlist[i]),
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
       annotate(geom = "text", x = 0, y = 0, label = "", color = 0, size = 11)
    
    b = ggplot(ftlist[[i]], aes(x = mean, y = HtoC_rate, fill = chem_space, size = intensity_mean)) +
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
        annotate(geom = "text", x = 0, y = 0, label = "", color = 0, size = 11)
    
    (final_plot = ggpubr::ggarrange(a, b, ncol = 2))

  })
  dev.off()
}