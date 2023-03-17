rm(list = ls())
options(digits = 6)
set.seed(212)
Sys.setenv(LANGUAGE="en")

library(tidyverse)
library(scales)
library(plotly)

"%notin%" <- base::Negate("%in%")

wd <- dirname(rstudioapi::getSourceEditorContext()$path) 
setwd(paste0(wd, "/NMR/"))

##-------------Nuggets Panade / No Panade--------------------------

sPan <- read.table("NuggetsPanade.txt", header = FALSE)
colnames(sPan) <- c("ID", "Int", "unk.", "shift")
sPan$sampletype <- "Pan"

sNoPan <- read.table("NuggetsNoPan.txt", header = FALSE)
colnames(sNoPan) <- c("ID", "Int", "unk.", "shift")
sNoPan$sampletype <- "NoPan"

sChick <- read.table("Chicken.txt", header = FALSE)
colnames(sChick) <- c("ID", "Int", "unk.", "shift")
sChick$sampletype <- "Chicken"

##scaling TSP
sChickmodified <- sChick
sChickmodified$Int <- str_remove(sChickmodified$Int, ",")
sChickmodified$Int <- as.numeric(sChickmodified$Int)
sChickmodified$Int <- sChickmodified$Int /2.5


data <- rbind(sPan, sNoPan)
data$sampletype <- as.factor(data$sampletype)
data$Int <- str_remove(data$Int, ",")
data$Int <- as.numeric(data$Int)

CP = c("Pan" = "darkblue", "NoPan" = "firebrick")

(p1 <- ggplot(data, aes(x = shift, y = Int, col = sampletype)) +
  geom_line() +
  scale_color_manual(values = CP) +
  scale_x_reverse(lim = c(2, 0.75)) +   ##c(12, -0.2)
  scale_y_continuous(limits = c(-10000, 1e+06),
                     labels = unit_format(unit = "M", scale = 1e-6)) +
  theme_classic() +
  theme(panel.grid = element_blank(),
        legend.position = "bottom"))


(p2 <- ggplot(data, aes(x = shift, y = Int, col = sampletype)) +
    geom_line() +
    scale_color_manual(values = CP) +
    scale_x_reverse(lim = c(4, 3.5)) +   ##c(12, -0.2)
    scale_y_continuous(limits = c(-10000, 1e+06),
                       labels = unit_format(unit = "M", scale = 1e-6)) +
    theme_classic() +
    theme(panel.grid = element_blank(),
          legend.position = "bottom"))

(p4 <- ggplot(data, aes(x = shift, y = Int, col = sampletype)) +
    geom_line() +
    scale_color_manual(values = CP) +
    scale_x_reverse(lim = c(8.1, 7)) +   ##c(12, -0.2)
    scale_y_continuous(limits = c(-10000, 6e+04),
                       labels = unit_format(unit = "k", scale = 1e-3)) +
    theme_classic() +
    theme(panel.grid = element_blank(),
          legend.position = "bottom"))

ggpubr::ggarrange(p4, p2, p1, nrow = 1)


##-------------NoChicken-Chicken and NoBeef-Beef comparison--------------------------
sNoPan <- read.table("NuggetsNoPan.txt", header = FALSE)
colnames(sNoPan) <- c("ID", "Int", "unk.", "shift")
sNoPan$sampletype <- "NoChicken"

sChick <- read.table("Chicken.txt", header = FALSE)
colnames(sChick) <- c("ID", "Int", "unk.", "shift")
sChick$sampletype <- "Chicken"

sChick$Int <- str_remove(sChick$Int, ",")
sChick$Int <- as.numeric(sChick$Int)
sChick$Int <- sChick$Int /3

data <- rbind(sChick, sNoPan)
data$sampletype <- as.factor(data$sampletype)
data$Int <- str_remove(data$Int, ",")
data$Int <- as.numeric(data$Int)

CP = c("NoChicken" = "darkblue", "Chicken" = "firebrick")

(pC1 <- ggplot(data, aes(x = shift, y = Int, col = sampletype)) +
    geom_line() +
    scale_color_manual(values = CP) +
    scale_x_reverse(lim = c(2, 0.75)) +   ##c(12, -0.2)
    scale_y_continuous(limits = c(-10000, 5e+06),
                       labels = unit_format(unit = "M", scale = 1e-6)) +
    theme_classic() +
    theme(panel.grid = element_blank(),
          legend.position = "bottom"))

(pC2 <- ggplot(data, aes(x = shift, y = Int, col = sampletype)) +
    geom_line() +
    scale_color_manual(values = CP) +
    scale_x_reverse(lim = c(8, 6)) +   ##c(12, -0.2)
    scale_y_continuous(limits = c(-10000, 1e+05),
                       labels = unit_format(unit = "k", scale = 1e-3)) +
    theme_classic() +
    theme(panel.grid = element_blank(),
          legend.position = "bottom"))

sNoBeef <- read.table("NoBeef.txt", header = FALSE)
colnames(sNoBeef) <- c("ID", "Int", "unk.", "shift")
sNoBeef$sampletype <- "NoBeef"

sBeef <- read.table("Beef.txt", header = FALSE)
colnames(sBeef) <- c("ID", "Int", "unk.", "shift")
sBeef$sampletype <- "Beef"

sBeef$Int <- str_remove(sBeef$Int, ",")
sBeef$Int <- as.numeric(sBeef$Int)
sBeef$Int <- sBeef$Int *20
sBeef$Int <- sBeef$Int + 5e4

data <- rbind(sNoBeef, sBeef)
data$sampletype <- as.factor(data$sampletype)
data$Int <- str_remove(data$Int, ",")
data$Int <- as.numeric(data$Int)

CP = c("NoBeef" = "darkblue", "Beef" = "firebrick")

(pB1 <- ggplot(data, aes(x = shift, y = Int, col = sampletype)) +
    geom_line() +
    scale_color_manual(values = CP) +
    scale_x_reverse(lim = c(2.2, 0.75)) +   ##c(12, -0.2)
    scale_y_continuous(limits = c(-10000, 0.5e+07),
                       labels = unit_format(unit = "M", scale = 1e-6)) +
    theme_classic() +
    theme(panel.grid = element_blank(),
          legend.position = "bottom"))
(pB2 <- ggplot(data, aes(x = shift, y = Int, col = sampletype)) +
    geom_line() +
    scale_color_manual(values = CP) +
    scale_x_reverse(lim = c(8, 6)) +   ##c(12, -0.2)
    scale_y_continuous(limits = c(-10000, 1e+05),
                       labels = unit_format(unit = "k", scale = 1e-3)) +
    theme_classic() +
    theme(panel.grid = element_blank(),
          legend.position = "bottom"))

pE <- ggplot() + 
  theme_void()

ggpubr::ggarrange(pC2, pC1, pE, pE, pB2, pB1, 
                  nrow = 3, ncol = 2, heights = c(1, 0.2, 1))

##-------------MixII experimnet--------------------------
SMixChick <- read.table("MixII-Chicken.txt", header = FALSE)
colnames(SMixChick) <- c("ID", "Int", "unk.", "shift")
SMixChick$sampletype <- "Chicken"

SMixSal <- read.table("MixII-Salmon.txt", header = FALSE)
colnames(SMixSal) <- c("ID", "Int", "unk.", "shift")
SMixSal$sampletype <- "Salmon"

SMixSal$Int <- str_remove(SMixSal$Int, ",")
SMixSal$Int <- as.numeric(SMixSal$Int)
SMixSal$Int <- SMixSal$Int *6

SMixges <- read.table("MixII-ges.txt", header = FALSE)
colnames(SMixges) <- c("ID", "Int", "unk.", "shift")
SMixges$sampletype <- "both"

SMixges$Int <- str_remove(SMixges$Int, ",")
SMixges$Int <- as.numeric(SMixges$Int)
SMixges$Int <- SMixges$Int /1.8

data <- rbind(SMixChick, SMixSal, SMixges)
data$sampletype <- factor(data$sampletype, levels = c("Chicken", "Salmon", "both"))
data$Int <- str_remove(data$Int, ",")
data$Int <- as.numeric(data$Int)

CP = c("Salmon" = "grey15", "Chicken" = "grey80", "both" = "firebrick")

(pMX1 <- ggplot(data, aes(x = shift, y = Int, col = sampletype)) +
    geom_line() +
    scale_color_manual(values = CP) +
    scale_x_reverse(lim = c(2, 0.8)) +   ##c(12, -0.2)
    scale_y_continuous(limits = c(-10000, 5e+06),
                       labels = unit_format(unit = "M", scale = 1e-6)) +
    theme_classic() +
    theme(panel.grid = element_blank(),
          legend.position = "bottom"))

(pMX2 <- ggplot(data, aes(x = shift, y = Int, col = sampletype)) +
    geom_line() +
    scale_color_manual(values = CP) +
    scale_x_reverse(lim = c(8, 6)) +   ##c(12, -0.2)
    scale_y_continuous(limits = c(-10000, 2e+05),
                       labels = unit_format(unit = "k", scale = 1e-3)) +
    theme_classic() +
    theme(panel.grid = element_blank(),
          legend.position = "bottom"))

ggpubr::ggarrange(pMX2, pMX1, nrow = 1)
                  



##-------plotly----------
p <- plot_ly(data, x = ~sampletype, y = ~shift, z = ~Int, 
             split = ~sampletype, 
             type = 'scatter3d', mode = 'lines',
             line = list(width = 4))

orca(p, "surface-plot.svg")  ##save with

