Sys.setenv(LANGUAGE="en")
memory.limit(size = 8e10)

library(bgcDH)
library(tidyverse)
library(magrittr)

"%notin%" <- base::Negate("%in%")

wd <- dirname(rstudioapi::getSourceEditorContext()$path) 
setwd(paste0(wd , "/files"))


##check headers!! check error scales according to dataset!
wiggles_bk(input.file = ".ascii", header = TRUE)
iso_filter(input.file = "last", header = TRUE, error = 0.75, iso = c("13C", "34S", "37Cl"))
md_filter(input.file = "last", error = 0.75, header = TRUE, adducts = c("[M-H]-", "[M+Cl]-"))

align_peaks(input.file = "last", header = TRUE, ppm.win = 0.75)

M = read.table(paste0(wd, "/files/wiggles_bk-filtered/iso_filter-filtered/md_filter-filtered/Matrix.txt"), header = T)
colnames(M) = stringr::str_remove(colnames(M), ".ascii")

setwd(wd)

openxlsx::write.xlsx(M, "2023-02-20-Matrix-Veggies.xlsx")