rm(list=ls())
library(tidyverse)
library(readxl)
library(tidyr)
library("writexl")

comer = read.csv("C:/Users/evasquez/Documents/gitlab/comercializacion/App/Base.csv")

write_xlsx(comer,"comercializacion.xlsx")
