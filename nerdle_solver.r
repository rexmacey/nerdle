# Nerdle Solver
library(dplyr)
source("C:/Users/xer09/OneDrive/Nerdle/nerdlefunctions.r")

dfEqChar <- readRDS("C:/Users/xer09/OneDrive/Nerdle/dfEqChar.rds")


guess1 <- findBestEquation(dfEqChar)
guess1

pass <- dfEqChar
pass <- pass %>% filter(p6 == "=")

pass <- excld(unlist(strsplit("1043","")),pass)
pass <- mustHaveNotInPos(c("2", "+"),c(4,3), pass) 

guess2 <- findBestEquation(pass)
guess2

pass <- excld("9",pass)

pass <- mustHaveNotInPos(unlist(strsplit("852","")), c(1,3,7), pass)

guess3 <- findBestEquation(pass)
guess3
