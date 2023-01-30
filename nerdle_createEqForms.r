# This is used to create possible equations.
library(dplyr)
library(stringr)
source("nerdleFunctions.r")
rm(dfEqForms, eqForms)
library(foreach)
library(doParallel)

validEquations <- readRDS("validEquations.rds")
potentialEqForms <- readRDS("potentialEqForms.rds")
strEquations <- readRDS("strEquations.rds")

displayStats <- function(potentialEqForms) {
  print(paste("N =", nrow(potentialEqForms)))
  print(head(potentialEqForms))
  print(tail(potentialEqForms))
}

convertEqToString <- function(potentialEqForms) {
  out <- potentialEqForms %>% 
    mutate(Equation = paste0(as.character(Var1),
                             as.character(Var2),
                             as.character(Var3),
                             as.character(Var4),
                             as.character(Var5),
                             as.character(Var6),
                             as.character(Var7),
                             as.character(Var8))) %>%
    select(Equation)
  return(unlist(out))
}

if (FALSE){
  potentialEqForms <- expand.grid("d", 
                                  c("d", "+", "-", "*", "/"),
                                  c("d", "+", "-", "*", "/"),
                                  c("d", "+", "-", "*", "/"),
                                  c("d", "+", "-", "*", "/", "="),
                                  c("d", "="),
                                  c("d", "="),
                                  "d")
  displayStats(potentialEqForms)
  
  # must be only one "="
  idx <- apply(potentialEqForms, 1, function(x) sum(x %in% "=") == 1)
  potentialEqForms <- potentialEqForms[idx, ]
  displayStats(potentialEqForms)
  
  # no more than 6 d
  idx <- apply(potentialEqForms, 1, function(x) sum(x %in% "d") <= 6)
  potentialEqForms <- potentialEqForms[idx, ]
  displayStats(potentialEqForms)
  
  strEquations <- convertEqToString(potentialEqForms)
  
  # eliminate consecutive operators
  consecOperators <- unlist(expand.grid(c("+","-","*","/"), c("+","-","*","/","=")) %>%
                              mutate(consecOp = paste0(as.character(Var1),
                                                       as.character(Var2))) %>%
                              select(consecOp))
  
  for (co in consecOperators) {
    idx <- stringr::str_detect(strEquations, fixed(co))
    potentialEqForms <- potentialEqForms[!idx, ]
    strEquations <- strEquations[!idx]
  }
  displayStats(potentialEqForms)
  
  saveRDS(potentialEqForms, "potentialEqForms.rds")
  saveRDS(strEquations, "strEquations.rds")
}


# validEquations <- tibble(i = numeric(0), eqForm = character(0))
# print(paste("Start", Sys.time()))
# for (i in 1:length(strEquations)) {
#   eqForm <- strEquations[i]
#   eqPos <- stringr::str_locate(eqForm, "=")[[1]][1]
#   nDigitsInForm <- stringr::str_count(eqForm, "d")
#   dPos <- stringr::str_locate_all(eqForm, "d")[[1]][,1]
#   nonDPos <- (1:8)[!(1:8 %in% dPos)]
#   posAfterEq <- nDigitsInForm - (8 - eqPos) + 1
#   minValue <- 10 ^ (nDigitsInForm - 1)
#   maxValue <- 10 ^ nDigitsInForm - 1
#   for (v in minValue:maxValue){
#     digits <- parseDigits(v, nDigitsInForm)
#     isEquation <- testEquation(digits, eqForm) # does the equation work?
#     if (!isEquation) break()
#     if (eqPos <= 6 & digits[posAfterEq] == 0) break()
#     for(j in 1:(length(nonDPos) - 1)) { # check digit after  operators.  
#       if(digits[nonDPos[j] + 1] == 0) break()
#     }
#       validEquations <- validEquations %>% add_row(i = v, eqForm = eqForm)
#   }
#   saveRDS(validEquations, "ValidEquations.rds")
#   print(paste(i, "of", length(strEquations), "Form", eqForm, "#Rows:", nrow(validEquations), "Time:", Sys.time()))
# }
# validEquations <- validEquations %>% mutate(eqForm = factor(validEquations$eqForm)) %>% rename(Digits = i, Form = eqForm)
# saveRDS(validEquations, "ValidEquations.rds")

nCores <- detectCores()
cl <- makeCluster(pmin(nCores - 1, 15))
registerDoParallel(cl)
validEquations <- tibble(Digits = numeric(0), Form = character(0))
print(paste("Start", Sys.time()))
for (i in 1:length(strEquations)) {
  startP <- Sys.time()
  eqForm <- strEquations[i]
  eqPos <- stringr::str_locate(eqForm, "=")[[1]][1]
  nDigitsInForm <- stringr::str_count(eqForm, "d")
  posAfterEq <- nDigitsInForm - (8 - eqPos) + 1
  dPos <- stringr::str_locate_all(eqForm, "d")[[1]][,1]
  nonDPos <- (1:8)[!(1:8 %in% dPos)]
  posAfterOp <- nonDPos[1:(length(nonDPos) - 1)] + 1
  minValue <- 10 ^ (nDigitsInForm - 1)
  maxValue <- 10 ^ nDigitsInForm - 1
  eq <- numeric(8)
  partest <- foreach (v = minValue:maxValue) %dopar% {
  # partest <- list()
  # for (v in minValue:maxValue) {
    digits <- parseDigits(v, nDigitsInForm)
    isEquation <- testEquation(digits, eqForm) # does the equation work?
    isNZAfterEq <- eqPos == 7 | digits[posAfterEq] != 0
    eq[dPos] <- digits
    digitsAfterOp <- eq[posAfterOp]
    isNZAfterOp <- !(0 %in% digitsAfterOp)
    if (isEquation & isNZAfterEq & isNZAfterOp) {
      c(v, eqForm)
    }
    else {
      NULL
    }
  }
  partest <- partest[lengths(partest) != 0]
  if (length(partest) > 0) {
    partest <- data.frame(matrix(unlist(partest), nrow=length(partest), byrow=TRUE),stringsAsFactors=FALSE) %>% rename(Digits = X1, Form = X2) %>% mutate(Digits = as.numeric(Digits))
    validEquations <- rbind(validEquations, partest)  
  }
  endP <- Sys.time()
  endP - startP
  saveRDS(validEquations, "ValidEquations.rds")
  print(paste(i, "of", length(strEquations), "Form", eqForm, "#Rows:", nrow(validEquations), "Time:", Sys.time()))
}
stopCluster(cl)

dfEqChar <- Equations2Char(validEquations)
saveRDS(dfEqChar, "dfEqChar.rds")
