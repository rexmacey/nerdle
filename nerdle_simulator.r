# nerdle Simulator

library(dplyr)
source("C:/Users/xer09/OneDrive/Nerdle/nerdlefunctions.r")
# dfEquations <- readRDS("C:/Users/xer09/OneDrive/Nerdle/validEquations.rds")
# dfEqChar <- Equations2Char(dfEquations)
# saveRDS(dfEqChar, "dfEqChar.rds")
dfEqChar <- readRDS("C:/Users/xer09/OneDrive/Nerdle/dfEqChar.rds")


# randomly pick an equation
set.seed(101)
eqToFind <- dfEqChar[sample(1:nrow(dfEqChar), 1), ]
eqToFind
sim <- list()
sim$EquationToFind <- eqToFind
sim$guesses <- data.frame(p1 = character(6), p2 = character(6), p3 = character(6), 
                      p4 = character(6), p5 = character(6), p6 = character(6), 
                      p7 = character(6), p8 = character(6))
sim$comparisons <- data.frame(p1 = numeric(6), p2 = numeric(6), p3 = numeric(6), 
                          p4 = numeric(6), p5 = numeric(6), p6 = numeric(6), 
                          p7 = numeric(6), p8 = numeric(6))
sim$Success <- FALSE
remainingEquations <- dfEqChar
for (i in 1:6) {
  guess <- findBestEquation(remainingEquations)
  sim$guesses[i, 1:8] <- guess
  comparison <- compareGuessToEquation(guess,eqToFind)
  sim$comparisons[i, 1:8] <- comparison
  eqToFind
  guess
  comparison
  if (sum(comparison) == 8) {
    print(paste("Win on guess", i, "with", guess))
    sim$Success <- TRUE
    break()
  }
  if (sum(comparison == 0) > 0) remainingEquations <- mustHaveNotInPos(unlist(guess[comparison == 0]), (1:8)[comparison == 0], remainingEquations)
  if (sum(comparison == 1) > 0) remainingEquations <- mustHaveInPos(unlist(guess[comparison == 1]), (1:8)[comparison == 1], remainingEquations)
  
  # only exclude when the digit is used once in the guess
  for (j in (1:8)[comparison == -1]) {
    ndjInGuess <- sum(guess %in% guess[j])
    ndjToExcl <- sum(guess[(1:8)[comparison == -1]] == as.integer(guess[j]))
    if (ndjInGuess == ndjToExcl) {
      remainingEquations <- excld(guess[j], remainingEquations)
    } else {
      remainingEquations <- excldByPos(guess[j], j, remainingEquations)
    }
  }
}



