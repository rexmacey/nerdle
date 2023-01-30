# functions for nerdle analysis

eqForms <- c(ddPddEdd = "dd+dd=dd",
             dd_ddEdd = "dd-dd=dd",
             ddd_ddEd = "ddd-dd=d",
             dddDddEd = "ddd/dd=d",
             dPddEddd = "d+dd=ddd",
             dTddEddd = "d*dd=ddd",
             dPd_ddEd = "d+d-dd=d",
             d_dPddEd = "d-d+dd=d",
             dTd_ddEd = "d*d-dd=d",
             ddTdDdEd = "dd*d/d=d",
             dTdDddEd = "d*d/dd=d",
             dddDdEdd = "ddd/d=dd",
             dTdTdEdd = "d*d*d=dd",
             ddd_dEdd = "ddd-d=dd",
             dPdPdEdd = "d+d+d=dd",
             dPd_dEdd = "d+d-d=dd",
             dd_d_dEd = "dd-d-d=d",
             dTdPdEdd = "d*d+d=dd",
             dTdDdEdd = "d*d/d=dd",
             ddMdPdEd = "dd-d+d=d",
             ddMdTdEd = "dd-d*d=d",
             dPdDdEdd = "d+d/d=dd",
             dDdPdEdd = "d/d+d=dd")

dfEqForms <- tibble(Form = names(eqForms), Equations = eqForms) %>%
  arrange(Equations)

findEquationsOfForm1 <- function(eqForm) {
  nDigitsInForm <- stringr::str_count(eqForm, "d")
  maxValue <- 10 ^ nDigitsInForm - 1
  out <- vector("list", maxValue + 1)
  ctr <- 1
  for (i in 0:maxValue){
    digits <- parseDigits(i, nDigitsInForm)
    if(testEquation(digits, eqForm)){
      out[ctr] <- unParseDigits(digits)
      ctr <- ctr + 1
    }
  }
  out <- purrr::compact(out)
  return(out)
}

parseDigits <- function(i, nDigits = NULL) {
  # parse an integer (eg 123456) into vector of digits (eg 1 2 3 4 5 6)
  if (is.null(nDigits)) {
    if (i >= 100000) {
      nDigits <- 6
    } else {
      nDigits <- 5
    }
  }
  out <- integer(nDigits)
  remainder <- i
  for (i in 1:nDigits) {
    out[i] <- floor(remainder / 10 ^ (nDigits - i))
    remainder <- remainder - out[i] * 10 ^ (nDigits - i)
  }
  return(out)
}

unParseDigits <- function(digits) {
  # convert vector of digits into integer
  n <- length(digits)
  out <- 0
  for (i in 1:n) {
    out <- out + digits[i] * 10 ^ (n - i)
  }
  return(out)
}

testEquation <- function(digits, eqForm) {
  # return true if valid equation, otherwise false
  nDigitsInForm <- stringr::str_count(eqForm, "d")
  if(nDigitsInForm != length(digits)) stop(paste("Error in testEquation. # digits in ", digits, 
                                                 "does not match", eqForm))
  eqPos <- stringr::str_locate(eqForm, "=")[1]
  dPos <- stringr::str_locate_all(eqForm, "d")[[1]][,1]
  equation <- eqForm
  for(i in 1:nDigitsInForm){
    equation <- stringr::str_replace(equation, "d", as.character(digits[i]))
  }
  lhs <- eval(parse(text = substr(equation, 1, eqPos - 1)))
  rhs <-eval(parse(text = stringr::str_sub(equation, -(nchar(equation) - eqPos), -1)))
  if(is.na(lhs == rhs)) {
    return (FALSE)
  } else {
    return(lhs == rhs)
  }
}

Equations2Char <- function(dfEquations) {
  nEq <- nrow(dfEquations)
  dfEqChar <- data.frame(p1 = character(nEq),
                         p2 = character(nEq),
                         p3 = character(nEq),
                         p4 = character(nEq),
                         p5 = character(nEq),
                         p6 = character(nEq),
                         p7 = character(nEq),
                         p8 = character(nEq))
  
  # convert Digits and Form to 8 character equation
  for (i in 1:nEq){
    eq <- dfEquations[i,]
    dloc <- stringr::str_locate_all(eq$Form, "d")[[1]][,1]
    dfEqChar[i,] <- unlist(strsplit(as.character(eq$Form),""))
    dfEqChar[i, dloc] <- as.character(parseDigits(eq$Digits))
  }  
  return(dfEqChar)
}

countByChar <- function(df) {
  out <- data.frame(Char = c(as.character(0:9), "+","-","*","/","="),
                    p1 = numeric(15),
                    p2 = numeric(15),
                    p3 = numeric(15),
                    p4 = numeric(15),
                    p5 = numeric(15),
                    p6 = numeric(15),
                    p7 = numeric(15),
                    p8 = numeric(15),
                    Total = numeric(15))
  for (i in 1:nrow(out)){
    ch <- out$Char[i]
    for (j in 1:8){
      out[i, 1 + j] <- sum(df[, j] == ch)
    }
  }
  out$Total <- apply(out[,2:9], 1, sum)
  return(out)
}

findBestEquation <- function(dfEqChar) {
  guess <- character(8)
  filteredEq <- dfEqChar
  charCnt <- countByChar(filteredEq)
  posRemaining <- 1:8
  nCharUsed <- data.frame(Char = charCnt[, "Char"],
                          n = integer(nrow(charCnt)))
  while (sum(guess == "") > 0 & nrow(filteredEq) > 1) {
    maxValue <- max(charCnt[, posRemaining + 1])
    maxValueIdx <- which(charCnt[, posRemaining + 1] == maxValue, arr.ind = TRUE)
    if (nrow(maxValueIdx) == 1) {
      maxChar <- charCnt[maxValueIdx[1, "row"], "Char"] # charCnt[maxValueIdx[1, "row"], "Char"]
      maxColName <- names(charCnt[, posRemaining + 1])[maxValueIdx[1, "col"]]
    } else {
      idxRowToUse <- which.min(nCharUsed[maxValueIdx[, 1], "n"])
      charCntRowToUse <- maxValueIdx[idxRowToUse, "row"]
      idxColToUse <- maxValueIdx[idxRowToUse, "col"]
      maxChar <-  charCnt[, ][charCntRowToUse, "Char"] 
      maxColName <- names(charCnt[, posRemaining + 1])[idxColToUse] # (posRemaining + 1)[maxValueIdx[which.min(nCharUsed[maxValueIdx[, 1], "n"]), "col"]] 
    }
    maxColNum <- as.integer(substr(maxColName, 2,2))
    guess[maxColNum] <- maxChar
    filteredEq <- filteredEq %>% filter(!!as.symbol(maxColName) == maxChar)
    charCnt <- countByChar(filteredEq)
    charCnt
    posRemaining <- posRemaining[-which(posRemaining == maxColNum)]
    nCharUsed[nCharUsed$Char == maxChar, "n"] <-  nCharUsed[nCharUsed$Char == maxChar, "n"] + 1
  }
  return(filteredEq[1,])  
}
# find best equation for a guess
# start with most common location of the = sign
# for each unique digit in an equation, add total values
# score for equation is the sum of the totals
# choose equation with high score.
# findBestEquation3 <- function(dfEqChar) {
#   temp <- countByChar(dfEqChar)
#   # where is = most common
#   bestEqualLoc <- temp %>% filter(Char == "=") %>% 
#     select(p1, p2, p3, p4, p5, p6, p7, p8)
#   bestEqualLoc <-  names(which.max(bestEqualLoc))
#   temp1 <- dfEqChar %>% filter(!!as.symbol(bestEqualLoc) == "=")
#   out <- 1
#   highScore <- -Inf
#   for (i in 1:nrow(temp1)) {
#     temp2 <- temp1[i,]
#     digitsInNumber <- (0:9)[(0:9) %in% (temp2[1, temp2 %in% (0:9)])]
#     temp <- countByChar(temp1)
#     score <- sum(temp[digitsInNumber + 1, 10])  
#     if (score > highScore) {
#       highScore <- score
#       out <- i
#     }
#   }
#   return(temp1[out,])
# }

excld <- function(d, df) {
  # remove equations with d in them.  d should be a char.  df has 8 char cols
  for (i in 1:length(d)) {
    df <- df %>% filter(p1 != d[i] & p2 != d[i] & p3 != d[i] & p4 != d[i] &
                          p5 != d[i] & p6 != d[i] & p7 != d[i] & p8 != d[i])
  }
  return(df)
}

excldByPos <- function(d, xPos, df) {
  if (length(d) != length(xPos)) stop("Error in exclByPos. Mismatch length of d and xPos")
  for (i in 1:length(d)) {
    df <- df[df[, xPos[i]] != d[i], ]
  }
  return(df)
}

incld <- function(d, df) {
  # must incl d somewhere.  d should be a char.  df has 8 char cols
  for (i in 1:length(d)) {
    df <- df %>% filter(p1 == d[i] | p2 == d[i] | p3 == d[i] | p4 == d[i] |
                          p5 == d[i] | p6 == d[i] | p7 == d[i] | p8 == d[i])
  }
  return(df)
}

mustHaveNotInPos <- function(d, xPos, df) {
  # must have d.  d may not be in xPos. d is char vec and xPos is int vec
  if (length(d) != length(xPos)) stop("Error in mustHaveNotInPos. Mismatch length of d and xPos")
  for (i in 1:length(d)) {
    df <- incld(d[i], df)
    df <- df[df[, xPos[i]] != d[i], ]
  }
  return(df)
}

mustHaveInPos <- function(d, xPos, df) {
  # must have d in position xPos. d is char vec and xPos is int vec
  if (length(d) != length(xPos)) stop("Error in mustHaveNotInPos. Mismatch length of d and xPos")
  for (i in 1:length(d)) {
    df <- df[df[, xPos[i]] == d[i], ]
  }
  return(df)
}

compareGuessToEquation <- function(guess, equation){
  # -1 Not in equation (black)
  # 0  In equation, wrong place (pink)
  # 1  In equation, correct spot
  out <- integer(8)
  for (i in 1:length(guess)) {
    if (guess[i] == equation[i]) {
      out[i] <- 1
    } else {
      if (guess[i] %in% equation) {
        out[i] <- 0
      } else {
        out[i] <- -1
      }
    }
  }
  return(out)  
}
