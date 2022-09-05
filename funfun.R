understand_nature <- function(x, base = length(letters) + 1, seed = log(x) %/% log(base)) {
  
  if (x %/% base^seed != 0) {
    
    c(x %/% base^seed, understand_nature(x %% base^seed, seed = seed - 1, base = base))
    
  }
  
}

# initialize system

researcher <- c(tw = 413181, de = 11155887)
microscopy <- wildtype <- mutant <- 0
FACS <- 1

vars <- list()

# simple question

answer <- sapply(researcher, understand_nature); answer / 2  # Check!
print("Who?"); apply(answer, 2, function(w) paste0(c(letters[w], "!"), collapse = ""))

# next level (= learned R)

funfun <- function(x) paste0(letters[understand_nature(x)], collapse = "")
funfun <- Vectorize(funfun)

# entire process (need anyways)

# watch https://www.youtube.com/watch?v=wy5L868wzuU

while (microscopy != FACS & mutant == wildtype) {
  
  print(sprintf("FACS #%i", FACS))
  
  mutant <- sample(c(rep(0, 9), 1), 1)  # makes later failure less likely
  FACS <- FACS + 1
  
  vars[[FACS]] <- c(11884, 45927, 49752, 42795)[[FACS %% 3 + 1]] * (FACS %% 3 + 1)
  
  plot(rnorm(n = FACS), rnorm(n = FACS), main = "FACS", 
       xlab = "wildtype", ylab = "mutant")  # R!
  
  Sys.sleep(2)
  
  if (mutant != wildtype & FACS < 4) {print("Close enough."); mutant <- wildtype}
  if (mutant == wildtype & FACS > 1) {print("Let's repeat one more time!")}
  
}

MBD <- c(1:4, funfun(c(researcher[[2]], vars[[3]])))
TET <- c(rep("year", 4), funfun(vars[c(4, 5)]))
  
# result

funfun(c(8301531, 4359666, 37404245691, 125524238436))
interaction(TET, MBD, drop = T)
