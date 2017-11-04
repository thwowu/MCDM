# probability to 0

# Start writing to an output file
sink('analysis-1or0-lp.txt')

# basic input 
budgd <- 10000

# preference
range1 <- c(5, 10) # [MIN, MAX] for NPV importance
range2 <- c(0, 0) # [MIN, MAX] for probability importance
range3 <- c(1, 3) # [MIN, MAX] for feasibility importance
range4 <- c(1, 3) # [MIN, MAX] for synergy importance
combination <- grid(range1, range2, range3, range4)

np <- nrow(dat)
cm <- prob_LP(dat, combination, budgd)

comb1 <- nrow(combination)
ln1 <- probsit_latex(dat, cm)
bes1 <- best(dat, cm)


range1 <- c(5, 10) # [MIN, MAX] for NPV importance
range2 <- c(0, 0) # [MIN, MAX] for probability importance
range3 <- c(1, 5) # [MIN, MAX] for feasibility importance
range4 <- c(1, 5) # [MIN, MAX] for synergy importance
combination <- grid(range1, range2, range3, range4)

np <- nrow(dat)
cm <- prob_LP(dat, combination, budgd)

comb2 <- nrow(combination)
ln2 <- probsit_latex(dat, cm)
bes2 <- best(dat, cm)

# Do some stuff here
cat("======================================================\n")
cat("Optimal investment decision with ROI                  \n")
cat("======================================================\n")
cat("scenario 1: ")
cat(trim(toString( noquote(bes1))))
cat("\nscenario 2: ")
cat(trim(toString( noquote(bes2))))


cat("\n\n======================================================\n")
cat("Probability of digital product investment decision (%)\n")
cat("======================================================\n")
print(ln1[,1]*100)
cat(sprintf("(scenario 1 from %d results by criteria combinations)\n\n", comb1))
print(ln2[,1]*100)
cat(sprintf("(scenario 2 from %d results by criteria combinations)\n\n", comb2))


# Stop writing to the file
sink()

# Append to the file
sink('analysis-output.txt', append=TRUE)
sink()


# Start writing to an output file
sink('analysis-1or0-ga.txt')
# preference
range1 <- c(5, 10) # [MIN, MAX] for NPV importance
range2 <- c(0, 0) # [MIN, MAX] for probability importance
range3 <- c(1, 3) # [MIN, MAX] for feasibility importance
range4 <- c(1, 3) # [MIN, MAX] for synergy importance
combination <- grid(range1, range2, range3, range4)

np <- nrow(dat)
cm <- prob_GA(dat, combination, budgd)

comb1 <- nrow(combination)
ln1 <- probsit_latex(dat, cm)
bes1 <- best(dat, cm)


range1 <- c(5, 10) # [MIN, MAX] for NPV importance
range2 <- c(0, 0) # [MIN, MAX] for probability importance
range3 <- c(1, 5) # [MIN, MAX] for feasibility importance
range4 <- c(1, 5) # [MIN, MAX] for synergy importance
combination <- grid(range1, range2, range3, range4)

np <- nrow(dat)
cm <- prob_GA(dat, combination, budgd)

comb2 <- nrow(combination)
ln2 <- probsit_latex(dat, cm)
bes2 <- best(dat, cm)

# Do some stuff here
cat("======================================================\n")
cat("Optimal investment decision with ROI                  \n")
cat("======================================================\n")
cat("scenario 1: ")
cat(trim(toString( noquote(bes1))))
cat("\nscenario 2: ")
cat(trim(toString( noquote(bes2))))


cat("\n\n======================================================\n")
cat("Probability of digital product investment decision (%)\n")
cat("======================================================\n")
print(ln1[,1]*100)
cat(sprintf("(scenario 1 from %d results by criteria combinations)\n\n", comb1))
print(ln2[,1]*100)
cat(sprintf("(scenario 2 from %d results by criteria combinations)\n\n", comb2))


# Stop writing to the file
sink()

# Append to the file
sink('analysis-output.txt', append=TRUE)
sink()



# feasibility to 0

# Start writing to an output file
sink('analysis-1or0f-lp.txt')
# preference
ptm <- proc.time()
range1 <- c(5, 10) # [MIN, MAX] for NPV importance
range2 <- c(1, 2) # [MIN, MAX] for probability importance
range3 <- c(0, 0) # [MIN, MAX] for feasibility importance
range4 <- c(1, 3) # [MIN, MAX] for synergy importance
combination <- grid(range1, range2, range3, range4)

np <- nrow(dat)
cm <- prob_LP(dat, combination, budgd)

comb1 <- nrow(combination)
ln1 <- probsit_latex(dat, cm)
bes1 <- best(dat, cm)


range1 <- c(5, 10) # [MIN, MAX] for NPV importance
range2 <- c(1, 2) # [MIN, MAX] for probability importance
range3 <- c(0, 0) # [MIN, MAX] for feasibility importance
range4 <- c(1, 5) # [MIN, MAX] for synergy importance
combination <- grid(range1, range2, range3, range4)

np <- nrow(dat)
cm <- prob_LP(dat, combination, budgd)

comb2 <- nrow(combination)
ln2 <- probsit_latex(dat, cm)
bes2 <- best(dat, cm)

# Do some stuff here
cat("======================================================\n")
cat("Optimal investment decision with ROI                  \n") 
cat("======================================================\n")
cat("scenario 1: ")
cat(trim(toString( noquote(bes1))))
cat("\nscenario 2: ")
cat(trim(toString( noquote(bes2))))


cat("\n\n======================================================\n")
cat("Probability of digital product investment decision (%)\n")
cat("======================================================\n")
print(ln1[,1]*100)
cat(sprintf("(scenario 1 from %d results by criteria combinations)\n\n", comb1))
print(ln2[,1]*100)
cat(sprintf("(scenario 2 from %d results by criteria combinations)\n\n", comb2))


# Stop writing to the file
sink()

# Append to the file
sink('analysis-output.txt', append=TRUE)
sink()


# Start writing to an output file
sink('analysis-1or0f-ga.txt')
# preference
range1 <- c(5, 10) # [MIN, MAX] for NPV importance
range2 <- c(1, 2) # [MIN, MAX] for probability importance
range3 <- c(0, 0) # [MIN, MAX] for feasibility importance
range4 <- c(1, 3) # [MIN, MAX] for synergy importance
combination <- grid(range1, range2, range3, range4)

np <- nrow(dat)
cm <- prob_GA(dat, combination, budgd)

comb1 <- nrow(combination)
ln1 <- probsit_latex(dat, cm)
bes1 <- best(dat, cm)


range1 <- c(5, 10) # [MIN, MAX] for NPV importance
range2 <- c(1, 2) # [MIN, MAX] for probability importance
range3 <- c(0, 0) # [MIN, MAX] for feasibility importance
range4 <- c(1, 5) # [MIN, MAX] for synergy importance
combination <- grid(range1, range2, range3, range4)

np <- nrow(dat)
cm <- prob_GA(dat, combination, budgd)

comb2 <- nrow(combination)
ln2 <- probsit_latex(dat, cm)
bes2 <- best(dat, cm)

# Do some stuff here
cat("======================================================\n")
cat("Optimal investment decision with ROI                  \n")
cat("======================================================\n")
cat("scenario 1: ")
cat(trim(toString( noquote(bes1))))
cat("\nscenario 2: ")
cat(trim(toString( noquote(bes2))))


cat("\n\n======================================================\n")
cat("Probability of digital product investment decision (%)\n")
cat("======================================================\n")
print(ln1[,1]*100)
cat(sprintf("(scenario 1 from %d results by criteria combinations)\n\n", comb1))
print(ln2[,1]*100)
cat(sprintf("(scenario 2 from %d results by criteria combinations)\n\n", comb2))


# Stop writing to the file
sink()

# Append to the file
sink('analysis-output.txt', append=TRUE)
sink()






# synergy to 0

# Start writing to an output file
sink('analysis-1or0s-lp.txt')
# preference
range1 <- c(5, 10) # [MIN, MAX] for NPV importance
range2 <- c(1, 2) # [MIN, MAX] for probability importance
range3 <- c(1, 3) # [MIN, MAX] for feasibility importance
range4 <- c(0, 0) # [MIN, MAX] for synergy importance
combination <- grid(range1, range2, range3, range4)

np <- nrow(dat)
cm <- prob_LP(dat, combination, budgd)

comb1 <- nrow(combination)
ln1 <- probsit_latex(dat, cm)
bes1 <- best(dat, cm)


range1 <- c(5, 10) # [MIN, MAX] for NPV importance
range2 <- c(1, 2) # [MIN, MAX] for probability importance
range3 <- c(1, 5) # [MIN, MAX] for feasibility importance
range4 <- c(0, 0) # [MIN, MAX] for synergy importance
combination <- grid(range1, range2, range3, range4)

np <- nrow(dat)
cm <- prob_LP(dat, combination, budgd)

comb2 <- nrow(combination)
ln2 <- probsit_latex(dat, cm)
bes2 <- best(dat, cm)

# Do some stuff here
cat("======================================================\n")
cat("Optimal investment decision with ROI                  \n")
cat("======================================================\n")
cat("scenario 1: ")
cat(trim(toString( noquote(bes1))))
cat("\nscenario 2: ")
cat(trim(toString( noquote(bes2))))


cat("\n\n======================================================\n")
cat("Probability of digital product investment decision (%)\n")
cat("======================================================\n")
print(ln1[,1]*100)
cat(sprintf("(scenario 1 from %d results by criteria combinations)\n\n", comb1))
print(ln2[,1]*100)
cat(sprintf("(scenario 2 from %d results by criteria combinations)\n\n", comb2))


# Stop writing to the file
sink()

# Append to the file
sink('analysis-output.txt', append=TRUE)
sink()


# Start writing to an output file
sink('analysis-1or0s-ga.txt')
# preference
range1 <- c(5, 10) # [MIN, MAX] for NPV importance
range2 <- c(1, 2) # [MIN, MAX] for probability importance
range3 <- c(1, 3) # [MIN, MAX] for feasibility importance
range4 <- c(0, 0) # [MIN, MAX] for synergy importance
combination <- grid(range1, range2, range3, range4)

np <- nrow(dat)
cm <- prob_GA(dat, combination, budgd)

comb1 <- nrow(combination)
ln1 <- probsit_latex(dat, cm)
bes1 <- best(dat, cm)


range1 <- c(5, 10) # [MIN, MAX] for NPV importance
range2 <- c(1, 2) # [MIN, MAX] for probability importance
range3 <- c(1, 5) # [MIN, MAX] for feasibility importance
range4 <- c(0, 0) # [MIN, MAX] for synergy importance
combination <- grid(range1, range2, range3, range4)

np <- nrow(dat)
cm <- prob_GA(dat, combination, budgd)

comb2 <- nrow(combination)
ln2 <- probsit_latex(dat, cm)
bes2 <- best(dat, cm)

# Do some stuff here
cat("======================================================\n")
cat("Optimal investment decision with ROI                  \n")
cat("======================================================\n")
cat("scenario 1: ")
cat(trim(toString( noquote(bes1))))
cat("\nscenario 2: ")
cat(trim(toString( noquote(bes2))))


cat("\n\n======================================================\n")
cat("Probability of digital product investment decision (%)\n")
cat("======================================================\n")
print(ln1[,1]*100)
cat(sprintf("(scenario 1 from %d results by criteria combinations)\n\n", comb1))
print(ln2[,1]*100)
cat(sprintf("(scenario 2 from %d results by criteria combinations)\n\n", comb2))


# Stop writing to the file
sink()

# Append to the file
sink('analysis-output.txt', append=TRUE)
sink()




# return to 0


# Start writing to an output file
sink('analysis-1or0r-lp.txt')


# preference
range1 <- c(0, 0) # [MIN, MAX] for NPV importance
range2 <- c(1, 2) # [MIN, MAX] for probability importance
range3 <- c(1, 3) # [MIN, MAX] for feasibility importance
range4 <- c(1, 3) # [MIN, MAX] for synergy importance
combination <- grid(range1, range2, range3, range4)

np <- nrow(dat)
cm <- prob_LP(dat, combination, budgd)

comb1 <- nrow(combination)
ln1 <- probsit_latex(dat, cm)
bes1 <- best(dat, cm)


range1 <- c(0, 0) # [MIN, MAX] for NPV importance
range2 <- c(1, 2) # [MIN, MAX] for probability importance
range3 <- c(1, 5) # [MIN, MAX] for feasibility importance
range4 <- c(1, 5) # [MIN, MAX] for synergy importance
combination <- grid(range1, range2, range3, range4)

np <- nrow(dat)
cm <- prob_LP(dat, combination, budgd)

comb2 <- nrow(combination)
ln2 <- probsit_latex(dat, cm)
bes2 <- best(dat, cm)

# Do some stuff here
cat("======================================================\n")
cat("Optimal investment decision with ROI                  \n")
cat("======================================================\n")
cat("scenario 1: ")
cat(trim(toString( noquote(bes1))))
cat("\nscenario 2: ")
cat(trim(toString( noquote(bes2))))


cat("\n\n======================================================\n")
cat("Probability of digital product investment decision (%)\n")
cat("======================================================\n")
print(ln1[,1]*100)
cat(sprintf("(scenario 1 from %d results by criteria combinations)\n\n", comb1))
print(ln2[,1]*100)
cat(sprintf("(scenario 2 from %d results by criteria combinations)\n\n", comb2))


# Stop writing to the file
sink()

# Append to the file
sink('analysis-output.txt', append=TRUE)
sink()


# Start writing to an output file
sink('analysis-1or0r-ga.txt')
# preference
range1 <- c(0, 0) # [MIN, MAX] for NPV importance
range2 <- c(1, 2) # [MIN, MAX] for probability importance
range3 <- c(1, 3) # [MIN, MAX] for feasibility importance
range4 <- c(1, 3) # [MIN, MAX] for synergy importance
combination <- grid(range1, range2, range3, range4)

np <- nrow(dat)
cm <- prob_GA(dat, combination, budgd)

comb1 <- nrow(combination)
ln1 <- probsit_latex(dat, cm)
bes1 <- best(dat, cm)

range1 <- c(0, 0) # [MIN, MAX] for NPV importance
range2 <- c(1, 2) # [MIN, MAX] for probability importance
range3 <- c(1, 5) # [MIN, MAX] for feasibility importance
range4 <- c(1, 5) # [MIN, MAX] for synergy importance
combination <- grid(range1, range2, range3, range4)

np <- nrow(dat)
cm <- prob_GA(dat, combination, budgd)

comb2 <- nrow(combination)
ln2 <- probsit_latex(dat, cm)
bes2 <- best(dat, cm)

# Do some stuff here
cat("======================================================\n")
cat("Optimal investment decision with ROI                  \n")
cat("======================================================\n")
cat("scenario 1: ")
cat(trim(toString( noquote(bes1))))
cat("\nscenario 2: ")
cat(trim(toString( noquote(bes2))))


cat("\n\n======================================================\n")
cat("Probability of digital product investment decision (%)\n")
cat("======================================================\n")
print(ln1[,1]*100)
cat(sprintf("(scenario 1 from %d results by criteria combinations)\n\n", comb1))
print(ln2[,1]*100)
cat(sprintf("(scenario 2 from %d results by criteria combinations)\n\n", comb2))


# Stop writing to the file
sink()

# Append to the file
sink('analysis-output.txt', append=TRUE)
sink()
