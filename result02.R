# linear programming (2)

# Start writing to an output file
sink('analysis-output-lpcompare.txt')
# preference
range1 <- c(5, 10) # [MIN, MAX] for NPV importance
range2 <- c(1, 1) # [MIN, MAX] for probability importance
range3 <- c(1, 3) # [MIN, MAX] for feasibility importance
range4 <- c(1, 3) # [MIN, MAX] for synergy importance
combination <- grid(range1, range2, range3, range4)

np <- nrow(dat)
np2 <- nrow(combination) - 6
cm <- prob_LP_compare(dat, combination, budgd)
ln <- probsit_latex(dat, cm)

# Do some stuff here
cat("=================================================================\n")
cat(sprintf("Bounds of Preference Score (from 0 to 10) for %d digital products\n", np))
cat("=================================================================\n")
print(grid_para(range1, range2, range3, range4))

cat("\n\n======================================================\n")
cat(" Digital product investment suggestion by combination \n")
cat("======================================================\n")
print(cm[,1:(ncol(cm)-2)])

cat("\n\n======================================================\n")
cat("Probability of digital product investment decision (%)\n")
cat("======================================================\n")
print(ln[,1]*100)
cat(sprintf("(from %d results by criteria combinations)\n\n", np2+6))

# Stop writing to the file
sink()

# Append to the file
sink('analysis-output.txt', append=TRUE)
sink()


# Genetic algorithm (2)

# Start writing to an output file
sink('analysis-output-ga1.txt')
range1 <- c(5, 10) # [MIN, MAX] for NPV importance
range2 <- c(1, 2) # [MIN, MAX] for probability importance
range3 <- c(1, 3) # [MIN, MAX] for feasibility importance
range4 <- c(1, 3) # [MIN, MAX] for synergy importance
combination <- grid(range1, range2, range3, range4)

np <- nrow(dat)
np2 <- nrow(combination) - 6
cm <- prob_GA_latex(dat, combination, budgd)
ln <- probsit_latex(dat, cm)

# Do some stuff here
cat("=================================================================\n")
cat(sprintf("Bounds of Preference Score (from 0 to 10) for %d digital products\n", np))
cat("=================================================================\n")
print(grid_para(range1, range2, range3, range4))

cat("\n\n======================================================\n")
cat("Probability of digital product investment decision (%)\n")
cat("======================================================\n")
print(ln[,1]*100)
cat(sprintf("(from %d results by criteria combinations)\n\n", np2+6))

# Stop writing to the file
sink()

# Append to the file
sink('analysis-output.txt', append=TRUE)
sink()


# Genetic (1)

# Start writing to an output file
sink('analysis-output-ga2.txt')
range1 <- c(5, 10) # [MIN, MAX] for NPV importance
range2 <- c(1, 2) # [MIN, MAX] for probability importance
range3 <- c(1, 5) # [MIN, MAX] for feasibility importance
range4 <- c(1, 5) # [MIN, MAX] for synergy importance
combination <- grid(range1, range2, range3, range4)

np <- nrow(dat)
np2 <- nrow(combination) - 6
cm <- prob_GA_latex(dat, combination, budgd)
ln <- probsit_latex(dat, cm)

# Do some stuff here
cat("=================================================================\n")
cat(sprintf("Bounds of Preference Score (from 0 to 10) for %d digital products\n", np))
cat("=================================================================\n")
print(grid_para(range1, range2, range3, range4))

cat("\n\n======================================================\n")
cat("Probability of digital product investment suggestion (%)\n")
cat("======================================================\n")
print(ln[,1]*100)
cat(sprintf("(from %d results by criteria combinations)\n\n", np2+6))

# Stop writing to the file
sink()

# Append to the file
sink('analysis-output.txt', append=TRUE)
sink()



# Hybrid (1)

# Start writing to an output file

sink('analysis-output-hybrid2.txt')

budgd <- 10000
range1 <- c(5, 10) # [MIN, MAX] for NPV importance
range2 <- c(1, 2) # [MIN, MAX] for probability importance
range3 <- c(1, 5) # [MIN, MAX] for feasibility importance
range4 <- c(1, 5) # [MIN, MAX] for synergy importance
combination <- grid(range1, range2, range3, range4)

np <- nrow(dat)
np2 <- nrow(combination)
cm <- prob_both_latex(dat, combination, budgd)
ln <- probsit_latex(dat, cm)

# Do some stuff here
cat("=================================================================\n")
cat(sprintf("Bounds of Preference Score (from 0 to 10) for %d digital products\n", np))
cat("=================================================================\n")
print(grid_para(range1, range2, range3, range4))

cat("\n\n======================================================\n")
cat("Probability of digital product investment decision (%)\n")
cat("======================================================\n")
print(ln[,1]*100)
cat(sprintf("(from %d results by criteria combinations)\n\n", np2))

# Stop writing to the file
sink()

# Append to the file
sink('analysis-output.txt', append=TRUE)
sink()


