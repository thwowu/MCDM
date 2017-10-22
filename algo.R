
# setting location
location <- getwd()
setwd(location)

library(xlsx)
library(lpSolveAPI)


# sorting table

# normalization function
normalization <- function(max, min, x) {
    y = (x - min) / (max - min)
    # to the 3rd decimal
    return (y)
}    

norml <- function(list){
    max = max(list)
    min = min(list)
    le = length(list)
    w = matrix(0, nrow = le, ncol = 1)

    # calculate the normalized value, by calling normalization function
    for (i in 1:le) {
     	x = list[i]
     	w[i] = normalization(max, min, x)
    }
    return (w)
	#return (as.data.frame(w, col.names = "NormedNPV"))
}


dat <- read.xlsx("NPV.xlsx", sheetName="NPV")
ncol <- length(dat[,1])  # get number of the rows

# normalized NPV
dat <- cbind(dat, data.frame(NormNPV = norml(dat[,'NPV'])))

# feasibiltiy normalization
ffdat <- read.xlsx("NPV.xlsx", sheetName="feasibility")[,-1]
ffdat <- ffdat[15,]
df <- t(as.vector(subset(ffdat, select = -c(Answers.ranging.from.1.to.5) ) ))
df = as.numeric(as.character(unlist(df)))
dat <- cbind(dat, data.frame(feasibility = norml(df)) )

# synergy scoring normalization
synergy <- read.xlsx("NPV.xlsx", sheetName="synergy") # it skips the first row which has no information
library(imputeTS)
synergy = na.replace(synergy, 0) # replace all the cells (NA) into 0

rr <- nrow(synergy)
ws = matrix(0, nrow = rr, ncol = 1)	
for (nn in 1:rr){
	ws[nn] = as.numeric(synergy[nn,][-1]) %*% dat[,'NormNPV']
}

dat <- cbind(dat, data.frame(Synergy = norml(ws)))

# Coefficients of the objective function

pre_norm <- function(list) {
    max = max(list)
    min = min(list)
    le = length(list)    
    sun = sum(preference)
    ws = matrix(0, nrow = 1, ncol = le)	
    for (i in 1:le){ws[i] = (list[i]) / sun}
    return (ws)
}    

coefficients <- function(preference){
	prenorm <- pre_norm(preference)
	NameList <- dat[ ,c("NormNPV","Probability", "feasibility","Synergy") ]
	kj = matrix(0, nrow = ncol, ncol = 1)	
	for (nn in 1:ncol){
		kj[nn] = as.numeric(NameList[nn,]) %*% t(prenorm)}
	return (kj)
}

# lpSolve (binary integer programming) 
LP <- function(dat, preference, budget){
	ncol <- length(dat[,1])  # get number of the rows

	lp_rowpicker <- make.lp(ncol=ncol) # number of projects
	set.type(lp_rowpicker, columns=1:ncol, type = c("binary"))
	# settng the upper and lower bound of the solution

	######## objective function ###############
	set.objfn(lp_rowpicker, c(coefficients(preference))) 
	lp.control(lp_rowpicker,sense='max')

	######### constraints ###############
	add.constraint(lp_rowpicker, c(dat[,"CAPEX"]), "<=", budget)
	delta <- rep(0, ncol)
	delta[5] <- 1 
	delta[6] <- -1 
	add.constraint(lp_rowpicker, c(delta), ">=",  0)

	solve(lp_rowpicker)
	return(get.variables(lp_rowpicker) )
}



# genetic algorithm function

evalFunc <- function(x) {
    current_solution_NPV <- as.vector(x) %*% coefficients(preference)
    current_solution_budget <- as.vector(x) %*% dat[,'CAPEX']      
    if (current_solution_budget > budget) {return(0)} 
    else if (x[6] == 1 && x[5] == 0){return(0)} # penalty function for Delta
    else 
    	return(-current_solution_NPV)
}

genalg_f <- function(sizz, poplu, mutat){
	GAmodel <- rbga.bin(size = sizz, popSize = poplu, iters = 100, 
		 mutationChance = mutat,  elitism = T, evalFunc = evalFunc)
	position <- which.min(GAmodel$evaluations)
	return(list(GAmodel$population[position,], -min(GAmodel$evaluations)))
}


genalg <- function(sizz, poplu, mutat){
	GAmodel <- rbga.bin(size = sizz, popSize = poplu, iters = 100, 
		 mutationChance = mutat,  elitism = T, evalFunc = evalFunc)
	position <- which.min(GAmodel$evaluations)
	return(GAmodel$population[position,])
}

prob <- function(roww, ncol){

	big <- matrix(0, ncol = ncol+1 , nrow = roww)
	prob = matrix(0, nrow = ncol, ncol = 1)

	number = 0.01
	interval = 0.01

	for (ss in 1:roww) {
		chromo <- genalg_f(ncol, 100, ss)

		chromosome <- as.numeric(unlist(chromo[1]))
		cccc <- length(chromosome)

		for (nunu in 1:cccc){
			big[ss, nunu] = chromosome[nunu]}
		big[ss, cccc+1] <- as.numeric(unlist(chromo[2]))
 
		number = number + interval	
	}

	for (hh in 1:ncol){
		prob[hh,] <- sum(big[,hh]) / (roww)
		rownames(prob) <- dat[,1]
		colnames(prob) <- c("picked prob")}

	return(prob)
}


# basic input 
budget <- 10000
preference = c(10, 2, 6, 4) # (value, probability, feasibility, synergy)

# linear programming
library(lpSolveAPI)

LP_choice <- LP(dat, preference, budget)


print(dat[LP_choice == 1, ][,1]) # selected products 
return <- LP_choice %*% dat[, "NPV"] # estimed investement return

# genetic algorithm 
library(genalg)
GA_choice <- genalg(ncol, 200, 0.01)

# get probablistic result
roww = 3 # numbers of mutation increase for this
k <- which(prob(10,14) > 0.5)
GA_choice <- rep(0,14)
GA_choice[c(k)] <- 1


throwin = function(matrix, position, list){
	for (kk in 1:length(list)) {
		matrix[position, kk] <- list[kk]
	}
	return (matrix)
}

# outpout formulation 
output <- matrix(0, nrow = 2, ncol = ncol + 2)
output[1] 

cs1 <- c(LP_choice, (LP_choice %*% dat[, "CAPEX"]), (LP_choice %*% dat[, "NPV"]))
output <- throwin(output, 1, cs1)
cs2 <- c(GA_choice, (GA_choice %*% dat[, "CAPEX"]), (GA_choice %*% dat[, "NPV"]))
output <- throwin(output, 2, cs2)

colnames(output) <- c(as.character(dat[,1]), "CAPEX", "NPV")
rownames(output) <- c("LP", "GA")

weight <- pre_norm(preference) * 100
sd <- format(c("NPV", "Probability", "feasibility", "synergy"), justify = "centre")
rownames(weight) <- c("%")
colnames(weight) <- sd
print(weight)

range1 <- c(10, 7) # [max, min] for NPV importance
range2 <- c(1, 0) # [max, min] for probability importance
range3 <- c(4, 2) # [max, min] for feasibility importance
range4 <- c(2, 2) # [max, min] for synergy importance

first_max = range1[1] + 1
first_min = range1[2] +1 
first = first_max - first_min + 1 

second_max = range2[1] + 1 
second_min = range2[2] + 1 
second = second_max - second_min + 1

third_max = range3[1] + 1
third_min = range3[2] + 1
third = third_max - third_min + 1

fourth_max = range4[1] + 1 
fourth_min = range4[2] + 1
fourth = fourth_max - fourth_min + 1

combination = matrix(0, ncol = 4, nrow = first*second*third*fourth)
step = 1

for (one in first_min:first_max ){
	for (two in second_min:second_max){
		for (three in third_min:third_max){
			for (four in fourth_min:fourth_max){
				onon <- one -1 
				twtw <- two -1 
				thth <- three - 1
				fofo <- four - 1
				number = c(onon, twtw, thth, fofo)
				for (ni in 1:4){
				    combination[step, ni] = number[ni]
				}
				step = step + 1
			}
		}
	}
}
print(combination)


