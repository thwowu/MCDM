# setting location
location <- getwd()
setwd(location)

library(xlsx)
library(lpSolveAPI)
library(genalg)

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
    sun = sum(list)
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
LP <- function(dat, budget){
	ncol <- length(dat[,1])  # get number of the rows

	lp_rowpicker <- make.lp(ncol=ncol) # number of projects
	set.type(lp_rowpicker, columns=1:ncol, type = c("binary"))
	# settng the upper and lower bound of the solution

	######## objective function ###############
	set.objfn(lp_rowpicker, c(coof)) 
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

genalg <- function(data, sizz, mutat, budget){

	assign("daa", data, envir = .GlobalEnv) 
	assign("budd", budget, envir = .GlobalEnv)

	GAmodel <- rbga.bin(size = sizz, popSize = 100, iters = 100, 
		 mutationChance = mutat,  elitism = T, evalFunc = evalF)
	position <- which.min(GAmodel$evaluations)
	return(GAmodel$population[position,])
}

evalF <- function(x) {
	    current_solution_NPV <- as.vector(x) %*% coof
	    current_solution_budget <- as.vector(x) %*% daa[,"CAPEX"]
	    if (current_solution_budget > budd) {return(0)} 
	    else if (x[6] == 1 && x[5] == 0){return(0)} # penalty function for Delta
	    else 
	    	return(-current_solution_NPV)
}	

throwin = function(matrix, position, list){
	for (kk in 1:length(list)) {
		matrix[position, kk] <- list[kk]
	}
	return (matrix)
}

grid <- function(range1, range2, range3, range4){

	first_max = range1[2] + 1
	first_min = range1[1] +1 
	first = first_max - first_min + 1 

	second_max = range2[2] + 1 
	second_min = range2[1] + 1 
	second = second_max - second_min + 1

	third_max = range3[2] + 1
	third_min = range3[1] + 1
	third = third_max - third_min + 1

	fourth_max = range4[2] + 1 
	fourth_min = range4[1] + 1
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
	return(combination)
}

probbb <- function(data, combination, budget){	
	projects <- nrow(data)
	num_of_combi <- nrow(combination)
	output <- matrix(0, nrow = num_of_combi * 2, ncol = projects + 2)

	for (number in 1:num_of_combi){
		posi2 <- number * 2
		posi1 <- posi2 - 1 
		preference <- combination[number,]

		prenorm <- pre_norm(combination[number,])
		NameList <- data[ ,c("NormNPV","Probability", "feasibility","Synergy") ]
		cof <- matrix(0, nrow = ncol, ncol = 1)	

		for (nn in 1:ncol){
			cof[nn,] <- as.numeric(NameList[nn,]) %*% t(prenorm)}
		assign("coof", cof, envir = .GlobalEnv) 


		LP_choice <- LP(data, budget)
		GA_choice <- genalg(data, projects, 0.01, budget)

		cs1 <- c(LP_choice, (LP_choice %*% data[, "CAPEX"]), (LP_choice %*% data[, "NPV"]))
		output <- throwin(output, posi1, cs1)

		cs2 <- c(GA_choice, (GA_choice %*% data[, "CAPEX"]), (GA_choice %*% data[, "NPV"]))
		output <- throwin(output, posi2, cs2)
	}

	colnames(output) <- c(as.character(data[,1]), "CAPEX", "NPV")
	rownames(output) <- rep(c("LP", "GA"), num_of_combi)
	return(output)
}



probsit <- function(data, matrix){
	projects <- nrow(data)
	roww <- nrow(matrix)
	prcob <- matrix(0, nrow = projects, ncol = 2)

	for (hh in 1:projects){
		prcob[hh,1] <- sum(matrix[,hh]) / (roww) * 100
		prcob[hh,2] <- 100 - prcob[hh,1]
	}

	rownames(prcob) <- data[,1]
	colnames(prcob) <- c("inv", "not inv")

	return(prcob)
}

# basic input 
budgd <- 10000

# preference
range1 <- c(8, 10) # [MIN, MAX] for NPV importance
range2 <- c(0, 1) # [MIN, MAX] for probability importance
range3 <- c(1, 2) # [MIN, MAX] for feasibility importance
range4 <- c(1, 1) # [MIN, MAX] for synergy importance

combination <- grid(range1, range2, range3, range4)

listss <- probbb(dat, combination, budgd)
probsit(dat, listss)
 

probsit_latex <- function(data, matrix){
	projects <- nrow(data)
	roww <- nrow(matrix)
	prcob <- matrix(0, nrow = projects, ncol = 5)

	for (hh in 1:projects){
		prcob[hh,1] <- "&"
		prcob[hh,2] <- round((sum(matrix[,hh]) / (roww) * 100), 2)
		prcob[hh,3] <- "&"
		prcob[hh,4] <- 100 - round((sum(matrix[,hh]) / (roww) * 100), 2)
		prcob[hh,5] <- "\\"
	}

	rownames(prcob) <- data[,1]
	colnames(prcob) <- c("&", "inv", "&", "not inv", "\\")

	return(prcob)
}

probsit_latex(dat, listss)


"""
for fun

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


# get probablistic result
roww = 3 # numbers of mutation increase for this
k <- which(prob(10,14) > 0.5)
GA_choice <- rep(0,14)
GA_choice[c(k)] <- 1


weight <- pre_norm(preference) * 100
sd <- format(c("NPV", "Probability", "feasibility", "synergy"), justify = "centre")
rownames(weight) <- c("%")
colnames(weight) <- sd
print(weight)


"""