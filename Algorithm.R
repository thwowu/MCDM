# setting location
location <- getwd()
setwd(location)

setwd("C:/Users/user/Downloads")

packages <- c("gdata", "xlsx", "lpSolveAPI", "genalg", "imputeTS")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

library(gdata)
library(xlsx)
library(lpSolveAPI)
library(genalg)
library(imputeTS)

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
ncol <- nrow(dat[,1])  # get number of the rows

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
	number <- nrow(dat)  # get number of the rows

	lp_rowpicker <- make.lp(ncol=number) # number of projects
	set.type(lp_rowpicker, columns=1:number, type = c("binary"))
	# settng the upper and lower bound of the solution

	######## objective function ###############
	set.objfn(lp_rowpicker, c(coof)) 
	lp.control(lp_rowpicker,sense='max')

	######### constraints ###############
	add.constraint(lp_rowpicker, c(dat[,"CAPEX"]), "<=", budget)
	delta <- rep(0, number)
	delta[5] <- 1 
	delta[6] <- -1 
	add.constraint(lp_rowpicker, c(delta), ">=",  0)

	darwin1 <- rep(0, number)
	darwin1[17] <- 1 
	darwin1[8] <- -1 
	add.constraint(lp_rowpicker, c(darwin1), ">=",  0)

	darwin2 <- rep(0, number)
	darwin2[17] <- 1 
	darwin2[9] <- -1 
	add.constraint(lp_rowpicker, c(darwin2), ">=",  0)

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
	    else if (x[8] == 1 && x[17] == 0){return(0)} # penalty function for Darwin Sun
	    else if (x[9] == 1 && x[17] == 0){return(0)} # penalty function for Darwin Shem
	    else if (x[6] == 1 && x[5] == 0){return(0)} # penalty function for Delta	    
	    else 
	    	return(-current_solution_NPV)
}

throwin = function(matrix, position, list){
	l = ncol(matrix)
	for (kk in 1:l) {
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

grid_para <- function(range1, range2, range3, range4){

	output <- matrix(0, nrow = 2, ncol = 4)
	
	output[1,1] = range1[1]
	output[1,2] = range2[1]
	output[1,3] = range3[1]
	output[1,4] = range4[1]
	
	output[2,1] = range1[2]
	output[2,2] = range2[2]
	output[2,3] = range3[2]
	output[2,4] = range4[2]

	colnames(output) = c("Return", "Probability", "Feasibility", "Synergy")
	rownames(output) = c("Min", "Max")
	return(output)
}

prob_LP <- function(data, combination, budget){	

	projects <- nrow(data)
	num_of_combi <- nrow(combination)
	output <- matrix(0, nrow = num_of_combi, ncol = projects + 3 + 4)

	for (number in 1:num_of_combi){
		preference <- combination[number,]

		prenorm <- pre_norm(combination[number,])
		NameList <- data[ ,c("NormNPV","Probability", "feasibility","Synergy") ]
		cof <- matrix(0, nrow = projects, ncol = 1)	

		for (nn in 1:projects){
			cof[nn,] <- as.numeric(NameList[nn,]) %*% t(prenorm)}
		assign("coof", cof, envir = .GlobalEnv) 

		LP_choice <- LP(data, budget)
		cs1 <- c(LP_choice, t(round(prenorm, 2)),(LP_choice %*% data[, "CAPEX"]), 
			     (LP_choice %*% data[, "NPV"]), ((LP_choice %*% data[, "NPV"])/budget))
		output <- throwin(output, number, cs1)
	}

	colnames(output) <- c(as.character(data[,1]), "Return", "Probability", 
						"feasibility", "synergy", "CAPEX", "NPV", "ROI")

	return(output)
}


prob_LP_latex <- function(data, combination, budget){	
	projects <- nrow(data)
	num_of_combi <- nrow(combination)
	output <- matrix(0, nrow = num_of_combi, ncol = projects + 1 )

	for (number in 1:num_of_combi){
		preference <- combination[number,]

		prenorm <- pre_norm(combination[number,])
		NameList <- data[ ,c("NormNPV","Probability", "feasibility","Synergy") ]
		cof <- matrix(0, nrow = projects, ncol = 1)	

		for (nn in 1:projects){
			cof[nn,] <- as.numeric(NameList[nn,]) %*% t(prenorm)}
		assign("coof", cof, envir = .GlobalEnv) 

		LP_choice <- LP(data, budget)
		cs1 <- c(LP_choice, (LP_choice %*% data[, "NPV"])/budget)
		output <- throwin(output, number, cs1)
	}

	nammm <- matrix(0, ncol = projects, nrow = 1)
	for (k in 1: projects){
		nammm[k] = paste("p", toString(k), sep = "")
	}
	colnames(output) <- c(as.character(nammm), "ROI")

	return(output)
}

prob_LP_compare <- function(data, combination, budget){	
	projects <- nrow(data)
	num_of_combi <- nrow(combination)
	output <- matrix(0, nrow = num_of_combi, ncol = projects + 3 + 4)

	for (number in 1:num_of_combi){
		preference <- combination[number,]

		prenorm <- pre_norm(combination[number,])
		NameList <- data[ ,c("NormNPV","Probability", "feasibility","Synergy") ]
		cof <- matrix(0, nrow = projects, ncol = 1)	

		for (nn in 1:projects){
			cof[nn,] <- as.numeric(NameList[nn,]) %*% t(prenorm)}
		assign("coof", cof, envir = .GlobalEnv) 

		LP_choice <- LP(data, budget)
		cs1 <- c(LP_choice, t(round(prenorm, 2)),(LP_choice %*% data[, "CAPEX"]), (LP_choice %*% data[, "NPV"]), (LP_choice %*% data[, "NPV"])/budget )
		output <- throwin(output, number, cs1)
	}

	nammm <- matrix(0, ncol = projects, nrow = 1)
	for (k in 1: projects){
		nammm[k] = paste("p", toString(k), sep = "")
	}
	colnames(output) <- c(as.character(nammm), "Return", "Probability", 
						"feasibility", "synergy", "CAPEX", "NPV", "ROI")
	return(output)
}



prob_GA <- function(data, combination, budget){	
	projects <- nrow(data)
	num_of_combi <- nrow(combination)
	output <- matrix(0, nrow = num_of_combi, ncol = projects + 3)

	for (number in 1:num_of_combi){
		preference <- combination[number,]
		prenorm <- pre_norm(combination[number,])
		NameList <- data[ ,c("NormNPV","Probability", "feasibility","Synergy") ]
		cof <- matrix(0, nrow = projects, ncol = 1)	

		for (nn in 1:projects){
			cof[nn,] <- as.numeric(NameList[nn,]) %*% t(prenorm)}
		assign("coof", cof, envir = .GlobalEnv) 

		GA_choice <- genalg(data, projects, 0.01, budget)
		cs2 <- c(GA_choice, (GA_choice %*% data[, "CAPEX"]), (GA_choice %*% data[, "NPV"]), (GA_choice %*% data[, "NPV"])/budget)
		output <- throwin(output, number, cs2)
	}

	colnames(output) <- c(as.character(data[,1]), "CAPEX", "NPV", "ROI")
	return(output)
}



prob_GA_latex <- function(data, combination, budget){	
	projects <- nrow(data)
	num_of_combi <- nrow(combination)
	output <- matrix(0, nrow = num_of_combi, ncol = projects + 1)

	for (number in 1:num_of_combi){
		preference <- combination[number,]
		prenorm <- pre_norm(combination[number,])
		NameList <- data[ ,c("NormNPV","Probability", "feasibility","Synergy") ]
		cof <- matrix(0, nrow = projects, ncol = 1)	

		for (nn in 1:projects){
			cof[nn,] <- as.numeric(NameList[nn,]) %*% t(prenorm)}
		assign("coof", cof, envir = .GlobalEnv) 

		GA_choice <- genalg(data, projects, 0.01, budget)
		cs2 <- c(GA_choice,  (GA_choice %*% data[, "NPV"])/budget )
		output <- throwin(output, number, cs2)
	}


	nammm <- matrix(0, ncol = projects, nrow = 1)
	for (k in 1: projects){
		nammm[k] = paste("p", toString(k), sep = "")
	}
	colnames(output) <- c(as.character(nammm), "ROI")

	return(output)
}


prob_GA_compare <- function(data, combination, budget){	
	projects <- nrow(data)
	num_of_combi <- nrow(combination)
	output <- matrix(0, nrow = num_of_combi, ncol = projects + 3 + 4)

	for (number in 1:num_of_combi){
		preference <- combination[number,]

		prenorm <- pre_norm(combination[number,])
		NameList <- data[ ,c("NormNPV","Probability", "feasibility","Synergy") ]
		cof <- matrix(0, nrow = projects, ncol = 1)	

		for (nn in 1:projects){
			cof[nn,] <- as.numeric(NameList[nn,]) %*% t(prenorm)}
		assign("coof", cof, envir = .GlobalEnv) 

		GA_choice <- genalg(data, projects, 0.01, budget)
		cs2 <- c(GA_choice, t(round(prenorm, 2)),(GA_choice %*% data[, "CAPEX"]), (GA_choice %*% data[, "NPV"]), (GA_choice %*% data[, "NPV"])/budget)
		output <- throwin(output, number, cs2)
	}

	nammm <- matrix(0, ncol = projects, nrow = 1)
	for (k in 1: projects){
		nammm[k] = paste("p", toString(k), sep = "")
	}
	colnames(output) <- c(as.character(nammm), "Return", "Probability", 
						"feasibility", "synergy", "CAPEX", "NPV", "ROI")
	return(output)
}


prob_both <- function(data, combination, budget){	
	projects <- nrow(data)
	num_of_combi <- nrow(combination)
	output <- matrix(0, nrow = num_of_combi * 2, ncol = projects + 3 + 4)

	for (number in 1:num_of_combi){
		posi2 <- number * 2
		posi1 <- posi2 - 1 
		preference <- combination[number,]

		prenorm <- pre_norm(combination[number,])
		NameList <- data[ ,c("NormNPV","Probability", "feasibility","Synergy") ]
		cof <- matrix(0, nrow = projects, ncol = 1)	

		for (nn in 1:projects){
			cof[nn,] <- as.numeric(NameList[nn,]) %*% t(prenorm)}
		assign("coof", cof, envir = .GlobalEnv) 


		LP_choice <- LP(data, budget)
		GA_choice <- genalg(data, projects, 0.01, budget)

		cs1 <- c(LP_choice, prenorm, (LP_choice %*% data[, "CAPEX"]), (LP_choice %*% data[, "NPV"]), (LP_choice %*% data[, "NPV"])/budget)
		output <- throwin(output, posi1, cs1)

		cs2 <- c(GA_choice, prenorm, (GA_choice %*% data[, "CAPEX"]), (GA_choice %*% data[, "NPV"]), (GA_choice %*% data[, "NPV"])/budget)
		output <- throwin(output, posi2, cs2)
	}

	colnames(output) <- c(as.character(data[,1]), "NormNPV","Probability", "feasibility","Synergy", "CAPEX", "NPV", "ROI")
	rownames(output) <- rep(c("LP", "GA"), num_of_combi)
	return(output)
}



prob_both_latex <- function(data, combination, budget){	
	projects <- nrow(data)
	num_of_combi <- nrow(combination)
	output <- matrix(0, nrow = num_of_combi * 2, ncol = projects + 1 )

	for (number in 1:num_of_combi){
		posi2 <- number * 2
		posi1 <- posi2 - 1 
		preference <- combination[number,]

		prenorm <- pre_norm(combination[number,])
		NameList <- data[ ,c("NormNPV","Probability", "feasibility","Synergy") ]
		cof <- matrix(0, nrow = projects, ncol = 1)	

		for (nn in 1:projects){
			cof[nn,] <- as.numeric(NameList[nn,]) %*% t(prenorm)}
		assign("coof", cof, envir = .GlobalEnv) 

		LP_choice <- LP(data, budget)
		GA_choice <- genalg(data, projects, 0.01, budget)

		output <- throwin(output, posi1, c(LP_choice, (LP_choice %*% data[, "NPV"])/budget))
		output <- throwin(output, posi2, c(GA_choice, (GA_choice %*% data[, "NPV"])/budget))
	}

	nammm <- matrix(0, ncol = projects, nrow = 1)
	for (k in 1: projects){nammm[k] = paste("p", toString(k), sep = "")}
	colnames(output) <- c(as.character(nammm), "ROI")

	return(output)
}


prob_both_compare <- function(data, combination, budget){	
	projects <- nrow(data)
	num_of_combi <- nrow(combination)
	output <- matrix(0, nrow = num_of_combi * 2, ncol = projects + 3 + 4)

	for (number in 1:num_of_combi){
		posi2 <- number * 2
		posi1 <- posi2 - 1 
		preference <- combination[number,]

		prenorm <- pre_norm(combination[number,])
		NameList <- data[ ,c("NormNPV","Probability", "feasibility","Synergy") ]
		cof <- matrix(0, nrow = projects, ncol = 1)	

		for (nn in 1:projects){
			cof[nn,] <- as.numeric(NameList[nn,]) %*% t(prenorm)}
		assign("coof", cof, envir = .GlobalEnv) 


		LP_choice <- LP(data, budget)
		GA_choice <- genalg(data, projects, 0.01, budget)
		
		# Linear Programming
		cs1 <- c(LP_choice, prenorm, (LP_choice %*% data[, "CAPEX"]), (LP_choice %*% data[, "NPV"]), (LP_choice %*% data[, "NPV"])/budget)
		output <- throwin(output, posi1, cs1)

		# Genetic algorithm
		cs2 <- c(GA_choice, prenorm, (GA_choice %*% data[, "CAPEX"]), (GA_choice %*% data[, "NPV"]), (GA_choice %*% data[, "NPV"])/budget)
		output <- throwin(output, posi2, cs2)
	}

	rownames(output) <- rep(c("LP", "GA"), num_of_combi)
	
	nammm <- matrix(0, ncol = projects, nrow = 1)
	for (k in 1: projects){
		nammm[k] = paste("p", toString(k), sep = "") }

	colnames(output) <- c(as.character(nammm), 
						  "Return", "Probability", "feasibility", "synergy", "CAPEX", "NPV", "ROI" )

	return(output)
}



probsit <- function(data, matrix){
	projects <- nrow(data)
	roww <- nrow(matrix)
	prcob <- matrix(0, nrow = projects, ncol = 2)

	for (hh in 1:projects){
		prcob[hh,1] <- paste(toString(round((sum(matrix[,hh])/(roww)*100),0)), 
			                  "%", sep = "")
		prcob[hh,2] <- paste(toString(round( (100 -(sum(matrix[,hh])/(roww)*100)), 0)), 
			                  "%", sep = "")
	}

	rownames(prcob) <- data[,1]
	colnames(prcob) <- c("Possibility to be invested", "Not inv")

	return(prcob)
}


probsit_latex <- function(data, matrix){
	projects <- nrow(data)
	roww <- nrow(matrix)
	prcob <- matrix(0, nrow = projects, ncol = 2)

	for (hh in 1:projects){
		prcob[hh,1] <- round((sum(matrix[,hh])/(roww)), 2) 
		prcob[hh,2] <- round(1 - (sum(matrix[,hh])/(roww)), 2) 
	}

	nammm <- matrix(0, ncol = 1, nrow = projects)
	for (k in 1: projects){
		nammm[k] = paste("p", toString(k), sep = "")
	}
	rownames(prcob) <- c(as.character(nammm))
	colnames(prcob) <- c("Possibility to be invested", "not inv")

	return(prcob)
}

# return the combination with the highest return 

best <- function(data, choice){
	sublist <- choice[which.max(choice[, "NPV"]),]
	s <- which(sublist == 1)
	itn <- length(s)

	nammm <- matrix(0, ncol = 1, nrow = itn)
	for (k in 1: itn){
		nammm[k] = paste("p", toString(s[k]), sep = "") }

	matr <- c(as.character(nammm), paste("ROI: ", round(sublist["ROI"],4) , sep = ""), recursive = FALSE  )
	return (matr)
}

