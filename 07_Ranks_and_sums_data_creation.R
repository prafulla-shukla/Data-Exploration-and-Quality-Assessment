# Author: Prafulla Shukla
# Date last modified: 17th December 2014
# Tasks:
	# 1. This code creates the data for plotting the picked variable against the dependent variable
	# 2. Purpose of such plot is to see the spatial relationship between the two variables
# How to use:
	# 1. Source this file into R environment: a function "rank_sum_check" is created in workspace
	# 2. Use: schema_function(your_df, dependent_var_name,outpath)
# Input:
	# data: your dataframe
	# dep: name of the dependent variable in the data, a string in double quotes
	# path: output 
	# bins: number of bins required by the user to group the variables
rank_sum_check = 

function(data, dep, path)
{

#.. Setting the user given path as the working directory
	setwd(path)

#.. Checking and installing required packages
	if ('doBy' %in% rownames(installed.packages()) == FALSE) {install.packages('doBy')}
	library(doBy)
	if ('reshape2' %in% rownames(installed.packages()) == FALSE) {install.packages('reshape2')}
	library(reshape2)
	if ('xlsx' %in% rownames(installed.packages()) == FALSE) {install.packages('xlsx')}
	library(xlsx)

#.. Obtaining basic information about the data frame
	a = sapply(data, class)
	tot_rows = nrow(data)
	
	num_vars = names(a)[which(a%in%c("numeric", "integer"))]
	non_num_vars = setdiff(names(a), num_vars)	

#.. Taking number of buckets needed as an input from the user
	print(paste0("Provide the number of buckets needed :"))
	bins = scan(n=1)
	
#.. Initializing a data frame to store Ranks and Plots data for all numeric variables
	b = as.data.frame(cbind(VARIABLE = "", RANK_GRP = 0, FREQUENCY = 0, MEAN_DEP=0, MEAN_INDEP=0, MIN_INDEP=0, MAX_INDEP=0))
	b[,1] = as.character(b[,1])
	b[,3:ncol(b)] = sapply(b[, 3:ncol(b)], as.numeric)
	b = b[-1,]

#.. Function to obtain Ranks and Plots data for a numeric variable
	num_ranks_function = 
	function(x)
	{
		ans = c("Frequency" = length(x), "Mean" = mean(x, na.rm = T),"Min" = min(x, na.rm = T), "Max" = max(x, na.rm =T) )
	}
	
#.. Initializing a data frame to store Linear Ranks and Plots data for all non-numeric variables
	abc = as.data.frame(cbind(VARIABLE = "", Levels = "",COUNT=0, mean_dep= 0))
	abc[,1:2] = sapply(abc[,1:2], as.character)
	abc[,3:ncol(abc)] = sapply(abc[,3:ncol(abc)], as.numeric)
	abc = abc[-1,]

#.. Function to obtain Linear Ranks and Plots data for a non-numeric variable
	cat_linear_function = 
	function(x)
	{
		ans = c("COUNT" = length(x),"mean_dep" = mean(x, na.rm = T))
	}

#.. Loop for creating ranks and plots data for each numeric variable
	for(idv in setdiff(num_vars, dep))
	{
	
		buckets = length(unique(quantile(data[, idv], probs = seq(0,1,1/bins))))-1

		data[,paste0(idv,"_bins_", bins)] = 
			cut(data[, idv], breaks = unique(quantile(data[, idv], probs = seq(0,1,1/bins))), include.lowest=T, labels = 1:buckets)

		data[,paste0(idv,"_bins_", bins)]  = as.numeric(as.character(data[,paste0(idv,"_bins_", bins)] ))

		a = summaryBy(as.formula(paste(paste(dep, idv, sep = "+"), paste0(idv,"_bins_", bins), sep = "~")), data = data, FUN = num_ranks_function)
		a[,"Variable"] = idv

		a[, paste0(idv, ".Frequency")] = NULL
		a[, paste0(dep, ".Min")] = NULL
		a[, paste0(dep, ".Max")] = NULL

		a = a[, c("Variable", setdiff(names(a),"Variable"))]

		names(a) = c("VARIABLE", "RANK_GRP", "FREQUENCY", "MEAN_DEP", "MEAN_INDEP", "MIN_INDEP", "MAX_INDEP")
		b = as.data.frame(rbind(b,a))

	}

	write.xlsx(b, file = "Ranks and plots_Numeric.xlsx", sheetName = "Ranks and plots",append = T,row.names = F, col.names = T)


#.. Loop for calculating Linear Ranks and Plots data for all non-numeric variable
	if(!(length(unique(data[,dep]))- sum(is.na(data[,dep]))==2))
	{
		for(idv in setdiff(non_num_vars, dep))
		{
	
			a = summaryBy(as.formula(paste(dep, idv, sep = "~")), data = data, FUN = cat_linear_function)
			a[,"Variable"] = idv
	
			a = a[, c("Variable", setdiff(names(a),"Variable"))]
	
			names(a) = c("VARIABLE", "Levels", "COUNT","mean_dep")
			abc = as.data.frame(rbind(abc,a))
	
		}
		write.xlsx(abc, file = "RanksPlots_Categorical_Linear.xlsx", sheetName = "R&P Categorical Linear",append = T,row.names = F, col.names = T)
	}

#.. Defining and executing specific tasks in case the dependent variable is of binary response type.............................
	if(length(unique(data[,dep]))- sum(is.na(data[,dep]))==2) 
	{

#..... Obtaining total goods and bads in the binary dependent variable
		total_bads = sum(data[,dep]==1, na.rm = T)
		total_goods = sum(data[,dep]==0, na.rm = T)

#..... Initializing a data frame to store information value of all numeric variables
		info_val = as.data.frame(cbind(VARIABLE = "", INFOVALUE = 1.00))
		info_val[,1] = as.character(info_val[,1])
		info_val[,2] = as.numeric(info_val[,2])
		info_val = info_val[-1,]

#..... Function to calculate information value of a numeric variable
		iv_function = 
		function(x)
		{
			bad_per = (sum(x==1,na.rm = T)/total_bads)
			good_per = (sum(x==0,na.rm = T)/total_goods)
			ans = c("IV" = ((sum(x==1,na.rm = T)/total_bads) - (sum(x==0,na.rm = T)/total_goods))*as.numeric(!(sum(x==1,na.rm = T)==0))*ifelse((sum(x==1,na.rm = T)==0)|(sum(x==0,na.rm = T)==0),0,log((sum(x==1,na.rm = T)/total_bads)/(sum(x==0,na.rm = T)/total_goods))))
		}

#..... Calculating information value and categorical logodds info for each numeric variable in case of binary dependent variable

		for(idv in setdiff(num_vars, dep))
		{
			iv_temp = summaryBy(as.formula(paste(dep, paste0(idv,"_bins_", bins), sep = "~")), data = data, FUN = iv_function)
			iv_result = as.data.frame(cbind(VARIABLE = idv, INFOVALUE = sum(iv_temp[, paste0(dep, ".IV")])))
		
			info_val = as.data.frame(rbind(info_val,iv_result))
		}

		write.xlsx(info_val, file = "Information Value.xlsx", sheetName = "Information Value", append = T, row.names = F, col.names = T)

#..... Initializing a data frame to store Logsitc Ranks and Plots data(logodds) for each level of all non-numeric variables
		ab = as.data.frame(cbind(VARIABLE = "", levels = "", Count=0, LOGODDS=0))
		ab[,1:2] = sapply(ab[,1:2], as.character)
		ab[,3:ncol(ab)] = sapply(ab[, 3:ncol(ab)], as.numeric)
		ab = ab[-1,]

#..... Function to calculate Logsitc Ranks and plots data(logodds) for each level of a non-numeric variable
		cat_log_function = 
		function(x)
		{
			ans = c("Count" = length(x),
				 "LOGODDS" = ifelse((sum(x==1)==0)|(sum(x==1)==0), 0, log((sum(x==0)*total_bads)/(sum(x==1)*total_goods)))
				 )
		}
		
#..... Ranks and plots for categorical variables in binary dependent variable

		for(idv in setdiff(non_num_vars, dep))
		{

			a = summaryBy(as.formula(paste(dep, idv, sep = "~")), data = data, FUN = cat_log_function)
			a[,"Variable"] = idv
	
			a = a[, c("Variable", setdiff(names(a),"Variable"))]
	
			names(a) = c("VARIABLE", "levels", "Count", "LOGODDS")
			ab = as.data.frame(rbind(ab,a))
		}
		write.xlsx(ab, file = "RanksPlots_Categorical_Logistic.xlsx", sheetName = "R&P Categorical Logistic",append = T,row.names = F, col.names = T)
	} 
		
}
#rank_sum_check(data, dep, "E:/Projects/DQM/Final Codes and sample outputs")
