# Author: Prafulla Shukla
# Date last modified: 17th December 2014
# Tasks:
	# 1. Produce an excel file containing DQM1 summary (basic details of dataset, numeric and non-numeric variables) of the dataset in user defined location
	# 2. Produce an excel file containing summary of all the numeric variables of the data frame, to be used as base imputation file
# How to use:
	# 1. Source this file into R environment: a function "schema_function" is created in workspace
	# 2. Use: schema_function(your_df, outpath)


schema_function <- function(mydata, outpath)
{
	setwd(outpath)
	#if ('xlsx' %in% rownames(installed.packages()) == FALSE) {install.packages('xlsx')}
	#library(xlsx)
	if ('e1071' %in% rownames(installed.packages()) == FALSE) {install.packages('e1071')}
	library(e1071)
	
	data_name <- deparse(substitute(mydata))
	mydata <- mydata
	a = sapply(mydata, class)

	num_vars = names(a)[which(a%in%c("numeric", "integer"))]
	non_num_vars = setdiff(names(a), num_vars)


# Collecting the variable types within the dataset

	classes = table(sapply(mydata, class))

# Creating overview of the data: Number of observations, variables, numberic and non-numeric variable counts
	
	data_overview = 
	as.data.frame(cbind( 
				"Dataset Name" = data_name, 
				"Total number of Observations" = nrow(mydata),
				"Total number of Variables" = ncol(mydata), 
				"Total number of Numeric Variables" = sum(classes[which(names(classes)%in%c("integer", "numeric"))]),
				"Total number of Character Variables" = sum(classes[which(!names(classes)%in%c("integer", "numeric"))])
				  ))
 
# All variables and corresponding variable types

	schema = as.data.frame(cbind("varname" = (names(sapply(mydata, class))), 
							"Variable Format" = sapply(mydata, class)), 
						row.names = 1:length(sapply(mydata, class)))
	write.csv(schema, file = paste0("Content_", data_name, ".csv"), row.names = F)

# Functions to count null values and percentage of null values for each variable
	null_counter <- function(x)
	{
		ifelse(!is.na(table(is.na(x))["TRUE"]), as.numeric(table(is.na(x))["TRUE"]),0)
	}

	null_fraction <- 
	function(x)
	{
		ans= c("%of Missing Values" = 
				ifelse(!is.na(table(is.na(x))["TRUE"]),paste(round(100*table(is.na(x))["TRUE"]/length(x),2), "%", sep = ""), "0.00%")
			)
	}

# For each variable, number of observations, unique values, missing and non-missing count 

	null_counts = 
	as.data.frame(cbind("varname" = names(mydata), 
				 "n" = as.numeric(sapply(mydata, FUN = length)), 
				 "# of Distinct Values" = as.numeric(sapply(mydata, FUN = function(x){length(unique(x,na.rm =T))})),
				 "nmiss" = (sapply(mydata,FUN = null_counter)),
				 "% of Missing Values" = (sapply(mydata,FUN = null_fraction))),
			row.names = 1:ncol(mydata))

	null_counts$"# of Non-missing Values" = 
		as.numeric(as.character(null_counts$"n"))-as.numeric(as.character(null_counts$"nmiss"))

# Merging information about variable types with the the information obtained above

	schema_1 = merge(x = schema, y = null_counts, by = "varname")
	check = match(names(mydata),schema_1[,"varname"])
	schema_1 <- schema_1[check,]


### MACRO	OVERVIEW OF THE DATA FINISHED, OUTPUT CONTAINS: VARIABLES, VARIABLE TYPES, MISSING AND DISTINCT COUNTS
#########################################################################################################################################


# Creating summary of Numeric variables in the given dataset 
	num_out_1 = data.frame(varname = num_vars)
	num_out_1[,"n"] = as.numeric(sapply(mydata[, num_vars], FUN = length))
	num_out_1[,"mean"] = sapply(mydata[, num_vars], FUN = function(x){round(mean(x, na.rm = T),2)})
	num_out_1[,"std"] = sapply(mydata[, num_vars], FUN = function(x){round(sd(x, na.rm = T),2)})
#	num_out_1[,"skewness"] = sapply(mydata[, num_vars], FUN = function(x){skewness(x, na.rm = T)})
#	num_out_1[,"CV"] = num_out_1$"std"/num_out_1$"mean"
	num_out_1[,"sum"] = sapply(mydata[, num_vars], FUN = function(x){sum(x, na.rm = T)})
#	num_out_1[,"kurtosis"] = sapply(mydata[, num_vars], FUN = function(x){kurtosis(x, na.rm = T)})
	num_out_1[, "nmiss"] = sapply(mydata[, num_vars],FUN = null_counter)
	rownames(num_out_1) = 1:nrow(num_out_1)

# Input probability breaks to be used for calculating quantiles of each numeric variable
	q_probs = c(seq(0,1, by = .01), seq(.991,.999,.001))  # User dependent, can be changed


# Computing quantile breaks of each numeric variable

	test = as.data.frame(sapply(mydata[, num_vars], function(x){quantile(x, probs=q_probs, na.rm = T)}))
	test1 = as.data.frame(t(test[, num_vars]))

	names(test1) = paste("p",100*q_probs, sep = "_")

	test1$"varname" = row.names(test1)
	row.names(test1) = 1:length(num_vars)

# Merging quantile breaks with the rest of the stats of each numeric variable	

	num_out = merge(num_out_1, test1, by = c("varname"), all.y = T, all.x = T)
	num_out_1 = num_out[, setdiff(names(num_out), c("Variable Format", "# of Distinct Values","% of Missing Values","# of Non-missing Values"))]
	num_out_1$Obs = 1:nrow(num_out_1)
	num_out_1 = num_out_1[,c("Obs", setdiff(names(num_out_1), "Obs"))]
	write.csv(num_out_1, file = paste0("numeric_vars_", data_name, ".csv"), row.names = F)
# Taking xlsx output of the numeric variables
#	num_output = createWorkbook()
#	num_sheet <- createSheet(num_output, sheetName="User Template Input")
#	addDataFrame(num_out_1, num_sheet, row.names = F)
#	saveWorkbook(num_output, "User Template input.xlsx")


# Numeric variables' Description finished here

########################################################################################################################

# Creating frequency statistics of non-numeric variables of the dataset

	character_variable_freq = data.frame("Variable" = NA, "Value" = NA, "Frequency Count"= NA, "Percent of Total Frequency" = NA)

	for(x in non_num_vars)
	{
	c = as.data.frame(cbind(rep(x, length(unique(mydata[,x]))),
						names(table(mydata[,x])), 
						table(mydata[,x]),
						paste(round(100*table(mydata[,x])/sum(table(mydata[,x])),2),"%", sep = "")
					  )
				)
	names(c) = names(character_variable_freq)
	character_variable_freq = as.data.frame(rbind(character_variable_freq, c))
	}

# Removing the first row of the final output which is by default populated with NA's (as created in the first line of this section)
	character_variable_freq = character_variable_freq[-1,]
	row.names(character_variable_freq) = 1:nrow(character_variable_freq)

	char_out = schema_1[which(schema_1$"varname" %in% non_num_vars), ]
	write.csv(char_out, file = paste0("char_vars_", data_name, ".csv"), row.names = F)	
# Character variables' description finished here
########################################################################################################################

#	out_list = list("Overall_Summary" = data_overview, 
#				"Numeric_Description" = num_out, 
#				"Character_Description" = char_out, 
#				"Character_variable_freq" =character_variable_freq)
	
	write.csv(character_variable_freq, file = paste0("char_vars_frequency_", data_name, ".csv"), row.names = F)	
# Taking xlsx output to be written in Single file

#	output = createWorkbook()

# Declaring sheet names

#	overall_summary_sheet <- createSheet(output, sheetName="Overall_Summary")
#	num_sheet <- createSheet(output, sheetName="Numeric_variables")
#	char_sheet <- createSheet(output, sheetName="Character_variables")
#	char_freq_sheet <- createSheet(output, sheetName = "Character_variable_freq")

# Adding results to corresponding sheets in the workbook

#	addDataFrame(data_overview, overall_summary_sheet, row.names = F)
#	addDataFrame(out_list[[2]], num_sheet, row.names = F)
#	addDataFrame(out_list[[3]], char_sheet, row.names = F)
#	addDataFrame(out_list[[4]], char_freq_sheet, row.names = F)
	
#	saveWorkbook(output, paste(data_name,"_", "data_description_DQM1.xlsx", sep = ""))

#ans = list("Overview" =data_overview, "Schema" = schema, "Numeric_Description" = num_out, "Character_Description" = character_variable_freq)
}



# schema_function(data, outpath)