# Author: Prafulla Shukla
# Date last modified: 17th December 2014
# Tasks:
	# 1. Return a data frame after loading the csv data into R from user defined path(including filename)
	# 2. Choose variables from user created csv containing variable names and return a data frame with those variables only

###############################################################################################################################

data_import = function(file_path)
{
	data = read.csv(file_path, as.is = T, header = T)

# Reading the csv data into R
	print("Do you have a variable list in csv? If Yes, then give Full path with filename, or type NO")

# Reading varlist file path from user, full path including the filename
	varlist_path = readline()

	if(!(varlist_path == "NO"))
	{
		varlist = read.csv(file = varlist_path, as.is = T, header = T)[,1]
		data  = data[, varlist]
		return(data)
		break
	}
	else	
	{return(data)}
}


# Implementation steps:

# source("C:/Users/prafulla.shukla/Desktop/DQM Module/codes/01_Data_import_Model_data_creation.R") #<- location and filename of this code on users system
# data_import("path/filename.csv")
