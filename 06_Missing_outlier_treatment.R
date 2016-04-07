# Author: Pratyay Karar, Prafulla Shukla
# Date last modified: 17th December 2014
# Tasks:
	# 1. Reads base imputation file from the excel file location provided by the user (pre-defined format)
	# 2. Performs outlier treatment and imputation of numeric variables only

# How to use:
	# 1. Source this file into R environment: a function "missing_outlier" is created in workspace
	# 2. Use: missing_outlier(your_df, "path_of_imputationfile.xlsx", "sheet_name")


# Defining a function which will do the missing value imputation and outlier treatment #######


missing_outlier=function(data, base_file_path, sheet_name)
	
	{
	if ('xlsx' %in% rownames(installed.packages()) == FALSE) {install.packages('xlsx')}
	library(xlsx)
	temp = read.xlsx(base_file_path, header = TRUE,sheet=sheet_name)
  
	###transposing the data which is read above ######

	temp.1 = as.data.frame(t(temp))


####changing the names of variables ####

	for(i in 1:dim(temp.1)[2])
	{
	  names(temp.1)[i] = as.character(temp.1[1,i])
	}

	for (i in 1:dim(data)[2])
	{
	  for(j in 1:dim(temp.1)[2])
	  {
    
    	if( names(data)[i]== names(temp.1)[j])
	    {
     		data[i][data[i] < as.numeric(as.vector(temp.1[,j]))[5]] =  as.numeric(as.vector(temp.1[,j]))[5]
      	data[i][data[i] > as.numeric(as.vector(temp.1[,j]))[7]] =  as.numeric(as.vector(temp.1[,j]))[7]
      
	      data[i][is.na(data[i])] =  as.numeric(as.vector(temp.1[,j]))[3]
         }
        
  }
}
return(data)

}