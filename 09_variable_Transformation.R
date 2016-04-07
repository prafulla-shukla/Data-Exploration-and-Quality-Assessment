# Authors: Pratyay Karar, Prafulla Shukla
# Date last modified: 17th December 2014
# Tasks:
	# 1. Perform variable transformations using the excel input that is created using template excel file
# How to use:
	# 1. Source this file into R environment: a function "var_transformation" is created in workspace
	# 2. Use: var_transformation(your_df, file_path, sheet_name)
	# 3. Inputs: a dataframe, full path of imputation excel file, sheet name containing transformation flags



var_transformation=function(data, file_path, sheet_name)
  
{

	if ('xlsx' %in% rownames(installed.packages()) == FALSE) {install.packages('xlsx')}
	library(xlsx)
	temp <- read.xlsx(file_path, sheetName=sheet_name,header = TRUE)
	temp.1 = as.data.frame(t(temp))
	for(i in 1:dim(temp.1)[2])
	{
	  names(temp.1)[i] = as.character(temp.1[1,i])
	}

	mb.train.temp.1=data


	for (i in 1:dim(mb.train.temp.1)[2])
	{
	  for(j in 1:dim(temp.1)[2])
	  {
    
    		if( names(mb.train.temp.1)[i]== names(temp.1)[j])
        	{
      		if (as.numeric(as.vector(temp.1[,j])[8])==1)
		      {
        			mb.train.temp.1[,paste("log_", names(temp.1)[j], sep = "")] = log(mb.train.temp.1[,names(temp.1)[j]]) 
      	      }
      
			if (as.numeric(as.vector(temp.1[,j])[9])==1)
     		 	{
				mb.train.temp.1[,paste("square_", names(temp.1)[j], sep = "")] = (mb.train.temp.1[,names(temp.1)[j]])^2 
        		}
      	     if (as.numeric(as.vector(temp.1[,j])[10])==1)
      		{
				mb.train.temp.1[,paste("cube_", names(temp.1)[j], sep = "")] = (mb.train.temp.1[,names(temp.1)[j]])^3
		     }
      	     if (as.numeric(as.vector(temp.1[,j])[11])==1)
	     		{
				mb.train.temp.1[,paste("inv_", names(temp.1)[j], sep = "")] = 1/(mb.train.temp.1[,names(temp.1)[j]]) 
			}
      		if (as.numeric(as.vector(temp.1[,j])[12])==1)
			{
				mb.train.temp.1[,paste("sqrt_", names(temp.1)[j], sep = "")] = sqrt(mb.train.temp.1[,names(temp.1)[j]]) 
           	}
         }
	} #end of for loop
}
return(mb.train.temp.1)
}
    
    
    
    
####summary(mb.train.temp.1)


#mb.train.1 = var_transformation(data=mb.train)