# Author: Prafulla Shukla
# Date last modified: 17th December 2014
# Tasks:
	# 1. Creates data for plotting lift charts of dependent and predicted variable based on user defined parameters
# How to use:
	# 1. Source this file into R environment: a function "lift_chart" is created in workspace
	# 2. Use: lift_chart(your_df, name_of_dep_var, name_of_predicted_variable, sortign_var, subsetting_variable_name, path_to_save_output)

# This code creates the data for plotting the dependent variable bucketed by the predicted variable

# Input:
	#  data: your dataframe
	#  dep: name of the dependent variable in the data, a string in double quotes
	#  pred: name of the predicted variable in the data, a string in double quotes
	#  sort_var: name of the variable in the data to rank order the data before plotting the dependent and predicted variables
	#  subset_var: name of the variable in the data to repeat the process for each value of subset_var
	#  path: location to save the output csv


model_validation = 
	function(data, dep, pred, sort_var, path)
	{

	setwd(path)
	library(doBy)
	library(reshape2)

	print("Provide the number of buckets needed: ")
	bins = scan(n=1)
	
#  Rank ordering the data based on sort_var
	data[, paste0(sort_var, "_Rank_")] = cut(data[, sort_var], breaks = quantile(data[, sort_var], probs = seq(0,1,1/bins)), labels = 1:bins,
							include.lowest = T)
	
	data <- data[,c(paste0(sort_var,"_Rank_"),dep, pred)]

#  Creating summary stastics for the purpose of lift charts
	
#	write.table(paste0("Data for Lift Chart, ranked by :", sort_var), file = "Model Validation Chart.csv",append = T, row.names = F, sep = ",")

	a = summaryBy(as.formula(paste(paste(pred, dep, sep = "+"), paste0(sort_var,"_Rank_"), sep = "~")), 
					data = data[,c(pred, dep,paste0(sort_var,"_Rank_"))], 
					FUN = function(x){ans = c(
								#	"Frequency" = length(x),
								#	"Sum" = sum(x, na.rm=T),
									"Mean" = mean(x, na.rm = T)
								#	,"Standard_Deviation" = sd(x, na.rm =T)
								#	,"Minimum" = min(x, na.rm=T)
								#	,"Maximum" = max(x, na.rm=T)
									)
								}
				)
	names(a) = c("Rank", "Predicted", "Observed")
	write.table(a, file = "Model Validation Chart.csv",append = T, row.names = F, sep = ",")
	temp = read.csv(file = "Model Validation Chart.csv", as.is = F, header = F )
	file.remove("Model Validation Chart.csv")
	write.xlsx(temp, file = "Model Validation Chart.xlsx", sheetName = "Model Validation Chart", row.names = F, col.names = F)
}

#lift_chart(data, dep, pred, sort_var, subset_var, path)
