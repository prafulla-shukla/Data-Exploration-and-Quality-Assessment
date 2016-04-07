# Author: Prafulla Shukla
# Date last modified: 17th December 2014
# Tasks:
	# 1. Provides various statistics of the dependent variable for each value of the selection variable in the dataset
	# 2. Create a copy of the dependent variable, with null values in the training subset of the dataframe
# How to use:
	# 1. Source this file into R environment: a function "dv_univariate_check" is created in workspace
	# 2. Use: dv_univariate_check(your_df, name_of_dep_var, subsetting_variable_name, path_to_save_output)

 
# Inputs: 
#	dataframe containing the dependent and selection variable
#	Name of the dependent variable as a string
#	Name of the subsetting variable

# Outputs:
#	
dv_univariate_check = function(data, dep, selection_var, path)
{

#.. Loading the required packages
	if ('nortest' %in% rownames(installed.packages()) == FALSE) {install.packages('nortest')}
	library(nortest)
	if ('robCompositions' %in% rownames(installed.packages()) == FALSE) {install.packages('robCompositions')}
	library(robCompositions)
	if ('e1071' %in% rownames(installed.packages()) == FALSE) {install.packages('e1071')}
	library(e1071)
	if ('xlsx' %in% rownames(installed.packages()) == FALSE) {install.packages('xlsx')}
	library(xlsx)

	setwd(path)

#.. Calculating various stastics of the dependent variable for each value of the selection variable

	for(k in sort(unique(data[,selection_var]),decreasing = T))
	{
	final_answer = list()
	data1 = data[which(data[,selection_var]%in%c(k)), ]
	# Calculating moments of dependent variable

	dep_kurtosis = kurtosis(data1[,dep], na.rm=T)
	dep_skewness = skewness(data1[,dep], na.rm=T)
	
	uncorrected_ss = sum((data1[, dep])^2, na.rm = T)
	corrected_ss = sum((data1[, dep]-mean(data1[, dep]))^2, na.rm =T)

	variance = var(data1[, dep], na.rm = T)
	std_deviation = sd(data1[, dep], na.rm = T)

	sum_observation = sum(data1[, dep], na.rm=T)

	std <- function(x) sd(x, na.rm = T)/sqrt(length(x))
	std_error_mean = std(data1[, dep])

	cv = 100*sd(data1[,dep])/mean(data1[,dep])

	a = as.data.frame(cbind("Measure" = c('N',	'Mean',	'Std Deviation',	'Skewness',	'Uncorrected SS',	'Coeff Variation'),
			"Value" = c(length(data1[,dep]),		mean(data1[,dep], na.rm = T),	 std_deviation,	   dep_skewness,     uncorrected_ss , cv)))

	b = as.data.frame(cbind("Measure" = c('Sum Weights',	'Sum Observations',	'Variance',	'Kurtosis',	'Corrected SS',	'Std Error Mean'),
			"Value" = c(length(data1[,dep]),	sum_observation,	variance,	dep_kurtosis,	corrected_ss,	std_error_mean)))

	final_answer$"Moments" = cbind(a,b, row.names = 1:nrow(a))

####################################################################################################################################

# Parameters for Normal Distribution
	final_answer$"Parameters for Normal Distribution" = as.data.frame(cbind("Parameter" =c("Mean","Std Dev") , 
										"Symbol" = c("Mu","Sigma"), 
										"Estimate" = c(mean(data1[,dep], na.rm = T),sd(data1[,dep], na.rm = T))
									)
							)
####################################################################################################################################

# Quantiles of the dependent variable

	standard_q_breaks = quantile(data1[,dep], probs = c(0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99,1), na.rm = T)
	quantile_data  =  as.data.frame(cbind("Quantile" = names(standard_q_breaks)[seq(length(standard_q_breaks),1,-1)], 
					"Estimate" = as.numeric(standard_q_breaks[seq(length(standard_q_breaks),1,-1)])))
	final_answer$"Quantiles" = quantile_data

####################################################################################################################################

# Tests for Location: Mu0=0
	T_val = t.test(data1[, dep], mu=0,alternative = c("greater"))
	t_stat= as.numeric(T_val[[1]])
	P_t_stat = as.numeric(T_val[[3]])

	S_val = wilcox.test(data1[, dep], mu = 0, correct=F)
	S_stat = as.numeric(S_val[[1]])
	P_S_stat = as.numeric(S_val[[3]])

	final_answer$"Tests for Location: Mu0=0" = as.data.frame(cbind("test" = c("Student's t","Signed Rank"),
						"Statistic" = c("t","S"),
						"Value" = c(t_stat,S_stat),
						"p Value" = c(P_t_stat, P_S_stat)))

#####################################################################################################################################

#Goodness-of-Fit Tests for Normal Distribution

	KS = lillie.test(data1[,dep])
	ks_stat = as.numeric(KS$statistic)
	P_ks_stat = as.numeric(KS$p.value)

	Cramer_Von = cvm.test(data1[,dep])
	cramer_von_stats = as.numeric(Cramer_Von$statistic)
	P_cramer_von_stats = as.numeric(Cramer_Von$p.value)

	AD = adtest(data1[,dep])
	anderson_darling_test = as.numeric(AD$statistic)
	P_anderson_darling_test = as.numeric(AD$p.value)

	final_answer$"Goodness-of-Fit Tests for Normal Distribution" = 
		as.data.frame(cbind( Test = c("Kolmogorov-Smirnov","Cramer-von Mises","Anderson-Darling"), 
				Statistic = c("D","W-Sq","A-Sq"), 
				Statistic_value = c(ks_stat ,cramer_von_stats, anderson_darling_test ),
				p_Value =c(P_ks_stat ,P_cramer_von_stats ,P_anderson_darling_test ) ),row.names = 1:3)

#####################################################################################################################################
# Obtaining Quantiles for Normal Distribution of dependent variable
	y = data1[, dep]

# Creates theoretical normal variate based on dependent variable
	a = qqnorm(y, mean = mean(y), sd = sd(y), plot.it = F) 

	norm_fit_data = as.data.frame(cbind("Theoretical" = a$"x", "Observed" = a$"y"))
	norm_fit_data = norm_fit_data[order(norm_fit_data$Observed, norm_fit_data$Theoretical, decreasing = F),]

	standard_q_breaks = quantile(norm_fit_data$Theoretical, probs = c(0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99,1), na.rm = T) # breaks taken from SAS output
	q_breaks = as.numeric(standard_q_breaks)

	norm_fit_data$bin = cut(norm_fit_data$Theoretical, 
						quantile(norm_fit_data$Theoretical, probs = c(0,0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99,1),na.rm = T),
						include.lowest=T, labels = 1:length(q_breaks))

	uni_norm_data = as.data.frame(cbind("Percent" =(names(standard_q_breaks)),"Estimated"=(q_breaks)))
	#sapply(uni_norm_data, class)

	uni_norm_data$Percent = as.character(uni_norm_data$Percent)
	uni_norm_data$Estimated = as.numeric(as.character(uni_norm_data$Estimated))

	for(z in 1:length(q_breaks) ){
		uni_norm_data[z,"Observed"] = tail(norm_fit_data[which(norm_fit_data$bin%in%c(z)), "Observed"])[1]
	}

	final_answer$"Quantiles for Normal Distribution"  = uni_norm_data
	write.table(paste0("Univariate Analysis of ", dep, " where Indicator = ", k), file = "Dependent Variable Distribution.csv", append = T, quote = F, row.names = F, sep = ",")

# Writing the output to csv file
	for(y in names(final_answer))
	{
		write.table(y,file = "Dependent Variable Distribution.csv", append = T, quote = F, row.names = F, sep = ",")
		write.table(final_answer[[y]], file = "Dependent Variable Distribution.csv", append = T, quote = F, row.names = F, sep = ",")
	}
	}
	temp = read.csv(file = "Dependent Variable Distribution.csv", header = F, as.is = F)
	write.xlsx(temp, file = "Dependent Variable Distribution.xlsx", sheetName = "Dependent Variable Distribution",row.names = F, col.names = F)
}


# dv_univariate_check(data ,"dep",selection_var= "sample", "E:/Projects/DQM/Final Codes and sample outputs/")