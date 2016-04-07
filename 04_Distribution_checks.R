# Author: Prafulla Shukla
# Date last modified: 17th December 2014
# Tasks:
	# 1. Checking different distribution of dependent variable to test its normality
# How to use:
	# 1. Source this file into R environment: a function "dep_plot" is created in workspace
	# 2. Use: dep_plot(your_df, name_of_dep_var, subset_var, path_to_save_plots)




###............Checking different distribution of dependent variable ................####


# 	Keep the sampling flag in the original dataset and use it accordingly for creating plots for training and test data sets
#	data: your data frame containing the variable to be checked for various distributions
#	dist_var: a string giving the name of the variable in the data set whose distribution is under inspection
#	subset_var: a flag or grouping variable, to compare distribution within each subset of the whole data
#	path: location where the user wants to save the plots


dep_plot = function(data, dist_var,subset_var, path)
{
	i = 1
	y = c(2, 3,"", 1)
	setwd(path)
	for(x in unique(data[, subset_var]))
	{
		vec <- data[which(data[,subset_var]==x), dist_var]
		print(i)
		png(paste0("univar", y[i],".png"))
		i=i+1
		print(i)

		#boxplot_dep = boxplot(vec, main ="Distribution of Actual", col="sienna")


		# Following are the curves for checking the distribution of the variable
		# Kindly comment out the plots which are not needed at the time of analysis at hand

	# Histogram

		hist(vec, breaks = 15, freq = F, xlab = dist_var, ylab = 'Probability',
			xlim = c(0, max(vec)+1),
			ylim = c(0,1.2*max(hist(vec, breaks = 15, freq = F, plot = F)$density)),
			main = paste0('Distribution Plots of ',dist_var, ' for ', subset_var, ' = ',x ))

	# Normal probability plot
		curve(dnorm(x, mean = mean(vec),sd = sd(vec)), add = T,col=1, lty = 1, lwd = 2, ylim = 0.038)

	# Kernal Density plot  
		#lines(density(vec, na.rm = T, from = 0, to = max(vec)),col=2, lty = 1, lwd = 2)

	# Gamma fit of the variable distribution
		curve(dgamma(x, shape = mean(vec)^2/var(vec), scale = var(vec)/mean(vec)), add = T,col=3,lty = 1, lwd = 2, ylim = 0.038)

	# Change the attributes accordingly
		legend("topright", c("Normal", "Gamma"),col=c(1,3), lty=c(1,1,1), lwd = c(2,2,2))
		dbeta
		dlnorm
		dev.off()

#		png(paste0("QQ_Plot_",dist_var," where ", subset_var," = ",x, ".png"))
		png(paste0("univar",y[i],".png"))

		i = i+1
		print(i)
	# q-q plot of the variable distribution
		qqnorm(vec, main = paste0('Normal Q-Q Plot of ',dist_var, ' for ', subset_var, ' = ',x ),xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",plot.it = T)
		dev.off()

	}
#	"Plots_of_age where sample = train.png" = "univar.png"
#	"QQ_Plot_age where sample = train.png" = "univar1.png"
#	"Plots_of_age where sample = test.png" = "univar2.png"
#	"QQ_Plot_age where sample = test.png" = "univar3.png"
}
	