# Author: Prafulla Shukla
# Date last modified: 17th December 2014
# Tasks:
	# 1. Creates a sampling flag within the user provided data frame, in proportions provided by the user
	# 2. Create a copy of the dependent variable, with null values in the training subset of the dataframe
# How to use:
	# 1. Source this file into R environment: a function "sampling" is created in workspace
	# 2. Use: sampling(your_df, name_of_dep_var)
	# 3. When asked, provide training and test sample proportions, one by one


######Code for splitting the data into Training and test #######

### input data from user: data
### input function argument from user: dep (dependent variable in the data)


sampling= function(data,dep)
{
	set.seed(786)
	
	print("Provide the training and THEN test sampling proportions (2 numbers betwen 0 and 1, sum should be 1): ")
	
	# Read two numbers from the user, one by one
	prob_seq = scan(n=2)
	random_sample = sample(c("train", "test"), size = nrow(data), prob = prob_seq, replace = T)
	
	data$sample = random_sample
	data.train = data[which(data$sample == "train"), ]
	data.test = data[which(data$sample == "test"), ]
	data[, paste0(dep, "_original")] = data[, dep]
	data[which(data$sample=="test"),dep ] = NA
	
	ans = list("complete_df" = data, "train_df" = data.train, "test_df" = data.test)
	
}


#sampling(Model.data, dep)