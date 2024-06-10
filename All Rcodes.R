#lack of financial literacy
# Step 1: Setup Loan and Payment Parameters
loan_amount <- 10000        # The amount of the loan borrowed
total_repayment <- 12000    # Total amount to be repaid over the loan period
monthly_payment <- 200      # The fixed monthly payment amount
months <- 5 * 12            # Total number of months over the 5-year period
simulations <- 10000        # Number of simulations to run

#Lack of financial literacy Missed payments parameters (assumed mean and standard 
#deviation)
mu_1 <- 60   
sigma_1 <- 30    

# Step 2: Define a range of threshold means to test
alpha_1_means <- seq(550,590, by=10)  # range from 550 to 590

# Step 3: Run simulations for each threshold mean and store results
results <- data.frame(ThresholdMean = numeric(), ProbabilityOfDefault = numeric())

set.seed(123)  # For reproducibility

for (alpha_1_mean in alpha_1_means) {
  default_count <- 0  # Counter for default cases
  
  for (i in 1:simulations) {
    # Generate missed payments for each month
    missed_payments <- rnorm(months, mean=mu_1, sd=sigma_1)
    
    # Generate random normal distributed threshold values for each 6-month period
    alpha_1 <- rnorm(months / 6, mean=alpha_1_mean, sd=70)
    
    # Calculate total missed payments every 6 months
    for (j in seq(1, months, by=6)) {
      period_index <- (j - 1) / 6 + 1
      threshold <- alpha_1[period_index]
      if (sum(missed_payments[j:(j+5)]) > threshold) {
        default_count <- default_count + 1
        break  # Stop checking further if already defaulted
      }
    }
  }
  
  # Step 4: Calculate the probability of default for the current threshold mean
  probability_of_default <- default_count / simulations
  
  # Store the result
  results <- rbind(results, data.frame(Alpha_1_mean = alpha_1_mean, ProbabilityOfDefault = probability_of_default))
}

# Calculate the mean of all probabilities of default
mean_probability_of_default <- mean(results$ProbabilityOfDefault)
mean_probability_of_default


# Step 5: Plot the results
library(ggplot2)   

ggplot(results, aes(x = Alpha_1_mean, y = ProbabilityOfDefault)) +
  geom_point() +
  geom_line() +
  labs(title = "Probability of Default due to lack of financial literacy",
       x = "Alpha_1_mean",
       y = "Probability of Default") +
  theme_minimal()



#lack of market research
# Step 1: Setup Loan and Payment Parameters
loan_amount <- 10000        # The amount of the loan borrowed
total_repayment <- 12000    # Total amount to be repaid over the loan period
monthly_payment <- 200      # The fixed monthly payment amount
months <- 5 * 12            # Total number of months over the 5-year period
simulations <- 10000        # Number of simulations to run

# Lack of market research 
#Missed payments parameters (assumed mean and standard deviation)
mu_2 <- 60   
sigma_2 <- 30   

# Step 2: Define a range of threshold means to test
alpha_2_means <- seq(600,640, by=10)  # range from 600 to 640 

# Step 3: Run simulations for each threshold mean and store results
results <- data.frame(ThresholdMean = numeric(), ProbabilityOfDefault = numeric())

set.seed(123)  # For reproducibility

for (alpha_2_mean in alpha_2_means) {
  default_count <- 0  # Counter for default cases
  
  for (i in 1:simulations) {
    # Generate missed payments for each month
    missed_payments <- rnorm(months, mean=mu_2, sd=sigma_2)
    
    # Generate random normal distributed threshold values for each 6-month period
    alpha_2 <- rnorm(months / 6, mean=alpha_2_mean, sd=70)
    
    # Calculate total missed payments every 6 months
    for (j in seq(1, months, by=6)) {
      period_index <- (j - 1) / 6 + 1
      threshold <- alpha_2[period_index]
      if (sum(missed_payments[j:(j+5)]) > threshold) {
        default_count <- default_count + 1
        break  # Stop checking further if already defaulted
      }
    }
  }
  
  # Step 4: Calculate the probability of default for the current threshold mean
  probability_of_default <- default_count / simulations
  
  # Store the result
  results <- rbind(results, data.frame(Alpha_2_mean = alpha_2_mean, ProbabilityOfDefault = probability_of_default))
}

# Calculate the mean of all probabilities of default
mean_probability_of_default <- mean(results$ProbabilityOfDefault)
mean_probability_of_default



# Step 5: Plot the results
library(ggplot2)   

ggplot(results, aes(x = Alpha_2_mean, y = ProbabilityOfDefault)) +
  geom_point() +
  geom_line() +
  labs(title = "Probability of Default due to lack of market research",
       x = "Alpha_2_mean",
       y = "Probability of Default") +
  theme_minimal()



#poor business operations management
# Step 1: Setup Loan and Payment Parameters
loan_amount <- 10000        # The amount of the loan borrowed
total_repayment <- 12000    # Total amount to be repaid over the loan period
monthly_payment <- 200      # The fixed monthly payment amount
months <- 5 * 12            # Total number of months over the 5-year period
simulations <- 10000        # Number of simulations to run

#Poor business operations management 
#Missed payments parameters (assumed mean and standard deviation)
mu_3 <- 60   
sigma_3 <- 30  

# Step 2: Define a range of threshold means to test
alpha_3_means <- seq(500,540, by=10)  #range from 500 to 540

# Step 3: Run simulations for each threshold mean and store results
results <- data.frame(ThresholdMean = numeric(), ProbabilityOfDefault = numeric())

set.seed(123)  # For reproducibility

for (alpha_3_mean in alpha_3_means) {
  default_count <- 0  # Counter for default cases
  
  for (i in 1:simulations) {
    # Generate missed payments for each month
    missed_payments <- rnorm(months, mean=mu_3, sd=sigma_3)
    
    # Generate random normal distributed threshold values for each 6-month period
    alpha_3 <- rnorm(months / 6, mean=alpha_3_mean, sd=70)
    
    # Calculate total missed payments every 6 months
    for (j in seq(1, months, by=6)) {
      period_index <- (j - 1) / 6 + 1
      threshold <- alpha_3[period_index]
      if (sum(missed_payments[j:(j+5)]) > threshold) {
        default_count <- default_count + 1
        break  # Stop checking further if already defaulted
      }
    }
  }
  
  # Step 4: Calculate the probability of default for the current threshold mean
  probability_of_default <- default_count / simulations
  
  # Store the result
  results <- rbind(results, data.frame(Alpha_3_mean = alpha_3_mean, ProbabilityOfDefault = probability_of_default))
}

# Calculate the mean of all probabilities of default
mean_probability_of_default <- mean(results$ProbabilityOfDefault)
mean_probability_of_default
# Step 5: Plot the results
library(ggplot2)   

ggplot(results, aes(x = Alpha_3_mean, y = ProbabilityOfDefault)) +
  geom_point() +
  geom_line() +
  labs(title = "Probability of Default due to poor business operations management",
       x = "Alpha_3_mean",
       y = "Probability of Default") +
  theme_minimal()







#economic downturns
# Step 1: Setup Loan and Payment Parameters
loan_amount <- 10000        # The amount of the loan borrowed
total_repayment <- 12000    # Total amount to be repaid over the loan period
monthly_payment <- 200      # The fixed monthly payment amount
months <- 5 * 12            # Total number of months over the 5-year period
simulations <- 10000        # Number of simulations to run

# Missed payments parameters (assumed mean and standard deviation)
lambda_1 <-90  # Assumed average missed payment amount each month
# Step 2: Define a range of threshold means to test
beta_1_means <- seq(510,550, by=10) 

# Step 3: Run simulations for each threshold mean and store results
results <- data.frame(Beta_1_mean = numeric(), ProbabilityOfDefault = numeric())

set.seed(123)  # For reproducibility

for (beta_1_mean in beta_1_means) {
  default_count <- 0  # Counter for default cases
  
  for (i in 1:simulations) {
    # Generate missed payments for each month
    missed_payments <- rpois(months, lambda_1)
    
    # Generate random normal distributed threshold values for each 6-month period
    beta_1 <- rnorm(months / 6, mean=beta_1_mean, sd=70)
    
    # Calculate total missed payments every 6 months
    for (j in seq(1, months, by=6)) {
      period_index <- (j - 1) / 6 + 1
      threshold <- beta_1[period_index]
      if (sum(missed_payments[j:(j+5)]) > threshold) {
        default_count <- default_count + 1
        break  # Stop checking further if already defaulted
      }
    }
  }
  
  # Step 4: Calculate the probability of default for the current threshold mean
  probability_of_default <- default_count / simulations
  
  # Store the result
  results <- rbind(results, data.frame(Beta_1_mean = beta_1_mean, ProbabilityOfDefault = probability_of_default))
}

# Calculate the mean of all probabilities of default
mean_probability_of_default <- mean(results$ProbabilityOfDefault)
mean_probability_of_default
# Step 5: Plot the results
library(ggplot2)   

ggplot(results, aes(x = Beta_1_mean, y = ProbabilityOfDefault)) +
  geom_point() +
  geom_line() +
  labs(title = "Probability of Default due to Economic downturns",
       x = "Beta_1_mean",
       y = "Probability of Default") +
  theme_minimal()







#Natural disasters
# Step 1: Setup Loan and Payment Parameters
loan_amount <- 10000        # The amount of the loan borrowed
total_repayment <- 12000    # Total amount to be repaid over the loan period
monthly_payment <- 200      # The fixed monthly payment amount
months <- 5 * 12            # Total number of months over the 5-year period
simulations <- 10000        # Number of simulations to run

# Missed payments parameters (assumed mean and standard deviation)
lambda_2 <-90  # Assumed average missed payment amount each month

# Step 2: Define a range of threshold means to test
beta_2_means <- seq(610,650, by=10)  

# Step 3: Run simulations for each threshold mean and store results
results <- data.frame(Beta_2_mean = numeric(), ProbabilityOfDefault = numeric())

set.seed(123)  # For reproducibility

for (beta_2_mean in beta_2_means) {
  default_count <- 0  # Counter for default cases
  
  for (i in 1:simulations) {
    # Generate missed payments for each month
    missed_payments <- rpois(months, lambda_2)
    
    # Generate random normal distributed threshold values for each 6-month period
    beta_2 <- rnorm(months / 6, mean=beta_2_mean, sd=70)
    
    # Calculate total missed payments every 6 months
    for (j in seq(1, months, by=6)) {
      period_index <- (j - 1) / 6 + 1
      threshold <- beta_2[period_index]
      if (sum(missed_payments[j:(j+5)]) > threshold) {
        default_count <- default_count + 1
        break  # Stop checking further if already defaulted
      }
    }
  }
  
  # Step 4: Calculate the probability of default for the current threshold mean
  probability_of_default <- default_count / simulations
  
  # Store the result
  results <- rbind(results, data.frame(Beta_2_mean = beta_2_mean, ProbabilityOfDefault = probability_of_default))
}

# Calculate the mean of all probabilities of default
mean_probability_of_default <- mean(results$ProbabilityOfDefault)
mean_probability_of_default
# Step 5: Plot the results
library(ggplot2)   

ggplot(results, aes(x = Beta_2_mean, y = ProbabilityOfDefault)) +
  geom_point() +
  geom_line() +
  labs(title = "Probability of Default due to Natural disasters",
       x = "Beta_2_mean",
       y = "Probability of Default") +
  theme_minimal()




#Political instability
# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# Missed payments parameters (assumed mean and standard deviation)
	lambda_3 <-90  # Assumed average missed payment amount each month
	
	# Step 2: Define a range of threshold means to test
	beta_3_means <- seq(560,600, by=10)  
	
	# Step 3: Run simulations for each threshold mean and store results
	results <- data.frame(Beta_3_mean = numeric(), ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (beta_3_mean in beta_3_means) {
		default_count <- 0  # Counter for default cases
		
		for (i in 1:simulations) {
			# Generate missed payments for each month
			missed_payments <- rpois(months, lambda_3)
			
			# Generate random normal distributed threshold values for each 6-month period
			beta_3 <- rnorm(months / 6, mean=beta_3_mean, sd=70)
			
			# Calculate total missed payments every 6 months
			for (j in seq(1, months, by=6)) {
				period_index <- (j - 1) / 6 + 1
				threshold <- beta_3[period_index]
				if (sum(missed_payments[j:(j+5)]) > threshold) {
					default_count <- default_count + 1
					break  # Stop checking further if already defaulted
				}
			}
		}
		
		# Step 4: Calculate the probability of default for the current threshold mean
		probability_of_default <- default_count / simulations
		
		# Store the result
		results <- rbind(results, data.frame(Beta_3_mean = beta_3_mean, ProbabilityOfDefault = probability_of_default))
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
# Step 5: Plot the results
	library(ggplot2)   
	
	ggplot(results, aes(x = Beta_3_mean, y = ProbabilityOfDefault)) +
	geom_point() +
	geom_line() +
	labs(title = "Probability of Default due to Political instability",
	x = "Beta_3_mean",
	y = "Probability of Default") +
	theme_minimal()
	
	
	
	
	#Loan deviation
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# Missed payments parameters (assumed mean and standard deviation)
	lambda_4 <-70 # Assumed average missed payment amount each month
	
	# Step 2: Define a range of threshold means to test
	gamma_1_means <- seq(500,540, by=10)  
	
	# Step 3: Run simulations for each threshold mean and store results
	results <- data.frame(Gamma_1_mean = numeric(), ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (gamma_1_mean in gamma_1_means) {
	  default_count <- 0  # Counter for default cases
	  
	  for (i in 1:simulations) {
	    # Generate missed payments for each month
	    missed_payments <- rpois(months, lambda_4)
	    
	    # Generate random normal distributed threshold values for each 6-month period
	    gamma_1 <- rnorm(months / 6, mean=gamma_1_mean, sd=70)
	    
	    # Calculate total missed payments every 6 months
	    for (j in seq(1, months, by=6)) {
	      period_index <- (j - 1) / 6 + 1
	      threshold <- gamma_1[period_index]
	      if (sum(missed_payments[j:(j+5)]) > threshold) {
	        default_count <- default_count + 1
	        break  # Stop checking further if already defaulted
	      }
	    }
	  }
	  
	  # Step 4: Calculate the probability of default for the current threshold mean
	  probability_of_default <- default_count / simulations
	  
	  # Store the result
	  results <- rbind(results, data.frame(Gamma_1_mean = gamma_1_mean, ProbabilityOfDefault = probability_of_default))
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
# Step 5: Plot the results
	library(ggplot2)   
	
	ggplot(results, aes(x = Gamma_1_mean, y = ProbabilityOfDefault)) +
	  geom_point() +
	  geom_line() +
	  labs(title = "Probability of Default due to Loan deviation",
	       x = "Gamma_1_mean",
	       y = "Probability of Default") +
	  theme_minimal()
	

	
		
	#High interest rates
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# Missed payments parameters (assumed mean and standard deviation)
	lambda_5 <-70 # Assumed average missed payment amount each month
	
	# Step 2: Define a range of threshold means to test
	gamma_2_means <- seq(550,590, by=10)  
	
	# Step 3: Run simulations for each threshold mean and store results
	results <- data.frame(Gamma_2_mean = numeric(), ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (gamma_2_mean in gamma_2_means) {
	  default_count <- 0  # Counter for default cases
	  
	  for (i in 1:simulations) {
	    # Generate missed payments for each month
	    missed_payments <- rpois(months, lambda_4)
	    
	    # Generate random normal distributed threshold values for each 6-month period
	    gamma_2 <- rnorm(months / 6, mean=gamma_2_mean, sd=70)
	    
	    # Calculate total missed payments every 6 months
	    for (j in seq(1, months, by=6)) {
	      period_index <- (j - 1) / 6 + 1
	      threshold <- gamma_2[period_index]
	      if (sum(missed_payments[j:(j+5)]) > threshold) {
	        default_count <- default_count + 1
	        break  # Stop checking further if already defaulted
	      }
	    }
	  }
	  
	  # Step 4: Calculate the probability of default for the current threshold mean
	  probability_of_default <- default_count / simulations
	  
	  # Store the result
	  results <- rbind(results, data.frame(Gamma_2_mean = gamma_2_mean, ProbabilityOfDefault = probability_of_default))
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	# Step 5: Plot the results
	library(ggplot2)   
	
	ggplot(results, aes(x = Gamma_2_mean, y = ProbabilityOfDefault)) +
	  geom_point() +
	  geom_line() +
	  labs(title = "Probability of Default due to High interest rates",
	       x = "Gamma_2_mean",
	       y = "Probability of Default") +
	  theme_minimal()
	

	
		
#Loan delay
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# Missed payments parameters (assumed mean and standard deviation)
	theta <-1/70 # Assumed average missed payment amount each month
	
	# Step 2: Define a range of threshold means to test
	gamma_3_means <- seq(800,1000, by=10)  
	
	# Step 3: Run simulations for each threshold mean and store results
	results <- data.frame(Gamma_3_mean = numeric(), ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (gamma_3_mean in gamma_3_means) {
	  default_count <- 0  # Counter for default cases
	  
	  for (i in 1:simulations) {
	    # Generate missed payments for each month
	    missed_payments <- rexp(months, theta)
	    
	    # Generate random normal distributed threshold values for each 6-month period
	    gamma_3 <- rnorm(months / 6, mean=gamma_3_mean, sd=70)
	    
	    # Calculate total missed payments every 6 months
	    for (j in seq(1, months, by=6)) {
	      period_index <- (j - 1) / 6 + 1
	      threshold <- gamma_3[period_index]
	      if (sum(missed_payments[j:(j+5)]) > threshold) {
	        default_count <- default_count + 1
	        break  # Stop checking further if already defaulted
	      }
	    }
	  }
	  
	  # Step 4: Calculate the probability of default for the current threshold mean
	  probability_of_default <- default_count / simulations
	  
	  # Store the result
	  results <- rbind(results, data.frame(Gamma_3_mean = gamma_3_mean, ProbabilityOfDefault = probability_of_default))
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	# Step 5: Plot the results
	library(ggplot2)   
	
	ggplot(results, aes(x = Gamma_3_mean, y = ProbabilityOfDefault)) +
	  geom_point() +
	  geom_line() +
	  labs(title = "Probability of Default due to Loan delay",
	       x = "Gamma_3_mean",
	       y = "Probability of Default") +
	  theme_minimal()

	
#Lack of financial literacy and economic downturns
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# Lack of financial literacy missed payments parameters (assumed mean and standard 
	#deviation)
	mu_1 <- 60  
	sigma_1<- 30
	
	# Economic downturns missed payments parameters (assumed mean)
	lambda_1<-90   
	
	# Step 2: Define a range of threshold means to test
	alpha_1_means <- seq(550, 590, by = 10)  # range for lack of financial literacy
	beta_1_means <- seq(510, 550, by = 10)   # range for economic downturns
	
	# Step 3: Run simulations for each threshold mean and store results
	results <- data.frame(Alpha_1_mean = numeric(), Beta_1_mean = numeric(), 
	                      ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (alpha_1_mean in alpha_1_means) {
	  for (beta_1_mean in beta_1_means) {
	    default_count <- 0  # Counter for default cases
	    
	    for (i in 1:simulations) {
	      # Generate missed payments due lack of financial literacy for the borrower over the 
	      #defined number of months
	      lack_financial_lit <- rnorm(months, mean = mu_1, sd = sigma_1)
	      
	      # Generate missed payments due to economic downturns for the borrower over the
	      #defined number of months
	      economic_downt <- rpois(months, lambda_1)
	      
	      # Generate random normal distributed threshold values for each 6-month period
	      alpha_1 <- rnorm(months / 6, mean = alpha_1_mean, sd = 70)
	      beta_1 <- rnorm(months / 6, mean = beta_1_mean, sd = 70)
	      
	      # Calculate total missed payments every 6 months
	      for (j in seq(1, months, by = 6)) {
	        period_index <- (j - 1) / 6 + 1
	        threshold_lack_financial_lit <- alpha_1[period_index]
	        threshold_economic_downt <- beta_1[period_index]
	        if ((sum(lack_financial_lit[j:(j + 5)]) > threshold_lack_financial_lit) &&
	            (sum(economic_downt[j:(j + 5)]) > threshold_economic_downt)) {
	          default_count <- default_count + 1
	          break  # Stop checking further if already defaulted
	        }
	      }
	    }
	    
	    # Step 4: Calculate the probability of default for the current threshold means
	    probability_of_default <- default_count / simulations
	    
	    # Store the result
	    results <- rbind(results, data.frame(Alpha_1_mean = alpha_1_mean, 
	                                         Beta_1_mean = beta_1_mean,
	                                         ProbabilityOfDefault = probability_of_default))
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default	
	
	
	
	
	
	
	
	#Lack of financial literacy and natural disasters
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# Lack of financial literacy missed payments parameters (assumed mean and standard 
	#deviation)
	mu_1 <- 60   
	sigma_1<- 30     
	
	# Natural disasters missed payments parameters (assumed mean)
	lambda_2<-90   
	
	# Step 2: Define a range of threshold means to test
	alpha_1_means <- seq(550, 590, by = 10)  # range for lack of financial literacy
	beta_2_means <- seq(610, 650, by = 10)   # range for natural disasters
	
	# Step 3: Run simulations for each threshold mean and store results
	results <- data.frame(Alpha_1_mean = numeric(), Beta_2_mean = numeric(), 
	                      ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (alpha_1_mean in alpha_1_means) {
	  for (beta_2_mean in beta_2_means) {
	    default_count <- 0  # Counter for default cases
	    
	    for (i in 1:simulations) {
	      # Generate missed payments due lack of financial literacy for the borrower over the 
	      #defined number of months
	      lack_financial_lit <- rnorm(months, mean = mu_1, sd = sigma_1)
	      
	      # Generate missed payments due to natural disasters for the borrower over the
	      #defined number of months
	      natural_disa <- rpois(months, lambda_2)
	      
	      # Generate random normal distributed threshold values for each 6-month period
	      alpha_1 <- rnorm(months / 6, mean = alpha_1_mean, sd = 70)
	      beta_2 <- rnorm(months / 6, mean = beta_2_mean, sd = 70)
	      
	      # Calculate total missed payments every 6 months
	      for (j in seq(1, months, by = 6)) {
	        period_index <- (j - 1) / 6 + 1
	        threshold_lack_financial_lit <- alpha_1[period_index]
	        threshold_natural_disa <- beta_2[period_index]
	        if ((sum(lack_financial_lit[j:(j + 5)]) > threshold_lack_financial_lit) &&
	            (sum(natural_disa[j:(j + 5)]) > threshold_natural_disa)) {
	          default_count <- default_count + 1
	          break  # Stop checking further if already defaulted
	        }
	      }
	    }
	    
	    # Step 4: Calculate the probability of default for the current threshold means
	    probability_of_default <- default_count / simulations
	    
	    # Store the result
	    results <- rbind(results, data.frame(Alpha_1_mean = alpha_1_mean, 
	                                         Beta_1_mean = beta_2_mean,
	                                         ProbabilityOfDefault = probability_of_default))
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	
	
	
	
	
	
	#lack of financial literacy and political instability
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# Lack of financial literacy missed payments parameters (assumed mean and standard 
	#deviation)
	mu_1 <- 60   
	
	sigma_1<- 30
	# Political instability missed payments parameter (assumed mean)
	lambda_3<-90   
	
	# Step 2: Define a range of threshold means to test
	alpha_1_means <- seq(550, 590, by = 10)  # range for lack of financial literacy
	beta_3_means <- seq(560, 600, by = 10)   # range for political instability
	
	# Step 3: Run simulations for each threshold mean and store results
	results <- data.frame(Alpha_1_mean = numeric(), Beta_3_mean = numeric(), 
	                      ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (alpha_1_mean in alpha_1_means) {
	  for (beta_3_mean in beta_3_means) {
	    default_count <- 0  # Counter for default cases
	    
	    for (i in 1:simulations) {
	      # Generate missed payments due lack of financial literacy for the borrower over the 
	      #defined number of months
	      lack_financial_lit <- rnorm(months, mean = mu_1, sd = sigma_1)
	      
	      # Generate missed payments due to political instability for the borrower over the
	      #defined number of months
	      political_insta <- rpois(months, lambda_3)
	      
	      # Generate random normal distributed threshold values for each 6-month period
	      alpha_1 <- rnorm(months / 6, mean = alpha_1_mean, sd = 70)
	      beta_3 <- rnorm(months / 6, mean = beta_3_mean, sd = 70)
	      
	      # Calculate total missed payments every 6 months
	      for (j in seq(1, months, by = 6)) {
	        period_index <- (j - 1) / 6 + 1
	        threshold_lack_financial_lit <- alpha_1[period_index]
	        threshold_political_insta <- beta_3[period_index]
	        if ((sum(lack_financial_lit[j:(j + 5)]) > threshold_lack_financial_lit) &&
	            (sum(political_insta[j:(j + 5)]) > threshold_political_insta)) {
	          default_count <- default_count + 1
	          break  # Stop checking further if already defaulted
	        }
	      }
	    }
	    
	    # Step 4: Calculate the probability of default for the current threshold means
	    probability_of_default <- default_count / simulations
	    
	    # Store the result
	    results <- rbind(results, data.frame(Alpha_1_mean = alpha_1_mean, 
	                                         Beta_3_mean = beta_3_mean,
	                                         ProbabilityOfDefault = probability_of_default))
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	
	
	#Lack of market research and economic downturns
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# Lack of market research missed payments parameters (assumed mean and standard  
	#deviation)
	mu_2 <- 60  
	sigma_2<- 30  
	
	# Economic downturns missed payments parameter (assumed mean)
	lambda_1<-90   
	
	# Step 2: Define a range of threshold means to test
	alpha_2_means <- seq(600, 640, by = 10)  # range for lack of market research
	beta_1_means <- seq(510, 550, by = 10)   # range for economic downturns
	
	# Step 3: Run simulations for each threshold mean and store results
	results <- data.frame(Alpha_2_mean = numeric(), Beta_1_mean = numeric(), 
	                      ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (alpha_2_mean in alpha_2_means) {
	  for (beta_1_mean in beta_1_means) {
	    default_count <- 0  # Counter for default cases
	    
	    for (i in 1:simulations) {
	      # Generate missed payments due lack of market research for the borrower over the 
	      #defined number of months
	      lack_market_res <- rnorm(months, mean = mu_2, sd = sigma_2)
	      
	      # Generate missed payments due to economic downturns for the borrower over the
	      #defined number of months
	      economic_downt <- rpois(months, lambda_1)
	      
	      # Generate random normal distributed threshold values for each 6-month period
	      alpha_2 <- rnorm(months / 6, mean = alpha_2_mean, sd = 70)
	      beta_1 <- rnorm(months / 6, mean = beta_1_mean, sd = 70)
	      
	      # Calculate total missed payments every 6 months
	      for (j in seq(1, months, by = 6)) {
	        period_index <- (j - 1) / 6 + 1
	        threshold_lack_market_res <- alpha_2[period_index]
	        threshold_economic_downt <- beta_1[period_index]
	        if ((sum(lack_market_res[j:(j + 5)]) >  threshold_lack_market_res) &&
	            (sum(economic_downt[j:(j + 5)]) > threshold_economic_downt)) {
	          default_count <- default_count + 1
	          break  # Stop checking further if already defaulted
	        }
	      }
	    }
	    
	    # Step 4: Calculate the probability of default for the current threshold means
	    probability_of_default <- default_count / simulations
	    
	    # Store the result
	    results <- rbind(results, data.frame(Alpha_2_mean = alpha_2_mean, 
	                                         Beta_1_mean = beta_1_mean,
	                                         ProbabilityOfDefault = probability_of_default))
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	
	# Lack of market research and Natural Disasters
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# Lack of market research missed payments parameters (assumed mean and standard 
	#deviation)
	mu_2 <- 60  
	sigma_2<- 30   
	
	# natural disasters missed payments parameter (assumed mean)
	lambda_2<-90   
	
	# Step 2: Define a range of threshold means to test
	%	alpha_2_means <- seq(600, 640, by = 10)  # range for lack of market research
	beta_2_means <- seq(610, 650, by = 10)   # range for natural disasters
	
	# Step 3: Run simulations for each threshold mean and store results
	results <- data.frame(Alpha_2_mean = numeric(), Beta_2_mean = numeric(), 
	                      ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (alpha_2_mean in alpha_2_means) {
	  for (beta_2_mean in beta_2_means) {
	    default_count <- 0  # Counter for default cases
	    
	    for (i in 1:simulations) {
	      # Generate missed payments due lack of market research for the borrower over the 
	      #defined number of months
	      lack_market_res <- rnorm(months, mean = mu_2, sd = sigma_2)
	      
	      # Generate missed payments due to natural disasters for the borrower over the
	      #defined number of months
	      natural_disa <- rpois(months, lambda_2)
	      
	      # Generate random normal distributed threshold values for each 6-month period
	      alpha_2 <- rnorm(months / 6, mean = alpha_2_mean, sd = 70)
	      beta_2 <- rnorm(months / 6, mean = beta_2_mean, sd = 70)
	      
	      # Calculate total missed payments every 6 months
	      for (j in seq(1, months, by = 6)) {
	        period_index <- (j - 1) / 6 + 1
	        threshold_lack_market_res <- alpha_2[period_index]
	        threshold_natural_disa <- beta_2[period_index]
	        if ((sum(lack_market_res[j:(j + 5)]) >  threshold_lack_market_res) &&
	            (sum(natural_disa[j:(j + 5)]) > threshold_natural_disa)) {
	          default_count <- default_count + 1
	          break  # Stop checking further if already defaulted
	        }
	      }
	    }
	    
	    # Step 4: Calculate the probability of default for the current threshold means
	    probability_of_default <- default_count / simulations
	    
	    # Store the result
	    results <- rbind(results, data.frame(Alpha_2_mean = alpha_2_mean, 
	                                         Beta_2_mean = beta_2_mean,
	                                         ProbabilityOfDefault = probability_of_default))
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	#Lack of market research and Political Instability
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# Lack of market research missed payments parameters (assumed mean and standard   
	#deviation)
	mu_2 <- 60  
	sigma_2<- 30     
	
	# political instability missed payments parameter (assumed mean)
	lambda_3<-90   
	
	# Step 2: Define a range of threshold means to test
	alpha_2_means <- seq(600, 640, by = 10)  #range for lack of market research
	beta_3_means <- seq(560, 600, by = 10)   #range for political instability
	
	# Step 3: Run simulations for each threshold mean and store results
	results <- data.frame(Alpha_2_mean = numeric(), Beta_3_mean = numeric(), 
	                      ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (alpha_2_mean in alpha_2_means) {
	  for (beta_3_mean in beta_3_means) {
	    default_count <- 0  # Counter for default cases
	    
	    for (i in 1:simulations) {
	      # Generate missed payments due lack of market research for the borrower over the 
	      #defined number of months
	      lack_market_res <- rnorm(months, mean = mu_2, sd = sigma_2)
	      
	      # Generate missed payments due to political instability for the borrower over the
	      #defined number of months
	      political_insta <- rpois(months, lambda_3)
	      
	      # Generate random normal distributed threshold values for each 6-month period
	      alpha_2 <- rnorm(months / 6, mean = alpha_2_mean, sd = 70)
	      beta_3 <- rnorm(months / 6, mean = beta_3_mean, sd = 70)
	      
	      # Calculate total missed payments every 6 months
	      for (j in seq(1, months, by = 6)) {
	        period_index <- (j - 1) / 6 + 1
	        threshold_lack_market_res <- alpha_2[period_index]
	        threshold_political_insta <- beta_3[period_index]
	        if ((sum(lack_market_res[j:(j + 5)]) >  threshold_lack_market_res) &&
	            (sum(political_insta[j:(j + 5)]) > threshold_political_insta)) {
	          default_count <- default_count + 1
	          break  # Stop checking further if already defaulted
	        }
	      }
	    }
	    
	    # Step 4: Calculate the probability of default for the current threshold means
	    probability_of_default <- default_count / simulations
	    
	    # Store the result
	    results <- rbind(results, data.frame(Alpha_2_mean = alpha_2_mean, 
	                                         Beta_3_mean = beta_3_mean,
	                                         ProbabilityOfDefault = probability_of_default))
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	
	#Poor business operations management and Economic Downturns
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# poor business operations management missed payments parameters (assumed mean and 
	#standard deviation)
	mu_3 <- 60  
	sigma_3<- 30     
	
	# Economic downturns missed payments parameter (assumed mean)
	lambda_1<-90   
	
	# Step 2: Define a range of threshold means to test
	alpha_3_means <- seq(500, 540, by = 10) #range for poor business operations management
	beta_1_means <- seq(510, 550, by = 10)   #range for economic downturns
	
	# Step 3: Run simulations for each threshold mean and store results
	results <- data.frame(Alpha_2_mean = numeric(), Beta_3_mean = numeric(), 
	                      ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (alpha_3_mean in alpha_3_means) {
	  for (beta_1_mean in beta_1_means) {
	    default_count <- 0  # Counter for default cases
	    
	    for (i in 1:simulations) {
	      # Generate missed payments due to poor business operations management for 
	      #the borrower over the defined number of months
	      poor_business_op <- rnorm(months, mean = mu_3, sd = sigma_3)
	      
	      # Generate missed payments due to economic downturns for the borrower over the
	      #defined number of months
	      economic_downt <- rpois(months, lambda_1)
	      
	      # Generate random normal distributed threshold values for each 6-month period
	      alpha_3 <- rnorm(months / 6, mean = alpha_3_mean, sd = 70)
	      beta_1 <- rnorm(months / 6, mean = beta_1_mean, sd = 70)
	      
	      # Calculate total missed payments every 6 months
	      for (j in seq(1, months, by = 6)) {
	        period_index <- (j - 1) / 6 + 1
	        threshold_poor_business_op <- alpha_3[period_index]
	        threshold_economic_downt <- beta_1[period_index]
	        if ((sum(poor_business_op[j:(j + 5)]) >  threshold_poor_business_op) &&
	            (sum(economic_downt[j:(j + 5)]) > threshold_economic_downt)) {
	          default_count <- default_count + 1
	          break  # Stop checking further if already defaulted
	        }
	      }
	    }
	    
	    # Step 4: Calculate the probability of default for the current threshold means
	    probability_of_default <- default_count / simulations
	    
	    # Store the result
	    results <- rbind(results, data.frame(Alpha_3_mean = alpha_3_mean, 
	                                         Beta_1_mean = beta_1_mean,
	                                         ProbabilityOfDefault = probability_of_default))
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	
	
	#Poor business operations management and Natural Disasters
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# poor business operations management missed payments parameters(assumed mean and 
	#standard deviation)
	mu_3 <- 60  
	sigma_3<- 30   
	
	# natural disasters missed payments parameter (assumed mean)
	lambda_2<-90   
	
	# Step 2: Define a range of threshold means to test
	alpha_3_means <- seq(500, 540, by = 10)  # Example range for poor business operations
	#management
	beta_2_means <- seq(610, 650, by = 10)   # Example range for natural disasters
	
	# Step 3: Run simulations for each threshold mean and store results
	results <- data.frame(Alpha_3_mean = numeric(), Beta_2_mean = numeric(), 
	                      ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (alpha_3_mean in alpha_3_means) {
	  for (beta_2_mean in beta_2_means) {
	    default_count <- 0  # Counter for default cases
	    
	    for (i in 1:simulations) {
	      # Generate missed payments due to poor business operations management for 
	      #the borrower over the defined number of months
	      poor_business_op <- rnorm(months, mean = mu_3, sd = sigma_3)
	      
	      # Generate missed payments due to natural disasters for the borrower over the
	      #defined number of months
	      natural_disa <- rpois(months, lambda_2)
	      
	      # Generate random normal distributed threshold values for each 6-month period
	      alpha_3 <- rnorm(months / 6, mean = alpha_3_mean, sd = 70)
	      beta_2 <- rnorm(months / 6, mean = beta_2_mean, sd = 70)
	      
	      # Calculate total missed payments every 6 months
	      for (j in seq(1, months, by = 6)) {
	        period_index <- (j - 1) / 6 + 1
	        threshold_poor_business_op <- alpha_3[period_index]
	        threshold_natural_disa <- beta_2[period_index]
	        if ((sum(poor_business_op[j:(j + 5)]) >  threshold_poor_business_op) &&
	            (sum(natural_disa[j:(j + 5)]) > threshold_natural_disa)) {
	          default_count <- default_count + 1
	          break  # Stop checking further if already defaulted
	        }
	      }
	    }
	    
	    # Step 4: Calculate the probability of default for the current threshold means
	    probability_of_default <- default_count / simulations
	    
	    # Store the result
	    results <- rbind(results, data.frame(Alpha_3_mean = alpha_3_mean, 
	                                         Beta_2_mean = beta_2_mean,
	                                         ProbabilityOfDefault = probability_of_default))
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	
	#Poor business operations management and Political Instability
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# poor business operations management missed payments parameters (assumed mean and  
	#standard deviation)
	mu_3 <- 60  
	sigma_3<- 30     
	
	# political instability missed payments parameter (assumed mean)
	lambda_3<-90   
	
	# Step 2: Define a range of threshold means to test
	alpha_3_means <- seq(500, 540, by = 10)  #  range for poor business operations 
	#management
	beta_3_means <- seq(560, 600, by = 10)   #  range for political instability
	
	# Step 3: Run simulations for each threshold mean and store results
	results <- data.frame(Alpha_3_mean = numeric(), Beta_3_mean = numeric(), 
	                      ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (alpha_3_mean in alpha_3_means) {
	  for (beta_3_mean in beta_3_means) {
	    default_count <- 0  # Counter for default cases
	    
	    for (i in 1:simulations) {
	      # Generate missed payments due to poor business operations management for the borrower over the 
	      #defined number of months
	      poor_business_op <- rnorm(months, mean = mu_3, sd = sigma_3)
	      
	      # Generate missed payments due to political instability for the borrower over the
	      #defined number of months
	      political_insta <- rpois(months, lambda_3)
	      
	      # Generate random normal distributed threshold values for each 6-month period
	      alpha_3 <- rnorm(months / 6, mean = alpha_3_mean, sd = 70)
	      beta_3 <- rnorm(months / 6, mean = beta_3_mean, sd = 70)
	      
	      # Calculate total missed payments every 6 months
	      for (j in seq(1, months, by = 6)) {
	        period_index <- (j - 1) / 6 + 1
	        threshold_poor_business_op <- alpha_3[period_index]
	        threshold_political_insta <- beta_3[period_index]
	        if ((sum(poor_business_op[j:(j + 5)]) >  threshold_poor_business_op) &&
	            (sum(political_insta[j:(j + 5)]) > threshold_political_insta)) {
	          default_count <- default_count + 1
	          break  # Stop checking further if already defaulted
	        }
	      }
	    }
	    
	    # Step 4: Calculate the probability of default for the current threshold means
	    probability_of_default <- default_count / simulations
	    
	    # Store the result
	    results <- rbind(results, data.frame(Alpha_3_mean = alpha_3_mean, 
	                                         Beta_3_mean = beta_3_mean,
	                                         ProbabilityOfDefault = probability_of_default))
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	
	
	#Lack of financial literacy and Loan deviation
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# Lack of financial literacy missed payments parameters (assumed mean and standard 
	#deviation)
	mu_1 <- 60  
	sigma_1<- 30   
	
	# loan deviation missed payments parameters (assumed mean)
	lambda_4<-70   
	
	# Step 2: Define a range of threshold means to test
	alpha_1_means <- seq(550, 590, by = 10)  # range for lack of financial literacy
	gamma_1_means <- seq(500, 540, by = 10)   # range for loan deviation
	
	# Step 3: Run simulations for each threshold mean and store results
	results <- data.frame(Alpha_1_mean = numeric(), Gamma_1_mean = numeric(), 
	                      ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (alpha_1_mean in alpha_1_means) {
	  for (gamma_1_mean in gamma_1_means) {
	    default_count <- 0  # Counter for default cases
	    
	    for (i in 1:simulations) {
	      # Generate missed payments due lack of financial literacy for the borrower over the 
	      #defined number of months
	      lack_financial_lit <- rnorm(months, mean = mu_1, sd = sigma_1)
	      
	      # Generate missed payments due to loan deviation for the borrower over the
	      #defined number of months
	      loan_deviation <- rpois(months, lambda_4)
	      
	      # Generate random normal distributed threshold values for each 6-month period
	      alpha_1 <- rnorm(months / 6, mean = alpha_1_mean, sd = 70)
	      gamma_1 <- rnorm(months / 6, mean = gamma_1_mean, sd = 70)
	      
	      # Calculate total missed payments every 6 months
	      for (j in seq(1, months, by = 6)) {
	        period_index <- (j - 1) / 6 + 1
	        threshold_lack_financial_lit <- alpha_1[period_index]
	        threshold_loan_deviation <- gamma_1[period_index]
	        if ((sum(lack_financial_lit[j:(j + 5)]) >  threshold_lack_financial_lit) &&
	            (sum(loan_deviation[j:(j + 5)]) > threshold_loan_deviation)) {
	          default_count <- default_count + 1
	          break  # Stop checking further if already defaulted
	        }
	      }
	    }
	    
	    # Step 4: Calculate the probability of default for the current threshold means
	    probability_of_default <- default_count / simulations
	    
	    # Store the result
	    results <- rbind(results, data.frame(Alpha_1_mean = alpha_1_mean, 
	                                         Gamma_1_mean = gamma_1_mean,
	                                         ProbabilityOfDefault = probability_of_default))
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	
	#Lack of financial literacy and High interest rates
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# Lack of financial literacy missed payments parameters (assumed mean and standard 
	#deviation)
	mu_1 <- 60  
	sigma_1<- 30     
	
	# high interest rates missed payments parameters (assumed mean)
	lambda_5<-70   
	
	# Step 2: Define a range of threshold means to test
	alpha_1_means <- seq(550, 590, by = 10)  # range for lack of financial literacy
	gamma_2_means <- seq(550, 590, by = 10)   # range for high interest rates
	
	# Step 3: Run simulations for each threshold mean and store results
	results <- data.frame(Alpha_1_mean = numeric(), Gamma_2_mean = numeric(), 
	                      ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (alpha_1_mean in alpha_1_means) {
	  for (gamma_2_mean in gamma_2_means) {
	    default_count <- 0  # Counter for default cases
	    
	    for (i in 1:simulations) {
	      # Generate missed payments due lack of financial literacy for the borrower over the 
	      #defined number of months
	      lack_financial_lit <- rnorm(months, mean = mu_1, sd = sigma_1)
	      
	      # Generate missed payments due to high interest rates for the borrower over the
	      #defined number of months
	      high_interest<- rpois(months, lambda_5)
	      
	      # Generate random normal distributed threshold values for each 6-month period
	      alpha_1 <- rnorm(months / 6, mean = alpha_1_mean, sd = 70)
	      gamma_2 <- rnorm(months / 6, mean = gamma_2_mean, sd = 70)
	      
	      # Calculate total missed payments every 6 months
	      for (j in seq(1, months, by = 6)) {
	        period_index <- (j - 1) / 6 + 1
	        threshold_lack_financial_lit <- alpha_1[period_index]
	        threshold_high_interest <- gamma_2[period_index]
	        if ((sum(lack_financial_lit[j:(j + 5)]) >  threshold_lack_financial_lit) &&
	            (sum(high_interest[j:(j + 5)]) > threshold_high_interest)) {
	          default_count <- default_count + 1
	          break  # Stop checking further if already defaulted
	        }
	      }
	    }
	    
	    # Step 4: Calculate the probability of default for the current threshold means
	    probability_of_default <- default_count / simulations
	    
	    # Store the result
	    results <- rbind(results, data.frame(Alpha_1_mean = alpha_1_mean, 
	                                         Gamma_2_mean = gamma_2_mean,
	                                         ProbabilityOfDefault = probability_of_default))
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	
	#Lack of financial literacy and Loan delay
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# Lack of financial literacy missed payments parameters (assumed mean and standard 
	#deviation)
	mu_1 <- 60
	sigma_1<- 30     
	
	# loan delay missed payments parameters (assumed mean)
	theta<-1/70   
	
	# Step 2: Define a range of threshold means to test
	alpha_1_means <- seq(550, 590, by = 10)  #  range for lack of financial literacy
	gamma_3_means <- seq(800, 1000, by = 10)   #  range for loan delay
	
	# Step 3: Run simulations for each threshold mean and store results
	results <- data.frame(Alpha_1_mean = numeric(), Gamma_2_mean = numeric(), 
	                      ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (alpha_1_mean in alpha_1_means) {
	  for (gamma_3_mean in gamma_3_means) {
	    default_count <- 0  # Counter for default cases
	    
	    for (i in 1:simulations) {
	      # Generate missed payments due to lack of financial literacy for the borrower over the 
	      #defined number of months
	      lack_financial_lit <- rnorm(months, mean = mu_1, sd = sigma_1)
	      
	      # Generate missed payments due to loan delay for the borrower over the
	      #defined number of months
	      loan_delay<- rexp(months, theta)
	      
	      # Generate random normal distributed threshold values for each 6-month period
	      alpha_1 <- rnorm(months / 6, mean = alpha_1_mean, sd = 70)
	      gamma_3 <- rnorm(months / 6, mean = gamma_3_mean, sd = 70)
	      
	      # Calculate total missed payments every 6 months
	      for (j in seq(1, months, by = 6)) {
	        period_index <- (j - 1) / 6 + 1
	        threshold_lack_financial_lit <- alpha_1[period_index]
	        threshold_loan_delay<- gamma_3[period_index]
	        if ((sum(lack_financial_lit[j:(j + 5)]) >  threshold_lack_financial_lit) &&
	            (sum(loan_delay[j:(j + 5)]) > threshold_loan_delay)) {
	          default_count <- default_count + 1
	          break  # Stop checking further if already defaulted
	        }
	      }
	    }
	    
	    # Step 4: Calculate the probability of default for the current threshold means
	    probability_of_default <- default_count / simulations
	    
	    # Store the result
	    results <- rbind(results, data.frame(Alpha_1_mean = alpha_1_mean, 
	                                         Gamma_3_mean = gamma_3_mean,
	                                         ProbabilityOfDefault = probability_of_default))
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	
	
	#Lack of market research and Loan deviation
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# Lack of market research missed payments parameters (assumed mean and standard    
	#deviation)
	mu_2 <- 60  
	sigma_2<- 30    
	
	# loan deviation missed payments parameters (assumed mean)
	lambda_4<-70   
	
	# Step 2: Define a range of threshold means to test
	alpha_2_means <- seq(600, 640, by = 10)  #  range for lack of market research
	gamma_1_means <- seq(500, 540, by = 10)   # range for economic downturns
	
	# Step 3: Run simulations for each threshold mean and store results
	results <- data.frame(Alpha_2_mean = numeric(), Gamma_1_mean = numeric(), 
	                      ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (alpha_2_mean in alpha_2_means) {
	  for (gamma_1_mean in gamma_1_means) {
	    default_count <- 0  # Counter for default cases
	    
	    for (i in 1:simulations) {
	      # Generate missed payments due lack of market research for the borrower over the 
	      #defined number of months
	      lack_market_res <- rnorm(months, mean = mu_2, sd = sigma_2)
	      
	      # Generate missed payments due to loan deviation for the borrower over the
	      #defined number of months
	      loan_deviation<- rpois(months, lambda_4)
	      
	      # Generate random normal distributed threshold values for each 6-month period
	      alpha_2 <- rnorm(months / 6, mean = alpha_2_mean, sd = 70)
	      gamma_1 <- rnorm(months / 6, mean = gamma_1_mean, sd = 70)
	      
	      # Calculate total missed payments every 6 months
	      for (j in seq(1, months, by = 6)) {
	        period_index <- (j - 1) / 6 + 1
	        threshold_lack_market_res <- alpha_2[period_index]
	        threshold_loan_deviation<- gamma_1[period_index]
	        if ((sum(lack_market_res[j:(j + 5)]) >  threshold_lack_market_res) &&
	            (sum(loan_deviation[j:(j + 5)]) > threshold_loan_deviation)) {
	          default_count <- default_count + 1
	          break  # Stop checking further if already defaulted
	        }
	      }
	    }
	    
	    # Step 4: Calculate the probability of default for the current threshold means
	    probability_of_default <- default_count / simulations
	    
	    # Store the result
	    results <- rbind(results, data.frame(Alpha_2_mean = alpha_2_mean, 
	                                         Gamma_1_mean = gamma_1_mean,
	                                         ProbabilityOfDefault = probability_of_default))
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	#Lack of market research and High interest rates
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# Lack of market research missed payments parameters (assumed mean and standard 
	#deviation)
	mu_2 <- 60  
	sigma_2<- 30   
	
	# high interest rates missed payments  parameters (assumed mean)
	lambda_5<-70   
	
	# Step 2: Define a range of threshold means to test
	alpha_2_means <- seq(600, 640, by = 10)  #  range for lack of market research
	gamma_2_means <- seq(550, 590, by = 10)   # range for high interest rates
	
	# Step 3: Run simulations for each threshold mean and store results
	results <- data.frame(Alpha_2_mean = numeric(), Gamma_2_mean = numeric(), 
	                      ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (alpha_2_mean in alpha_2_means) {
	  for (gamma_2_mean in gamma_2_means) {
	    default_count <- 0  # Counter for default cases
	    
	    for (i in 1:simulations) {
	      # Generate missed payments due lack of market research for the borrower over the 
	      #defined number of months
	      lack_market_res <- rnorm(months, mean = mu_2, sd = sigma_2)
	      
	      # Generate missed payments due to high interest rates for the borrower over the
	      #defined number of months
	      high_interest<- rpois(months, lambda_5)
	      
	      # Generate random normal distributed threshold values for each 6-month period
	      alpha_2 <- rnorm(months / 6, mean = alpha_2_mean, sd = 70)
	      gamma_2 <- rnorm(months / 6, mean = gamma_2_mean, sd = 70)
	      
	      # Calculate total missed payments every 6 months
	      for (j in seq(1, months, by = 6)) {
	        period_index <- (j - 1) / 6 + 1
	        threshold_lack_market_res <- alpha_2[period_index]
	        threshold_high_interest<- gamma_2[period_index]
	        if ((sum(lack_market_res[j:(j + 5)]) >  threshold_lack_market_res) &&
	            (sum(high_interest[j:(j + 5)]) > threshold_high_interest)) {
	          default_count <- default_count + 1
	          break  # Stop checking further if already defaulted
	        }
	      }
	    }
	    
	    # Step 4: Calculate the probability of default for the current threshold means
	    probability_of_default <- default_count / simulations
	    
	    # Store the result
	    results <- rbind(results, data.frame(Alpha_2_mean = alpha_2_mean, 
	                                         Gamma_2_mean = gamma_2_mean,
	                                         ProbabilityOfDefault = probability_of_default))
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	
	#Lack of market research and Loan delay
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# Lack of market research misssed payments parameters (assumed mean and standard  
	#deviation)
	mu_2 <- 60  
	sigma_2<- 30    
	
	# loan delay missed payments parameters (assumed mean)
	theta<-1/70   
	
	# Step 2: Define a range of threshold means to test
	alpha_2_means <- seq(600, 640, by = 10)  #  range for lack of market research
	gamma_3_means <- seq(800, 1000, by = 10)   # range for loan delay
	
	# Step 3: Run simulations for each threshold mean and store results
	results <- data.frame(Alpha_2_mean = numeric(), Gamma_2_mean = numeric(), 
	                      ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (alpha_2_mean in alpha_2_means) {
	  for (gamma_3_mean in gamma_3_means) {
	    default_count <- 0  # Counter for default cases
	    
	    for (i in 1:simulations) {
	      # Generate missed payments due to lack of market researchfor the borrower over the 
	      #defined number of months
	      lack_market_res <- rnorm(months, mean = mu_2, sd = sigma_2)
	      
	      # Generate missed payments due to loan delay for the borrower over the
	      #defined number of months
	      loan_delay<- rexp(months, theta)
	      
	      # Generate random normal distributed threshold values for each 6-month period
	      alpha_2 <- rnorm(months / 6, mean = alpha_2_mean, sd = 70)
	      gamma_3 <- rnorm(months / 6, mean = gamma_3_mean, sd = 70)
	      
	      # Calculate total missed payments every 6 months
	      for (j in seq(1, months, by = 6)) {
	        period_index <- (j - 1) / 6 + 1
	        threshold_lack_market_res <- alpha_2[period_index]
	        threshold_loan_delay<- gamma_3[period_index]
	        if ((sum(lack_market_res[j:(j + 5)]) >  threshold_lack_market_res) &&
	            (sum(loan_delay[j:(j + 5)]) > threshold_loan_delay)) {
	          default_count <- default_count + 1
	          break  # Stop checking further if already defaulted
	        }
	      }
	    }
	    
	    # Step 4: Calculate the probability of default for the current threshold means
	    probability_of_default <- default_count / simulations
	    
	    # Store the result
	    results <- rbind(results, data.frame(Alpha_2_mean = alpha_2_mean, 
	                                         Gamma_3_mean = gamma_3_mean,
	                                         ProbabilityOfDefault = probability_of_default))
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	
	
	
	#Poor business operations management and Loan deviation
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# poor business operations management missed payments parameters (assumed mean and   
	#standard deviation)
	mu_3 <- 60  
	sigma_3<- 30 
	
	# loan deviation missed payments parameters (assumed mean)
	lambda_4<-70   
	
	# Step 2: Define a range of threshold means to test
	alpha_3_means <- seq(500, 540, by = 10)  # range for poor business operations    
	#management
	gamma_1_means <- seq(500, 540, by = 10)   # range for loan deviation
	
	# Step 3: Run simulations for each threshold mean and store results
	results <- data.frame(Alpha_3_mean = numeric(), Gamma_1_mean = numeric(), 
	                      ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (alpha_3_mean in alpha_3_means) {
	  for (gamma_1_mean in gamma_1_means) {
	    default_count <- 0  # Counter for default cases
	    
	    for (i in 1:simulations) {
	      # Generate missed payments due to poor business operations management for 
	      #the borrower over the defined number of months
	      poor_business_op <- rnorm(months, mean = mu_3, sd = sigma_3)
	      
	      # Generate missed payments due to loan deviation for the borrower over the
	      #defined number of months
	      loan_deviation<- rpois(months, lambda_4)
	      
	      # Generate random normal distributed threshold values for each 6-month period
	      alpha_3 <- rnorm(months / 6, mean = alpha_3_mean, sd = 70)
	      gamma_1 <- rnorm(months / 6, mean = gamma_1_mean, sd = 70)
	      
	      # Calculate total missed payments every 6 months
	      for (j in seq(1, months, by = 6)) {
	        period_index <- (j - 1) / 6 + 1
	        threshold_poor_business_op <- alpha_3[period_index]
	        threshold_loan_deviation<- gamma_1[period_index]
	        if ((sum(poor_business_op[j:(j + 5)]) >  threshold_poor_business_op) &&
	            (sum(loan_deviation[j:(j + 5)]) > threshold_loan_deviation)) {
	          default_count <- default_count + 1
	          break  # Stop checking further if already defaulted
	        }
	      }
	    }
	    
	    # Step 4: Calculate the probability of default for the current threshold means
	    probability_of_default <- default_count / simulations
	    
	    # Store the result
	    results <- rbind(results, data.frame(Alpha_3_mean = alpha_3_mean, 
	                                         Gamma_1_mean = gamma_1_mean,
	                                         ProbabilityOfDefault = probability_of_default))
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	
	
	#Poor business operations management and High interest rates}
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# poor business operations management parameters (assumed mean and standard deviation)
	mu_3 <- 60  
	sigma_3<- 30    
	
	# high interest rates missed payments parameters (assumed mean)
	lambda_5<-70   
	
	# Step 2: Define a range of threshold means to test
	alpha_3_means <- seq(500, 540, by = 10)  #  range for poor business operations 
	#management
	gamma_2_means <- seq(550, 590, by = 10)   # range for high interest rates
	
	# Step 3: Run simulations for each threshold mean and store results
	results <- data.frame(Alpha_3_mean = numeric(), Gamma_2_mean = numeric(), 
	                      ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (alpha_3_mean in alpha_3_means) {
	  for (gamma_2_mean in gamma_2_means) {
	    default_count <- 0  # Counter for default cases
	    
	    for (i in 1:simulations) {
	      # Generate missed payments due to poor business operations management for #the borrower over the defined number of months
	      poor_business_op <- rnorm(months, mean = mu_3, sd = sigma_3)
	      
	      # Generate missed payments due to high interest rates for the borrower over 
	      #the defined number of months
	      high_interest<- rpois(months, lambda_5)
	      
	      # Generate random normal distributed threshold values for each 6-month period
	      alpha_3 <- rnorm(months / 6, mean = alpha_3_mean, sd = 70)
	      gamma_2 <- rnorm(months / 6, mean = gamma_2_mean, sd = 70)
	      
	      # Calculate total missed payments every 6 months
	      for (j in seq(1, months, by = 6)) {
	        period_index <- (j - 1) / 6 + 1
	        threshold_poor_business_op <- alpha_3[period_index]
	        threshold_high_interest<- gamma_2[period_index]
	        if ((sum(poor_business_op[j:(j + 5)]) >  threshold_poor_business_op) &&
	            (sum(high_interest[j:(j + 5)]) > threshold_high_interest)) {
	          default_count <- default_count + 1
	          break  # Stop checking further if already defaulted
	        }
	      }
	    }
	    
	    # Step 4: Calculate the probability of default for the current threshold means
	    probability_of_default <- default_count / simulations
	    
	    # Store the result
	    results <- rbind(results, data.frame(Alpha_3_mean = alpha_3_mean, 
	                                         Gamma_2_mean = gamma_2_mean,
	                                         ProbabilityOfDefault = probability_of_default))
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	
	
	## Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# poor business operations management missed payments parameters (assumed mean and 
	#standard deviation)
	mu_3 <- 60  
	sigma_3<- 30     
	
	# loan delay  missed payments parameters (assumed mean)
	theta<-1/70   
	
	# Step 2: Define a range of threshold means to test
	alpha_3_means <- seq(500, 540, by = 10)  # range for poor business operations 
	#management
	gamma_3_means <- seq(800, 1000, by = 10)   # range for loan delay 
	
	# Step 3: Run simulations for each threshold mean and store results
	results <- data.frame(Alpha_3_mean = numeric(), Gamma_3_mean = numeric(), 
	                      ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (alpha_3_mean in alpha_3_means) {
	  for (gamma_3_mean in gamma_3_means) {
	    default_count <- 0  # Counter for default cases
	    
	    for (i in 1:simulations) {
	      # Generate missed payments due to poor business operations management for 
	      #the borrower over the defined number of months
	      poor_business_op <- rnorm(months, mean = mu_3, sd = sigma_3)
	      
	      # Generate missed payments due to loan delay for the borrower over the
	      #defined number of months
	      loan_delay<- rexp(months, theta)
	      
	      # Generate random normal distributed threshold values for each 6-month period
	      alpha_3 <- rnorm(months / 6, mean = alpha_3_mean, sd = 70)
	      gamma_3 <- rnorm(months / 6, mean = gamma_3_mean, sd = 70)
	      
	      # Calculate total missed payments every 6 months
	      for (j in seq(1, months, by = 6)) {
	        period_index <- (j - 1) / 6 + 1
	        threshold_poor_business_op <- alpha_3[period_index]
	        threshold_loan_delay<- gamma_3[period_index]
	        if ((sum(poor_business_op[j:(j + 5)]) >  threshold_poor_business_op) &&
	            (sum(loan_delay[j:(j + 5)]) > threshold_loan_delay)) {
	          default_count <- default_count + 1
	          break  # Stop checking further if already defaulted
	        }
	      }
	    }
	    
	    # Step 4: Calculate the probability of default for the current threshold means
	    probability_of_default <- default_count / simulations
	    
	    # Store the result
	    results <- rbind(results, data.frame(Alpha_3_mean = alpha_3_mean, 
	                                         Gamma_3_mean = gamma_3_mean,
	                                         ProbabilityOfDefault = probability_of_default))
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	
	#Economic downturns and Loan deviation
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# Economic downturns missed payments parameter
	lambda_1<-90
	
	# Loan deviation missed payments parameter
	lambda_4<-70  
	
	# Step 2: Define a range of threshold means to test
	beta_1_means <- seq(510, 550, by = 10)  # range for economic downturns
	gamma_1_means <- seq(500, 540, by = 10)   # range for loan deviation
	
	# Step 3: Run simulations for each threshold mean and store results
	results <- data.frame(Beta_1_mean = numeric(), Gamma_1_mean = numeric(), 
	                      ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (beta_1_mean in beta_1_means) {
	  for (gamma_1_mean in gamma_1_means) {
	    default_count <- 0  # Counter for default cases
	    
	    for (i in 1:simulations) {
	      # Generate missed payments due to loan deviation for the borrower over the 
	      #defined number of months
	      loan_deviation<- rpois(months, lambda_4)
	      
	      # Generate missed payments due to economic downturns for the borrower over the
	      #defined number of months
	      economic_downt<- rpois(months, lambda_1)
	      
	      # Generate random normal distributed threshold values for each 6-month period
	      beta_1 <- rnorm(months / 6, mean = beta_1_mean, sd = 70)
	      gamma_1 <- rnorm(months / 6, mean = gamma_1_mean, sd = 70)
	      
	      # Calculate total missed payments every 6 months
	      for (j in seq(1, months, by = 6)) {
	        period_index <- (j - 1) / 6 + 1
	        threshold_economic_downt <- beta_1[period_index]
	        threshold_loan_deviation<- gamma_1[period_index]
	        if ((sum(economic_downt[j:(j + 5)]) >  threshold_economic_downt) &&
	            (sum(loan_deviation[j:(j + 5)]) > threshold_loan_deviation)) {
	          default_count <- default_count + 1
	          break  # Stop checking further if already defaulted
	        }
	      }
	    }
	    
	    # Step 4: Calculate the probability of default for the current threshold means
	    probability_of_default <- default_count / simulations
	    
	    # Store the result
	    results <- rbind(results, data.frame(Beta_1_mean = beta_1_mean, 
	                                         Gamma_1_mean = gamma_1_mean,
	                                         ProbabilityOfDefault = probability_of_default))
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	
	#Economic downturns and High interest rates
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# Economic downturns missed payments parameters 
	lambda_1<-90
	
	# high interest rates missed payments parameter
	lambda_5<-70  
	
	# Step 2: Define a range of threshold means to test
	beta_1_means <- seq(510, 550, by = 10)  #  range for economic downturns
	gamma_2_means <- seq(550, 590, by = 10)   # range for high interest rates
	
	# Step 3: Run simulations for each threshold mean and store results
	results <- data.frame(Beta_1_mean = numeric(), Gamma_2_mean = numeric(), 
	                      ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (beta_1_mean in beta_1_means) {
	  for (gamma_2_mean in gamma_2_means) {
	    default_count <- 0  # Counter for default cases
	    
	    for (i in 1:simulations) {
	      # Generate missed payments due to high interest rates for the borrower
	      #over the defined number of months
	      high_interest<- rpois(months, lambda_5)
	      
	      # Generate missed payments due to economic downturns for the borrower over 
	      #the defined number of months
	      economic_downt<- rpois(months, lambda_1)
	      
	      # Generate random normal distributed threshold values for each 6-month period
	      beta_1 <- rnorm(months / 6, mean = beta_1_mean, sd = 70)
	      gamma_2 <- rnorm(months / 6, mean = gamma_2_mean, sd = 70)
	      
	      # Calculate total missed payments every 6 months
	      for (j in seq(1, months, by = 6)) {
	        period_index <- (j - 1) / 6 + 1
	        threshold_economic_downt <- beta_1[period_index]
	        threshold_high_interest<- gamma_2[period_index]
	        if ((sum(economic_downt[j:(j + 5)]) >  threshold_economic_downt) &&
	            (sum(high_interest[j:(j + 5)]) > threshold_high_interest)) {
	          default_count <- default_count + 1
	          break  # Stop checking further if already defaulted
	        }
	      }
	    }
	    
	    # Step 4: Calculate the probability of default for the current threshold means
	    probability_of_default <- default_count / simulations
	    
	    # Store the result
	    results <- rbind(results, data.frame(Beta_1_mean = beta_1_mean, 
	                                         Gamma_2_mean = gamma_2_mean,
	                                         ProbabilityOfDefault = probability_of_default))
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	
	
	
	#Economic downturns and Loan delay
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# Economic downturns missed payments parameter
	lambda_1<-90
	
	# Loan delay missed payments parameter
	theta<-1/70  
	
	# Step 2: Define a range of threshold means to test
	beta_1_means <- seq(510, 550, by = 10)  # range for economic downturns
	gamma_3_means <- seq(800, 1000, by = 10)   # range for Loan delay
	
	# Step 3: Run simulations for each threshold mean and store results
	results <- data.frame(Beta_1_mean = numeric(), Gamma_3_mean = numeric(), 
	                      ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (beta_1_mean in beta_1_means) {
	  for (gamma_3_mean in gamma_3_means) {
	    default_count <- 0  # Counter for default cases
	    
	    for (i in 1:simulations) {
	      # Generate missed payments due to loan delay for the borrower over the 
	      #defined number of months
	      loan_delay<- rexp(months, theta)
	      
	      # Generate missed payments due to economic downturns for the borrower over the
	      #defined number of months
	      economic_downt<- rpois(months, lambda_1)
	      
	      # Generate random normal distributed threshold values for each 6-month period
	      beta_1 <- rnorm(months / 6, mean = beta_1_mean, sd = 70)
	      gamma_3 <- rnorm(months / 6, mean = gamma_3_mean, sd = 70)
	      
	      # Calculate total missed payments every 6 months
	      for (j in seq(1, months, by = 6)) {
	        period_index <- (j - 1) / 6 + 1
	        threshold_economic_downt <- beta_1[period_index]
	        threshold_loan_delay<- gamma_3[period_index]
	        if ((sum(economic_downt[j:(j + 5)]) >  threshold_economic_downt) &&
	            (sum(loan_delay[j:(j + 5)]) > threshold_loan_delay)) {
	          default_count <- default_count + 1
	          break  # Stop checking further if already defaulted
	        }
	      }
	    }
	    
	    # Step 4: Calculate the probability of default for the current threshold means
	    probability_of_default <- default_count / simulations
	    
	    # Store the result
	    results <- rbind(results, data.frame(Beta_1_mean = beta_1_mean, 
	                                         Gamma_3_mean = gamma_3_mean,
	                                         ProbabilityOfDefault = probability_of_default))
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	
	#Natural disasters and Loan deviation
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# natural disasters missed payments parameter
	lambda_2<-90
	
	# Loan deviation missed payments parameter
	lambda_4<-70  
	
	# Step 2: Define a range of threshold means to test
	beta_2_means <- seq(610, 650, by = 10)  # range for natural disasters
	gamma_1_means <- seq(500, 540, by = 10)   # range for loan deviation
	
	# Step 3: Run simulations for each threshold mean and store results
	results <- data.frame(Beta_2_mean = numeric(), Gamma_1_mean = numeric(), 
	                      ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (beta_2_mean in beta_2_means) {
	  for (gamma_1_mean in gamma_1_means) {
	    default_count <- 0  # Counter for default cases
	    
	    for (i in 1:simulations) {
	      # Generate missed payments due to loan deviation for the borrower over the 
	      #defined number of months
	      loan_deviation<- rpois(months, lambda_4)
	      
	      # Generate missed payments due to natural disasters for the borrower over the
	      #defined number of months
	      natural_disa<- rpois(months, lambda_2)
	      
	      # Generate random normal distributed threshold values for each 6-month period
	      beta_2 <- rnorm(months / 6, mean = beta_2_mean, sd = 70)
	      gamma_1 <- rnorm(months / 6, mean = gamma_1_mean, sd = 70)
	      
	      # Calculate total missed payments every 6 months
	      for (j in seq(1, months, by = 6)) {
	        period_index <- (j - 1) / 6 + 1
	        threshold_natural_disa <- beta_2[period_index]
	        threshold_loan_deviation<- gamma_1[period_index]
	        if ((sum(natural_disa[j:(j + 5)]) >  threshold_natural_disa) &&
	            (sum(loan_deviation[j:(j + 5)]) > threshold_loan_deviation)) {
	          default_count <- default_count + 1
	          break  # Stop checking further if already defaulted
	        }
	      }
	    }
	    
	    # Step 4: Calculate the probability of default for the current threshold means
	    probability_of_default <- default_count / simulations
	    
	    # Store the result
	    results <- rbind(results, data.frame(Beta_2_mean = beta_2_mean, 
	                                         Gamma_1_mean = gamma_1_mean,
	                                         ProbabilityOfDefault = probability_of_default))
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	
	
	#Natural disasters and High interest rates
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# natural disasters missed payments parameter
	lambda_2<-90
	
	# high interest rates missed payments parameter
	lambda_5<-70  
	
	# Step 2: Define a range of threshold means to test
	beta_2_means <- seq(610, 650, by = 10)  # range for natural disasters
	gamma_2_means <- seq(550, 590, by = 10)   # range for high interest rates
	
	# Step 3: Run simulations for each threshold mean and store results
	results <- data.frame(Beta_2_mean = numeric(), Gamma_2_mean = numeric(), 
	                      ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (beta_2_mean in beta_2_means) {
	  for (gamma_2_mean in gamma_2_means) {
	    default_count <- 0  # Counter for default cases
	    
	    for (i in 1:simulations) {
	      # Generate missed payments due to high interest rates for the borrower over the 
	      #defined number of months
	      high_interest<- rpois(months, lambda_5)
	      
	      # Generate missed payments due to natural disasters for the borrower over the
	      #defined number of months
	      natural_disa<- rpois(months, lambda_2)
	      
	      # Generate random normal distributed threshold values for each 6-month period
	      beta_2 <- rnorm(months / 6, mean = beta_2_mean, sd = 70)
	      gamma_2 <- rnorm(months / 6, mean = gamma_2_mean, sd = 70)
	      
	      # Calculate total missed payments every 6 months
	      for (j in seq(1, months, by = 6)) {
	        period_index <- (j - 1) / 6 + 1
	        threshold_natural_disa <- beta_2[period_index]
	        threshold_high_interest<- gamma_2[period_index]
	        if ((sum(natural_disa[j:(j + 5)]) >  threshold_natural_disa) &&
	            (sum(high_interest[j:(j + 5)]) > threshold_high_interest)) {
	          default_count <- default_count + 1
	          break  # Stop checking further if already defaulted
	        }
	      }
	    }
	    
	    # Step 4: Calculate the probability of default for the current threshold means
	    probability_of_default <- default_count / simulations
	    
	    # Store the result
	    results <- rbind(results, data.frame(Beta_2_mean = beta_2_mean, 
	                                         Gamma_2_mean = gamma_2_mean,
	                                         ProbabilityOfDefault = probability_of_default))
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	
	
	
	#Natural disasters and Loan delay
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# natural disasters missed payments parameter
	lambda_2<-90
	
	# Loan delay missed payments parameter
	theta<-1/70  
	
	# Step 2: Define a range of threshold means to test
	beta_2_means <- seq(610, 650, by = 10)  # range for natural disasters
	gamma_3_means <- seq(800, 1000, by = 10)   # range for loan delay
	
	# Step 3: Run simulations for each threshold mean and store results
	results <- data.frame(Beta_2_mean = numeric(), Gamma_3_mean = numeric(), 
	                      ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (beta_2_mean in beta_2_means) {
	  for (gamma_3_mean in gamma_3_means) {
	    default_count <- 0  # Counter for default cases
	    
	    for (i in 1:simulations) {
	      # Generate missed payments due to loan delay for the borrower over the 
	      #defined number of months
	      loan_delay<- rexp(months, theta)
	      
	      # Generate missed payments due to natural disasters for the borrower over the
	      #defined number of months
	      natural_disa<- rpois(months, lambda_2)
	      
	      # Generate random normal distributed threshold values for each 6-month period
	      beta_2 <- rnorm(months / 6, mean = beta_2_mean, sd = 70)
	      gamma_3 <- rnorm(months / 6, mean = gamma_3_mean, sd = 70)
	      
	      # Calculate total missed payments every 6 months
	      for (j in seq(1, months, by = 6)) {
	        period_index <- (j - 1) / 6 + 1
	        threshold_natural_disa <- beta_2[period_index]
	        threshold_loan_delay<- gamma_3[period_index]
	        if ((sum(natural_disa[j:(j + 5)]) >  threshold_natural_disa) &&
	            (sum(loan_delay[j:(j + 5)]) > threshold_loan_delay)) {
	          default_count <- default_count + 1
	          break  # Stop checking further if already defaulted
	        }
	      }
	    }
	    
	    # Step 4: Calculate the probability of default for the current threshold means
	    probability_of_default <- default_count / simulations
	    
	    # Store the result
	    results <- rbind(results, data.frame(Beta_2_mean = beta_2_mean, 
	                                         Gamma_3_mean = gamma_3_mean,
	                                         ProbabilityOfDefault = probability_of_default))
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	
	
#Political instability and Loan deviation
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# political instability missed payments parameter
	lambda_3<-90
	
	# loan deviation missed payments parameter
	lambda_4<-70  
	
	# Step 2: Define a range of threshold means to test
	beta_3_means <- seq(560, 600, by = 10)  # range for political instability
	gamma_1_means <- seq(500, 540, by = 10)   # range for loan deviation
	
	# Step 3: Run simulations for each threshold mean and store results
	results <- data.frame(Beta_3_mean = numeric(), Gamma_1_mean = numeric(), 
	                      ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (beta_3_mean in beta_3_means) {
	  for (gamma_1_mean in gamma_1_means) {
	    default_count <- 0  # Counter for default cases
	    
	    for (i in 1:simulations) {
	      # Generate missed payments due to loan deviation for the borrower over the 
	      #defined number of months
	      loan_deviation<- rpois(months, lambda_4)
	      
	      # Generate missed payments due to political instability for the borrower over the
	      #defined number of months
	      political_insta<- rpois(months, lambda_3)
	      
	      # Generate random normal distributed threshold values for each 6-month period
	      beta_3 <- rnorm(months / 6, mean = beta_3_mean, sd = 70)
	      gamma_1 <- rnorm(months / 6, mean = gamma_1_mean, sd = 70)
	      
	      # Calculate total missed payments every 6 months
	      for (j in seq(1, months, by = 6)) {
	        period_index <- (j - 1) / 6 + 1
	        threshold_political_insta<- beta_3[period_index]
	        threshold_loan_deviation<- gamma_1[period_index]
	        if ((sum(political_insta[j:(j + 5)]) >  threshold_political_insta) &&
	            (sum(loan_deviation[j:(j + 5)]) > threshold_loan_deviation)) {
	          default_count <- default_count + 1
	          break  # Stop checking further if already defaulted
	        }
	      }
	    }
	    
	    # Step 4: Calculate the probability of default for the current threshold means
	    probability_of_default <- default_count / simulations
	    
	    # Store the result
	    results <- rbind(results, data.frame(Beta_3_mean = beta_3_mean, 
	                                         Gamma_1_mean = gamma_1_mean,
	                                         ProbabilityOfDefault = probability_of_default))
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	
	
	
#Political instability and High interest rates
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# political instability missed payments parameter
	lambda_3<-90
	
	# high interest rates missed payments parameters (assumed mean)
	lambda_5<-70  
	
	# Step 2: Define a range of threshold means to test
	beta_3_means <- seq(560, 600, by = 10)  # range for political instability
	gamma_2_means <- seq(550, 590, by = 10)   # range for high interest rates
	
	# Step 3: Run simulations for each threshold mean and store results
	results <- data.frame(Beta_3_mean = numeric(), Gamma_2_mean = numeric(), 
	                      ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (beta_3_mean in beta_3_means) {
	  for (gamma_2_mean in gamma_2_means) {
	    default_count <- 0  # Counter for default cases
	    
	    for (i in 1:simulations) {
	      # Generate missed payments due to high interest rates for the borrower over the 
	      #defined number of months
	      high_interest<- rpois(months, lambda_5)
	      
	      # Generate missed payments due to political instability for the borrower over the
	      #defined number of months
	      political_insta<- rpois(months, lambda_3)
	      
	      # Generate random normal distributed threshold values for each 6-month period
	      beta_3 <- rnorm(months / 6, mean = beta_3_mean, sd = 70)
	      gamma_2 <- rnorm(months / 6, mean = gamma_2_mean, sd = 70)
	      
	      # Calculate total missed payments every 6 months
	      for (j in seq(1, months, by = 6)) {
	        period_index <- (j - 1) / 6 + 1
	        threshold_political_insta<- beta_3[period_index]
	        threshold_high_interest<- gamma_2[period_index]
	        if ((sum(political_insta[j:(j + 5)]) >  threshold_political_insta) &&
	            (sum(high_interest[j:(j + 5)]) > threshold_high_interest)) {
	          default_count <- default_count + 1
	          break  # Stop checking further if already defaulted
	        }
	      }
	    }
	    
	    # Step 4: Calculate the probability of default for the current threshold means
	    probability_of_default <- default_count / simulations
	    
	    # Store the result
	    results <- rbind(results, data.frame(Beta_3_mean = beta_3_mean, 
	                                         Gamma_2_mean = gamma_2_mean,
	                                         ProbabilityOfDefault = probability_of_default))
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	
	#Political instability and Loan delay
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# political instability missed payments parameters (assumed mean and standard deviation)
	lambda_3<-90
	
	# loan delay missed payments parameters (assumed mean)
	theta<-1/70  
	
	# Step 2: Define a range of threshold means to test
	beta_3_means <- seq(560, 600, by = 10)  # range for political instability
	gamma_3_means <- seq(800, 1000, by = 10)   # range for loan delay
	
	# Step 3: Run simulations for each threshold mean and store results
	results <- data.frame(Beta_3_mean = numeric(), Gamma_3_mean = numeric(), 
	                      ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (beta_3_mean in beta_3_means) {
	  for (gamma_3_mean in gamma_3_means) {
	    default_count <- 0  # Counter for default cases
	    
	    for (i in 1:simulations) {
	      # Generate missed payments due to loan delay for the borrower over the 
	      #defined number of months
	      loan_delay<- rexp(months, theta)
	      
	      # Generate missed payments due to political instability for the borrower over the
	      #defined number of months
	      political_insta<- rpois(months, lambda_3)
	      
	      # Generate random normal distributed threshold values for each 6-month period
	      beta_3 <- rnorm(months / 6, mean = beta_3_mean, sd = 70)
	      gamma_3 <- rnorm(months / 6, mean = gamma_3_mean, sd = 70)
	      
	      # Calculate total missed payments every 6 months
	      for (j in seq(1, months, by = 6)) {
	        period_index <- (j - 1) / 6 + 1
	        threshold_political_insta<- beta_3[period_index]
	        threshold_loan_delay<- gamma_3[period_index]
	        if ((sum(political_insta[j:(j + 5)]) >  threshold_political_insta) &&
	            (sum(loan_delay[j:(j + 5)]) > threshold_loan_delay)) {
	          default_count <- default_count + 1
	          break  # Stop checking further if already defaulted
	        }
	      }
	    }
	    
	    # Step 4: Calculate the probability of default for the current threshold means
	    probability_of_default <- default_count / simulations
	    
	    # Store the result
	    results <- rbind(results, data.frame(Beta_3_mean = beta_3_mean, 
	                                         Gamma_3_mean = gamma_3_mean,
	                                         ProbabilityOfDefault = probability_of_default))
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	
	#Lack of financial literacy,Economic downturns and Loan deviation
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# Parameters for missed payments due to various factors
	mu_1 <- 60                 # Mean for missed payments due to lack of financial literacy
	sigma_1 <- 30              # Standard deviation for missed payments due to lack of 
	#financial literacy
	lambda_1 <- 90             # Mean for missed payments due to economic downturns
	lambda_4 <- 70             # Mean for missed payments due to loan deviation
	
	# Step 2: Define a range of threshold means to test for each factor
	alpha_1_means <- seq(550, 590, by = 10)  # Range for lack of financial literacy
	beta_1_means <- seq(510, 550, by = 10)   # Range for economic downturns
	gamma_1_means <- seq(500, 540, by = 10)  # Range for loan deviation
	
	# Step 3: Run simulations for each combination of threshold means and store results
	results <- data.frame(Alpha_1_mean = numeric(), Beta_1_mean = numeric(), 
	                      Gamma_1_mean = numeric(), ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (alpha_1_mean in alpha_1_means) {
	  for (beta_1_mean in beta_1_means) {
	    for (gamma_1_mean in gamma_1_means) {
	      default_count <- 0  # Counter for default cases
	      
	      for (i in 1:simulations) {
	        # Generate missed payments due to each factor over the defined number of months
	        lack_financial_lit <- rnorm(months, mean = mu_1, sd = sigma_1)  # Factor 1
	        economic_downt <- rpois(months, lambda_1)                       # Factor 2
	        loan_deviation <- rpois(months, lambda_4)                       # Factor 3
	        
	        # Generate random normal distributed threshold values for each 6-month period
	        alpha_1 <- rnorm(months / 6, mean = alpha_1_mean, sd = 70)
	        beta_1 <- rnorm(months / 6, mean = beta_1_mean, sd = 70)
	        gamma_1 <- rnorm(months / 6, mean = gamma_1_mean, sd = 70)
	        
	        # Calculate total missed payments every 6 months
	        for (j in seq(1, months, by = 6)) {
	          period_index <- (j - 1) / 6 + 1
	          threshold_lack_financial_lit <- alpha_1[period_index]
	          threshold_economic_downt <- beta_1[period_index]
	          threshold_loan_deviation <- gamma_1[period_index]
	          if ((sum(lack_financial_lit[j:(j + 5)]) > threshold_lack_financial_lit) &&
	              (sum(economic_downt[j:(j + 5)]) > threshold_economic_downt) &&
	              (sum(loan_deviation[j:(j + 5)]) > threshold_loan_deviation)) {
	            default_count <- default_count + 1
	            break  # Stop checking further if already defaulted
	          }
	        }
	      }
	      
	      # Step 4: Calculate the probability of default for the current combination of threshold means
	      probability_of_default <- default_count / simulations
	      
	      # Store the result
	      results <- rbind(results, data.frame(Alpha_1_mean = alpha_1_mean, 
	                                           Beta_1_mean = beta_1_mean, 
	                                           Gamma_1_mean = gamma_1_mean, 
	                                           ProbabilityOfDefault = probability_of_default))
	    }
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	#Lack of financial literacy,Economic downturns and High interest rates
	## Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# Parameters for missed payments due to various factors
	mu_1 <- 60  # Mean for missed payments due to lack of financial literacy
	sigma_1 <- 30#Standard deviation for missed payments due to lack of financialliteracy
	lambda_1 <- 90             # Mean for missed payments due to economic downturns
	lambda_5 <- 70             # Mean for missed payments due to high interest rates
	
	# Step 2: Define a range of threshold means to test for each factor
	alpha_1_means <- seq(550, 590, by = 10)  # Range for lack of financial literacy
	beta_1_means <- seq(510, 550, by = 10)   # Range for economic downturns
	gamma_2_means <- seq(550, 590, by = 10)  # Range for  high interest rates
	
	# Step 3: Run simulations for each combination of threshold means and store results
	results <- data.frame(Alpha_1_mean = numeric(), Beta_1_mean = numeric(), 
	                      Gamma_2_mean = numeric(), ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (alpha_1_mean in alpha_1_means) {
	  for (beta_1_mean in beta_1_means) {
	    for (gamma_2_mean in gamma_2_means) {
	      default_count <- 0  # Counter for default cases
	      
	      for (i in 1:simulations) {
	        # Generate missed payments due to each factor over the defined number of months
	        lack_financial_lit <- rnorm(months, mean = mu_1, sd = sigma_1)  # Factor 1
	        economic_downt <- rpois(months, lambda_1)                       # Factor 2
	        high_interest <- rpois(months, lambda_5)                       # Factor 3
	        
	        # Generate random normal distributed threshold values for each 6-month period
	        alpha_1 <- rnorm(months / 6, mean = alpha_1_mean, sd = 70)
	        beta_1 <- rnorm(months / 6, mean = beta_1_mean, sd = 70)
	        gamma_2 <- rnorm(months / 6, mean = gamma_2_mean, sd = 70)
	        
	        # Calculate total missed payments every 6 months
	        for (j in seq(1, months, by = 6)) {
	          period_index <- (j - 1) / 6 + 1
	          threshold_lack_financial_lit <- alpha_1[period_index]
	          threshold_economic_downt <- beta_1[period_index]
	          threshold_high_interest <- gamma_2[period_index]
	          if ((sum(lack_financial_lit[j:(j + 5)]) > threshold_lack_financial_lit) &&
	              (sum(economic_downt[j:(j + 5)]) > threshold_economic_downt) &&
	              (sum(high_interest[j:(j + 5)]) > threshold_high_interest)) {
	            default_count <- default_count + 1
	            break  # Stop checking further if already defaulted
	          }
	        }
	      }
	      
	      # Step 4: Calculate the probability of default for the current combination of threshold means
	      probability_of_default <- default_count / simulations
	      
	      # Store the result
	      results <- rbind(results, data.frame(Alpha_1_mean = alpha_1_mean, 
	                                           Beta_1_mean = beta_1_mean, 
	                                           Gamma_2_mean = gamma_2_mean, 
	                                           ProbabilityOfDefault = probability_of_default))
	    }
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	
	#Lack of financial literacy,Economic downturns and Loan delay
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# Parameters for missed payments due to various factors
	mu_1 <- 60                 # Mean for missed payments due to lack of financial literacy
	sigma_1 <- 30# Standard deviation for missed payments due to lack of financial literacy
	lambda_1 <- 90             # Mean for missed payments due to economic downturns
	theta <- 1/70             # Mean for missed payments due to loan delay
	
	# Step 2: Define a range of threshold means to test for each factor
	alpha_1_means <- seq(550, 590, by = 10)  # Range for lack of financial literacy
	beta_1_means <- seq(510, 550, by = 10)   # Range for economic downturns
	gamma_3_means <- seq(800, 1000, by = 10)  # Range for loan delay
	
	# Step 3: Run simulations for each combination of threshold means and store results
	results <- data.frame(Alpha_1_mean = numeric(), Beta_1_mean = numeric(), 
	                      Gamma_3_mean = numeric(), ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (alpha_1_mean in alpha_1_means) {
	  for (beta_1_mean in beta_1_means) {
	    for (gamma_3_mean in gamma_3_means) {
	      default_count <- 0  # Counter for default cases
	      
	      for (i in 1:simulations) {
	        # Generate missed payments due to each factor over the defined number of months
	        lack_financial_lit <- rnorm(months, mean = mu_1, sd = sigma_1)  # Factor 1
	        economic_downt <- rpois(months, lambda_1)                       # Factor 2
	        loan_delay <- rexp(months, theta)                       # Factor 3
	        
	        # Generate random normal distributed threshold values for each 6-month period
	        alpha_1 <- rnorm(months / 6, mean = alpha_1_mean, sd = 70)
	        beta_1 <- rnorm(months / 6, mean = beta_1_mean, sd = 70)
	        gamma_3 <- rnorm(months / 6, mean = gamma_3_mean, sd = 70)
	        
	        # Calculate total missed payments every 6 months
	        for (j in seq(1, months, by = 6)) {
	          period_index <- (j - 1) / 6 + 1
	          threshold_lack_financial_lit <- alpha_1[period_index]
	          threshold_economic_downt <- beta_1[period_index]
	          threshold_loan_delay <- gamma_3[period_index]
	          if ((sum(lack_financial_lit[j:(j + 5)]) > threshold_lack_financial_lit) &&
	              (sum(economic_downt[j:(j + 5)]) > threshold_economic_downt) &&
	              (sum(loan_delay[j:(j + 5)]) > threshold_loan_delay)) {
	            default_count <- default_count + 1
	            break  # Stop checking further if already defaulted
	          }
	        }
	      }
	      
	      # Step 4: Calculate the probability of default for the current combination of threshold means
	      probability_of_default <- default_count / simulations
	      
	      # Store the result
	      results <- rbind(results, data.frame(Alpha_1_mean = alpha_1_mean, 
	                                           Beta_1_mean = beta_1_mean, 
	                                           Gamma_3_mean = gamma_3_mean, 
	                                           ProbabilityOfDefault = probability_of_default))
	    }
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	
	
	#Lack of financial literacy,Natural disasters and Loan deviation
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# Parameters for missed payments due to various factors
	mu_1 <- 60                 # Mean for missed payments due to lack of financial literacy
	sigma_1 <- 30# Standard deviation for missed payments due to lack of financial literacy
	lambda_2 <- 90             # Mean for missed payments due to natural disasters
	lambda_4 <- 70             # Mean for missed payments due to loan deviation
	
	# Step 2: Define a range of threshold means to test for each factor
	alpha_1_means <- seq(550, 590, by = 10)  # Range for lack of financial literacy
	beta_2_means <- seq(610, 650, by = 10)   # Range for natural disasters
	gamma_1_means <- seq(500, 540, by = 10)  # Range for loan deviation
	
	# Step 3: Run simulations for each combination of threshold means and store results
	results <- data.frame(Alpha_1_mean = numeric(), Beta_2_mean = numeric(), 
	                      Gamma_1_mean = numeric(), ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (alpha_1_mean in alpha_1_means) {
	  for (beta_2_mean in beta_2_means) {
	    for (gamma_1_mean in gamma_1_means) {
	      default_count <- 0  # Counter for default cases
	      
	      for (i in 1:simulations) {
	        # Generate missed payments due to each factor over the defined number of months
	        lack_financial_lit <- rnorm(months, mean = mu_1, sd = sigma_1)  # Factor 1
	        natural_disa <- rpois(months, lambda_2)                       # Factor 2
	        loan_deviation <- rpois(months, lambda_4)                       # Factor 3
	        
	        # Generate random normal distributed threshold values for each 6-month period
	        alpha_1 <- rnorm(months / 6, mean = alpha_1_mean, sd = 70)
	        beta_2 <- rnorm(months / 6, mean = beta_2_mean, sd = 70)
	        gamma_1 <- rnorm(months / 6, mean = gamma_1_mean, sd = 70)
	        
	        # Calculate total missed payments every 6 months
	        for (j in seq(1, months, by = 6)) {
	          period_index <- (j - 1) / 6 + 1
	          threshold_lack_financial_lit <- alpha_1[period_index]
	          threshold_natural_disa <- beta_2[period_index]
	          threshold_loan_deviation <- gamma_1[period_index]
	          if ((sum(lack_financial_lit[j:(j + 5)]) > threshold_lack_financial_lit) &&
	              (sum(economic_downt[j:(j + 5)]) > threshold_natural_disa) &&
	              (sum(loan_deviation[j:(j + 5)]) > threshold_loan_deviation)) {
	            default_count <- default_count + 1
	            break  # Stop checking further if already defaulted
	          }
	        }
	      }
	      
	      # Step 4: Calculate the probability of default for the current combination of threshold means
	      probability_of_default <- default_count / simulations
	      
	      # Store the result
	      results <- rbind(results, data.frame(Alpha_1_mean = alpha_1_mean, 
	                                           Beta_2_mean = beta_2_mean, 
	                                           Gamma_1_mean = gamma_1_mean, 
	                                           ProbabilityOfDefault = probability_of_default))
	    }
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	
	
	
	#Lack of financial literacy,Natural disasters and High interest rates
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# Parameters for missed payments due to various factors
	mu_1 <- 60                 # Mean for missed payments due to lack of financial literacy
	sigma_1 <- 30# Standard deviation for missed payments due to lack of financial literacy
	lambda_2 <- 90             # Mean for missed payments due to natural disasters
	lambda_5 <- 70             # Mean for missed payments due to high interest rates
	
	# Step 2: Define a range of threshold means to test for each factor
	alpha_1_means <- seq(550, 590, by = 10)  # Range for lack of financial literacy
	beta_2_means <- seq(610, 650, by = 10)   # Range for natural disasters
	gamma_2_means <- seq(550, 590, by = 10)  # Range for high interest rates
	
	# Step 3: Run simulations for each combination of threshold means and store results
	results <- data.frame(Alpha_1_mean = numeric(), Beta_2_mean = numeric(), 
	                      Gamma_2_mean = numeric(), ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (alpha_1_mean in alpha_1_means) {
	  for (beta_2_mean in beta_2_means) {
	    for (gamma_2_mean in gamma_2_means) {
	      default_count <- 0  # Counter for default cases
	      
	      for (i in 1:simulations) {
	        # Generate missed payments due to each factor over the defined number of months
	        lack_financial_lit <- rnorm(months, mean = mu_1, sd = sigma_1)  # Factor 1
	        natural_disa <- rpois(months, lambda_2)                       # Factor 2
	        high_interest <- rpois(months, lambda_5)                       # Factor 3
	        
	        # Generate random normal distributed threshold values for each 6-month period
	        alpha_1 <- rnorm(months / 6, mean = alpha_1_mean, sd = 70)
	        beta_2 <- rnorm(months / 6, mean = beta_2_mean, sd = 70)
	        gamma_2 <- rnorm(months / 6, mean = gamma_2_mean, sd = 70)
	        
	        # Calculate total missed payments every 6 months
	        for (j in seq(1, months, by = 6)) {
	          period_index <- (j - 1) / 6 + 1
	          threshold_lack_financial_lit <- alpha_1[period_index]
	          threshold_natural_disa <- beta_2[period_index]
	          threshold_high_interest <- gamma_2[period_index]
	          if ((sum(lack_financial_lit[j:(j + 5)]) > threshold_lack_financial_lit) &&
	              (sum(natural_disa[j:(j + 5)]) > threshold_natural_disa) &&
	              (sum(high_interest[j:(j + 5)]) > threshold_high_interest)) {
	            default_count <- default_count + 1
	            break  # Stop checking further if already defaulted
	          }
	        }
	      }
	      
	      # Step 4: Calculate the probability of default for the current combination of threshold means
	      probability_of_default <- default_count / simulations
	      
	      # Store the result
	      results <- rbind(results, data.frame(Alpha_1_mean = alpha_1_mean, 
	                                           Beta_2_mean = beta_2_mean, 
	                                           Gamma_2_mean = gamma_2_mean, 
	                                           ProbabilityOfDefault = probability_of_default))
	    }
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	
	
	#Lack of financial literacy,Natural disasters and Loan delay
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# Parameters for missed payments due to various factors
	mu_1 <- 60               # Mean for missed payments due to lack of financial literacy
	sigma_1 <- 30# Standard deviation for missed payments due to lack of financial literacy
	lambda_2 <- 90             # Mean for missed payments due to natural disasters
	theta <- 1/70             # Mean for missed payments due to loan delay
	
	# Step 2: Define a range of threshold means to test for each factor
	alpha_1_means <- seq(550, 590, by = 10)  # Range for lack of financial literacy
	beta_2_means <- seq(610, 650, by = 10)   # Range for natural disasters
	gamma_3_means <- seq(800, 1000, by = 10)  # Range for loan delay
	
	# Step 3: Run simulations for each combination of threshold means and store results
	results <- data.frame(Alpha_1_mean = numeric(), Beta_2_mean = numeric(), 
	                      Gamma_3_mean = numeric(), ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (alpha_1_mean in alpha_1_means) {
	  for (beta_2_mean in beta_2_means) {
	    for (gamma_3_mean in gamma_3_means) {
	      default_count <- 0  # Counter for default cases
	      
	      for (i in 1:simulations) {
	        # Generate missed payments due to each factor over the defined number of months
	        lack_financial_lit <- rnorm(months, mean = mu_1, sd = sigma_1)  # Factor 1
	        natural_disa <- rpois(months, lambda_2)                       # Factor 2
	        loan_delay <- rexp(months, theta)                       # Factor 3
	        
	        # Generate random normal distributed threshold values for each 6-month period
	        alpha_1 <- rnorm(months / 6, mean = alpha_1_mean, sd = 70)
	        beta_2 <- rnorm(months / 6, mean = beta_2_mean, sd = 70)
	        gamma_3 <- rnorm(months / 6, mean = gamma_3_mean, sd = 70)
	        
	        # Calculate total missed payments every 6 months
	        for (j in seq(1, months, by = 6)) {
	          period_index <- (j - 1) / 6 + 1
	          threshold_lack_financial_lit <- alpha_1[period_index]
	          threshold_natural_disa <- beta_2[period_index]
	          threshold_loan_delay <- gamma_3[period_index]
	          if ((sum(lack_financial_lit[j:(j + 5)]) > threshold_lack_financial_lit) &&
	              (sum(natural_disa[j:(j + 5)]) > threshold_natural_disa) &&
	              (sum(loan_delay[j:(j + 5)]) > threshold_loan_delay)) {
	            default_count <- default_count + 1
	            break  # Stop checking further if already defaulted
	          }
	        }
	      }
	      
	      # Step 4: Calculate the probability of default for the current combination of threshold means
	      probability_of_default <- default_count / simulations
	      
	      # Store the result
	      results <- rbind(results, data.frame(Alpha_1_mean = alpha_1_mean, 
	                                           Beta_2_mean = beta_2_mean, 
	                                           Gamma_3_mean = gamma_3_mean, 
	                                           ProbabilityOfDefault = probability_of_default))
	    }
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	
	
	
	#Lack of financial literacy,Political instability and Loan deviation
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# Parameters for missed payments due to various factors
	mu_1 <- 60         # Mean for missed payments due to lack of financial literacy
	sigma_1 <- 30# Standard deviation for missed payments due to lack of financial literacy
	lambda_3 <- 90     # Mean for missed payments due to political instability
	lambda_4 <- 70     # Mean for missed payments due to loan deviation
	
	# Step 2: Define a range of threshold means to test for each factor
	alpha_1_means <- seq(550, 590, by = 10)  # Range for lack of financial literacy
	beta_3_means <- seq(560, 600, by = 10)   # Range for political instability
	gamma_1_means <- seq(500, 540, by = 10)  # Range for loan deviation
	
	# Step 3: Run simulations for each combination of threshold means and store results
	results <- data.frame(Alpha_1_mean = numeric(), Beta_3_mean = numeric(), 
	                      Gamma_1_mean = numeric(), ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (alpha_1_mean in alpha_1_means) {
	  for (beta_3_mean in beta_3_means) {
	    for (gamma_1_mean in gamma_1_means) {
	      default_count <- 0  # Counter for default cases
	      
	      for (i in 1:simulations) {
	        # Generate missed payments due to each factor over the defined number of months
	        lack_financial_lit <- rnorm(months, mean = mu_1, sd = sigma_1)  # Factor 1
	        political_insta <- rpois(months, lambda_3)                       # Factor 2
	        loan_deviation <- rpois(months, lambda_4)                       # Factor 3
	        
	        # Generate random normal distributed threshold values for each 6-month period
	        alpha_1 <- rnorm(months / 6, mean = alpha_1_mean, sd = 70)
	        beta_3 <- rnorm(months / 6, mean = beta_3_mean, sd = 70)
	        gamma_1 <- rnorm(months / 6, mean = gamma_1_mean, sd = 70)
	        
	        # Calculate total missed payments every 6 months
	        for (j in seq(1, months, by = 6)) {
	          period_index <- (j - 1) / 6 + 1
	          threshold_lack_financial_lit <- alpha_1[period_index]
	          threshold_political_insta <- beta_3[period_index]
	          threshold_loan_deviation <- gamma_1[period_index]
	          if ((sum(lack_financial_lit[j:(j + 5)]) > threshold_lack_financial_lit) &&
	              (sum(political_insta[j:(j + 5)]) > threshold_political_insta) &&
	              (sum(loan_deviation[j:(j + 5)]) > threshold_loan_deviation)) {
	            default_count <- default_count + 1
	            break  # Stop checking further if already defaulted
	          }
	        }
	      }
	      
	      # Step 4: Calculate the probability of default for the current combination of threshold means
	      probability_of_default <- default_count / simulations
	      
	      # Store the result
	      results <- rbind(results, data.frame(Alpha_1_mean = alpha_1_mean, 
	                                           Beta_3_mean = beta_3_mean, 
	                                           Gamma_1_mean = gamma_1_mean, 
	                                           ProbabilityOfDefault = probability_of_default))
	    }
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	
	
	
#Lack of financial literacy,Political instability and High interest rates
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# Parameters for missed payments due to various factors
	mu_1 <- 60                 # Mean for missed payments due to lack of financial literacy
	sigma_1 <- 30              # Standard deviation for missed payments due to lack of 
	#financial literacy
	lambda_3 <- 90             # Mean for missed payments due to political instability
	lambda_5 <- 70             # Mean for missed payments due to high interest rates
	
	# Step 2: Define a range of threshold means to test for each factor
	alpha_1_means <- seq(550, 590, by = 10)  # Range for lack of financial literacy
	beta_3_means <- seq(560, 600, by = 10)   # Range for political instability
	gamma_2_means <- seq(550, 590, by = 10)  # Range for high interest rates
	
	# Step 3: Run simulations for each combination of threshold means and store results
	results <- data.frame(Alpha_1_mean = numeric(), Beta_3_mean = numeric(), 
	                      Gamma_2_mean = numeric(), ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (alpha_1_mean in alpha_1_means) {
	  for (beta_3_mean in beta_3_means) {
	    for (gamma_2_mean in gamma_2_means) {
	      default_count <- 0  # Counter for default cases
	      
	      for (i in 1:simulations) {
	        # Generate missed payments due to each factor over the defined number of months
	        lack_financial_lit <- rnorm(months, mean = mu_1, sd = sigma_1)  # Factor 1
	        political_insta <- rpois(months, lambda_3)                       # Factor 2
	        high_interest <- rpois(months, lambda_5)                       # Factor 3
	        
	        # Generate random normal distributed threshold values for each 6-month period
	        alpha_1 <- rnorm(months / 6, mean = alpha_1_mean, sd = 70)
	        beta_3 <- rnorm(months / 6, mean = beta_3_mean, sd = 70)
	        gamma_2 <- rnorm(months / 6, mean = gamma_2_mean, sd = 70)
	        
	        # Calculate total missed payments every 6 months
	        for (j in seq(1, months, by = 6)) {
	          period_index <- (j - 1) / 6 + 1
	          threshold_lack_financial_lit <- alpha_1[period_index]
	          threshold_political_insta <- beta_3[period_index]
	          threshold_high_interest <- gamma_2[period_index]
	          if ((sum(lack_financial_lit[j:(j + 5)]) > threshold_lack_financial_lit) &&
	              (sum(political_insta[j:(j + 5)]) > threshold_political_insta) &&
	              (sum(high_interest[j:(j + 5)]) > threshold_high_interest)) {
	            default_count <- default_count + 1
	            break  # Stop checking further if already defaulted
	          }
	        }
	      }
	      
	      # Step 4: Calculate the probability of default for the current combination of threshold means
	      probability_of_default <- default_count / simulations
	      
	      # Store the result
	      results <- rbind(results, data.frame(Alpha_1_mean = alpha_1_mean, 
	                                           Beta_3_mean = beta_3_mean, 
	                                           Gamma_2_mean = gamma_2_mean, 
	                                           ProbabilityOfDefault = probability_of_default))
	    }
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	
	
	#Lack of financial literacy,Political instability and Loan delay
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# Parameters for missed payments due to various factors
	mu_1 <- 60                 # Mean for missed payments due to lack of financial literacy
	sigma_1 <- 30# Standard deviation for missed payments due to lack of financial
	#literacy
	lambda_3 <- 90             # Mean for missed payments due to political instability
	theta <- 1/70             # Mean for missed payments due to loan delay
	
	# Step 2: Define a range of threshold means to test for each factor
	alpha_1_means <- seq(550, 590, by = 10)  # Range for lack of financial literacy
	beta_3_means <- seq(560, 600, by = 10)   # Range for economic downturns
	gamma_3_means <- seq(800, 1000, by = 10)  # Range for loan delay
	
	# Step 3: Run simulations for each combination of threshold means and store results
	results <- data.frame(Alpha_1_mean = numeric(), Beta_3_mean = numeric(), 
	                      Gamma_3_mean = numeric(), ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (alpha_1_mean in alpha_1_means) {
	  for (beta_3_mean in beta_3_means) {
	    for (gamma_3_mean in gamma_3_means) {
	      default_count <- 0  # Counter for default cases
	      
	      for (i in 1:simulations) {
	        # Generate missed payments due to each factor over the defined number of months
	        lack_financial_lit <- rnorm(months, mean = mu_1, sd = sigma_1)  # Factor 1
	        political_insta <- rpois(months, lambda_3)                       # Factor 2
	        loan_delay <- rexp(months, theta)                       # Factor 3
	        
	        # Generate random normal distributed threshold values for each 6-month period
	        alpha_1 <- rnorm(months / 6, mean = alpha_1_mean, sd = 70)
	        beta_3 <- rnorm(months / 6, mean = beta_3_mean, sd = 70)
	        gamma_3 <- rnorm(months / 6, mean = gamma_3_mean, sd = 70)
	        
	        # Calculate total missed payments every 6 months
	        for (j in seq(1, months, by = 6)) {
	          period_index <- (j - 1) / 6 + 1
	          threshold_lack_financial_lit <- alpha_1[period_index]
	          threshold_political_insta <- beta_3[period_index]
	          threshold_loan_delay <- gamma_3[period_index]
	          if ((sum(lack_financial_lit[j:(j + 5)]) > threshold_lack_financial_lit) &&
	              (sum(political_insta[j:(j + 5)]) > threshold_political_insta) &&
	              (sum(loan_delay[j:(j + 5)]) > threshold_loan_delay)) {
	            default_count <- default_count + 1
	            break  # Stop checking further if already defaulted
	          }
	        }
	      }
	      
	      # Step 4: Calculate the probability of default for the current combination of threshold means
	      probability_of_default <- default_count / simulations
	      
	      # Store the result
	      results <- rbind(results, data.frame(Alpha_1_mean = alpha_1_mean, 
	                                           Beta_3_mean = beta_3_mean, 
	                                           Gamma_3_mean = gamma_3_mean, 
	                                           ProbabilityOfDefault = probability_of_default))
	    }
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	
	
	
	
	
	
	
	#Lack of market research,Economic downturns and Loan deviation
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# Parameters for missed payments due to various factors
	mu_2 <- 60                 # Mean for missed payments due to lack of market research
	sigma_2 <- 30# Standard deviation for missed payments due to lack of market research
	lambda_1 <- 90             # Mean for missed payments due to economic downturns
	lambda_4 <- 70             # Mean for missed payments due to loan deviation
	
	# Step 2: Define a range of threshold means to test for each factor
	alpha_2_means <- seq(600, 640, by = 10)  # Range for lack of market research
	beta_1_means <- seq(510, 550, by = 10)   # Range for economic downturns
	gamma_1_means <- seq(500, 540, by = 10)  # Range for loan deviation
	
	# Step 3: Run simulations for each combination of threshold means and store results
	results <- data.frame(Alpha_2_mean = numeric(), Beta_1_mean = numeric(), 
	                      Gamma_1_mean = numeric(), ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (alpha_2_mean in alpha_2_means) {
	  for (beta_1_mean in beta_1_means) {
	    for (gamma_1_mean in gamma_1_means) {
	      default_count <- 0  # Counter for default cases
	      
	      for (i in 1:simulations) {
	        # Generate missed payments due to each factor over the defined number of months
	        lack_market_res <- rnorm(months, mean = mu_2, sd = sigma_2)  # Factor 1
	        economic_downt <- rpois(months, lambda_1)                       # Factor 2
	        loan_deviation <- rpois(months, lambda_4)                       # Factor 3
	        
	        # Generate random normal distributed threshold values for each 6-month period
	        alpha_2 <- rnorm(months / 6, mean = alpha_2_mean, sd = 70)
	        beta_1 <- rnorm(months / 6, mean = beta_1_mean, sd = 70)
	        gamma_1 <- rnorm(months / 6, mean = gamma_1_mean, sd = 70)
	        
	        # Calculate total missed payments every 6 months
	        for (j in seq(1, months, by = 6)) {
	          period_index <- (j - 1) / 6 + 1
	          threshold_lack_market_res <- alpha_2[period_index]
	          threshold_economic_downt <- beta_1[period_index]
	          threshold_loan_deviation <- gamma_1[period_index]
	          if ((sum(lack_market_res[j:(j + 5)]) > threshold_lack_market_res) &&
	              (sum(economic_downt[j:(j + 5)]) > threshold_economic_downt) &&
	              (sum(loan_deviation[j:(j + 5)]) > threshold_loan_deviation)) {
	            default_count <- default_count + 1
	            break  # Stop checking further if already defaulted
	          }
	        }
	      }
	      
	      # Step 4: Calculate the probability of default for the current combination of threshold means
	      probability_of_default <- default_count / simulations
	      
	      # Store the result
	      results <- rbind(results, data.frame(Alpha_2_mean = alpha_2_mean, 
	                                           Beta_1_mean = beta_1_mean, 
	                                           Gamma_1_mean = gamma_1_mean, 
	                                           ProbabilityOfDefault = probability_of_default))
	    }
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	# Lack of market research,Economic downturns and High interest rates
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# Parameters for missed payments due to various factors
	mu_2 <- 60                 # Mean for missed payments due to lack of market research
	sigma_2 <- 30# Standard deviation for missed payments due to lack of market research
	lambda_1 <- 90             # Mean for missed payments due to economic downturns
	lambda_5 <- 70             # Mean for missed payments due to high interest rates
	
	# Step 2: Define a range of threshold means to test for each factor
	alpha_2_means <- seq(600, 640, by = 10)  # Range for lack of market research
	beta_1_means <- seq(510, 550, by = 10)   # Range for economic downturns
	gamma_2_means <- seq(550, 590, by = 10)  # Range for high interest rates
	
	# Step 3: Run simulations for each combination of threshold means and store results
	results <- data.frame(Alpha_2_mean = numeric(), Beta_1_mean = numeric(), 
	                      Gamma_2_mean = numeric(), ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (alpha_2_mean in alpha_2_means) {
	  for (beta_1_mean in beta_1_means) {
	    for (gamma_2_mean in gamma_2_means) {
	      default_count <- 0  # Counter for default cases
	      
	      for (i in 1:simulations) {
	        # Generate missed payments due to each factor over the defined number of months
	        lack_market_res <- rnorm(months, mean = mu_2, sd = sigma_2)  # Factor 1
	        economic_downt <- rpois(months, lambda_1)                       # Factor 2
	        high_interest <- rpois(months, lambda_5)                       # Factor 3
	        
	        # Generate random normal distributed threshold values for each 6-month period
	        alpha_2 <- rnorm(months / 6, mean = alpha_2_mean, sd = 70)
	        beta_1 <- rnorm(months / 6, mean = beta_1_mean, sd = 70)
	        gamma_2 <- rnorm(months / 6, mean = gamma_2_mean, sd = 70)
	        
	        # Calculate total missed payments every 6 months
	        for (j in seq(1, months, by = 6)) {
	          period_index <- (j - 1) / 6 + 1
	          threshold_lack_market_res <- alpha_2[period_index]
	          threshold_economic_downt <- beta_1[period_index]
	          threshold_high_interest <- gamma_2[period_index]
	          if ((sum(lack_market_res[j:(j + 5)]) > threshold_lack_market_res) &&
	              (sum(economic_downt[j:(j + 5)]) > threshold_economic_downt) &&
	              (sum(high_interest[j:(j + 5)]) > threshold_high_interest)) {
	            default_count <- default_count + 1
	            break  # Stop checking further if already defaulted
	          }
	        }
	      }
	      
	      # Step 4: Calculate the probability of default for the current combination of threshold means
	      probability_of_default <- default_count / simulations
	      
	      # Store the result
	      results <- rbind(results, data.frame(Alpha_2_mean = alpha_2_mean, 
	                                           Beta_1_mean = beta_1_mean, 
	                                           Gamma_2_mean = gamma_2_mean, 
	                                           ProbabilityOfDefault = probability_of_default))
	    }
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	
	
	
	
#Lack of market research,Economic downturns and Loan delay
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# Parameters for missed payments due to various factors
	mu_2 <- 60                 # Mean for missed payments due to lack of market research
	sigma_2 <- 30   # Standard deviation for missed payments due to lack of market research
	lambda_1 <- 90             # Mean for missed payments due to political instability
	theta <- 1/70             # Mean for missed payments due to loan delay
	
	# Step 2: Define a range of threshold means to test for each factor
	alpha_2_means <- seq(600, 640, by = 10)  # Range for lack of market research
	beta_1_means <- seq(510, 550, by = 10)   # Range for economic downturns
	gamma_3_means <- seq(800, 1000, by = 10)  # Range for loan delay
	
	# Step 3: Run simulations for each combination of threshold means and store results
	results <- data.frame(Alpha_2_mean = numeric(), Beta_1_mean = numeric(), 
	                      Gamma_3_mean = numeric(), ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (alpha_2_mean in alpha_2_means) {
	  for (beta_1_mean in beta_1_means) {
	    for (gamma_3_mean in gamma_3_means) {
	      default_count <- 0  # Counter for default cases
	      
	      for (i in 1:simulations) {
	        # Generate missed payments due to each factor over the defined number of months
	        lack_financial_lit <- rnorm(months, mean = mu_2, sd = sigma_2)  # Factor 1
	        economic_downt <- rpois(months, lambda_1)                       # Factor 2
	        loan_delay <- rexp(months, theta)                       # Factor 3
	        
	        # Generate random normal distributed threshold values for each 6-month period
	        alpha_2 <- rnorm(months / 6, mean = alpha_2_mean, sd = 70)
	        beta_1 <- rnorm(months / 6, mean = beta_1_mean, sd = 70)
	        gamma_3 <- rnorm(months / 6, mean = gamma_3_mean, sd = 70)
	        
	        # Calculate total missed payments every 6 months
	        for (j in seq(1, months, by = 6)) {
	          period_index <- (j - 1) / 6 + 1
	          threshold_lack_market_res <- alpha_2[period_index]
	          threshold_economic_downt <- beta_1[period_index]
	          threshold_loan_delay <- gamma_3[period_index]
	          if ((sum(lack_market_res[j:(j + 5)]) > threshold_lack_market_res) &&
	              (sum(economic_downt[j:(j + 5)]) > threshold_economic_downt) &&
	              (sum(loan_delay[j:(j + 5)]) > threshold_loan_delay)) {
	            default_count <- default_count + 1
	            break  # Stop checking further if already defaulted
	          }
	        }
	      }
	      
	      # Step 4: Calculate the probability of default for the current combination of threshold means
	      probability_of_default <- default_count / simulations
	      
	      # Store the result
	      results <- rbind(results, data.frame(Alpha_2_mean = alpha_2_mean, 
	                                           Beta_1_mean = beta_1_mean, 
	                                           Gamma_3_mean = gamma_3_mean, 
	                                           ProbabilityOfDefault = probability_of_default))
	    }
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	
	
	
	#Lack of market research,Natural disasters and Loan deviation
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# Parameters for missed payments due to various factors
	mu_2 <- 60                 # Mean for missed payments due to lack of market research
	sigma_2 <- 30# Standard deviation for missed payments due to lack of market research
	lambda_2 <- 90             # Mean for missed payments due to economic downturns
	lambda_4 <- 70             # Mean for missed payments due to loan deviation
	
	# Step 2: Define a range of threshold means to test for each factor
	alpha_2_means <- seq(600, 640, by = 10)  # Range for lack of market research
	beta_2_means <- seq(610, 650, by = 10)   # Range for economic downturns
	gamma_1_means <- seq(500, 540, by = 10)  # Range for loan deviation
	
	# Step 3: Run simulations for each combination of threshold means and store results
	results <- data.frame(Alpha_2_mean = numeric(), Beta_2_mean = numeric(), 
	                      Gamma_1_mean = numeric(), ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (alpha_2_mean in alpha_2_means) {
	  for (beta_2_mean in beta_2_means) {
	    for (gamma_1_mean in gamma_1_means) {
	      default_count <- 0  # Counter for default cases
	      
	      for (i in 1:simulations) {
	        # Generate missed payments due to each factor over the defined number of months
	        lack_market_res <- rnorm(months, mean = mu_2, sd = sigma_2)  # Factor 1
	        natural_disa <- rpois(months, lambda_2)                       # Factor 2
	        loan_deviation <- rpois(months, lambda_4)                       # Factor 3
	        
	        # Generate random normal distributed threshold values for each 6-month period
	        alpha_2 <- rnorm(months / 6, mean = alpha_2_mean, sd = 70)
	        beta_2 <- rnorm(months / 6, mean = beta_2_mean, sd = 70)
	        gamma_1 <- rnorm(months / 6, mean = gamma_1_mean, sd = 70)
	        
	        # Calculate total missed payments every 6 months
	        for (j in seq(1, months, by = 6)) {
	          period_index <- (j - 1) / 6 + 1
	          threshold_lack_market_res <- alpha_2[period_index]
	          threshold_natural_disa <- beta_2[period_index]
	          threshold_loan_deviation <- gamma_1[period_index]
	          if ((sum(lack_market_res[j:(j + 5)]) > threshold_lack_market_res) &&
	              (sum(natural_disa[j:(j + 5)]) > threshold_natural_disa) &&
	              (sum(loan_deviation[j:(j + 5)]) > threshold_loan_deviation)) {
	            default_count <- default_count + 1
	            break  # Stop checking further if already defaulted
	          }
	        }
	      }
	      
	      # Step 4: Calculate the probability of default for the current combination of threshold means
	      probability_of_default <- default_count / simulations
	      
	      # Store the result
	      results <- rbind(results, data.frame(Alpha_2_mean = alpha_2_mean, 
	                                           Beta_2_mean = beta_2_mean, 
	                                           Gamma_1_mean = gamma_1_mean, 
	                                           ProbabilityOfDefault = probability_of_default))
	    }
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	
	
	
	
	#Lack of market research,Natural disasters and High interest rates
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# Parameters for missed payments due to various factors
	mu_2 <- 60                 # Mean for missed payments due to lack of market research
	sigma_2 <- 30# Standard deviation for missed payments due to lack of market research
	lambda_2 <- 90             # Mean for missed payments due to natural disasters
	lambda_5 <- 70             # Mean for missed payments due to high interest rates
	
	# Step 2: Define a range of threshold means to test for each factor
	alpha_2_means <- seq(600, 640, by = 10)  # Range for lack of market research
	beta_2_means <- seq(610, 650, by = 10)   # Range for natural disasters
	gamma_2_means <- seq(550, 590, by = 10)  # Range for high interest rates
	
	# Step 3: Run simulations for each combination of threshold means and store results
	results <- data.frame(Alpha_2_mean = numeric(), Beta_2_mean = numeric(), 
	                      Gamma_2_mean = numeric(), ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (alpha_2_mean in alpha_2_means) {
	  for (beta_2_mean in beta_2_means) {
	    for (gamma_2_mean in gamma_2_means) {
	      default_count <- 0  # Counter for default cases
	      
	      for (i in 1:simulations) {
	        # Generate missed payments due to each factor over the defined number of months
	        lack_market_res <- rnorm(months, mean = mu_2, sd = sigma_2)  # Factor 1
	        natural_disa <- rpois(months, lambda_2)                       # Factor 2
	        high_interest <- rpois(months, lambda_5)                       # Factor 3
	        
	        # Generate random normal distributed threshold values for each 6-month period
	        alpha_2 <- rnorm(months / 6, mean = alpha_2_mean, sd = 70)
	        beta_2 <- rnorm(months / 6, mean = beta_2_mean, sd = 70)
	        gamma_2 <- rnorm(months / 6, mean = gamma_2_mean, sd = 70)
	        
	        # Calculate total missed payments every 6 months
	        for (j in seq(1, months, by = 6)) {
	          period_index <- (j - 1) / 6 + 1
	          threshold_lack_market_res <- alpha_2[period_index]
	          threshold_natural_disa <- beta_2[period_index]
	          threshold_high_interest <- gamma_2[period_index]
	          if ((sum(lack_market_res[j:(j + 5)]) > threshold_lack_market_res) &&
	              (sum(natural_disa[j:(j + 5)]) > threshold_natural_disa) &&
	              (sum(high_interest[j:(j + 5)]) > threshold_high_interest)) {
	            default_count <- default_count + 1
	            break  # Stop checking further if already defaulted
	          }
	        }
	      }
	      
	      # Step 4: Calculate the probability of default for the current combination of threshold means
	      probability_of_default <- default_count / simulations
	      
	      # Store the result
	      results <- rbind(results, data.frame(Alpha_2_mean = alpha_2_mean, 
	                                           Beta_2_mean = beta_2_mean, 
	                                           Gamma_2_mean = gamma_2_mean, 
	                                           ProbabilityOfDefault = probability_of_default))
	    }
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	
	
	
	#Lack of market research,Natural disasters and Loan delay
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# Parameters for missed payments due to various factors
	mu_2 <- 60                 # Mean for missed payments due to lack of market research
	sigma_2 <- 30   # Standard deviation for missed payments due to lack of market research
	lambda_2 <- 90             # Mean for missed payments due to natural disasters
	theta <- 1/70             # Mean for missed payments due to loan delay
	
	# Step 2: Define a range of threshold means to test for each factor
	alpha_2_means <- seq(600, 640, by = 10)  # Range for lack of market research
	beta_2_means <- seq(610, 650, by = 10)   # Range for natural disasters
	gamma_3_means <- seq(800, 1000, by = 10)  # Range for loan delay
	
	# Step 3: Run simulations for each combination of threshold means and store results
	results <- data.frame(Alpha_2_mean = numeric(), Beta_2_mean = numeric(), 
	                      Gamma_3_mean = numeric(), ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (alpha_2_mean in alpha_2_means) {
	  for (beta_2_mean in beta_2_means) {
	    for (gamma_3_mean in gamma_3_means) {
	      default_count <- 0  # Counter for default cases
	      
	      for (i in 1:simulations) {
	        # Generate missed payments due to each factor over the defined number of months
	        lack_financial_lit <- rnorm(months, mean = mu_2, sd = sigma_2)  # Factor 1
	        natural_disa <- rpois(months, lambda_2)                       # Factor 2
	        loan_delay <- rexp(months, theta)                       # Factor 3
	        
	        # Generate random normal distributed threshold values for each 6-month period
	        alpha_2 <- rnorm(months / 6, mean = alpha_2_mean, sd = 70)
	        beta_2 <- rnorm(months / 6, mean = beta_2_mean, sd = 70)
	        gamma_3 <- rnorm(months / 6, mean = gamma_3_mean, sd = 70)
	        
	        # Calculate total missed payments every 6 months
	        for (j in seq(1, months, by = 6)) {
	          period_index <- (j - 1) / 6 + 1
	          threshold_lack_market_res <- alpha_2[period_index]
	          threshold_natural_disa <- beta_2[period_index]
	          threshold_loan_delay <- gamma_3[period_index]
	          if ((sum(lack_market_res[j:(j + 5)]) > threshold_lack_market_res) &&
	              (sum(natural_disa[j:(j + 5)]) > threshold_natural_disa) &&
	              (sum(loan_delay[j:(j + 5)]) > threshold_loan_delay)) {
	            default_count <- default_count + 1
	            break  # Stop checking further if already defaulted
	          }
	        }
	      }
	      
	      # Step 4: Calculate the probability of default for the current combination of threshold means
	      probability_of_default <- default_count / simulations
	      
	      # Store the result
	      results <- rbind(results, data.frame(Alpha_2_mean = alpha_2_mean, 
	                                           Beta_2_mean = beta_2_mean, 
	                                           Gamma_3_mean = gamma_3_mean, 
	                                           ProbabilityOfDefault = probability_of_default))
	    }
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	
	
	
	
	#Lack of market research,Political instability and Loan deviation
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# Parameters for missed payments due to various factors
	mu_2 <- 60                 # Mean for missed payments due to lack of market research
	sigma_2 <- 30# Standard deviation for missed payments due to lack of market research
	lambda_3 <- 90             # Mean for missed payments due to political instability
	lambda_4 <- 70             # Mean for missed payments due to loan deviation
	
	# Step 2: Define a range of threshold means to test for each factor
	alpha_2_means <- seq(600, 640, by = 10)  # Range for lack of market research
	beta_3_means <- seq(560, 600, by = 10)   # Range for political instability
	gamma_1_means <- seq(500, 550, by = 10)  # Range for loan deviation
	
	# Step 3: Run simulations for each combination of threshold means and store results
	results <- data.frame(Alpha_2_mean = numeric(), Beta_3_mean = numeric(), 
	                      Gamma_1_mean = numeric(), ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (alpha_2_mean in alpha_2_means) {
	  for (beta_3_mean in beta_3_means) {
	    for (gamma_1_mean in gamma_1_means) {
	      default_count <- 0  # Counter for default cases
	      
	      for (i in 1:simulations) {
	        # Generate missed payments due to each factor over the defined number of months
	        lack_market_res <- rnorm(months, mean = mu_2, sd = sigma_2)  # Factor 1
	        political_insta <- rpois(months, lambda_3)                       # Factor 2
	        loan_deviation <- rpois(months, lambda_4)                       # Factor 3
	        
	        # Generate random normal distributed threshold values for each 6-month period
	        alpha_2 <- rnorm(months / 6, mean = alpha_2_mean, sd = 70)
	        beta_3 <- rnorm(months / 6, mean = beta_3_mean, sd = 70)
	        gamma_1 <- rnorm(months / 6, mean = gamma_1_mean, sd = 70)
	        
	        # Calculate total missed payments every 6 months
	        for (j in seq(1, months, by = 6)) {
	          period_index <- (j - 1) / 6 + 1
	          threshold_lack_market_res <- alpha_2[period_index]
	          threshold_political_insta <- beta_3[period_index]
	          threshold_loan_deviation <- gamma_1[period_index]
	          if ((sum(lack_market_res[j:(j + 5)]) > threshold_lack_market_res) &&
	              (sum(political_insta[j:(j + 5)]) > threshold_political_insta) &&
	              (sum(loan_deviation[j:(j + 5)]) > threshold_loan_deviation)) {
	            default_count <- default_count + 1
	            break  # Stop checking further if already defaulted
	          }
	        }
	      }
	      
	      # Step 4: Calculate the probability of default for the current combination of threshold means
	      probability_of_default <- default_count / simulations
	      
	      # Store the result
	      results <- rbind(results, data.frame(Alpha_2_mean = alpha_2_mean, 
	                                           Beta_3_mean = beta_3_mean, 
	                                           Gamma_1_mean = gamma_1_mean, 
	                                           ProbabilityOfDefault = probability_of_default))
	    }
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	
	#Lack of market research,Political instability and High interest rates
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# Parameters for missed payments due to various factors
	mu_2 <- 60                 # Mean for missed payments due to lack of market research
	sigma_2 <- 30# Standard deviation for missed payments due to lack of market research
	lambda_3 <- 90             # Mean for missed payments due to political instability
	lambda_5 <- 70             # Mean for missed payments due to high interest rates
	
	# Step 2: Define a range of threshold means to test for each factor
	alpha_2_means <- seq(600, 640, by = 10)  # Range for lack of market research
	beta_3_means <- seq(560, 600, by = 10)   # Range for political instability
	gamma_2_means <- seq(550, 590, by = 10)  # Range for high interest rates
	
	# Step 3: Run simulations for each combination of threshold means and store results
	results <- data.frame(Alpha_2_mean = numeric(), Beta_3_mean = numeric(), 
	                      Gamma_2_mean = numeric(), ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (alpha_2_mean in alpha_2_means) {
	  for (beta_3_mean in beta_3_means) {
	    for (gamma_2_mean in gamma_2_means) {
	      default_count <- 0  # Counter for default cases
	      
	      for (i in 1:simulations) {
	        # Generate missed payments due to each factor over the defined number of months
	        lack_market_res <- rnorm(months, mean = mu_2, sd = sigma_2)  # Factor 1
	        political_insta <- rpois(months, lambda_3)                       # Factor 2
	        high_interest <- rpois(months, lambda_5)                       # Factor 3
	        
	        # Generate random normal distributed threshold values for each 6-month period
	        alpha_2 <- rnorm(months / 6, mean = alpha_2_mean, sd = 70)
	        beta_3 <- rnorm(months / 6, mean = beta_3_mean, sd = 70)
	        gamma_2 <- rnorm(months / 6, mean = gamma_2_mean, sd = 70)
	        
	        # Calculate total missed payments every 6 months
	        for (j in seq(1, months, by = 6)) {
	          period_index <- (j - 1) / 6 + 1
	          threshold_lack_market_res <- alpha_2[period_index]
	          threshold_political_insta <- beta_3[period_index]
	          threshold_high_interest <- gamma_2[period_index]
	          if ((sum(lack_market_res[j:(j + 5)]) > threshold_lack_market_res) &&
	              (sum(political_insta[j:(j + 5)]) > threshold_political_insta) &&
	              (sum(high_interest[j:(j + 5)]) > threshold_high_interest)) {
	            default_count <- default_count + 1
	            break  # Stop checking further if already defaulted
	          }
	        }
	      }
	      
	      # Step 4: Calculate the probability of default for the current combination of threshold means
	      probability_of_default <- default_count / simulations
	      
	      # Store the result
	      results <- rbind(results, data.frame(Alpha_2_mean = alpha_2_mean, 
	                                           Beta_3_mean = beta_3_mean, 
	                                           Gamma_2_mean = gamma_2_mean, 
	                                           ProbabilityOfDefault = probability_of_default))
	    }
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	
	
	
	
	
	
	#Lack of market research,Political instability and Loan delay
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# Parameters for missed payments due to various factors
	mu_2 <- 60                 # Mean for missed payments due to lack of market research
	sigma_2 <- 30   # Standard deviation for missed payments due to lack of market research
	lambda_3 <- 90             # Mean for missed payments due to political instability
	theta <- 1/70             # Mean for missed payments due to loan delay
	
	# Step 2: Define a range of threshold means to test for each factor
	alpha_2_means <- seq(600, 640, by = 10)  # Range for lack of market research
	beta_3_means <- seq(560, 600, by = 10)   # Range for political instability
	gamma_3_means <- seq(800, 1000, by = 10)  # Range for loan delay
	
	# Step 3: Run simulations for each combination of threshold means and store results
	results <- data.frame(Alpha_2_mean = numeric(), Beta_3_mean = numeric(), 
	                      Gamma_3_mean = numeric(), ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (alpha_2_mean in alpha_2_means) {
	  for (beta_3_mean in beta_3_means) {
	    for (gamma_3_mean in gamma_3_means) {
	      default_count <- 0  # Counter for default cases
	      
	      for (i in 1:simulations) {
	        # Generate missed payments due to each factor over the defined number of months
	        lack_financial_lit <- rnorm(months, mean = mu_2, sd = sigma_2)  # Factor 1
	        political_insta <- rpois(months, lambda_3)                       # Factor 2
	        loan_delay <- rexp(months, theta)                       # Factor 3
	        
	        # Generate random normal distributed threshold values for each 6-month period
	        alpha_2 <- rnorm(months / 6, mean = alpha_2_mean, sd = 70)
	        beta_3 <- rnorm(months / 6, mean = beta_3_mean, sd = 70)
	        gamma_3 <- rnorm(months / 6, mean = gamma_3_mean, sd = 70)
	        
	        # Calculate total missed payments every 6 months
	        for (j in seq(1, months, by = 6)) {
	          period_index <- (j - 1) / 6 + 1
	          threshold_lack_market_res <- alpha_2[period_index]
	          threshold_political_insta <- beta_3[period_index]
	          threshold_loan_delay <- gamma_3[period_index]
	          if ((sum(lack_market_res[j:(j + 5)]) > threshold_lack_market_res) &&
	              (sum(political_insta[j:(j + 5)]) > threshold_political_insta) &&
	              (sum(loan_delay[j:(j + 5)]) > threshold_loan_delay)) {
	            default_count <- default_count + 1
	            break  # Stop checking further if already defaulted
	          }
	        }
	      }
	      
	      # Step 4: Calculate the probability of default for the current combination of threshold means
	      probability_of_default <- default_count / simulations
	      
	      # Store the result
	      results <- rbind(results, data.frame(Alpha_2_mean = alpha_2_mean, 
	                                           Beta_3_mean = beta_3_mean, 
	                                           Gamma_3_mean = gamma_3_mean, 
	                                           ProbabilityOfDefault = probability_of_default))
	    }
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	
	
	
	#Poor business operations management,Economic downturns and Loan deviation
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# Parameters for missed payments due to various factors
	mu_3 <- 60# Mean for missed payments due to poor business operations management
	sigma_3 <- 30# Standard deviation for missed payments due to poor business operations  
	#management
	lambda_1 <- 90          # Mean for missed payments due to economic downturns
	lambda_4 <- 70          # Mean for missed payments due to loan deviation
	
	# Step 2: Define a range of threshold means to test for each factor
	alpha_3_means <- seq(500, 540, by = 10)  # Range for poor business operations management
	beta_1_means <- seq(510, 550, by = 10)   # Range for economic downturns
	gamma_1_means <- seq(500, 540, by = 10)  # Range for loan deviation
	
	# Step 3: Run simulations for each combination of threshold means and store results
	results <- data.frame(Alpha_3_mean = numeric(), Beta_1_mean = numeric(), 
	                      Gamma_1_mean = numeric(), ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (alpha_3_mean in alpha_3_means) {
	  for (beta_1_mean in beta_1_means) {
	    for (gamma_1_mean in gamma_1_means) {
	      default_count <- 0  # Counter for default cases
	      
	      for (i in 1:simulations) {
	        # Generate missed payments due to each factor over the defined number of months
	        poor_business_op <- rnorm(months, mean = mu_3, sd = sigma_3)  # Factor 1
	        economic_downt <- rpois(months, lambda_1)                       # Factor 2
	        loan_deviation <- rpois(months, lambda_4)                       # Factor 3
	        
	        # Generate random normal distributed threshold values for each 6-month period
	        alpha_3 <- rnorm(months / 6, mean = alpha_3_mean, sd = 70)
	        beta_1 <- rnorm(months / 6, mean = beta_1_mean, sd = 70)
	        gamma_1 <- rnorm(months / 6, mean = gamma_1_mean, sd = 70)
	        
	        # Calculate total missed payments every 6 months
	        for (j in seq(1, months, by = 6)) {
	          period_index <- (j - 1) / 6 + 1
	          threshold_poor_business_op <- alpha_3[period_index]
	          threshold_economic_downt <- beta_1[period_index]
	          threshold_loan_deviation <- gamma_1[period_index]
	          if ((sum(poor_business_op[j:(j + 5)]) > threshold_poor_business_op) &&
	              (sum(economic_downt[j:(j + 5)]) > threshold_economic_downt) &&
	              (sum(loan_deviation[j:(j + 5)]) > threshold_loan_deviation)) {
	            default_count <- default_count + 1
	            break  # Stop checking further if already defaulted
	          }
	        }
	      }
	      
	      # Step 4: Calculate the probability of default for the current combination of threshold means
	      probability_of_default <- default_count / simulations
	      
	      # Store the result
	      results <- rbind(results, data.frame(Alpha_3_mean = alpha_3_mean, 
	                                           Beta_1_mean = beta_1_mean, 
	                                           Gamma_1_mean = gamma_1_mean, 
	                                           ProbabilityOfDefault = probability_of_default))
	    }
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	
	#Poor business operations management,Economic downturns and High interest rates# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# Parameters for missed payments due to various factors
	mu_3 <- 60# Mean for missed payments due to poor business operations management
	sigma_3 <- 30# Standard deviation for missed payments due to poor business operations management
	lambda_1 <- 90             # Mean for missed payments due to economic downturns
	lambda_5 <- 70             # Mean for missed payments due to high interest rates
	
	# Step 2: Define a range of threshold means to test for each factor
	alpha_3_means <- seq(500, 540, by = 10)  # Range for lack of market research
	beta_1_means <- seq(510, 550, by = 10)   # Range for economic downturns
	gamma_2_means <- seq(550, 590, by = 10)  # Range for high interest rates
	
	# Step 3: Run simulations for each combination of threshold means and store results
	results <- data.frame(Alpha_3_mean = numeric(), Beta_1_mean = numeric(), 
	                      Gamma_2_mean = numeric(), ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (alpha_3_mean in alpha_3_means) {
	  for (beta_1_mean in beta_1_means) {
	    for (gamma_2_mean in gamma_2_means) {
	      default_count <- 0  # Counter for default cases
	      
	      for (i in 1:simulations) {
	        # Generate missed payments due to each factor over the defined number of months
	        poor_business_op <- rnorm(months, mean = mu_3, sd = sigma_3)  # Factor 1
	        economic_downt <- rpois(months, lambda_1)                       # Factor 2
	        high_interest <- rpois(months, lambda_5)                       # Factor 3
	        
	        # Generate random normal distributed threshold values for each 6-month period
	        alpha_3 <- rnorm(months / 6, mean = alpha_3_mean, sd = 70)
	        beta_1 <- rnorm(months / 6, mean = beta_1_mean, sd = 70)
	        gamma_2 <- rnorm(months / 6, mean = gamma_2_mean, sd = 70)
	        
	        # Calculate total missed payments every 6 months
	        for (j in seq(1, months, by = 6)) {
	          period_index <- (j - 1) / 6 + 1
	          threshold_poor_business_op <- alpha_3[period_index]
	          threshold_economic_downt <- beta_1[period_index]
	          threshold_high_interest <- gamma_2[period_index]
	          if ((sum(poor_business_op[j:(j + 5)]) > threshold_poor_business_op) &&
	              (sum(economic_downt[j:(j + 5)]) > threshold_economic_downt) &&
	              (sum(high_interest[j:(j + 5)]) > threshold_high_interest)) {
	            default_count <- default_count + 1
	            break  # Stop checking further if already defaulted
	          }
	        }
	      }
	      
	      # Step 4: Calculate the probability of default for the current combination of threshold means
	      probability_of_default <- default_count / simulations
	      
	      # Store the result
	      results <- rbind(results, data.frame(Alpha_3_mean = alpha_3_mean, 
	                                           Beta_1_mean = beta_1_mean, 
	                                           Gamma_2_mean = gamma_2_mean, 
	                                           ProbabilityOfDefault = probability_of_default))
	    }
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	
	
	#Poor business operations management,Economic downturns and Loan delay
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# Parameters for missed payments due to various factors
	mu_3 <- 60  # Mean for missed payments due to poor business operations management
	sigma_3 <- 30 # Standard deviation for missed payments due to poor business operations management
	lambda_1 <- 90             # Mean for missed payments due to economic downturns
	theta <- 1/70             # Mean for missed payments due to loan delay
	
	# Step 2: Define a range of threshold means to test for each factor
	alpha_3_means <- seq(500, 540, by = 10)  # Range for poor business operations management
	beta_1_means <- seq(510, 550, by = 10)   # Range for economic downturns
	gamma_3_means <- seq(800, 1000, by = 10)  # Range for loan delay
	
	# Step 3: Run simulations for each combination of threshold means and store results
	results <- data.frame(Alpha_3_mean = numeric(), Beta_1_mean = numeric(), 
	                      Gamma_3_mean = numeric(), ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (alpha_3_mean in alpha_3_means) {
	  for (beta_1_mean in beta_1_means) {
	    for (gamma_3_mean in gamma_3_means) {
	      default_count <- 0  # Counter for default cases
	      
	      for (i in 1:simulations) {
	        # Generate missed payments due to each factor over the defined number of months
	        poor_business_op <- rnorm(months, mean = mu_3, sd = sigma_3)  # Factor 1
	        economic_downt <- rpois(months, lambda_1)                       # Factor 2
	        loan_delay <- rexp(months, theta)                       # Factor 3
	        
	        # Generate random normal distributed threshold values for each 6-month period
	        alpha_3 <- rnorm(months / 6, mean = alpha_3_mean, sd = 70)
	        beta_1 <- rnorm(months / 6, mean = beta_1_mean, sd = 70)
	        gamma_3 <- rnorm(months / 6, mean = gamma_3_mean, sd = 70)
	        
	        # Calculate total missed payments every 6 months
	        for (j in seq(1, months, by = 6)) {
	          period_index <- (j - 1) / 6 + 1
	          threshold_poor_business_op <- alpha_3[period_index]
	          threshold_economic_downt <- beta_1[period_index]
	          threshold_loan_delay <- gamma_3[period_index]
	          if ((sum(poor_business_op[j:(j + 5)]) > threshold_poor_business_op) &&
	              (sum(economic_downt[j:(j + 5)]) > threshold_economic_downt) &&
	              (sum(loan_delay[j:(j + 5)]) > threshold_loan_delay)) {
	            default_count <- default_count + 1
	            break  # Stop checking further if already defaulted
	          }
	        }
	      }
	      
	      # Step 4: Calculate the probability of default for the current combination of threshold means
	      probability_of_default <- default_count / simulations
	      
	      # Store the result
	      results <- rbind(results, data.frame(Alpha_3_mean = alpha_3_mean, 
	                                           Beta_1_mean = beta_1_mean, 
	                                           Gamma_3_mean = gamma_3_mean, 
	                                           ProbabilityOfDefault = probability_of_default))
	    }
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	
	#Poor business operations management,Natural disasters and Loan deviation
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# Parameters for missed payments due to various factors
	mu_3 <- 60# Mean for missed payments due to poor business operations management
	sigma_3 <- 30# Standard deviation for missed payments due to poor business operations 
	#management
	lambda_2 <- 90             # Mean for missed payments due to natural disasters
	lambda_4 <- 70             # Mean for missed payments due to loan deviation
	
	# Step 2: Define a range of threshold means to test for each factor
	alpha_3_means <- seq(500, 540, by = 10)  # Range for poor business operations management
	beta_2_means <- seq(610, 650, by = 10)   # Range for natural disasters
	gamma_1_means <- seq(500, 540, by = 10)  # Range for loan deviation
	
	# Step 3: Run simulations for each combination of threshold means and store results
	results <- data.frame(Alpha_3_mean = numeric(), Beta_2_mean = numeric(), 
	                      Gamma_1_mean = numeric(), ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (alpha_3_mean in alpha_3_means) {
	  for (beta_2_mean in beta_2_means) {
	    for (gamma_1_mean in gamma_1_means) {
	      default_count <- 0  # Counter for default cases
	      
	      for (i in 1:simulations) {
	        # Generate missed payments due to each factor over the defined number of months
	        poor_business_op <- rnorm(months, mean = mu_3, sd = sigma_3)  # Factor 1
	        natural_disa <- rpois(months, lambda_2)                       # Factor 2
	        loan_deviation <- rpois(months, lambda_4)                       # Factor 3
	        
	        # Generate random normal distributed threshold values for each 6-month period
	        alpha_3 <- rnorm(months / 6, mean = alpha_3_mean, sd = 70)
	        beta_2 <- rnorm(months / 6, mean = beta_2_mean, sd = 70)
	        gamma_1 <- rnorm(months / 6, mean = gamma_1_mean, sd = 70)
	        
	        # Calculate total missed payments every 6 months
	        for (j in seq(1, months, by = 6)) {
	          period_index <- (j - 1) / 6 + 1
	          threshold_poor_business_op <- alpha_3[period_index]
	          threshold_natural_disa <- beta_2[period_index]
	          threshold_loan_deviation <- gamma_1[period_index]
	          if ((sum(poor_business_op[j:(j + 5)]) > threshold_poor_business_op) &&
	              (sum(natural_disa[j:(j + 5)]) > threshold_natural_disa) &&
	              (sum(loan_deviation[j:(j + 5)]) > threshold_loan_deviation)) {
	            default_count <- default_count + 1
	            break  # Stop checking further if already defaulted
	          }
	        }
	      }
	      
	      # Step 4: Calculate the probability of default for the current combination of threshold means
	      probability_of_default <- default_count / simulations
	      
	      # Store the result
	      results <- rbind(results, data.frame(Alpha_3_mean = alpha_3_mean, 
	                                           Beta_2_mean = beta_2_mean, 
	                                           Gamma_1_mean = gamma_1_mean, 
	                                           ProbabilityOfDefault = probability_of_default))
	    }
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	
	
	
#Poor business operations management,Natural disasters and High interest rates
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# Parameters for missed payments due to various factors
	mu_3 <- 60# Mean for missed payments due to poor business operations management
	sigma_3 <- 30# Standard deviation for missed payments due to poor business operations
	#management
	lambda_2 <- 90             # Mean for missed payments due to natural disasters
	lambda_5 <- 70             # Mean for missed payments due to high interest rates
	
	# Step 2: Define a range of threshold means to test for each factor
	alpha_3_means <- seq(500, 540, by = 10) # Range for poor business operations management
	beta_2_means <- seq(610, 650, by = 10)   # Range for natural disasters
	gamma_2_means <- seq(550, 590, by = 10)  # Range for high interest rates
	
	# Step 3: Run simulations for each combination of threshold means and store results
	results <- data.frame(Alpha_3_mean = numeric(), Beta_2_mean = numeric(), 
	                      Gamma_2_mean = numeric(), ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (alpha_3_mean in alpha_3_means) {
	  for (beta_2_mean in beta_2_means) {
	    for (gamma_2_mean in gamma_2_means) {
	      default_count <- 0  # Counter for default cases
	      
	      for (i in 1:simulations) {
	        # Generate missed payments due to each factor over the defined number of months
	        poor_business_op <- rnorm(months, mean = mu_3, sd = sigma_3)  # Factor 1
	        natural_disa <- rpois(months, lambda_2)                       # Factor 2
	        high_interest <- rpois(months, lambda_5)                       # Factor 3
	        
	        # Generate random normal distributed threshold values for each 6-month period
	        alpha_3 <- rnorm(months / 6, mean = alpha_3_mean, sd = 70)
	        beta_2 <- rnorm(months / 6, mean = beta_2_mean, sd = 70)
	        gamma_2 <- rnorm(months / 6, mean = gamma_2_mean, sd = 70)
	        
	        # Calculate total missed payments every 6 months
	        for (j in seq(1, months, by = 6)) {
	          period_index <- (j - 1) / 6 + 1
	          threshold_poor_business_op <- alpha_3[period_index]
	          threshold_natural_disa <- beta_2[period_index]
	          threshold_high_interest <- gamma_2[period_index]
	          if ((sum(poor_business_op[j:(j + 5)]) > threshold_poor_business_op) &&
	              (sum(natural_disa[j:(j + 5)]) > threshold_natural_disa) &&
	              (sum(high_interest[j:(j + 5)]) > threshold_high_interest)) {
	            default_count <- default_count + 1
	            break  # Stop checking further if already defaulted
	          }
	        }
	      }
	      
	      # Step 4: Calculate the probability of default for the current combination of threshold means
	      probability_of_default <- default_count / simulations
	      
	      # Store the result
	      results <- rbind(results, data.frame(Alpha_3_mean = alpha_3_mean, 
	                                           Beta_2_mean = beta_2_mean, 
	                                           Gamma_2_mean = gamma_2_mean, 
	                                           ProbabilityOfDefault = probability_of_default))
	    }
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	
	
	#Poor business operations management,Natural disasters and Loan delay
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# Parameters for missed payments due to various factors
	mu_3 <- 60  # Mean for missed payments due to poor business operations management
	sigma_3 <- 30 # Standard deviation for missed payments due to poor business operations 
	#management
	lambda_2 <- 90             # Mean for missed payments due to natural disasters
	theta <- 1/70             # Mean for missed payments due to loan delay
	
	# Step 2: Define a range of threshold means to test for each factor
	alpha_3_means <- seq(500, 540, by = 10)  # Range for poor business operations management
	beta_2_means <- seq(610, 650, by = 10)   # Range for natural disasters
	gamma_3_means <- seq(800, 1000, by = 10)  # Range for loan delay
	
	# Step 3: Run simulations for each combination of threshold means and store results
	results <- data.frame(Alpha_3_mean = numeric(), Beta_2_mean = numeric(), 
	                      Gamma_3_mean = numeric(), ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (alpha_3_mean in alpha_3_means) {
	  for (beta_2_mean in beta_2_means) {
	    for (gamma_3_mean in gamma_3_means) {
	      default_count <- 0  # Counter for default cases
	      
	      for (i in 1:simulations) {
	        # Generate missed payments due to each factor over the defined number of months
	        poor_business_op <- rnorm(months, mean = mu_3, sd = sigma_3)  # Factor 1
	        natural_disa <- rpois(months, lambda_2)                       # Factor 2
	        loan_delay <- rexp(months, theta)                       # Factor 3
	        
	        # Generate random normal distributed threshold values for each 6-month period
	        alpha_3 <- rnorm(months / 6, mean = alpha_3_mean, sd = 70)
	        beta_2 <- rnorm(months / 6, mean = beta_2_mean, sd = 70)
	        gamma_3 <- rnorm(months / 6, mean = gamma_3_mean, sd = 70)
	        
	        # Calculate total missed payments every 6 months
	        for (j in seq(1, months, by = 6)) {
	          period_index <- (j - 1) / 6 + 1
	          threshold_poor_business_op <- alpha_3[period_index]
	          threshold_natural_disa <- beta_2[period_index]
	          threshold_loan_delay <- gamma_3[period_index]
	          if ((sum(poor_business_op[j:(j + 5)]) > threshold_poor_business_op) &&
	              (sum(natural_disa[j:(j + 5)]) > threshold_natural_disa) &&
	              (sum(loan_delay[j:(j + 5)]) > threshold_loan_delay)) {
	            default_count <- default_count + 1
	            break  # Stop checking further if already defaulted
	          }
	        }
	      }
	      
	      # Step 4: Calculate the probability of default for the current combination of threshold means
	      probability_of_default <- default_count / simulations
	      
	      # Store the result
	      results <- rbind(results, data.frame(Alpha_3_mean = alpha_3_mean, 
	                                           Beta_2_mean = beta_2_mean, 
	                                           Gamma_3_mean = gamma_3_mean, 
	                                           ProbabilityOfDefault = probability_of_default))
	    }
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	
	
	
	
	
	#Poor business operations management,Political instability and Loan deviation
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# Parameters for missed payments due to various factors
	mu_3 <- 60# Mean for missed payments due to poor business operations management
	sigma_3 <- 30# Standard deviation for missed payments due to poor business operations  
	#management
	lambda_3 <- 90             # Mean for missed payments due to political instability
	lambda_4 <- 70             # Mean for missed payments due to loan deviation
	
	# Step 2: Define a range of threshold means to test for each factor
	alpha_3_means <- seq(500, 540, by = 10) # Range for poor business operations management
	beta_3_means <- seq(560, 600, by = 10)   # Range for political instability
	gamma_1_means <- seq(500, 540, by = 10)  # Range for loan deviation
	
	# Step 3: Run simulations for each combination of threshold means and store results
	results <- data.frame(Alpha_3_mean = numeric(), Beta_3_mean = numeric(), 
	                      Gamma_1_mean = numeric(), ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (alpha_3_mean in alpha_3_means) {
	  for (beta_3_mean in beta_3_means) {
	    for (gamma_1_mean in gamma_1_means) {
	      default_count <- 0  # Counter for default cases
	      
	      for (i in 1:simulations) {
	        # Generate missed payments due to each factor over the defined number of months
	        poor_business_op <- rnorm(months, mean = mu_3, sd = sigma_3)  # Factor 1
	        political_insta <- rpois(months, lambda_3)                       # Factor 2
	        loan_deviation <- rpois(months, lambda_4)                       # Factor 3
	        
	        # Generate random normal distributed threshold values for each 6-month period
	        alpha_3 <- rnorm(months / 6, mean = alpha_3_mean, sd = 70)
	        beta_3 <- rnorm(months / 6, mean = beta_3_mean, sd = 70)
	        gamma_1 <- rnorm(months / 6, mean = gamma_1_mean, sd = 70)
	        
	        # Calculate total missed payments every 6 months
	        for (j in seq(1, months, by = 6)) {
	          period_index <- (j - 1) / 6 + 1
	          threshold_poor_business_op <- alpha_3[period_index]
	          threshold_political_insta <- beta_3[period_index]
	          threshold_loan_deviation <- gamma_1[period_index]
	          if ((sum(poor_business_op[j:(j + 5)]) > threshold_poor_business_op) &&
	              (sum(political_insta[j:(j + 5)]) > threshold_political_insta) &&
	              (sum(loan_deviation[j:(j + 5)]) > threshold_loan_deviation)) {
	            default_count <- default_count + 1
	            break  # Stop checking further if already defaulted
	          }
	        }
	      }
	      
	      # Step 4: Calculate the probability of default for the current combination of threshold means
	      probability_of_default <- default_count / simulations
	      
	      # Store the result
	      results <- rbind(results, data.frame(Alpha_3_mean = alpha_3_mean, 
	                                           Beta_3_mean = beta_3_mean, 
	                                           Gamma_1_mean = gamma_1_mean, 
	                                           ProbabilityOfDefault = probability_of_default))
	    }
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	
	#Poor business operations management,Political instability and High interest rates
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# Parameters for missed payments due to various factors
	mu_3 <- 60# Mean for missed payments due to poor business operations management
	sigma_3 <- 30# Standard deviation for missed payments due to poor business operations 
	#management
	lambda_3 <- 90             # Mean for missed payments due to political instability
	lambda_5 <- 70             # Mean for missed payments due to high interest rates
	
	# Step 2: Define a range of threshold means to test for each factor
	alpha_3_means <- seq(500, 540, by = 10) # Range for poor business operations management
	beta_3_means <- seq(560, 600, by = 10)   # Range for political instability
	gamma_2_means <- seq(550, 590, by = 10)  # Range for high interest rates
	
	# Step 3: Run simulations for each combination of threshold means and store results
	results <- data.frame(Alpha_3_mean = numeric(), Beta_3_mean = numeric(), 
	                      Gamma_2_mean = numeric(), ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (alpha_3_mean in alpha_3_means) {
	  for (beta_3_mean in beta_3_means) {
	    for (gamma_2_mean in gamma_2_means) {
	      default_count <- 0  # Counter for default cases
	      
	      for (i in 1:simulations) {
	        # Generate missed payments due to each factor over the defined number of months
	        poor_business_op <- rnorm(months, mean = mu_3, sd = sigma_3)  # Factor 1
	        political_insta <- rpois(months, lambda_3)                       # Factor 2
	        high_interest <- rpois(months, lambda_5)                       # Factor 3
	        
	        # Generate random normal distributed threshold values for each 6-month period
	        alpha_3 <- rnorm(months / 6, mean = alpha_3_mean, sd = 70)
	        beta_3 <- rnorm(months / 6, mean = beta_3_mean, sd = 70)
	        gamma_2 <- rnorm(months / 6, mean = gamma_2_mean, sd = 70)
	        
	        # Calculate total missed payments every 6 months
	        for (j in seq(1, months, by = 6)) {
	          period_index <- (j - 1) / 6 + 1
	          threshold_poor_business_op <- alpha_3[period_index]
	          threshold_political_insta <- beta_3[period_index]
	          threshold_high_interest <- gamma_2[period_index]
	          if ((sum(poor_business_op[j:(j + 5)]) > threshold_poor_business_op) &&
	              (sum(political_insta[j:(j + 5)]) > threshold_political_insta) &&
	              (sum(high_interest[j:(j + 5)]) > threshold_high_interest)) {
	            default_count <- default_count + 1
	            break  # Stop checking further if already defaulted
	          }
	        }
	      }
	      
	      # Step 4: Calculate the probability of default for the current combination of threshold means
	      probability_of_default <- default_count / simulations
	      
	      # Store the result
	      results <- rbind(results, data.frame(Alpha_3_mean = alpha_3_mean, 
	                                           Beta_3_mean = beta_3_mean, 
	                                           Gamma_2_mean = gamma_2_mean, 
	                                           ProbabilityOfDefault = probability_of_default))
	    }
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	
	
	
	
	
	#Poor business operations management,Political instability and Loan delay
	# Step 1: Setup Loan and Payment Parameters
	loan_amount <- 10000        # The amount of the loan borrowed
	total_repayment <- 12000    # Total amount to be repaid over the loan period
	monthly_payment <- 200      # The fixed monthly payment amount
	months <- 5 * 12            # Total number of months over the 5-year period
	simulations <- 10000        # Number of simulations to run
	
	# Parameters for missed payments due to various factors
	mu_3 <- 60  # Mean for missed payments due to poor business operations management
	sigma_3 <- 30 # Standard deviation for missed payments due to poor business operations 
	#management
	lambda_3 <- 90             # Mean for missed payments due to political instability
	theta <- 1/70             # Mean for missed payments due to loan delay
	
	# Step 2: Define a range of threshold means to test for each factor
	alpha_3_means <- seq(500, 540, by = 10)  #Range for poor business operations management
	beta_3_means <- seq(560, 600, by = 10)   # Range for political instability
	gamma_3_means <- seq(800, 1000, by = 10)  # Range for loan delay
	
	# Step 3: Run simulations for each combination of threshold means and store results
	results <- data.frame(Alpha_3_mean = numeric(), Beta_3_mean = numeric(), 
	                      Gamma_3_mean = numeric(), ProbabilityOfDefault = numeric())
	
	set.seed(123)  # For reproducibility
	
	for (alpha_3_mean in alpha_3_means) {
	  for (beta_3_mean in beta_3_means) {
	    for (gamma_3_mean in gamma_3_means) {
	      default_count <- 0  # Counter for default cases
	      
	      for (i in 1:simulations) {
	        # Generate missed payments due to each factor over the defined number of months
	        poor_business_op <- rnorm(months, mean = mu_3, sd = sigma_3)  # Factor 1
	        political_insta <- rpois(months, lambda_3)                       # Factor 2
	        loan_delay <- rexp(months, theta)                       # Factor 3
	        
	        # Generate random normal distributed threshold values for each 6-month period
	        alpha_3 <- rnorm(months / 6, mean = alpha_3_mean, sd = 70)
	        beta_3 <- rnorm(months / 6, mean = beta_3_mean, sd = 70)
	        gamma_3 <- rnorm(months / 6, mean = gamma_3_mean, sd = 70)
	        
	        # Calculate total missed payments every 6 months
	        for (j in seq(1, months, by = 6)) {
	          period_index <- (j - 1) / 6 + 1
	          threshold_poor_business_op <- alpha_3[period_index]
	          threshold_political_insta <- beta_3[period_index]
	          threshold_loan_delay <- gamma_3[period_index]
	          if ((sum(poor_business_op[j:(j + 5)]) > threshold_poor_business_op) &&
	              (sum(political_insta[j:(j + 5)]) > threshold_political_insta) &&
	              (sum(loan_delay[j:(j + 5)]) > threshold_loan_delay)) {
	            default_count <- default_count + 1
	            break  # Stop checking further if already defaulted
	          }
	        }
	      }
	      
	      # Step 4: Calculate the probability of default for the current combination of threshold means
	      probability_of_default <- default_count / simulations
	      
	      # Store the result
	      results <- rbind(results, data.frame(Alpha_3_mean = alpha_3_mean, 
	                                           Beta_3_mean = beta_3_mean, 
	                                           Gamma_3_mean = gamma_3_mean, 
	                                           ProbabilityOfDefault = probability_of_default))
	    }
	  }
	}
	
	# Calculate the mean of all probabilities of default
	mean_probability_of_default <- mean(results$ProbabilityOfDefault)
	mean_probability_of_default
	