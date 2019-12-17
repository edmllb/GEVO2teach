### Intall and load packages
list.of.packages <- c("ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, library, character.only = TRUE)

# Some colours
grey = "#d1d2d4"
fill_colour = "#2678b2"
line_colour = "#222222"
red_colour = "#d4292f"


prepare_data <- function(data) {
  ### Prepare the data for using it in the coming functions
  
  # Make scores numeric
  data$DiffprepostA <- as.numeric(data$DiffprepostA)
  data$TotalpreA <- as.numeric(data$TotalpreA)
  # Make the schemes of work factors
  data$SoW <- as.factor(data$SoW)
  
  return(data)
}

calculate_loess_residuals <- function(data) {
  ### Get the loess residuals from the model

  # Build the loess model
  data.lo <- loess(data$DiffprepostA~data$TotalpreA)
  # create a new dataframe with the residuals and the SoWs
  residuals_data <- data.frame(residuals = data.lo$residuals, SoW = data$SoW)
  
  return(residuals_data)
}

calc_interactions <- function(SoW1, SoW2, SoW3, SoW4) {
 
  ### Calculate the interaction effects
  ### https://onlinelibrary.wiley.com/doi/full/10.1111/j.1365-2753.2007.00872.x
  ###
  ### Format, e.g.
  ### 
  ###                   |              Lesson 1       
  ### ------------------|------------------------------       
  ### Lesson 2          |  Hunting Moths   |  Paper Moths
  ### ------------------|------------------|-------------
  ### Trilobites        |       SoW1       |     SoW3 
  ###                   |                  |
  ### Pentadactyl limb  |       SoW2       |     SoW4    
  
  # Lesson 1 Means
  mean_SoW1_SoW2 <- mean(c(SoW1, SoW2))
  mean_SoW3_SoW4 <- mean(c(SoW3, SoW4))
  
  # Lesson 2 Means
  mean_SoW1_SoW3 <- mean(c(SoW1, SoW3))
  mean_SoW2_SoW4 <- mean(c(SoW2, SoW4))

  # Grand Mean
  grand_mean <- mean(c(SoW1, SoW2, SoW3, SoW4))
  
  # # Effect of Lesson 1
  hm_effect <- mean_SoW1_SoW2 - grand_mean
  pm_effect <- mean_SoW3_SoW4 - grand_mean

  # Effect of Lesson 2
  t_effect <- mean_SoW1_SoW3 - grand_mean 
  pl_effect <- mean_SoW2_SoW4 - grand_mean
  
  # Interaction effects
  SoW1_interaction <- SoW1 - grand_mean - hm_effect - t_effect
  SoW2_interaction <- SoW2 - grand_mean - hm_effect - pl_effect
  SoW3_interaction <- SoW3 - grand_mean - pm_effect - t_effect
  SoW4_interaction <- SoW4 - grand_mean - pm_effect - pl_effect
  
  return(c(SoW1_interaction, SoW2_interaction, SoW3_interaction, SoW4_interaction))
}


get_instance_interactions <- function(data) {
  
  # get the residuals for the set of data
  resids <- calculate_loess_residuals(data)
  # for each of the schemes of work 1-4, get the mean residual
  mean_residuals <- c()
  for (i in 1:4) {
    mean_SoW_residual <- mean(resids$residuals[resids$SoW == i])
    mean_residuals <- c(mean_residuals, mean_SoW_residual)
  }
  # calculate the scheme of work interactions
  interactions <- calc_interactions(mean_residuals[1], mean_residuals[2], mean_residuals[3], mean_residuals[4])
  
  return(interactions)
}

calculate_dataset_interactions <- function(data, bootstrap_total = 1000) {
  # Boostrap the interactions
  # Randomly shuffle the SoW column
  
  # prepare the output dataframe
  output_data = data.frame(run = character(), SoW1 = numeric(), SoW2 = numeric(), SoW3 = numeric(), SoW4 = numeric(), stringsAsFactors = FALSE)

  # prepare the data
  data <- prepare_data(data)
  
  # get the true interactions
  true_interactions <- get_instance_interactions(data)
  
  # add the true data
  output_data[nrow(output_data) + 1,] <- c("true", true_interactions)

  print(paste("Bootstrapping interactions x ", bootstrap_total, " ...", sep = ""))
  for (i in 1:bootstrap_total) {
    sample_data <- data
    sample_data$SoW <- sample(sample_data$SoW, replace = FALSE)
    instance_interactions <- get_instance_interactions(sample_data)
    output_data[nrow(output_data) + 1,] <- c("sim", instance_interactions)

    if (i %% 10 == 0) {
      print(paste("Completed iteration ", i, sep = ""))
    }
  }

  output_data$SoW1 <- as.numeric(output_data$SoW1)
  output_data$SoW2 <- as.numeric(output_data$SoW2)
  output_data$SoW3 <- as.numeric(output_data$SoW3)
  output_data$SoW4 <- as.numeric(output_data$SoW4)
  
  return(output_data)
}

get_outputs <- function(outputs) {
  # Prepare the output
  # Return the true value, median simulant interaction and empirical p value
  
  true_interactions <- outputs[outputs$run == "true",]
  sim_interactions <- outputs[outputs$run != "true",]

  final_outputs <- data.frame(
    SoW = character(),
    Interaction = double(),
    Simulants = double(),
    Min_Simulant_Interaction = double(),
    Median_Simulant_Interaction = double(),
    Max_Simulant_Interaction = double(),
    Empirical_P = double(),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:4) {
    col = paste("SoW", i, sep = "")
    
    true <- true_interactions[[col]]
    sims <- sim_interactions[[col]]
    
    if (true > 0) {
      # the case where the true interaction effect is positive, so want a greater than one-tailed test
      empirical_p <- (sum(sims >= true) + 1) / (length(sims) + 1)
    } else {
      # the case where the true interaction effect is negative, so want a less than one-tailed test
      empirical_p <- (sum(sims <= true) + 1) / (length(sims) + 1)
    }
    
    final_outputs[nrow(final_outputs) + 1, ] = c(col, true, length(sims), min(sims), median(sims), max(sims), empirical_p)
  }
  
  return(final_outputs)
}

get_absolute_interaction_effects <- function(outputs, data_name) {
  # Get the schemes of work, convert to absolute values
  SoWs <- abs(outputs[, 2:5])
  # Get the sums for each row of absolute values
  sums <- rowSums(SoWs, na.rm = FALSE, dims = 1)
  aes <- data.frame(run = outputs$run, absolute_effect = sums)
  # Separate the true value from the simulants
  sims <- aes[aes$run != "true",]
  true <- aes[aes$run == "true",]
  
  # Plot of the absolute interaction effects
  x_min = min(min(sims$absolute_effect), true$absolute_effect)
  x_max = max(max(sims$absolute_effect), true$absolute_effect) + 0.5
  
  plot <- ggplot() + 
    geom_histogram(aes(x = sims$absolute_effect, fill = "fill_colour"), col = line_colour, bins = 50) + 
    scale_x_continuous(limits = c(x_min, x_max)) + 
    labs(x = "Sum of absolute interaction effects", y = "Count") +
    geom_vline(aes(xintercept = true$absolute_effect, col = "red_colour"), lwd = 1.5) +
    scale_fill_manual(name = element_blank(), values = c(fill_colour = fill_colour), labels = "Simulant Samples") + 
    scale_color_manual(name = element_blank(), values = c(red_colour = red_colour), labels = "True Sample") +
    theme_minimal() +
    theme(
      legend.margin = unit(0, "cm")
    )

  # Print and save the plot
  print(plot)
  filepath = paste("./", data_name, "_absolute_interaction.pdf", sep = "")
  print(filepath)
  ggsave(filename = filepath, plot = plot, width = 8, height = 5)
  
  p <- (nrow(sims[sims$absolute_effect >= true$absolute_effect,]) + 1) / (nrow(sims) + 1)
  
  output = data.frame(
      True_Absolute_Interaction_Sum = double(),
      Min_Absolute_Interaction_Sim = double(),
      Median_Absolute_Interaction_Sim = double(),
      Max_Absolute_Interaction_Sim = double(),
      Empirical_P = double(),
      stringsAsFactors = FALSE
    )
  
  output[nrow(output) + 1, ] = c(true$absolute_effect, min(sims$absolute_effect), median(sims$absolute_effect), max(sims$absolute_effect), p)

  return(output)
}

run_analysis <- function(data, data_name, bootstrap_total = 1000) {
  # Wrapper to run the analysis
  outputs <- calculate_dataset_interactions(data, bootstrap_total = bootstrap_total)
  interaction_outputs <- get_outputs(outputs)
  absolute_effect_outputs <- get_absolute_interaction_effects(outputs, data_name)

  # save outputs
  write.csv(interaction_outputs, file = paste(data_name, "_interaction_outputs.csv", sep = ""), row.names = F)
  write.csv(absolute_effect_outputs, file = paste(data_name, "_absolute_interaction_effects.csv", sep = ""), row.names = F)
}

run_analysis(summarydatanonas, data_name, bootstrap_total = 100)




####
#### Read the data in here
#### 

## set working directory (output files will be saved here)
# setwd("../")
#
filepath <- "~/Downloads/Student_summarynonasT1.csv"
# filepath <- "~/Downloads/Student_summarynonasT2.csv"
data_name = "tranche1"
summarydatanonas <- read.csv(filepath, header = TRUE, stringsAsFactors = FALSE)

# Run the analysis
run_analysis(summarydatanonas, data_name, bootstrap_total = 10000)




