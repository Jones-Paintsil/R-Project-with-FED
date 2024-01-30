#############################################################################
# Generating a list to store the results
regression_results <- list()

# Get unique values of full_names
unique_names <- unique(dat1$industry)

# Loop through unique names and perform linear regression for each group
for (name in unique_names) {
  dat1_reg <- dat1 %>% filter(industry == name, Year==2019)
  
  # Linear regression for the current group
  mod_name <- lm(excess_return ~ `Mkt-RF` + SMB + HML + 
                   RMW + CMA, data = dat1_reg)
  
  # Assign each results to the regression window
  regression_results[[paste0("mod_", name)]] <- mod_name
  #assign(paste0("mod_", name), mod_name)
}


# Filter data for the year 2020
dat1_2020 <- dat1 %>% filter(Year == 2020)

# Create an empty data frame to store abnormal returns
abnormal_returns_df <- data.frame(Date = dat1_2020$Date, Abnormal_Return = numeric(nrow(dat1_2020)))

# Loop through each observation in the 2020 dataset
for (i in 1:nrow(dat1_2020)) {
  industry <- dat1_2020$industry[i]  
  
  # Retrieve coefficients for the corresponding industry model from 2019
  coefficients <- coef(regression_results[[paste0("mod_", industry)]])
  
  # Extract factor values for the current observation in 2020
  factor_values_2020 <- c(1, dat1_2020$`Mkt-RF`[i], dat1_2020$SMB[i], dat1_2020$HML[i], dat1_2020$RMW[i], dat1_2020$CMA[i])
  
  # Calculate expected returns based on the 2019 model for the 2020 factors
  expected_return_2020 <- sum(coefficients * factor_values_2020)
  
  # Calculate abnormal returns: Actual Excess Return (2020) - Expected Return (based on 2019 model using 2020 factors)
  abnormal_return <- dat1_2020$excess_return[i] - expected_return_2020
  
  # Store abnormal return for each day in the data frame
  abnormal_returns_df$Abnormal_Return[i] <- abnormal_return
}


###########################################################################
#                 Generating the Residuals # Abnormal Returns
###########################################################################

# Generating a list to store the results
residuals_df <- data.frame()
# Get unique values of full_names
unique_names <- unique(dat1$industry)

for (name in unique_names) {
  dat1_reg <- dat1 %>% filter(industry == name)
  
  # Linear regression for the current group
  mod_name <- lm(excess_return ~ `Mkt-RF` + SMB + HML + 
                   RMW + CMA, data = dat1_reg)
  
  # Calculating the residuals
  residuals <- residuals(mod_name)
  n         <- length(residuals)
  
  data <- data.frame(Date = dat1_reg$Date,industry = dat1_reg$industry,
                     full_names = rep(name, n), ab_return_res = residuals)
  
  residuals_df <- rbind(residuals_df, data)
  
}















##################################################################################
# Modification
################################################################################



# Create a list to store the individual models
model_list <- list()

# Using for loop to run linear regression for each industry
for (name in unique_names) {
  mod2 <- regression_results[[name]]
  
  # Store the model in the list
  model_list[[name]] <- mod2
}

# Pass the list of models to stargazer in a single call
stargazer::stargazer(do.call(c, model_list))




















#################################################################################
#                           Coefficient Plots
################################################################################

# Create a Word document
doc <- read_docx()

# Loop through your ggplot2 plots and save them as image files
for (name in unique_names) {
  mod2 <- regression_results[[name]]
  
  # Extract the coefficients into a data frame
  coef_data <- tidy(mod2)
  
  # Calculate p-values for each coefficient
  coef_data$p_value <- summary(mod2)$coefficients[, "Pr(>|t|)"]
  
  # Create a new column to convert p_value to "True" or "False"
  coef_data <- coef_data %>% 
    mutate(pval = if_else(p_value < 0.05, "True", "False"))
  
  # Create a ggplot2 plot of coefficients with significance markers
  p <- ggplot(coef_data, aes(x = term, y = estimate, color = pval)) +
    geom_point() +
    geom_errorbar(aes(ymin = estimate - std.error,
                      ymax = estimate + std.error), width = 0.2) +
    geom_text(data = subset(coef_data, pval == "True"), 
              aes(label = ifelse(p_value < 0.001, "***",
                                 felse(p_value < 0.01, "**", "*")),
                  hjust = -0.1, vjust = 0.5)) +
    labs(title = name, x = "Coefficient", y = "Estimate") +
    theme_minimal()
  
  print(p)
  
  ggsave(filename = paste0("plot_", name, ".png"), plot = p, width = 7, height = 5)  
  
  doc <- doc %>% body_add_img(src = paste0("plot_", name, ".png"),
                              width = 6, height = 4)  
}

print(doc, target = "regression_plots.docx")


