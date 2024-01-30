--------------------------------------------------------------------------------
  #                                  Project 
  # Name: Jones Arkoh Paintsil
  # Date: 10/28/2023
  # ECOG315
  #                                   Topic
  #  Pandemic and Stock Returns: The effects of COVID-19 on industry 
  #        profitability and investor attitudes towards risk.
  #-----------------------------------------------------------------------------
require(readr)
library(tidyverse)
library(broom)
library(stargazer)
library(dplyr)

# I am using two separate data frame for my project. The first is daily average
# stock market returns. The second is the returns on the US market portfolio.

# Importing the daily average value-weighted returns
industry_portfolios <- read_csv("industry_portfolios_daily.csv",show_col_types = FALSE) %>%
  # turn date from chr format to date format
  mutate(Date = as.Date(as.character(Date), format = "%Y%m%d")) %>%
  #format = "%Y%m%d"                  
  # make year categorical variable
  mutate(Year = as.numeric(substr(Date, 1, 4))) %>%
  # subset to 2019:2021
  filter(Year >= 2018 & Year <= 2021) 


# importing the market portfolios
F_F_Research_Data_5 <- read_csv("F-F_Research_Data_5_Factors_2x3_daily.CSV", 
                                skip=3,show_col_types = FALSE) %>%
  # turn date from chr format to date format
  mutate(Date = as.Date(as.character(Date), format = "%Y%m%d")) %>%
  # make year categorical variable
  mutate(Year = as.numeric(substr(Date, 1, 4))) %>%
  # subset to 2019:2021
  filter(Year >= 2018 & Year <= 2021) %>% 
  # I am generating market returns for the US market because I will need this 
  # variable in computing the abnormal returns for the industries.
  mutate(mkt = `Mkt-RF` + RF) %>%
  select(-Year)


#           Joining the daily average returns and market returns
project_df <- industry_portfolios %>% left_join(F_F_Research_Data_5,by="Date")


# I am working with 12 industries. Each industry has its daily weighted average
# returns. Working on it as column variable for each of the 12 industries is tedious
# and for such reason, I am going to make it longer.


# make a subset of the data and make it longer and I am calling it "dat1"
dat1 <-project_df %>% 
                  select(Date, Year, NoDur:mkt) %>% 
                  filter(Year >= 2019 & Year <= 2021) %>% 
                  pivot_longer(cols = NoDur:Other) %>%
                  rename(industry = name) %>%
                  arrange(industry) %>% 
                  rename(average_returns =value) 

# I need to give the industry full names because I will need in the process
industry_names <- data.frame( industry = c("NoDur", "Durbl", "Manuf", "Enrgy", "Chems", "BusEq", 
                                           "Telcm", "Utils", "Shops", "Hlth", "Money", "Other"),  
                              full_names = c("Consumer Non Durable", "Consumer Durables", "Manufacturing", "Energy", 
                                             "Chemicals & Allied product", "Busisness & Equipment", 
                                             "Telecommunication", "Utilities", "Shops", "Healthcare,Medical Equipment",
                                             "Money", "Other"))

dat1 <- dat1 %>% 
        left_join(industry_names, by =("industry"))



#--------------------------------------------------------------------------------
# I need to compute additional variables, for example, the cumulative average
# returns for each of the industries, abnormal returns etc.
#-------------------------------------------------------------------------------

dat1  <-  dat1 %>%
          group_by(industry) %>%
          mutate(cum_average=cumsum(average_returns)) %>% 
          mutate(abnorm_returns=average_returns-mkt) %>%
          mutate(cm_abnorm_returns=cumsum(abnorm_returns)) %>% 
          mutate(excess_return=(average_returns-RF)) %>% 
          mutate(industries=substr(industry, 1,3))

#-------------------------------------------------------------------------------
#                         Test of Means
# Conducting one sample two tail test for the entire period under study.
t.test(dat1$average_returns-dat1$mkt) #Test of means


# Conducting one sample two tail test for each of the industries
t_test_results <- dat1 %>%
                  group_by(industry) %>%
                  summarize(ttest = 
                  list(broom::tidy(t.test(average_returns, mkt))))

# Extracting the statistics from the t results
t_test_final <- t_test_results %>%
                mutate(ttest = map(ttest,~ .x[c("estimate1","estimate2","estimate","p.value")]))

# Extracting the results to a table
results_df <- t_test_results %>%
              unnest(ttest) %>%
              select(industry, statistic, estimate,estimate1,estimate2, p.value) 
write_csv(results_df, "one_sample.csv")

#                       Summary statistics 
summary_ungrouped <- dat1 %>%
                     group_by(full_names) %>%
                     summarize(
                     N = n(),
                     mean = round(mean(abnorm_returns, na.rm = TRUE), 3),
                     Std.Dev = round(sd(abnorm_returns, na.rm = TRUE), 3),
                     Min = round(min(abnorm_returns, na.rm = TRUE), 3),
                     Pctl25 = round(quantile(abnorm_returns, 0.25, na.rm = TRUE), 3),
                     median = round(median(abnorm_returns, na.rm = TRUE), 3),
                     Pctl75 = round(quantile(abnorm_returns, 0.75, na.rm = TRUE), 3),
                     Max =    round(max(abnorm_returns, na.rm = TRUE), 3))  
write_csv(summary_ungrouped, "sum.csv")


#                 visualization of the cumulative average returns
ggplot(data=dat1, aes(Date, cm_abnorm_returns, group=industries,color=industries)) + 
                  geom_line(linewidth=1) + 
                  labs(x = "Years", y = "Percent",  title = "Figure 1: Cumulative Abnormal Returns") +
                  theme(plot.title = element_text(hjust = 0.5))

#===============================================================================

# Computing the standard deviations
#===============================================================================
sd_ab <- dat1  %>%
         group_by(Year,industries) %>%
         summarize(sd=sd(abnorm_returns), .groups = "drop") %>% 
         arrange(desc(sd))

# visualization of the standard deviation by year Points
ggplot(sd_ab,aes(x=industries, y=sd, color=factor(Year))) +
                 geom_point(size=5) +
                 geom_text(aes(label = industry), vjust = -0.5, hjust = 1) %>% 
                 labs(x = "Industry", y = "standard deviation",
                 title = "Figure 2: Abnormal Returns by Year")
                 



#-------------------------------------------------------------------------------
# Now, I need to join the standard deviations back to the main data frame and 
# I am going to call it "dat2"
#-------------------------------------------------------------------------------

# Joining the standard Deviations
dat2 <- dat1 %>% 
        left_join(sd_ab,by=c("Year", "industries"))


#                   2019 standard Deviations
# Let me called it part A; where I am comparing 2019 standard deviations to 2020
# cumulative abnormal returns. First, I need to filter the standard deviations
# to 2019. 

sd_2019 <- dat2 %>%
           filter(Year==2019) %>%
           select(Date,industries, sd) %>% 
           group_by(industry) %>% 
           mutate(id =row_number())

# Similarly, I am filtering the data frame to only 2020 
s20_cum_av <- dat2 %>%
              filter(Year ==2020) %>%
              select(-sd,-Date) %>% 
              group_by(industry) %>% 
              mutate(id = row_number())

# I need to join these two data frame. I tried joining it by industry but the 
# observations became just too many which I suspect something went wrong. Because
# of that I generated an id, a count for each of the days in 2019/2020 for a 
# particular industry. I realized that the length of days for the 2020 data frame is 
# longer than that of the 2019 and so to prevent having some few missing observation
# I matched the 2020 cumulative returns to the 2019 standard deviations.

combined_19_20 <- sd_2019 %>% 
                  left_join(s20_cum_av,by=c("id","industries"))


# Calculating the group variable: risky versus safe. Here, I am generating a 
# group variable based on the median standard deviations. If a particular industry
# has a standard deviation above this median value, it is classified as risky, else,
# is safe.

combined_19_20 <- combined_19_20 %>%
                 mutate(median=median(sd)) %>% 
                 mutate(risky=ifelse(sd> 0.54, 1, 0))
table(combined_19_20$risky)

# Test of means   risky versus safe
# Now, I am performing 2 sample two tail test to compare the difference in
# cumulative abnormal returns between risky and safe industries.

case1 <- t.test(combined_19_20$abnorm_returns~combined_19_20$risky)
# Table to store the t-test results
test_results1 <- data.frame(
                Variable = "cumulative abnormal Returns",
                Category = "risky vs safe",
                t_stat = case1$statistic,
                p_value = case1$p.value,
                Mean_Risky = mean(combined_19_20$abnorm_returns[combined_19_20$risky== 1], na.rm = TRUE),
                Mean_Safe = mean(combined_19_20$abnorm_returns[combined_19_20$risky == 0], na.rm = TRUE))

#===============================================================================
#                        2020 Standard Deviations
# Repeating the process by comparing 2020 standard deviation to 2021 cumulative 
#===============================================================================

#                   Abnormal returns
sd_2020 <- dat2 %>%
            filter(Year==2020) %>%
            select(Date,industries, sd) %>% 
            group_by(industry) %>% 
            mutate(id =row_number())

# Similarly, I am filtering the data frame to only 2020 
s21_cum_av <- dat2 %>%
              filter(Year ==2021) %>%
              select(-sd,-Date) %>% 
              group_by(industry) %>% 
              mutate(id = row_number())

# Joining the data frame for 2020 sd versus 2021 cumulative abnormal returns
combined_20_21 <- sd_2020 %>% 
                  left_join(s21_cum_av,by=c("id","industries"))

quantile(combined_20_21$sd)
combined_20_21 <- combined_20_21 %>%
                  mutate(median=median(sd)) %>% 
                  mutate(risky=ifelse(sd>median, 1, 0))

table(combined_20_21$risky)

# Test of means   risky versus safe
case2 <- t.test(combined_20_21$abnorm_returns~combined_20_21$risky)

# Table to store the t-test results
test_results2 <- data.frame(
                Variable = "cumulative abnormal Returns",
                Category = "risky vs safe",
                t_stat = case2$statistic,
                p_value = case2$p.value,
                Mean_Risky = mean(combined_20_21$abnorm_returns[combined_20_21$risky == 1], na.rm = TRUE),
                Mean_Safe = mean(combined_20_21$abnorm_returns[combined_20_21$risky == 0], na.rm = TRUE))


# Let me combined the two tail t-test results
test_combined<- rbind(test_results1, test_results2) %>% 
                mutate(Year=c(2020,2021))
write_csv(test_combined, "test_results.csv")

# Summary of Group variable
summary_naiv_0 <- combined_19_20 %>%
                  filter(risky == 0) %>%
                  group_by(full_names) %>%
                  summarize(
                  N = n(),
                  mean = round(mean(abnorm_returns, na.rm = TRUE), 2),
                  Std.Dev = round(sd(abnorm_returns, na.rm = TRUE), 2),
                  Min = round(min(abnorm_returns, na.rm = TRUE), 2),
                  Pctl25 = round(quantile(abnorm_returns, 0.25, na.rm = TRUE), 2),
                  median = round(median(abnorm_returns, na.rm = TRUE), 2),
                  Pctl75 = round(quantile(abnorm_returns, 0.75, na.rm = TRUE), 2),
                  Max = round(max(abnorm_returns, na.rm = TRUE), 2)) %>% 
                  mutate(Risky="safe")


summary_naiv_1 <- combined_19_20 %>%
                  filter(risky ==1) %>%
                  group_by(full_names) %>%
                  summarize(
                  N = n(),
                  mean = round(mean(abnorm_returns, na.rm = TRUE), 2),
                  Std.Dev = round(sd(abnorm_returns, na.rm = TRUE), 2),
                  Min = round(min(abnorm_returns, na.rm = TRUE), 2),
                  Pctl25 = round(quantile(abnorm_returns, 0.25, na.rm = TRUE), 2),
                  median = round(median(abnorm_returns, na.rm = TRUE), 2),
                  Pctl75 = round(quantile(abnorm_returns, 0.75, na.rm = TRUE), 2),
                  Max = round(max(abnorm_returns, na.rm = TRUE), 2)) %>% 
                  mutate(Risky="Risky")

# Joining of Risky and Safe industries conditional summary Statistics
summary_bind <- rbind(summary_naiv_0, summary_naiv_1) %>% 
  relocate('Risky')
write_csv(summary_bind, "sum_grp.csv")

#================================================================================

                                        # Graphs
#================================================================================

# I want to create a label instead of using zero and one
fill_color <- scale_fill_manual(values = c("1" = "red", "0" = "blue"),
                                labels = c("1" = "risky", "0" = "safe"))

# Histogram of the variable used in T-test with custom fill scale
ggplot(combined_19_20, aes(x=industries, y=abnorm_returns, fill = factor(risky))) +
                geom_bar(stat = "summary", fun = "mean") +
                labs(x = "Industry", y = "Percent",
                title = "Figure 3: Abnormal Returns in 2020") +
                fill_color +
                theme(plot.title = element_text(hjust = 0.5))
                #scale_y_continuous(limits = c(-40, 80), breaks = seq(-40, 80, by = 20))


# Histogram of the variable used in T-test with custom fill scale
ggplot(combined_20_21, aes(x=industries, y=abnorm_returns,fill = factor(risky))) +
               geom_bar(stat ="summary", fun = "mean") +
               labs(x = "Industry", y = "Percent",
               title = "Figure 4: Abnormal Returns in 2021") +
               fill_color +
               theme(plot.title = element_text(hjust = 0.5)) 
               #scale_y_continuous(limits = c(-40, 80), breaks = seq(-40, 80, by = 20))



image_1<-image_read("C:\\Users\\KOJO NYARKOH\\OneDrive\\Desktop\\R_project\\image_1.png")
image_2<-image_read("C:\\Users\\KOJO NYARKOH\\OneDrive\\Desktop\\R_project\\image_2.png")
appended_image <- image_append(c(image_1, image_2), stack = TRUE)
image_write(appended_image,"image_3.png")
#===============================================================================



#                               PART 2



#==============================================================================
#                       Fama and French Five Factor Model
#==============================================================================


#==============================================================================
dat2 <-         project_df %>% 
                select(Date, Year, NoDur:mkt) %>% 
                filter(Year >= 2018 & Year <= 2021) %>% 
                pivot_longer(cols = NoDur:Other) %>%
                rename(industry = name) %>%
                arrange(industry) %>% 
                rename(average_returns =value) %>% 
                mutate(excess_return=(average_returns-RF)) 

###############################################################################
#                       Regression Analysis
##############################################################################

#             Using for loop for the regression

# Get unique values of industry names and years
names <- unique(dat2$industry)
years <- unique(dat2$Year)

# Initialize an empty list to store regression results
regression_results <- list()

# Loop through unique names and years and perform linear regression for each combination
for (name in names) {
  for (year in years) {
dat2_reg <- dat2 %>% 
           filter(industry == name, Year == year)
    
#   Linear regression for the current group and year
mod <- lm(excess_return ~ `Mkt-RF` + SMB + HML + RMW + CMA, data = dat2_reg)
    
    # Assign results to the regression window with a unique name combining industry and year
    regression_results[[paste0("mod_", name, "_", year)]] <- mod
  }
}



#############################################################################
#     Using Stargazer for reporting the Regression output
#############################################################################
?stargazer
attach(regression_results)

# Extracting model names
model_names <- c("BusEq", "Chems", "Durbl", "Enrgy", "Hlth", "Manuf","Money", 
                 "NoDur", "Other", "Shops", "Telcm", "Utils")

stargazer(regression_results[1:6],type="html",out="results.html", title.align = "center",
          title="Fema and French Factor Model",column.labels = model_names)


model_names2=c("Money", "NoDur", "Other", "Shops", "Telcm", "Utils")
stargazer(regression_results[7:12],type="html", out="results1.html",
          title="Fema and French Factor Model",column.labels = model_names2)



# Define the year you want to extract regression results for
year_20 <- 2020  # Replace this with the desired year

# Filter regression models for the selected year
models <- lapply(names, function(name) {
  # Extract models for the specific year and industry
  selected_model <- regression_results[[paste0("mod_", name, "_", year_20)]]
  return(selected_model)
})

# Use stargazer to output the regression results for the selected year
stargazer(models, type = "html", out = "results_20.html", title.align = "center",
          title = paste("Fama and French Factor Model - Year", year_20))
#
# Extract only the coefficients
# Initialize an empty list to store coefficients by year
coefficients_by_year <- list()

# Loop through unique years and extract coefficients for each year's model
for (year in years) {
  year_model <- paste0("mod_", year)
  
  # Assuming regression_results contains models named as "mod_2019", "mod_2020", etc.
  coefficients <- coef(regression_results[[year_model]])
  
  # Store coefficients for each year in the list
  coefficients_by_year[[as.character(year)]] <- coefficients
}



#==============================================================================
# Import Expected Return
expect_return <- read.csv("expect_return.csv")
residuals_df  <- dat2 %>% 
                 filter(Year >= 2019 & Year <= 2021) %>% 
                 left_join(expect_return, by=c("Year", "industry")) %>%
                 group_by(industry) %>% 
                 mutate(industries=substr(industry,1,3)) %>% 
                 mutate(ab_return_res=excess_return-expected_return) %>% 
                 mutate(cm_ab_res=cumsum(ab_return_res))            
#===============================================================================
                
                
                 
#                 visualization of the Residuals

ggplot(data=residuals_df, aes(Date,ab_return_res, group=industry,color=industry)) + 
  geom_line() + 
  labs(x = "Years", y = "Abnormal Returns", title = "Abnormal Returns") +
  theme(plot.title = element_text(hjust = 0.5))


#===============================================================================
#                 visualization of Cumulative return
#===============================================================================

ggplot(data=residuals_df, aes(Date,cm_ab_res, group=industry,color=industry)) + 
        geom_line(linewidth=1) + 
        labs(x = "Years", y = "Percent",title = "Figure 5: Cumulative Abnormal Returns") +
        theme(plot.title = element_text(hjust = 0.5))

#===============================================================================
# Summary statistics for the ungrouped
summary_upres <- residuals_df %>%
                    group_by(industry) %>%
                    summarize(
                    N = n(),
                    mean = round(mean(ab_return_res, na.rm = TRUE), 3),
                    Std.Dev = round(sd(ab_return_res, na.rm = TRUE), 3),
                    Min = round(min(ab_return_res, na.rm = TRUE), 3),
                    Pctl25 = round(quantile(ab_return_res, 0.25, na.rm = TRUE), 3),
                    median = round(median(ab_return_res, na.rm = TRUE), 3),
                    Pctl75 = round(quantile(ab_return_res, 0.75, na.rm = TRUE), 3),
                    Max = round(max(ab_return_res, na.rm = TRUE), 3)) 

# Writing out the output
write_csv(summary_upres, "sum_res.csv")



#===============================================================================
#               Computing the standard deviations
#===============================================================================
sd_ab_res <- residuals_df  %>%
             group_by(Year,industries) %>%
             summarize(sd=sd(ab_return_res), .groups = "drop") %>% 
             arrange(desc(sd))
#===============================================================================
#             visualization of the standard deviation by year
#===============================================================================
ggplot(sd_ab_res,aes(x=industries, y=sd, color=factor(Year))) +
             geom_point(size=5) +
             labs(x = "Industry", y = "standard devation from residuals", 
             title = "Figure 6: Abnormal Returns by Year")

#===============================================================================
#                   Joining the standard Deviations
#===============================================================================
residuals_df <- residuals_df %>% 
                left_join(sd_ab_res,by=c("Year", "industries"))

#==============================================================================
#                   2019 standard Deviations
#============================================================================== 

sd_2019_res  <- residuals_df  %>%
                filter(Year==2019) %>%
                select(Date,industries, sd) %>% 
                group_by(industries) %>% 
                mutate(id =row_number())
#==============================================================================
# Similarly, I am filtering the data frame to only 2020 
s20_cum_av_res  <- residuals_df %>%
               filter(Year ==2020) %>%
               select(-sd,-Date) %>% 
               group_by(industry) %>% 
               mutate(id = row_number())
#==============================================================================
#             joining
#==============================================================================
combined_19_20_res <- sd_2019_res %>% 
                  left_join(s20_cum_av_res,by=c("id","industries"))

# Creating a dummy of risky vs safe
combined_19_20_res <- combined_19_20_res %>%
                      mutate(median=median(combined_19_20_res$sd, na.rm = TRUE)) %>% 
                      mutate(risky = ifelse(sd > median, 1, 0))
table(combined_19_20_res$risky)


# Test of means   risky versus safe
case1_res <- t.test(combined_19_20_res$ab_return_res~combined_19_20_res$risky)


# Table to store the t-test results
test_results_res1 <- data.frame(
                    Variable = "Abnormal Returns",
                    Category = "risky vs safe",
                    t_stat   = case1_res$statistic,
                    p_value  = case1_res$p.value,
                    Mean_Risky = mean(combined_19_20_res$ab_return_res[
                    combined_19_20_res$risky == 1], na.rm = TRUE),
                    Mean_Safe  = mean(combined_19_20_res$ab_return_res[
                   combined_19_20_res$risky == 0], na.rm = TRUE))

# Histogram of the variable used in T-test with custom fill scale
# I want to create a label instead of using zero and one
fill_color <- scale_fill_manual(values = c("1" = "red", "0" = "blue"),
                                labels = c("1" = "risky", "0" = "safe"))

ggplot(combined_19_20_res, aes(x = industries, y = ab_return_res, fill = factor(risky))) +
              geom_bar(stat = "summary", fun="mean") +
              labs(x = "industry", y = "Percent", 
              title = "Figure 7: Abnormal Returns in 2020") +
              fill_color +
              theme(plot.title = element_text(hjust = 0.5))

#                        2020 Standard Deviations
# Repeating the process by comparing 2020 standard deviation to 2021 cumulative 
# abnormal returns
sd_2020_res <- residuals_df %>%
                 filter(Year==2020) %>%
                 select(Date,industries, sd) %>% 
                 group_by(industries) %>% 
                 mutate(id =row_number())

# Similarly, I am filtering the data frame to only 2020 
s21_cum_av_res <- residuals_df %>%
                  filter(Year ==2021) %>%
                  select(-sd,-Date) %>% 
                  group_by(industry) %>% 
                  mutate(id = row_number())

# Joining the data frame for 2020 sd versus 2021 cumulative abnormal returns
combined_20_21_res <- sd_2020_res %>% 
                      left_join(s21_cum_av_res,by=c("id","industries"))

combined_20_21_res <- combined_20_21_res %>%
                      mutate(median=median(combined_20_21_res$sd, na.rm = TRUE)) %>% 
                      mutate(risky = ifelse(sd > median, 1, 0))
table(combined_20_21_res$risky)

# Test of means   risky versus safe
case2_res <- t.test(combined_20_21_res$ab_return_res~combined_20_21_res$risky)

# Table to store the t-test results
test_results2_res <- data.frame(
  Variable = "Abnormal Returns",
  Category = "risky vs safe",
  t_stat = case2_res$statistic,
  p_value = case2_res$p.value,
  Mean_Risky = mean(combined_20_21_res$ab_return_res[combined_20_21_res$risky == 1], na.rm = TRUE),
  Mean_Safe = mean(combined_20_21_res$ab_return_res[combined_20_21_res$risky == 0], na.rm = TRUE))


# Let me combined the two tail t-test results
test_combined_res <- rbind(test_results_res1, test_results2_res) %>% 
                     mutate(Year=c(2020,2021))
write_csv(test_combined_res, "ttest_res.csv")


# Histogram of the variable used in T-test with custom fill scale
# I want to create a label instead of using zero and one
fill_color <- scale_fill_manual(values = c("1" = "red", "0" = "blue"),
                  labels = c("1" = "risky", "0" = "safe"))

ggplot(combined_20_21_res, aes(x = industries, y = ab_return_res, fill = factor(risky))) +
                geom_bar(stat = "summary", fun="mean") +
                labs(x = "industry", y = "Percet", 
                title = "Figure 8: Abnormal Returns in 2021") +
                fill_color +
                theme(plot.title = element_text(hjust = 0.5))


# Summary of Group variable
summary_res_0 <- combined_19_20_res %>%
  filter(risky == 0) %>%
  group_by(industries) %>%
  summarize(
    N = n(),
    mean = round(mean(ab_return_res, na.rm = TRUE), 2),
    Std.Dev = round(sd(ab_return_res, na.rm = TRUE), 2),
    Min = round(min(ab_return_res, na.rm = TRUE), 2),
    Pctl25 = round(quantile(ab_return_res, 0.25, na.rm = TRUE), 2),
    median = round(median(ab_return_res, na.rm = TRUE), 2),
    Pctl75 = round(quantile(ab_return_res, 0.75, na.rm = TRUE), 2),
    Max = round(max(ab_return_res, na.rm = TRUE), 2)) %>% 
  mutate(Risky="safe")


summary_res_1 <- combined_19_20_res %>%
  filter(risky ==1) %>%
  group_by(industries) %>%
  summarize(
    N = n(),
    mean = round(mean(ab_return_res, na.rm = TRUE), 2),
    Std.Dev = round(sd(ab_return_res, na.rm = TRUE), 2),
    Min = round(min(ab_return_res, na.rm = TRUE), 2),
    Pctl25 = round(quantile(ab_return_res, 0.25, na.rm = TRUE), 2),
    median = round(median(ab_return_res, na.rm = TRUE), 2),
    Pctl75 = round(quantile(ab_return_res, 0.75, na.rm = TRUE), 2),
    Max = round(max(ab_return_res, na.rm = TRUE), 2)) %>% 
  mutate(Risky="Risky")

# Joining of Risky and Safe industries conditional summary Statistics
summary_bind <- rbind(summary_res_0, summary_res_1) %>% 
  relocate('Risky')
write_csv(summary_bind, "sum_grp_res.csv")


