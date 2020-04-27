#########################################
# STEP 0: Load packages
#########################################
# Note: this process could take a couple of minutes
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org") 
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org") 
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org") 
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org") 
if(!require(rstudioapi)) install.packages("rstudioapi", repos = "http://cran.us.r-project.org") 
if(!require(ggcorrplot)) install.packages("ggcorrplot", repos = "http://cran.us.r-project.org") 
if(!require(grDevices)) install.packages("grDevices", repos = "http://cran.us.r-project.org") 
if(!require(grid)) install.packages("grid", repos = "http://cran.us.r-project.org") 
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org") 
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org") 
if(!require(rattle)) install.packages("rattle", repos = "http://cran.us.r-project.org") 
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org") 
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org") 
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org") 


#########################################
# STEP 1: Load Dataset
#########################################
# CHANGE DATA LOCATION SOURCE HERE
africa_data <- setDT(read_csv("/Users/Peter/Desktop/harvard prof cert/9 - Capstone/Individual/african_crises.csv"))
africa_data <- setDT(africa_data %>% rename(
                        Country = country,
                        Year = year,
                        Syst = systemic_crisis,
                        EX = exch_usd,
                        Dom = domestic_debt_in_default,
                        Sov = sovereign_external_debt_default,
                        GDP_w = gdp_weighted_default,
                        CPI = inflation_annual_cpi,
                        Ind = independence,
                        Curr = currency_crises,
                        Infl = inflation_crises,
                        Bank = banking_crisis))

#########################################
# STEP 2: Inspect, Transfrom, Analyse
#########################################
theme_set(theme_pubr()) #common theme for all plots

# --- Inspection
rows_africa <- nrow(africa_data) 
cols_africa <- ncol(africa_data) 
numb_countries <- uniqueN(africa_data$country) #number of countries in dataset
numb_years <-uniqueN(africa_data$Year) # range of years
min_years <-min(africa_data$Year) #minimum year recorded
max_years <-max(africa_data$Year) #maxmium year recorded
head_africa <- head(africa_data) # first few rows of dataset
summ_stats <- summary(africa_data) # descriptive statistics
crisis_summary <- africa_data[,sum(Syst), Country] %>% 
  rename("Number of Systemic Crises"=V1)

# --- Transformations
# make banking crisis column numeric
africa_data <- africa_data[, Bank:=case_when(
                                Bank=="crisis" ~ 1, 
                                T ~ 0)] 
# create % change in FX rate, per year per country
africa_data <- africa_data[, EX_change := 100*(EX-shift(EX))/EX,
                           by = Country]
# create % change in inflation rate, per year per country
africa_data <- africa_data[, CPI_change := 100*(CPI-shift(CPI))/CPI,
                           by = Country]

# fix up issue with currency crisis being "2"
# according to the original source data, these 2's are valid currency crisis and should be 1's
curr_crisis_2 <- africa_data[Curr==2] #check which rows have this
curr_crisis_prop_old <- africa_data[,.N,Curr][order(-N)] %>% 
  mutate('Currency%' = paste0(round(100*N/sum(N),0),"%")) %>% 
  rename('Currency_Crisis Count' = N, 'Currency_Crisis'=Curr) #proprotion of records with currency crisis
africa_data <- africa_data[, Curr:=case_when(Curr==0 ~ 0,T ~ 1)] #replace non-0 values with 1
curr_crisis_prop_new <- africa_data[,.N,Curr][order(-N)] %>% 
  mutate('Currency%' = paste0(round(100*N/sum(N),0),"%")) %>% 
  rename('Currency_Crisis Count' = N, 'Currency_Crisis'=Curr) #check proportions
rm(curr_crisis_prop_old,curr_crisis_prop_new)
# drop unneeded columns 
unwanted_cols <- c("case", "cc3")
africa_data <- africa_data %>% select(-all_of(unwanted_cols))
rm(unwanted_cols)
# remove duplicates if any
africa_data <- setDT(africa_data %>% distinct()) 
# replace null & inf values with 0
africa_data[is.na(africa_data)] <- 0
africa_data[africa_data == "Inf"] <- 0
africa_data[africa_data == "-Inf"] <- 0
# sumary of cleaned data
str(africa_data) #check that all columns are numeric, except country name
head_africa_data_clean <- head(africa_data)
rows_africa_clean <- nrow(africa_data) 
cols_africa_clean <- ncol(africa_data)

# --- Analysis
# proportions summary tables
bank_crisis_prop <- africa_data[,.N,Bank][order(-N)] %>% 
  mutate('Proportion '= paste0(round(100*N/sum(N),0),"%")) %>% 
  rename('Count' = N, Value=Bank) %>%
  mutate(Column='Banking Crisis')#proprotion of records with banking crisis
infl_crisis_prop <- africa_data[,.N,Infl][order(-N)] %>% 
  mutate('Proportion '= paste0(round(100*N/sum(N),0),"%")) %>% 
  rename('Count' = N, Value=Infl) %>%
  mutate(Column='Inflation Crisis')#proprotion of records with inflation crisis
curr_crisis_prop <- africa_data[,.N,Curr][order(-N)] %>% 
  mutate('Proportion '= paste0(round(100*N/sum(N),0),"%")) %>% 
  rename('Count' = N, Value=Curr) %>%
  mutate(Column='Currency Crisis') #proprotion of records with currency crisis
syst_crisis_prop <- africa_data[,.N,Syst][order(-N)] %>% 
  mutate('Proportion '= paste0(round(100*N/sum(N),0),"%")) %>% 
  rename('Count' = N, Value=Syst) %>%
  mutate(Column='Systemic Crisis') #proprotion of records with systemic crisis
dom_debt_prop <- africa_data[,.N,Dom][order(-N)] %>% 
  mutate('Proportion '= paste0(round(100*N/sum(N),0),"%")) %>% 
  rename('Count' = N, Value=Dom) %>%
  mutate(Column='Domestic Debt in Default') #proprotion of records with domesic debt in default
sov_debt_prop <- africa_data[,.N,Sov][order(-N)] %>% 
  mutate('Proportion '= paste0(round(100*N/sum(N),0),"%")) %>% 
  rename('Count' = N, Value=Sov) %>%
  mutate(Column='Sovereign Debt in Default') #proprotion of records with foreign debt in default
gpd_weight_prop <- africa_data[,.N,GDP_w][order(-N)] %>% 
  mutate('Proportion '= paste0(round(100*N/sum(N),0),"%")) %>% 
  rename('Count' = N, Value=GDP_w) %>%
  mutate(Column='GDP Weighted Default') #proprotion of records with GDP weighted default 
indep_prop <- africa_data[,.N,Ind][order(-N)] %>% 
  mutate('Proportion '= paste0(round(100*N/sum(N),0),"%")) %>% 
  rename('Count' = N, Value=Ind) %>%
  mutate(Column='Independence') #proprotion of records with independence 
#combine all above tables into one summary table
all_proportions <- rbind(syst_crisis_prop,
                         bank_crisis_prop,
                         infl_crisis_prop,
                         curr_crisis_prop,
                         dom_debt_prop,
                         sov_debt_prop,
                         gpd_weight_prop,
                         indep_prop) %>% select(Column, everything())
rm(syst_crisis_prop,
   bank_crisis_prop,
   infl_crisis_prop,
   curr_crisis_prop,
   dom_debt_prop,
   gpd_weight_prop,
   indep_prop)

# correlation matrix
correlations_all <- cor(africa_data %>% select(-c('Country', 'Year')), use = "complete.obs")
correlations_all_plot <- ggcorrplot(correlations_all, 
                             type = "lower",
                             method="circle",
                             tl.srt = 45) 
rm(correlations_all)

# Inflation 
infl_OT <- africa_data %>% #charting inflation rates per economy
                ggplot(aes(x=Year,
                            y=CPI,
                            color=Country)) + facet_wrap(~Country, scales = "free", ncol=3) +
                geom_line() + labs(x="Year", y="Inflation Rate %", title="Inflation Rate")+
                theme(legend.position = "none")
infl_change_OT <- africa_data %>% filter(abs(CPI_change)<200) %>%#charting annual inflation rate change per economy
                ggplot(aes(x=Year,
                           y=CPI_change,
                           color=Country)) + facet_wrap(~Country, scales = "free", ncol=3) +
                geom_line() + labs(x="Year", y="Annual Change %", title="Inflation Rate - Annual Change - 200% or less")+
                theme(legend.position = "none")

# Exchange rate
exch_OT <- africa_data %>% #charting exchange rate per economy
                ggplot(aes(x=Year,
                           y=EX,
                           color=Country)) + facet_wrap(~Country, scales = "free", ncol=3) +
                geom_line() + labs(x="Year", y="Exchange Rate to USD", title="Exchange Rate to USD")+
                theme(legend.position = "none")
exch_change_OT <- africa_data %>% #charting exchange inflation rate change per economy
                ggplot(aes(x=Year,
                           y=EX_change, 
                           group=Country,
                           color=Country)) + facet_wrap(~Country, scales = "free", ncol=3) +
                geom_line() + labs(x="Year", y="Annual Change %", title="Exchange Rate to USD - Annual Change")+
                theme(legend.position = "none")



#########################################
# STEP 3: Split data into Train & Test
# 80/20 split 
#########################################
africa_data <- africa_data %>% mutate(Syst = factor(Syst)) #make outcome factor
set.seed(1)
africa_test_index <- createDataPartition(y = africa_data$Syst, times = 1, p = 0.2, list = FALSE)
africa_train <- africa_data[-africa_test_index,] #create training set
africa_test <- africa_data[africa_test_index,] #create test set
rm(africa_test_index) #no longer needed 
africa_test <- africa_test %>% 
  semi_join(africa_train, by = "Country") %>% 
  semi_join(africa_train, by = "Year") #matches counrties & years in training & test sets
nrow_africa_train <- nrow(africa_train)
nrow_africa_test <- nrow(africa_test)
# 3 significant digits
options(digits = 3)



#########################################
# STEP 4: Models
#########################################

# GLM  
set.seed(1) #Country
train_glm_Country <- train(Syst ~ Country, method = "glm", data=africa_train)
y_hat_glm_Country <- predict(train_glm_Country, africa_test)
Accuracy_glm_Country <- confusionMatrix(data = y_hat_glm_Country, reference = africa_test$Syst)$overall["Accuracy"] #0.918
set.seed(1) #Bank + CPI_Change
train_glm_Bank_EX <- train(Syst ~ Bank + EX_change, method = "glm", data=africa_train)
y_hat_glm_Bank_EX <- predict(train_glm_Bank_EX, africa_test)
Accuracy_glm_Bank_EX <- confusionMatrix(data = y_hat_glm_Bank_EX, reference = africa_test$Syst)$overall["Accuracy"] #0.966
set.seed(1) #Bank + Curr 
train_glm_Bank_Curr <- train(Syst ~ Bank + Curr, method = "glm", data=africa_train)
y_hat_glm_Bank_Curr <- predict(train_glm_Bank_Curr, africa_test)
Accuracy_glm_Bank_Curr <- confusionMatrix(data = y_hat_glm_Bank_Curr, reference = africa_test$Syst)$overall["Accuracy"] #0.971
#combine GLM accuracies into summary table
GLM_Summary <- setDT(list(rbind(Accuracy_glm_Country,
                                Accuracy_glm_Bank_EX,
                                Accuracy_glm_Bank_Curr))) %>% 
                      mutate(Model = c(1,2,3), Accuracy=round(Accuracy,3)) %>% select(Model, Accuracy)
#remove unneeded objects
rm(train_glm_Country,y_hat_glm_Country,Accuracy_glm_Country,
   train_glm_Bank_EX,y_hat_glm_Bank_EX,Accuracy_glm_Bank_EX,
   train_glm_Bank_Curr,y_hat_glm_Bank_Curr)


#Decision Tree - Bank + EX + Sov
set.seed(1)
cp_vector <- seq(0, 0.05, 0.002) #vector for range of possible complexity parameters
train_tree <- train(Syst ~ ., #train decision tree model on all columns (except Country & Year)
                    method="rpart", 
                    data = africa_train %>% select(-Country, - Year),
                    tuneGrid = data.frame(cp = cp_vector))
compelxity_parameter <- ggplot(train_tree, highlight = TRUE) + labs(title="Accuracy vs Complexity Parameter") #cp=0.028
y_hat_tree <- predict(train_tree, africa_test, type = "raw")
Accuracy_decision_tree <- confusionMatrix(data = y_hat_tree, reference = africa_test$Syst)$overall["Accuracy"] #0.971
fancyRpartPlot(train_tree$finalModel,sub="", shadow=F) #creates decision tree visual, with yes/no labels & proportions
decision_tree_plot <- recordPlot() #collect graphic in console (fancyRpartPlot cannot be saved as an object)
plot.new() #cleans plot colsole
#remove unneeded objects
rm(cp_vector,train_tree,y_hat_tree)


#Random Forest
set.seed(1)
mtry_vector <-seq(1,8) #vector for range of number of parameters
train_rf <- train(Syst ~ ., #train decision tree model on all columns (except Country & Year)
                  method="rf", 
                  ntree=100,
                  data = africa_train %>% select(-Country, -Year),
                  tuneGrid = data.frame(mtry = mtry_vector))
random_predictors <-ggplot(train_rf, highlight = TRUE) + labs(title="Accuracy vs Number of Predictors") #mtry=6 is max accuracy
y_hat_rf <- predict(train_rf, africa_test, type = "raw")
number_of_trees <- train_rf$finalModel$forest$ntree
Accuracy_random_forest <-confusionMatrix(data = y_hat_rf, reference = africa_test$Syst)$overall["Accuracy"] #0.981
importance <- ggplot(varImp(train_rf)) + labs(title="Important Predictors in Random Forest")#which variables are important
#remove unneeded objects
rm(mtry_vector,train_rf,y_hat_rf)


#Combine Max Accuracy 
Accuracy_Summary <- setDT(list(rbind(Accuracy_glm_Bank_Curr,
                                     Accuracy_decision_tree,
                                     Accuracy_random_forest))) %>% 
  mutate(Model = c("GLM","Decision Tree","Random Forest"), Accuracy=round(Accuracy,3)) %>% select(Model, Accuracy)

#saves all objects to be used in markdown file - uses current location of file
save.image(paste0(getSourceEditorContext()$path,"data"))

