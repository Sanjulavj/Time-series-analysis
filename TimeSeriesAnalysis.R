#Objective1 
#k-means clustering
#Sanjula Jayawardana w1790259



###########loarding libaries 



knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)
library(tidymodels)
library(readxl)
library(neuralnet)
library(knitr)

ExData <- read_excel("E:/2nd year 2nd sem/Machine Learning/Final Submission/ExchangeUSDFinal.xlsx") %>%
  janitor::clean_names() %>%
  mutate(date_in_ymd = ymd(yyyy_mm_dd)) %>%
  select(-1) %>%
  select(date_in_ymd,everything())

#view(ExData)


#######################getting all the data proeccessing to a lag series 


usd_exchange = ExData %>%
  mutate(day_1_setA = lag(ExData$usd_eur,1),
         day_1_setB  = lag(ExData$usd_eur,1),
         day_2_setB  = lag(ExData$usd_eur,2),
         day_1_setC  = lag(ExData$usd_eur,1),
         day_2_setC  = lag(ExData$usd_eur,2),
         day_3_setC  = lag(ExData$usd_eur,3),
         day_1_setD  = lag(ExData$usd_eur,1),
         day_2_setD  = lag(ExData$usd_eur,2),
         day_5_setD  = rollmean(usd_eur,5, fill = NA),
         day_10_setD  = rollmean(usd_eur,10, fill = NA)) %>%
  
  
  drop_na()

###############plotting the lag series


usd_exchange %>%
  pivot_longer(cols = 3,names_to = "kind",values_to = "rate") %>%
  ggplot(aes(date_in_ymd,rate, color = kind)) +
  geom_line() +
  facet_wrap(~kind) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1
  )) +
  labs(x = "",
       title = "First Input Variables Set") +
  theme(legend.position = "none")


usd_exchange %>%
  pivot_longer(cols = c(4,5),names_to = "kind",values_to = "rate") %>%
  ggplot(aes(date_in_ymd,rate, color = kind)) +
  geom_line() +
  facet_wrap(~kind) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1
  )) +
  labs(x = "",
       title = "Second Input Variables Sets") +
  theme(legend.position = "none")

usd_exchange %>%
  pivot_longer(cols = 6:8,names_to = "kind",values_to = "rate") %>%
  ggplot(aes(date_in_ymd,rate, color = kind)) +
  geom_line() +
  facet_wrap(~kind) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1
  )) +
  labs(x = "",
       title = "Third Input Variables Set") +
  theme(legend.position = "none")

usd_exchange %>%
  pivot_longer(cols = 9:12,names_to = "kind",values_to = "rate") %>%
  ggplot(aes(date_in_ymd,rate, color = kind)) +
  geom_line() +
  facet_wrap(~kind) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1
  )) +
  labs(x = "",
       title = "Fourth Input Variables Set") +
  theme(legend.position = "none")

###########normalizing the dataset


normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

usd_normalized = usd_exchange %>%
  mutate(across(2:12, ~normalize(.x)))

summary(usd_normalized)

set.seed(123)
usd_train <- usd_normalized[1:400,]
usd_test <- usd_normalized[401:491,]



###########unnormalizing the dataset
unnormalize <- function(x, min, max) {
  return( (max - min)*x + min ) }

######### min and max of the training values
usd_min_train <- min(usd_exchange[1:400,2])
usd_max_train <- max(usd_exchange[1:400,2])

########min and max of the testing values
usd_min_test <- min(usd_exchange[401:491,2])
usd_max_test <- max(usd_exchange[401:491,2])



#################getting the stat indices of MAE,MAPE,RMSE

relevant_pred_stat <- function(true_value, predicted_value, model_kind) {
  rbind((tibble(truth = true_value,
                prediction = predicted_value) %>%
           metrics(truth,prediction) %>%
           mutate(type = model_kind)),(tibble(truth = true_value,
                                              prediction = predicted_value) %>%
                                         mape(truth,prediction) %>%
                                         mutate(type = model_kind)))
}



relevant_pred_stat <- function(true_value, predicted_value, model_kind) {
  rbind((tibble(truth = true_value,
                prediction = predicted_value) %>%
           metrics(truth,prediction) %>%
           mutate(type = model_kind)),(tibble(truth = true_value,
                                              prediction = predicted_value) %>%
                                         mape(truth,prediction) %>%
                                         mutate(type = model_kind)))
}





relevant_pred_stat <- function(true_value, predicted_value, model_kind) {
  rbind((tibble(truth = true_value,
                prediction = predicted_value) %>%
           metrics(truth,prediction) %>%
           mutate(type = model_kind)),(tibble(truth = true_value,
                                              prediction = predicted_value) %>%
                                         mape(truth,prediction) %>%
                                         mutate(type = model_kind)))
}



set.seed(12345)
# function setup that creates 2 layer model
model_two_hidden_layers = function(hidden,sec_hidden) {
  nn_model_true = neuralnet(usd_eur ~ day_1_setB+day_2_setB , data=usd_train, hidden=c(
    hidden,sec_hidden), linear.output=TRUE, learningrate = 0.12)
  #plot(nn_model_true)
  
  
  pred <- predict(nn_model_true, usd_test)
  
  validation_df <- data.frame(c(usd_test$date_in_ymd),c(pred),c(usd_test$usd_eur))
  #view(validation_df)
  ###############################
  p = ggplot() + 
    geom_line(data = validation_df, aes(x = c.usd_test.date_in_ymd., y = c.pred.), color = "blue") +
    geom_line(data = validation_df, aes(x = c.usd_test.date_in_ymd., y = c.usd_test.usd_eur.), color = "red") +
    xlab('Dates') +
    ylab('percent.change')
  print(p)
  ############################
  
  
  train_results = compute(nn_model_true,usd_test[,2:3])
  truthcol = usd_exchange[401:491,2]$usd_eur
  predcol = unnormalize(train_results$net.result,usd_min_train, usd_max_train)[,1]
  relevant_pred_stat(truthcol,predcol,
                     "Two Hidden Layers") %>%
    mutate(hidden_layers = paste0(hidden, " and ",sec_hidden),
           input_set = "Set D") %>%
    filter(.metric != "rsq")
}

modelStart.time <- Sys.time()

# creation of different models with varying number of nodes
#results_two_hidden_layers = bind_rows(
  #lapply(1:10, function(n) {
    #bind_rows(
    #  lapply(1:5, function(m) {
      #  bind_rows(
          #lapply(1:5, function(l) {
           # model_two_hidden_layers(n,m,l)
            
          #})
        #)
      #}))})) %>%
  #janitor::clean_names()

results_two_hidden_layers = bind_rows(
  lapply(1:10, function(n) {
  bind_rows(
  lapply(1:5, function(m) {
   model_two_hidden_layers(n,m)
   })
   )
})) %>%
 janitor::clean_names()


modelEnd.time <- Sys.time()
time.taken <- modelEnd.time - modelStart.time
time.taken
# save the stat indices to a dataframe
set_a_models_two_layers = results_two_hidden_layers %>%
  select(-estimator) %>%
  pivot_wider(names_from = metric, values_from = estimate) %>%
  arrange(rmse)
kable(set_a_models_two_layers[1:10,])












