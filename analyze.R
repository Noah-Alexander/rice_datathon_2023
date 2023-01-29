library(tidyverse)
library(ggcorrplot)

df <- read_csv("chevron_data_v2.csv")

df2 <- df |> mutate(State = factor(State), 
                    TotalAmountofAssistance = log(TotalAmountofAssistance)) |> 
  select(!c(Population, GDP, pop_density))

t <- df2["Year"] != 2020

df2 <- df2 |> select(!Year)

train <- df2[t>0,]
test <- df2[!(t>0),]

lm = lm(TotalAmountofAssistance~., data = train)
summary(lm)

yhat = predict.glm(lm, test, type = "response")
rmse1 = sqrt(sum(((exp(yhat))-(exp(test$TotalAmountofAssistance)))^2)/50)
#test_rmse = 32000

#so basically, we need to do some extra testing to try and find some other features

#knn- then use cluster as variable in model
library(class)
set.seed(128)

df2 <- df |> select(!c(State,TotalAmountofAssistance, GDP, Population, pop_density, Year))

df2 <- scale(df2)

clusters <- stats::kmeans(df2, centers = 5)
clusters

df_clustered <- df |> mutate(cluster_id = factor(clusters$cluster))

#now we want to see if this helps visualize the data

boxplot(log(TotalAmountofAssistance)~ cluster_id, data=df_clustered,
  xlab="Cluster Id Assigned",
  ylab="Federal Aid Given",
  title="Federal Aid Compared to Clusters from K Nearest Neighbors")

ggplot(data=df_clustered, mapping = aes(x=CO2_Emissions, y=log(TotalAmountofAssistance), color = cluster_id)) +
  geom_point()#this shows there is a good split

df_cluster1 <- df_clustered[(df_clustered["cluster_id"] == 1)>0,]
corr1 <- round(cor(df_cluster1[-33]), 1)
ggcorrplot(corr1) #weak

df_cluster2 <- df_clustered[(df_clustered["cluster_id"] == 2)>0,]
corr2 <- round(cor(df_cluster2[-33]), 1)
ggcorrplot(corr2) #medium

df_cluster3 <- df_clustered[(df_clustered["cluster_id"] == 3)>0,]
corr3 <- round(cor(df_cluster3[-33]), 1)
ggcorrplot(corr3) #medium

df_cluster4 <- df_clustered[(df_clustered["cluster_id"] == 3)>0,]
corr4 <- round(cor(df_cluster4[-33]), 1)
ggcorrplot(corr4) #medium

#do lm with clusters
df_c = read_csv("chevron_data_v2.csv")
df_c <- df_c |> mutate(State = factor(State),
                       Year = factor(Year),
                       cluster_id = factor(clusters$cluster),
                       TotalAmountofAssistance = log(TotalAmountofAssistance)) |> 
  select(!c(GDP, Population, pop_density))
t <- df_c["Year"] != 2020
df_c <- df_c |> select(!Year)

train <- df_c[t>0,]
test <- df_c[!(t>0),]

lm2 = lm(TotalAmountofAssistance~., data = train)
summary(lm2)
yhat = predict.glm(lm2, test, type = "response")
rmse2 = sqrt(sum(((yhat)-(test$TotalAmountofAssistance))^2)/50)
#48220 rmse, still bad


#trying a neural network

df = read_csv("Investment_Data_Train_transformed.csv")
df_2 <- df |> mutate(State = factor(State))
t <- df_2["Year"] != 2019
df_2 <- df_2 |> select(!Year)

train <- df_2[t>0,]
test <- df_2[!(t>0),]

library(neuralnet)
library(NeuralNetTools)
library(nnet)

model = nnet(TotalAmountofAssistance ~ ., 
             data = train, 
             size = c(20, 10, 5), 
             rang = 0.001, 
             decay = 0.001, 
             maxit = 500,
             MaxNWts = 2000)
print(model)