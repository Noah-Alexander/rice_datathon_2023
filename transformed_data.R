library(tidyverse)

#transforming of data, data validation was done in terminal

df = read_csv("Investment_Data_Train.csv")

df2 <- df[!(rowSums(is.na(df)) > 0),]

df3 <- df2 |> 
  select(MSN, State, Year, Amount, CO2_Emissions, 
         TotalNumberofInvestments, TotalAmountofAssistance) |>
  mutate(TotalAmountofAssistance = TotalAmountofAssistance/1000,
         MSN = factor(MSN), State = factor(State), Year = factor(Year))

#write.csv(df3, "Investment_Data_Train_clean.csv")

df4 <- df3 |> 
  pivot_wider(names_from = MSN,
              values_from = Amount)

write.csv(df4, "Investment_Data_Train_transformed.csv")

#transform the 2020 data

data_2020 = read_csv("Investment_Data_2020.csv")

df2 <- data_2020[!(rowSums(is.na(data_2020)) > 0),]

df3 <- df2 |> 
  select(MSN, State, Year, Amount, CO2_Emissions, 
         TotalNumberofInvestments, TotalAmountofAssistance) |>
  mutate(TotalAmountofAssistance = TotalAmountofAssistance/1000,
         MSN = factor(MSN), State = factor(State), Year = factor(Year))

#write.csv(df3, "Investment_Data_Train_clean.csv")

df4 <- df3 |> 
  pivot_wider(names_from = MSN,
              values_from = Amount)

write.csv(df4, "Investment_Data_2020_transformed.csv")

#merged in excel by hand