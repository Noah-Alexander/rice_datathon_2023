library(tidyverse)
library(usmap)

#visualizations for presentation

df <- read_csv("chevron_data_v2.csv")

df <- df |> mutate(state = factor(State),
                    TotalAmountofAssistance = (TotalAmountofAssistance))

investment <- df |> group_by(state) |> 
  summarize(Total_Invested = sum(TotalAmountofAssistance),
            mean_invested = mean(TotalAmountofAssistance),
            avg_investment = Total_Invested/sum(TotalNumberofInvestments),
            re_per_prod = sum(REPRB)/sum(TEPRB))


plot_usmap(data=investment, values = "Total_Invested", regions = "states") + 
  labs(title = "Heatmap of Total Federal Investments in Renewable Energies",
       subtitle = "2015-2020, 1000's of dollars") + 
  theme(panel.background=element_blank()) #this map

plot_usmap(data=investment, values = "mean_invested", regions = "states") + 
  labs(title = "Heatmap of Average Federal Investments in Renewable Energies",
       subtitle = "This is a blank map of the United States.", 
       ) + 
  theme(panel.background=element_blank())

plot_usmap(data=investment, values = "avg_investment", regions = "states") + 
  labs(title = "U.S. States",
       subtitle = "This is a blank map of the United States.") + 
  theme(panel.background=element_blank())

plot_usmap(data=investment, values = "re_per_prod", regions = "states") + 
  labs(title = "Percent of Renewable Energy Produced",
       subtitle = "2015-2020") + 
  theme(panel.background=element_blank()) #this map

t1 <- df[df["Year"]==2015,] |> select(state, REPRB, TEPRB)
t1 <- t1 |> mutate(re_per_prod = REPRB/TEPRB)
t2 <- df[df["Year"]==2020,] |> select(state, REPRB, TEPRB)
t2 <- t2 |> mutate(re_per_prod = REPRB/TEPRB)
t3 <- t2 |> mutate(Change_in_Production = re_per_prod - t1$re_per_prod)

plot_usmap(data=t3, values = "Change_in_Production", regions = "states") + 
  labs(title = "Change in Percent of Renewable Energy Produced from 2015 to 2020") + 
  theme(panel.background=element_blank()) #this map
