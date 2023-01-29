library(tidyverse)
library(usmap)
library(ggcorrplot)

df <- read_csv("chevron_data_v2.csv")

df3 <- df |> mutate(state = factor(State),
                    TotalAmountofAssistance = (TotalAmountofAssistance*1000),)
df4 <- df3 |> select(!c(State,Year,GDP,Population,pop_density))

corr <- round(cor(df4), 1)
ggcorrplot(corr)
#there is no high correlating terms in the dataset with the wanted output

ggplot(data=df, mapping = aes(x=GDP/Population, y=(TotalAmountofAssistance), color = Year)) +
  geom_point()

ggplot(data=df, mapping = aes(x=CO2_Emissions, y=(TotalAmountofAssistance), color = Year)) +
  geom_point()

ggplot(data=df, mapping = aes(x=TotalNumberofInvestments, y=(TotalAmountofAssistance), color = Year)) +
  geom_point()

ggplot(data=df, mapping = aes(x=Population/TEPRB, y=(TotalAmountofAssistance))) +
  geom_point() #+ facet_wrap(~Year) #lower people get more funding

ggplot(data=df, mapping = aes(x=Population/TETCB, y=log(TotalAmountofAssistance))) +
  geom_point() + facet_wrap(~Year) #not a lot going on here

ggplot(data=df, mapping = aes(x=(Population/CO2_Emissions), y=(TotalAmountofAssistance))) +
  geom_point()

ggplot(data=df, mapping = aes(x=(BDFDB+BDPRP+BFFDB+BFPRP+EMFDB+ENPRP), y=(TotalAmountofAssistance), color = Year)) +
  geom_point() + facet_wrap(~Year) #all the biomass things

ggplot(data=df, mapping = aes(x=(GETCB+HYTCB+NUETB+SOTCB+WYTCB)/TEPRB, y=log(TotalAmountofAssistance), color = Year)) +
  geom_point() #+ facet_wrap(~Year)

ggplot(data=df3, mapping = aes(x=TEPRB/TotalNumberofInvestments, y=TotalAmountofAssistance, color = Year)) +
  geom_point() #1/x kinda relationship

ggplot(data=df, mapping = aes(x=BDPRP, y=log(TotalAmountofAssistance), color = Year)) +
  geom_point()

ggplot(data=df, mapping = aes(x=BFFDB, y=log(TotalAmountofAssistance), color = Year)) +
  geom_point()

ggplot(data=df, mapping = aes(x=BFPRP, y=log(TotalAmountofAssistance), color = Year)) +
  geom_point()

ggplot(data=df, mapping = aes(x=Year, y=log(TotalAmountofAssistance))) +
  geom_point()

investment <- df3 |> group_by(state) |> 
  summarize(total_invested = sum(TotalAmountofAssistance))

plot_usmap(data=investment, values = "total_invested", regions = "states") + 
  labs(title = "U.S. States",
       subtitle = "This is a blank map of the United States.") + 
  theme(panel.background=element_blank())

plot_usmap(data=df3, values = "num_unexp_ind", regions = "states") + 
  labs(title = "U.S. States",
       subtitle = "This is a blank map of the United States.") + 
  theme(panel.background=element_blank())

ggplot(data=df3, mapping = aes(x=num_unexp_ind, y=TotalAmountofAssistance)) + 
  geom_point()

