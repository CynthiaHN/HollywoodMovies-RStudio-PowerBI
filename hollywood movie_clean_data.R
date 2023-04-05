#import dat and install package 
df <- read.csv('https://public.tableau.com/app/sample-data/HollywoodsMostProfitableStories.csv')
view(df)
install.packages('tidyverse')
library(tidyverse)
str(df)

#clean data
colSums(is.na(df))
df <- na.omit(df)
colSums(is.na(df))
dim(df[duplicated(df$Film,)])[1]
df$Profitability <- round(df$Profitability, digit=2)
dim(df)

#box plot
ggplot(df,aes(x=Profitability, y=Worldwide.Gross))+
  geom_boxplot(outlier.colour='red', outlier.shape = 1)+
  scale_x_continuous(label=scales::comma)+
  coord_cartesian(ylim= c(0,1000))
Q1 <- quantile(df$Profitability, .25)
Q3 <- quantile(df$Profitability, .75)
IQR <- IQR(df$Profitability)
no_outliers <- subset(df,df$Profitability> (Q1-1.5*IQR) & df$Profitability< (Q3+1.5*IQR))
dim(no_outliers)

Q1<- quantile(no_outliers$Worldwide.Gross, .25)
Q3<- quantile(no_outliers$Worldwide.Gross, .75)
IQR <- IQR(no_outliers$Worldwide.Gross)
df1 <- subset(no_outliers,no_outliers$Worldwide.Gross> (Q1-1.5*IQR) & no_outliers$Worldwide.Gross< (Q3+1.5*IQR))
dim(df1)

#scatter plot
ggplot(df1,aes(x=Lead.Studio, y=Rotten.Tomatoes..))+
  geom_point()+
  scale_y_continuous(label=scales::comma)+
  coord_cartesian(ylim= c(0,110))+
  theme(axis.text= element_text(angle= 90))

#bar chart
ggplot(df1, aes(x=Year))+ geom_bar()

write.csv(df1, 'clean_df,csv')

  
