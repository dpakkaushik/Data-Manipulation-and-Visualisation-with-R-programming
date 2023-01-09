#Assignment Retail Case Study

#Attaching Libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(plotly)



#Setting working directory to file's location
setwd("C:/Users/user/Downloads/assignment/R - Credit card case study/R case study 2 (Credit card)")



#Reading data and creating data frames 
DF_Customer_Acqusition <-  read.csv("Customer Acqusition.csv")
DF_Repayment <- read.csv("Repayment.csv")
DF_spend <- read.csv("spend.csv")


#Formatting date column changing it to class date 
DF_Repayment$Month <- as.Date(DF_Repayment$Month ,format = "%d-%b-%y")
DF_spend$Month <- as.Date(DF_spend$Month ,format = "%d-%b-%y")


#SL.No. 1 in repayment dataset is Null thus assigning it value
DF_Repayment$SL.No.[1] <- 1



#Q(1a) In case age is less than 18 replace it with mean age
DF_Customer_Acqusition$Age <- ifelse(DF_Customer_Acqusition$Age < 18,mean(DF_Customer_Acqusition$Age),
                              DF_Customer_Acqusition$Age) 
View(DF_Customer_Acqusition)



#Q(1b) In case spend amount is more than the limit,replace it with 50% of that customers limit.

#Applying inner join on Customer Accusation and Spend data frame
Combined_data <- inner_join(DF_Customer_Acqusition,DF_spend,by=c("Customer"="Customer"))

#Replacing Spent amount where it is bigger than Limit
Combined_data$Amount <- ifelse(Combined_data$Amount >Combined_data$Limit, 0.5*Combined_data$Limit,Combined_data$Amount)
View(Combined_data)




#Q(1c) In case Repayment amount is greater than Limit ,replace it with limit.

#Applying inner join on Customer Accusation and Repayment data frame and replacing repayment amount where it is 
#bigger than Limit 
Combined_data_2 <- inner_join(DF_Customer_Acqusition,DF_Repayment,by=c("Customer"="Customer"))
Combined_data_2$Amount <- ifelse(Combined_data_2$Amount > Combined_data_2$Limit,Combined_data_2$Limit,Combined_data_2$Amount)
View(Combined_data_2)






#Q2(a) How many distinct customers exist?

count(distinct(DF_Customer_Acqusition,Customer))

#Q2(b) How many distinct categories (Type of services) exist?

count(distinct(Combined_data,Type))

#Q2(c) What is the average monthly spent by Customers?

#Mutating Month and Year column using Lubridate
Combined_data <- Combined_data %>% mutate(year_x = year(Month),month_x = month(Month))
Combined_data

#Monthly Average spent Customer wise (Average = Total/Number of intervals)
Monthly_Average_Spent <-  Combined_data %>% select(month_x,Amount) %>% group_by(month_x) %>% 
                          summarise(Avg_Monthly_Spent = mean(Amount))


#Q2(d) What is the average monthly repayment by Customers?

Combined_data_2 <- inner_join(DF_Customer_Acqusition,DF_Repayment,by=c("Customer"="Customer")) %>% 
                   mutate(Month_y = month(Month),Year_y = year(Month))
View(Combined_data_2)

#Monthly Average Repayment Customer wise (Average = Total/Number of intervals)
Monthly_Average_Repayment <- Combined_data_2 %>% select(Month_y,Amount) %>% group_by(Month_y) %>% 
                             summarise(Avg_Monthly_Repayment = mean(Amount))



#Q2(e) If monthly profit is 2.9 percent what is the profit of the bank for each month?

#Joining Monthly data sets
Join_Monthly_data <- inner_join(Monthly_Average_Repayment,Monthly_Average_Spent, by = c("Month_y" = "month_x") )
#Adding profit column to monthly data set as Profit
Join_Monthly_data$Profit <- (Monthly_Average_Repayment$Avg_Monthly_Repayment -  Monthly_Average_Spent$Avg_Monthly_Spent)* 0.026

View(Join_Monthly_data)



#Q2(f) What are the top 5 product types?

Combined_data %>% select( Type,Amount) %>% group_by(Type) %>% summarise(Product_Sum = sum(Amount)) %>% 
                  arrange(desc(Product_Sum)) %>% head(5)



#Q2(g) Which city having maximum spent?

Combined_data %>% select(City,Amount) %>% group_by(City) %>% summarise(City_Wise_Sum = sum(Amount)) %>%
                  arrange(desc(City_Wise_Sum)) %>% head(1)


#Q2(h) Which age group spend more money?

Combined_data %>% select(Age ,Amount) %>% group_by(Age) %>% summarise(Age_Wise_Sum = sum(Amount)) %>%
  arrange(desc(Age_Wise_Sum)) %>% head(1)



#Q2(i) Top 10 customers in terms of repayment ?
Combined_data_2 %>% select(Customer,Amount) %>% group_by(Customer) %>% summarise(Customer_Repayment = sum(Amount)) %>%
                    arrange(desc(Customer_Repayment)) %>% head(10)




#Q3 Calculate city wise spend on each product on yearly basis, also provide graphical representation for the same.

Grouped_data <- Combined_data  %>% group_by(City,Product,year_x)%>% summarise(Total_Amount = sum(Amount))
View(Grouped_data)


#PLOT_1
#Plotting City wise yearly data and Assigning it to variable
yearly_plot <- ggplot(Grouped_data,aes(x = City,y = Total_Amount,fill = Product)) + geom_bar(stat = "identity")+
              facet_grid(Product~year_x)+ theme_dark()+ scale_fill_manual(values = c('blue','yellow','red')) + 
              labs(x = "Cities", y = "Sales",title = "City Wise Spend on product on yearly basis") 

#Scaling of Y Axis
yearly_plot <- yearly_plot + scale_y_continuous(breaks = c(2000000, 4000000,6000000,8000000,10000000,12000000,14000000),
            labels = c("2MN", "4MN","6MN","8MN","10MN","12MN","14MN"))

#View final plot
yearly_plot



#PLOT_Type2 (Dodge Type)
#Plotting City wise yearly Dodge bar plot and Assigning it to variable
yearly_plot_1 <- ggplot(Grouped_data,aes(x = City,y = Total_Amount,fill = Product)) + geom_bar(stat = "identity",
                 position = "dodge") + facet_grid(year_x~.)+ theme_bw() + labs(x = "Cities", y = "Spent Amount" ,
                 title = "City Wise Yearly Spend") + scale_fill_manual(values = c('blue','yellow','#778899')) +
                 theme_wsj() + theme(legend.position = "bottom")

#Scaling of Y Axis
yearly_plot_1 <- yearly_plot_1 + scale_y_continuous(breaks = c(2000000, 4000000,6000000,8000000,10000000,12000000,120000000),
                                                labels = c("2MN", "4MN","6MN","8MN","10MN","12MN","x"))

#View final plot
ggplotly(yearly_plot_1)





#Q4(a) Monthly comparison of total spent city wise?

#Preparing monthly spent dataframe
Grouped_data_1 <- Combined_data %>% select(Amount,month_x,City) %>% group_by(City,month_x) %>% 
                  summarise(Total_Amount = sum(Amount))

View(Grouped_data_1)

#Plotting
Monthly_Plot <- ggplot(Grouped_data_1,aes(x = month_x ,y = Total_Amount)) + geom_bar(stat = "identity", aes(fill = factor(City))) +
                  facet_grid(City~.) + labs(x = "Months",y = "Spent Amount",title = "City Wise Monthly Spent") + theme_grey()
    
#Scaling axes
Monthly_Plot <- Monthly_Plot + scale_y_continuous(breaks = c(3000000, 6000000,9000000,12000000,15000000,18000000),
                labels = c("3MN", "6MN","9MN","12MN","15MN","18MN")) + scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                labels = c("Jan","Feb","Mar","Apr","May","June","July","Aug","Sep","Oct","Nov","Dec"))
#View final plot
ggplotly(Monthly_Plot)



#Q4(b) Compare yearly spent on Air ticket year wise

#Preparing monthly spent dataframe
Grouped_data_2 <- Combined_data %>% select(Amount,year_x ,Type) %>% filter(Type == "AIR TICKET") %>%
                  group_by(year_x) %>% summarise(Total_Amount = sum(Amount))

#Plotting
Airticket_Plot <- ggplot(Grouped_data_2, aes(x = year_x, y = Total_Amount)) + geom_bar(stat = "identity",
                  aes(fill = factor(year_x))) + labs(x = "YEAR", y = "Spend On Tickets", title = "Yearly Comparison") + 
                  guides(fill=guide_legend(title="Years"))

#Scaling Axes
Airticket_Plot <- Airticket_Plot + scale_y_continuous(breaks = c(3000000, 6000000,9000000,12000000,15000000,18000000),
                  labels = c("3MN", "6MN","9MN","12MN","15MN","18MN"))
 
#View final plot
ggplotly(Airticket_Plot)





#Q4(c) Comparison for monthly spent on each product.

#Prepairing Data for plot
Grouped_data_3 <- Combined_data %>% select(Amount,month_x,Type) %>% group_by(Type, month_x) %>% 
                  summarise(Total_Amount = sum(Amount))

#Plotting
Monthly_plot_2 <- ggplot(Grouped_data_3,aes(x = month_x,y = Total_Amount)) + geom_bar(stat = "identity",
                  aes(fill = factor(Type))) + labs(x = "Months", y = "Spent Amount", title = "Comparison for monthly spent on each product") +
                  guides(fill=guide_legend(title="Products")) + facet_grid(Type~.)

#Scaling Axes
Monthly_plot_2 <- Monthly_plot_2 + scale_y_continuous(breaks = c(3000000, 6000000,9000000,12000000,15000000,18000000),
                  labels = c("3MN", "6MN","9MN","12MN","15MN","18MN")) + scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                  labels = c("Jan","Feb","Mar","Apr","May","June","July","Aug","Sep","Oct","Nov","Dec"))

#View final plot
ggplotly(Monthly_plot_2)





#Q5 Write a user defined function.

#Creating a Function two variables(Product, grouping variable- Time Period)
Top_Customer <- function(Product_x,Time_Period){
  
  #Defining condition to choose grouping Yearly or monthly as per input
   if (Time_Period == "yearly"){
    group_var <- quo(Year_y)
  } else if (Time_Period == "monthly"){
    group_var <- quo(Month_y)
  }
  #Query to extract required data
   c = Combined_data_2 %>% select(Customer,City,Product,Year_y,Month_y,Amount) %>% 
    filter(Product == Product_x ) %>% group_by(Customer,!!group_var) %>%
    summarise(Amount_Spent = sum(Amount)) %>% arrange(desc(Amount_Spent)) %>% head(10)
    return(c)
}

#Entered Product as Gold and grouping variable as yearly
View(Top_Customer("Gold","yearly"))

#Entered product as Gold and grouping variable as monthly
View(Top_Customer("Gold","monthly"))





