##BUSINESS PROBLEM: 
##  In order to effectively produce quality decisions in the modern credit card 
##industry, knowledge must be gained through effective data analysis and 
##modeling. Through the use of dynamic data-driven decision-making tools and 
##procedures, information can be gathered to successfully evaluate all aspects of 
##credit card operations. 
##PSPD Bank has banking operations in more than 50 countries across the globe. 
##Mr. Jim Watson, CEO, wants to evaluate areas of bankruptcy, fraud, and 
##collections, respond to customer requests for help with proactive offers and 
##service. 
##DATA AVAILABLE - 
##This book has the following sheets: 
##Customer Acquisition: At the time of card issuing, company maintains 
##the details of customers. 
##??? Spend (Transaction data): Credit card spend for each customer 
##Repayment: Credit card Payment done by customer 
##Following are some of Watson's questions to a Consultant (like you) to 
##understand the customers spend & repayment behavior. 


### 1. In the above dataset, 
##a. Incase age is less than 18, replace it with mean of age values. 
##b. Incase spend amount is more than the limit, replace it with 50% of that 
##customer's limit. (customer's limit provided in acquisition table is the per 
##                   transaction limit on his card) 
##


require(dplyr)
require(lubridate)
require(ggplot2)

Customer_Acqusition <- read.csv("Customer Acqusition.csv")
Customer_Acqusition$Age
spend <- read.csv("spend.csv")
Repayment <- read.csv("Repayment.csv")

head(Customer_Acqusition)
head(spend)
head(Repayment)

###THERE IS AN EMPTY COLUMN IN REPAYMENT DATA
###REMOVE EMPTY DATA FROM THE COLUMN 
Repayment$X <- NULL


##Check for NA VALUE

sum(is.na(Customer_Acqusition))
sum(is.na(spend))
sum(is.na(Repayment))

summary(Repayment)









##1. In the above dataset,

##a. Incase age is less than 18, replace it with mean of age valuE

Age <- ifelse(Customer_Acqusition$Age <18, round(mean(Customer_Acqusition$Age, na.rm = TRUE),0), Customer_Acqusition$Age )
Customer_Acqusition$Age <- Age

##All the customers with age less than 18 have been replaced by mean of the age.

##b. Incase spend amount is more than the limit, replace it with 50% of that 

CUSTOMER_SPENT <- dplyr::left_join(Customer_Acqusition, spend, by = "Customer")  

sum(is.na(CUSTOMER_SPENT))

Amount <- ifelse(CUSTOMER_SPENT$Amount > CUSTOMER_SPENT$Limit, CUSTOMER_SPENT$Limit*0.50, CUSTOMER_SPENT$Amount  )
CUSTOMER_SPENT$Amount <- Amount

##c. Incase the repayment amount is more than the limit, replace the 
## repayment with the limit


CUSTOMER_Repayment <- dplyr::left_join(Customer_Acqusition, Repayment, by = "Customer")  

sum(is.na(CUSTOMER_Repayment))

##NA IS PRESENT IN USELESS COLUMN. THUS THERE IS NO NEED TO REMOVE THAT


Amount <- ifelse(CUSTOMER_Repayment$Amount > CUSTOMER_Repayment$Limit, CUSTOMER_Repayment$Limit, CUSTOMER_Repayment$Amount)
CUSTOMER_Repayment$Amount <- Amount


###2. From the above dataset create the following summaries: 


##  a. How many distinct customers exist? 
  
DISTINCT_CUSTOMER <- dplyr::distinct(Customer_Acqusition, Customer)

  count(DISTINCT_CUSTOMER)
  
###b. How many distinct categories exist? 
  
  Distinct_Categories <- Customer_Acqusition %>% dplyr::group_by(Segment) %>% dplyr::summarise(Co_uNT = n() )
  
  head(Distinct_Categories)
  
### c. What is the average monthly spend by customers? 
  
  ##CUSTOMER SPENT DATA WILL BE USED HERE 
  
  
CUSTOMER_SPENT$Month <-  lubridate::dmy(CUSTOMER_SPENT$Month)

CUSTOMER_SPENT$YEAR <- lubridate::year(CUSTOMER_SPENT$Month)

CUSTOMER_SPENT$MON_TH <- lubridate::month(CUSTOMER_SPENT$Month, label = TRUE)

Average_monthly_Spent <- CUSTOMER_SPENT %>% dplyr::group_by(YEAR,MON_TH) %>% dplyr::summarise(Average_Monthly_Spent = mean(Amount))


View(Average_monthly_Spent)
  
  
  
  
### d. What is the average monthly repayment by customers?


##CUSTOMER SPENT DATA WILL BE USED HERE 


CUSTOMER_Repayment$Month <-  lubridate::dmy(CUSTOMER_Repayment$Month)

CUSTOMER_Repayment$YEAR <- lubridate::year(CUSTOMER_Repayment$Month)

CUSTOMER_Repayment$MON_TH <- lubridate::month(CUSTOMER_Repayment$Month, label = TRUE)

Average_monthly_Repayment <- CUSTOMER_Repayment %>% dplyr::group_by(YEAR,MON_TH) %>% dplyr::summarise(Average_Monthly_Repayment = mean(Amount))

View(Average_monthly_Repayment)








### e. If the monthly rate of interest is 2.9%, what is the profit for the bank for 
###each month? (Profit is defined as interest earned on Monthly Profit. 


             ### Monthly Profit = Monthly repayment - Monthly spend. Interest is 
             ### earned only on positive profits and not on negative amounts) 


monthly_Spent <- CUSTOMER_SPENT %>% dplyr::group_by(YEAR,MON_TH) %>% dplyr::summarise(Average_Monthly = sum(Amount))

monthly_Repayment <- CUSTOMER_Repayment %>% dplyr::group_by(YEAR,MON_TH) %>% dplyr::summarise(Average_Monthly = sum(Amount))

For_Profit <- cbind(monthly_Spent, monthly_Repayment)

For_Profit <- dplyr::rename(For_Profit, "SPENT" = "Average_Monthly...3", "REPAY" = "Average_Monthly...6")
For_Profit <- dplyr::rename(For_Profit, "YEAR" = "YEAR...1", "MONTH" = "MON_TH...2")

##DROP USELESS COLUMN

For_Profit$YEAR...4 <- NULL
For_Profit$MON_TH...5 <- NULL
 
For_Profit$PROFIT <- For_Profit$REPAY- For_Profit$SPENT

PROFIT_PLUS <- For_Profit[For_Profit$PROFIT > 0, ]



PROFIT_PLUS$Profit_AMOUNT <- (PROFIT_PLUS$PROFIT*2.9)/100

View(PROFIT_PLUS)





### f. What are the top 5 product types? 
  
  
  TOP_5_PRODUCT <- spend %>% dplyr::group_by(Type) %>% dplyr::summarise(COU_NT = n()) %>% dplyr::arrange(desc(COU_NT))
  
Top_5_Type  <- head(TOP_5_PRODUCT,5)
  
View(Top_5_Type)
  
  
  
### g. Which city is having maximum spend? 
    
    ###CUSTOMER SPENT DATA WILL BE USED HERE 



Maximum_Spend <- CUSTOMER_SPENT %>% dplyr::group_by(City) %>% dplyr::summarise(Total_Spend = 
                                                sum(Amount), Percentage = 
           round(sum(Amount)*100/sum(CUSTOMER_SPENT$Amount),2) ) %>% dplyr::arrange(desc(Total_Spend))
    
  View(Maximum_Spend)
    
    
###h. Which age group is spending more money? 
  
  CUSTOMER_SPENT$AGE_GROUP <-   ifelse(CUSTOMER_SPENT$Age <36, "YOUNG",
         ifelse(CUSTOMER_SPENT$Age <54, "MID_AGE", 
                ifelse(CUSTOMER_SPENT$Age <72, "MATURE", "OLD")))
  
  
  MOST_SPENT <- CUSTOMER_SPENT %>% dplyr::group_by(AGE_GROUP) %>% dplyr::summarise(Total_Spent = 
                              sum(Amount), Percentage = 
        round(sum(Amount)*100/sum(CUSTOMER_SPENT$Amount),2) ) %>% dplyr::arrange(desc(Percentage))
  
  
  View(MOST_SPENT)
  
  
### i. Who are the top 10 customers in terms of repayment?
    
    Top_Customers <- CUSTOMER_Repayment %>% dplyr::group_by(Customer) %>% dplyr::summarise(Total_Repayment = 
                                                         sum(Amount)) %>% dplyr::arrange(desc(Total_Repayment))


  Top_10_Customers <- head(Top_Customers,10)
View(Top_10_Customers)



###3. Calculate the city wise spend on each product on yearly basis. Also include a 
### graphical representation for the same. 


YEARLY_CITY_PRODUCT_SPEND <-  CUSTOMER_SPENT %>% dplyr::group_by(YEAR,City,Product) %>%  dplyr::summarise(Total_Spend = sum(Amount))
View(CITY_PRODUCT_SPEND)
ggplot2::ggplot(data = YEARLY_CITY_PRODUCT_SPEND) + aes(x= City, y = Total_Spend, fill = Product ) +
  geom_bar(stat = "identity", position = "dodge") + facet_grid(.~YEAR)



###4. Create graphs for 

### a. Monthly comparison of total spends, city wise 


CITY_WISE_spend <- CUSTOMER_SPENT %>% dplyr::group_by(City,MON_TH) %>% dplyr::summarise(TOTAL_SPEND =
                                                                  sum(Amount))
 View(CITY_WISE_spend)
 
ggplot2::ggplot(data = CITY_WISE_spend) + aes(x= City, y = TOTAL_SPEND, fill = MON_TH ) +
  geom_bar(stat = "identity", position = "dodge") 



### b. Comparison of yearly spend on air tickets 

Air_Ticket_Data<-    CUSTOMER_SPENT[CUSTOMER_SPENT$Type == "AIR TICKET",]

rownames(Air_Ticket_Data) <- NULL

 

YEARLY_AIR_TICKET <- Air_Ticket_Data %>% dplyr::group_by(YEAR) %>% dplyr::summarise(TOTAL_SPEND = sum(Amount))

ggplot2::ggplot(data = YEARLY_AIR_TICKET) + aes(x= YEAR, y = TOTAL_SPEND ) +
  geom_bar(stat = "identity", position = "dodge")




### c. Comparison of monthly spend for each product (look for any seasonality 
                                              ##   that exists in terms of spend) 


MONTHLY_SPEND <- CUSTOMER_SPENT %>% dplyr::group_by(MON_TH,Product) %>% dplyr::summarise(ToTAL_SPEND =
                                        sum(Amount))

ggplot2::ggplot(data = MONTHLY_SPEND) + aes(x= MON_TH, y = ToTAL_SPEND, fill = Product ) +
  geom_bar(stat = "identity", position = "dodge")


####  Spend is Decreasing in last Months as Compared to Starting Months


##5. Write user defined R function to perform the following analysis: 
  ##You need to find top 10 customers for each city in terms of their repayment 
##amount by different products and by different time periods i.e. year or 
##month. The user should be able to specify the product 
##(Gold/Silver/Platinum) and time period (yearly or monthly) and the function 
##should automatically take these inputs while identifying the top 10 
##customers.



UDF_TOP10 <- function(DA_TA,YE_AR,Prod) { 
P_DATA =   DA_TA[(DA_TA$YEAR == YE_AR) & (DA_TA$Product == Prod),]
require(dplyr)
 Pre_Result = P_DATA %>% dplyr::group_by(Customer) %>% dplyr::summarise(TOTALS_PEND = sum(Amount)) %>% dplyr::arrange(desc(TOTALS_PEND))
  Result = head(Pre_Result,10)
  print(Result) 
  
  }


## USe Character Data For Product ( Eg - "Gold", "Silver", "Platinum")

 UDF_TOP10(CUSTOMER_Repayment, 2004,"Gold")
 UDF_TOP10(CUSTOMER_Repayment, 2004, "Silver")
 UDF_TOP10(CUSTOMER_Repayment, 2004, "Platinum")
