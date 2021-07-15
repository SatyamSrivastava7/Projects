
##############
  'Project:- Retail Analysis Case Study'
  'Name:- Satyam Srivastava'
  'Institute- NIT Jamshedpur'
  'Submission Date:- 05-05-2021'
#############
    
    
library(dplyr)
library(ggplot2)

getwd()
path = "F:/Data_Science_Internship/Retail Analysis"
setwd(path)

customer <- read.csv("Customer.csv", header= TRUE, stringsAsFactors = FALSE, na.strings = c("","NA"))
product <- read.csv("prod_cat_info.csv", header= TRUE, stringsAsFactors = FALSE, na.strings = c("","NA"))
transaction <- read.csv("Transactions.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = c("","NA"))

View(customer)
View(product)
View(transaction)

c1= merge(customer, transaction, by.x = 'customer_Id', by.y = "cust_id")
View(c1)



#1. Merge the datasets Customers, Product Hierarchy and Transactions as Customer_Final.

Customer_Final= merge(c1, product, by.x = "prod_cat_code", by.y = "prod_cat_code")       #Merging Datasets
View(Customer_Final)



#2. Prepare a summary report for the merged data set.

names(Customer_Final)
str(Customer_Final)
head(Customer_Final, 5)
summary(Customer_Final)

table(Customer_Final$prod_cat_code)
table(Customer_Final$Gender)
table(Customer_Final$Store_type)
table(Customer_Final$prod_cat)




#3. Generate histograms for all continuous variables and frequency bars for categorical variables.

ggplot(Customer_Final, aes(x = Tax)) + geom_histogram(color = "red", linetype = "dashed",fill = "black")
ggplot(Customer_Final, aes(x = total_amt)) + geom_histogram(color = "red", linetype = "dashed",fill = "blue")

ggplot(Customer_Final, aes(x = factor(Gender))) + geom_bar(fill = "coral") + theme_classic()
ggplot(Customer_Final, aes(x = factor(prod_cat))) + geom_bar(fill = "coral") + theme_classic()
ggplot(Customer_Final, aes(x = factor(Store_type))) + geom_bar(fill = "coral") + theme_classic()
ggplot(Customer_Final, aes(x = factor(prod_subcat))) + geom_bar(fill = "coral") + theme_classic()





#4. Calculate the following information using the merged dataset :

#a. Time period of the available transaction data

df2= na.omit(as.Date(Customer_Final$tran_date, format = "%d-%m-%Y"))
time_period= max(df2) - min(df2)                                             #Gives the time period of the transaction data


#b. Count of transactions where the total amount of transaction was negative

Amt_Trn= count(Customer_Final, total_amt<0, sort= TRUE)                      #The true count mentions the total negative transactions





#5. Analyze which product categories are more popular among females vs male customers.

M_df= filter(Customer_Final, Gender== 'M')           #Male customers
ggplot(M_df, aes(x= factor(prod_cat))) + geom_bar(fill= "coral") + theme_classic()  #Male customers having popular product category 

F_df= filter(Customer_Final, Gender== 'F')           #Female customers
ggplot(F_df, aes(x= factor(prod_cat))) + geom_bar(fill= "coral") + theme_classic()  #Female customers having popular product category 





#6. Which City code has the maximum customers and what was the percentage of customers from that city?


  p= group_by(Customer_Final, city_code)
  max_count= max(na.omit(count(p, sort= TRUE)))
  tot_count= sum(na.omit(Customer_Final$city_code))
  print(max_count*100/tot_count)                   #Signifies the percentage of customers from the city having maximum customers
  
  

  
  
#7. Which store type sells the maximum products by value and by quantity?

ggplot(Customer_Final, aes(x= Store_type)) + geom_bar(fill= "coral") + theme_dark()  #by quantity

#----- Calculating the store type sells the maximum products by value-------

unique(Customer_Final$Store_type)

q= filter(Customer_Final, Store_type== "e-Shop")
s=sum(q$total_amt)

q1= filter(Customer_Final, Store_type== "Flagship store")
s1=sum(q1$total_amt)

q2= filter(Customer_Final, Store_type== "MBR")
s2=sum(q2$total_amt)

q3= filter(Customer_Final, Store_type== "TeleShop")
s3=sum(q3$total_amt)

name=c("e-Shop", "Flagship store", "MBR", "Teleshop")
sales=c(s, s1, s2, s3)
df= data.frame(name, sales)

df[which.max(df$sales),]$name                    #Signifies the name of Store having Maximum sells






#8. What was the total amount earned from the "Electronics" and "Clothing"categories from Flagship Stores?
  
df1=filter(Customer_Final, Store_type == "Flagship store")

T_amt_Ele= filter(df1, prod_cat== "Electronics")
sum(T_amt_Ele$total_amt)                   #Total amount earned from "Electronics"

T_amt_Clo= filter(df1, prod_cat== "Clothing")
sum(T_amt_Clo$total_amt)                   #Total amount earned from "Clothing"





#9. What was the total amount earned from "Male" customers under the"Electronics" category?

Ele_cat= filter(Customer_Final, prod_cat== "Electronics")

T_amt_Male= filter(Ele_cat, Gender== "M")
sum(T_amt_Male$total_amt)                  # Total amount earned from Male customers under Electronics category






#10. How many customers have more than 10 unique transactions, after removing all transactions which have any negative amounts?

P_Tran= filter(Customer_Final, total_amt>0)
p1= group_by(P_Tran, customer_Id)

c= data.frame(count(p1))
No_cust= filter(c, n>10)
arrange(No_cust, desc(n))
count(No_cust)                         #Prints the no. of customers having more than 10 unique transactions having positive amounts.






#11. For all customers aged between 25 - 35, find out:

d10= na.omit(as.Date(Customer_Final$DOB, format = "%d-%m-%Y"))
max(d10)
num_day= as.Date("2021-05-05") - d10
age1= as.integer(num_day/365)                        #Calculating age of all the customers
cust_1= mutate(Customer_Final, age1)                         #Mutating it to the Customer_Final dataset
cust_1
age_25_35 = filter(cust_1, age1>=25 & age1<=35)       #Dataset of Customers having age between 25 and 35

#a. What was the total amount spent for "Electronics" and "Books"product categories?

ele_amt= filter(age_25_35, prod_cat== "Electronics")
sum(ele_amt$total_amt)

ele_book= filter(age_25_35, prod_cat== "Books")
sum(ele_book$total_amt)

#b. What was the total amount spent by these customers between 1st Jan, 2014 to 1st Mar, 2014

library(lubridate)

ap= data.frame(tr_dt= c(age_25_35$tran_date))

mdy <- mdy(ap$tr_dt) 
dmy <- dmy(ap$tr_dt) 
mdy[is.na(mdy)] <- dmy[is.na(mdy)] # some dates are ambiguous, here we give 
ap$tr_dt <- mdy
age_25_35$tran_date = ap$tr_dt
bet_1Jan_1Mar= filter(age_25_35, as.Date("2014-01-01")<=age_25_35$tran_date & as.Date("2014-03-01")>= age_25_35$tran_date)
sum(bet_1Jan_1Mar$total_amt)           #Total amount spent by customers between 1st Jan, 2014 to 1st Mar, 2014

##########################################################################################################################
