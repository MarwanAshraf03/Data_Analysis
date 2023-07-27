install.packages("dplyr")
install.packages("stats")
install.packages("arules")
install.packages("gtools")
library("dplyr")
library("stats")
library("arules")
library("gtools")

#take data from user
datasetPath <- readline("Enter Path (ex: c:/xxx/xxx):")

dataset<-read.csv(datasetPath)
dataset

#compare cash and credit total

table(dataset$paymentType)
pie(
x= table(dataset$paymentType),
  main="Compare cash and Credit total",
  col = c("black","red"),
  init.angle=45
)

#compare  age and sum of total spending


total_age<-group_by(dataset,age)
total_age<-summarize(total_age,totalSpending =sum(total))
total_age 
plot(x=total_age$age, y=total_age$totalSpending,main = "Age and Total spending",
     xlab ="Age",ylab="Total" )

#show each city total spending

total_spending<-group_by(dataset,city)
total_spending<-summarize(total_spending,totalsp=sum(total))
total_spending<-arrange(total_spending,desc(total_spending$totalsp))
par(
  mar=c(5,6,4,1),
  barplot(
  height =total_spending$totalsp,
  name=total_spending$city,
  col="red",
  main = "Distribution of city total spending",
  xlab="Total",
  horiz=TRUE,
  las=1
))
#display the distribution of total spending

boxplot(
  x=dataset$total,
  main="Distribution of total spending")


#put all previous plots in one dashboard

par(
    mar=c(5,5.5,4,2),
    mfrow=c(2,2))

pie(
  x= table(dataset$paymentType),
  main="Compare cash and Credit total",
  col = c("black","red"),
  init.angle=45
)

plot(x=total_age$age, y=total_age$totalSpending,main = "Age and Total spending",
     xlab ="Age",ylab="Total" )

barplot(
  height =total_spending$totalsp,
  name=total_spending$city,
  col="red",
  main = "Distribution of city total spending",
  xlab="Total",
  ylab = "",
  horiz=TRUE,
  las=1
)
boxplot(
  x=dataset$total,
  main="Distribution of total spending")

#-------Split the customers to (n)groups using KMeans------#

#cleaning the data
datapoints <- read.csv(datasetPath)
datapoints <- select(datapoints, customer, age, total)
datapoints <- group_by(datapoints, customer ,age)
datapoints

datapoints <- as.data.frame(summarise(datapoints, SumTotal = sum(total)))
datapoints

#number of clusters
x <- readline(prompt = "How many clusters? ")

#Kmeans
KM <- kmeans(datapoints [,c(FALSE,TRUE,TRUE)], centers = x)
cluster <- KM$cluster

#final table
result <- cbind(datapoints,cluster)
result <- arrange(result, cluster,customer)
result



#------Association rules using Apriori algorithm------#

#input from user
itemsPath <- readline("Enter Path (ex: c:/xxx/xxx): ")
data_items<-read.transactions(itemsPath,sep =",")
inspect(data_items)
min_support<-as.numeric(readline("what is the minimum support? "))
min_confidince<-as.numeric(readline("what is the minimum confidince? "))


#Apriori algorithm
if( (min_support<=1 & min_support>=0.001) & (min_confidince<=1 & min_confidince>=0.001) ){
apriori_rules <- apriori(data_items,parameter = list(supp = min_support, conf = min_confidince,minlen=2))
inspect(apriori_rules)
}else{
 print("please enter numbers between 0.001 and 1") 
}