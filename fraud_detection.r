transactions = read.csv("c:/Users/John/Desktop/Fraud_Data.csv")
ip2country = read.csv("c:/Users/John/Desktop/IpAddress_to_Country.csv")

str(transactions)
str(ip2country)

ip_indexes = sapply(transactions$ip_address,
                    function(x){findInterval(x, ip2country$lower_bound_ip_address)})

#Not working; why?
transactions$country = ip2country$country[ip_indexes]

#IP index of 0 makes no sense
summary(ip_indexes)

#This is the exact number we're off by
sum(ip_indexes == 0)

#These people have ip addresses lower than any address in the table
which(ip_indexes == 0)
head(transactions, 18)

#Join to get country information, impute unknown for people with invalid ip's
valid = subset(transactions, ip_indexes != 0)
invalid = subset(transactions, ip_indexes == 0)

valid$country = ip2country[ip_indexes[ip_indexes != 0], "country"]
invalid$country = "unknown"

transactions = rbind(valid, invalid)

#Change time columns from factor to time
transactions$signup_time = as.POSIXlt(transactions$signup_time, tz = "GMT")
transactions$purchase_time = as.POSIXlt(transactions$purchase_time, tz = "GMT")

mean(transactions$class[transactions$country == "unknown"])

#Does a repeat device mean anything?
dupe_devices = transactions[duplicated(transactions$device_id),]
mean(dupe_devices$class) #Wow!

length(unique(dupe_devices$device_id))

transactions$same_device = duplicated(transactions$device_id)

#Number of days before making a purchase
transactions$purchase_lag = as.Date(transactions$purchase_time) - as.Date(transactions$signup_time)

summary(glm(class ~ purchase_lag, data = transactions, family = binomial)) #Useful!

#Does a repeat IP mean anything?
mean(transactions[duplicated(transactions$ip_address),"class"])

transactions$same_ip = duplicated(transactions$ip_address)

#Find risky countries
library(dplyr)
agg = transactions[,c("country","class")] %>% group_by(country) %>% 
  summarize("avg_fraud" = mean(class),"num_purchases" = length(class))

#Grab all countries that have at least 2x the normal fraud rate
risky_countries = as.data.frame(agg[order(agg$avg_fraud),])[167:nrow(agg),"country"]
transactions$risky = transactions$country %in% risky_countries

transactions$signup_time = as.Date(transactions$signup_time)
transactions$purchase_time = as.Date(transactions$purchase_time)
transactions$class = as.factor(transactions$class)

library(caret)
cv = trainControl(method="cv", number=10, savePredictions = TRUE)
rf_mdl = train(class ~ . - country - ip_address - device_id - user_id,
               data = transactions, trControl = cv, method = "rf", metric = "f1")

log_mdl = glm(class ~ . - country - ip_address - device_id - user_id,
              data = transactions, family = binomial)
