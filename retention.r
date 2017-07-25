# Retention data
# The challenge here was to transform the initial dataset into a format which
# keeps a headcount of employee numbers per company per day.
# Further analysis of the data shows that it was likely randomly generated --
# could not find any meaningful business insights.

library(dplyr)
library(ggplot2)
hr = read.csv("c:/users/john/Desktop/employee_retention_data.csv")

head(hr)
summary(hr)
str(hr)

#Need to convert date fields
hr$join_date_conv = as.Date(hr$join_date)
hr$quit_date_conv = as.Date(hr$quit_date)
hr$left = is.na(hr$quit_date_conv)

#No employee working for multiple companies
length(hr$employee_id) == length(unique(hr$employee_id))

#Distribution of companies
table(hr$company_id)

#We'll impute a bogus value to make the next computation easier
hr$quit_date_conv[is.na(hr$quit_date_conv)] = as.Date("3000-01-01")

#Create a dataframe consisting of the unique combinations of date and company
headcount = expand.grid(unique(hr$company_id),unique(hr$join_date_conv))
names(headcount) = c("company_id","date")
head(headcount)

for(i in 1:nrow(headcount)){
  headcount[i,3] = sum(headcount[i,2] >= hr$join_date_conv & 
                         headcount[i,1] == hr$company_id & 
                         headcount[i,2] <= hr$quit_date_conv)
}

#Reset the quit date column
hr$quit_date_conv[hr$quit_date_conv == "3000-01-01"] = NA

#Averages by company
hr %>% group_by(company_id) %>% summarise(avgSal = mean(salary),
                                          numEmp = length(salary),
                                          numQuit = sum(is.na(quit_date_conv)),
                                          avgSen = mean(seniority),
                                          attrition = mean(left))

#Averages by department
hr %>% group_by(dept) %>% summarize(avgSal = mean(salary),
                                    numEmp = length(salary),
                                    avgSen = mean(seniority),
                                    attrition = mean(left))

#Averages by churn status
hr %>% group_by(left) %>% summarize(avgSal = mean(salary),
                                    numEmp = length(salary),
                                    avgSen = mean(seniority))

#This data doesn't look very realistic
headcount %>% ggplot(aes(x = date, y = V3, group = company_id, color = factor(company_id))) + geom_line(size = 1)

