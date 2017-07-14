# Translation A/B Testing

# Premise:
# Given website conversion data for A/B test, determine if new country-level
# translations are actually performing worse than using a single Spanish translation


# Insights:
# Including Spain in the statistics biases the test = 0 conversion rate
# This is because Spain's values for test are all zero and they are the biggest converters

# Similarly, including Argentina in the overall calculation biases the test = 1 rate
# This is because Argentina is heavily-weighted towards test = 1 and has the worst
# conversion rate of any country

# A function was written which will take a dataframe and output whenever
# the response variable is off by a factor of 1.5 (40/60 split or worse)
# when aggregated by any variable that has less than 20 unique values
# for all variables in the dataframe

test = read.csv("c:/Users/John/Desktop/test_table.csv")
user = read.csv("c:/Users/John/Desktop/user_table.csv")

#Let's confirm that the results were actually "worse"
names(test)
names(user)

aggregate(conversion ~ test, data = merged, FUN = mean)

#Spain could be biasing these results
#Need to merge dfs to get country info
merged = merge(test, user, by = "user_id", all.x = TRUE)

#Overall conversion rate still looks worse
aggregate(conversion ~ test, data = subset(merged, country != "Spain"), FUN = mean)

#Some countries better, some worse
aggregate(conversion ~ test + country, data = merged, FUN = mean)


#Need to check for other confounding factors
names(merged)

#Nothing of note in browser_language
aggregate(conversion ~ test, data = subset(merged, country != "Spain" & browser_language == "ES"), FUN = mean)

aggregate(conversion ~ test + browser_language, data = merged, FUN = mean)

#Summary looks good
lapply(split(merged, merged$test), summary)

#It looks like some countries are very imbalanced
aggregate(conversion ~ test + country, data = subset(merged, country != "Spain"), FUN = function(x) c(round(mean(x),3), length(x)))


library(ggplot2)
library(dplyr)

#Plot is ugly, but it shows us that Argentina has a very low conversion rate
#and very imbalanced for "test" variable
merged %>% 
  group_by(test, country) %>% 
  summarize(conv_rate = mean(conversion), n = length(conversion)) %>%
  ggplot(., aes(x = country, y = n, color = test, fill = conv_rate)) + geom_bar(stat = "identity")

#Once we remove the two biasing countries, we can see that the overall
#conversion rates are much more similar
aggregate(conversion ~ test, data = subset(merged, country != "Spain" & country != "Argentina"), FUN = mean)

#Let's write a function to test if there are imbalances in our classes
#based upon any arbitrary segmenting variable
#Note that this function would not work properly if there were more than
#two possibilities for the response variable
test_validity = function(df, response = "test", exclude = c("user_id")){
  columns = names(df)
  for(column in columns){
    if(column %in% exclude | column == response) next
    agg = aggregate(eval(parse(text = response)) ~ eval(parse(text = column)), data = df, FUN = mean)
    if(nrow(agg) < 20 & any(agg[,2] < .4 | agg[,2] > .6)){
      warning(paste0("Imbalanced classes in column " ,column, " for value in ", row.names(agg[agg[,2] < .4 | agg[,2] > .6,])[1]))
    }
  }
}

#From the answer key:  We could use a partition tree to predict
#membership in the test set to discover these biases
