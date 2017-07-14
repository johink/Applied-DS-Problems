# Conversion Rate

# Premise:
# Examine Google Analytics website data and draw conclusions /
# make recommendations to optimize site performance

# Summary of insights:
# Chinese website performing extremely poorly; what's wrong?
# Above-average conversion from customers below 30; exploit further
# Very low performance from customers over 50; is this a function of our product or our site?
# Returning users convert very well; try to encourage repeat visits (email campaign?)
# Users with high pageviews tend to convert; remarketing opportunity for those who didn't convert?
# Direct traffic converting at lower rates; are direct customers having trouble finding what they're looking for?




conversions = read.csv("c:/Users/John/Desktop/conversion_data.csv")
head(conversions)

#Goal is to predict conversion, a binary response
#This is a classification task

#Examine data structure
str(conversions)

#Examine data distributions
summary(conversions)

#US makes up majority of traffic
#Age max is 123?
#Returning users roughly 69% of traffic
#SEO brings in most users, ads/direct roughly tied
#Conversion rate around 3.23%

#Some invalid age values.  Only six rows out of 316,200 are over 70,
#so we can safely discard these
sum(conversions$age > 70)

conversions = conversions[conversions$age <= 70,]

#Conversion rates by country
by_country = table(conversions$country, conversions$converted)
by_country / rowSums(by_country)

#Another way to do it!
aggregate(converted ~ country, data = conversions, mean)

#China conversion rate _extremely_ low, but good amount of traffic comes from there
#Possibly some issue with our Chinese site?

names(conversions)

#How do new users compare to returning users?
aggregate(converted ~ new_user, data = conversions, mean)

#Return users about 5x as likely to convert than new users

#Is any marketing channel more effective?
aggregate(converted ~ source, data = conversions, mean)

#Direct appears to be less effective than ads / SEO
#Is it too difficult for users to get to the content they seek from our home page?

#Increase in age correlates slightly with a decrease in conversion
#More sophisticated analysis required
cor(conversions$age, conversions$converted)

#Pages visited correlates very highly with conversion
#This makes sense because several pages must be visited to convert
cor(conversions$total_pages_visited, conversions$converted)

#Someone converted in two pages.  Very efficient!
min(conversions$total_pages_visited[conversions$converted == 1])

#Let's plot the distributions to better see how pages affects conversion
converts_by_pages = aggregate(source ~ converted + total_pages_visited, data = conversions, length)

library(ggplot2)

#Let's normalize the values by conversion status so we can more easily compare the groups
converts_by_pages$source[converts_by_pages$converted == 1] = converts_by_pages$source[converts_by_pages$converted == 1] / sum(converts_by_pages$source[converts_by_pages$converted == 1])
converts_by_pages$source[converts_by_pages$converted == 0] = converts_by_pages$source[converts_by_pages$converted == 0] / sum(converts_by_pages$source[converts_by_pages$converted == 0])

p = ggplot(data = converts_by_pages, aes(x = total_pages_visited, y = source, group = converted, fill = converted)) + geom_bar(stat = "identity", position = "dodge")
p

#Clearly, distributions for pages visited by converters comes from a different distribution
#This could be an incidence of 'data leakage.'  Not sure what we could do with this information, anyways
#Would we redesign the website to force people to visit more pages?  That sounds like a bad idea
#Pages_visited probably a red herring


#Is there any discrepancy in marketing channel performance by country?
names(conversions)
mytable = table(conversions$country, conversions$source, conversions$converted)
mytable  #Nope!

#Let's build a model
logmdl = glm(converted ~ country + age + I(age * age) + new_user + source, data = conversions, family = binomial)
summary(logmdl)

#We see a significant non-linear relationship present for age
agg_age = aggregate(converted ~ age, data = conversions, FUN = mean)

#This makes it clear that we perform better with younger users
ggplot(agg_age, aes(x = age, y = converted)) + geom_point()
