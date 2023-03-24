library(tidyverse) # general
library(ggalt) # dumbbell plots
library(countrycode) # continent
library(rworldmap) # quick country-level heat maps
library(gridExtra) # plots
library(broom) # significant trends within countries

theme_set(theme_light())


# 1) Import & data cleaning
#刪除了 7 個國家（<= 3 年的數據總數）
#刪除了 2016 年的數據（很少有國家有數據，有的國家經常缺少數據）
#刪除HDI欄位 因為2/3 的數據缺失

wkDir = "C:/Users/asus/Desktop/R_class/0621/";   setwd(wkDir)
data = read.csv(paste0(wkDir,"master.csv"),header = TRUE)

names(data) 
# [1] "country"        "year"           "sex"            "age"            "suicides_no"    "population"    
# [7] "gdp_for_year"   "gdp_per_capita" "generation"     "continent"  

#欄位名稱重新命名
# data <- data %>% 
#   select(-c(`HDI.for.year`, `suicides.100k.pop`)) %>%
#   rename(gdp_for_year = `gdp_for_year....`, 
#          gdp_per_capita = `gdp_per_capita....`, 
#          country_year = `country.year`,
#          country = `嚜盧ountry`) %>%
#   as.data.frame()

 data <- data %>% 
   select(-c(`HDI.for.year`, `suicides.100k.pop`)) %>%
   rename(gdp_for_year = `gdp_for_year....`, 
          gdp_per_capita = `gdp_per_capita....`, 
          country_year = `country.year`) %>%
   as.data.frame()


data <- data %>%
  filter(year != 2016) %>% select(-country_year) #刪除2016年的資料 因為資料不齊

minimum_years <- data %>% group_by(country) %>% summarize(rows = n(), 
years = rows / 12) %>%arrange(years) #資料小於等於三年的country刪除

data <- data %>%
  filter(!(country %in% head(minimum_years$country, 7))) #刪除剛剛所找到的國家

#重整欄位內容
data$age <- gsub(" years", "", data$age)

data$sex <- ifelse(data$sex == "male", "Male", "Female")

#用國家名稱去分洲
data$continent <- countrycode(sourcevar = data[, "country"],
                              origin = "country.name",
                              destination = "continent")

# Nominal factors
data_nominal <- c('country', 'sex', 'continent')
data[data_nominal] <- lapply(data[data_nominal], function(x){factor(x)})

data$suicides_100k <- data$suicides_no / data$population * 100000

# Making age ordinal
data$age <- factor(data$age, 
                   ordered = T, 
                   levels = c("5-14",
                              "15-24", 
                              "25-34", 
                              "35-54", 
                              "55-74", 
                              "75+"))

# Making generation ordinal
data$generation <- factor(data$generation, 
                          ordered = T, 
                          levels = c("G.I. Generation", 
                                     "Silent",
                                     "Boomers", 
                                     "Generation X", 
                                     "Millenials", 
                                     "Generation Z"))

data <- as_tibble(data)


global_average <- (sum(as.numeric(data$suicides_no)) / sum(as.numeric(data$population))) * 100000
# view the finalized data
glimpse(data)


## **Key Insights**

##* Suicide rates are **decreasing globally.** **(2.1)**
#  * Of those **countries** that show clear linear trends over time, **2/3 are decreasing.** **(2.5.2)**
#  * On average, **suicide rate increases with age.** **(2.4)**
#  * This remains true when controlling for continent in the Americas, Asia & Europe, but not for Africa & Oceania. **(2.8)**
#  * There is a *weak* **positive relationship between a countries GDP (per capita) and suicide rate.** **(2.10)**
#  * The **highest suicide rate** ever recorded in a demographic (for 1 year) is **225 (per 100k population).** **(2.12)**
#  * There is an **overrepresentation of men** in suicide deaths at every level of analysis (globally, at a continent and country level). Globally, the male rate is ~3.5x higher. **(2.3) (2.6) (2.7)**
  
  
  


# **Global Analysis**

## **Global Trend**

#The dashed line is the **global average suicide rate** from 1985 - 2015: **13.1 deaths** (per 100k, per year).

#```{r}
#-----------------全球自殺比率走勢--------------------------------#
suicides_table = xtabs(suicides_no~year, data=data) 
population_table = xtabs(population~year, data=data) 
suicides_per_table = (suicides_table / population_table) *100000

suicides_table = as.data.frame(suicides_per_table)
#-----------------全球自殺比率走勢--------------------------------#

data %>%
  group_by(year) %>%
  summarize(population = sum(population), 
            suicides = sum(suicides_no), 
            suicides_per_100k = (suicides / population) * 100000) %>%
  ggplot(aes(x = year, y = suicides_per_100k)) + 
  geom_line(col = "deepskyblue3", size = 1) + 
  geom_point(col = "deepskyblue3", size = 2) + 
  geom_hline(yintercept = global_average, linetype = 2, color = "grey35", size = 1) +
  labs(title = "Global Suicides (per 100k)",
       x = "Year", 
       y = "Suicides per 100k") + 
  scale_x_continuous(breaks = seq(1985, 2015, 2)) + 
  scale_y_continuous(breaks = seq(10, 20))
#```
data %>%
  group_by(year) %>%
  summarize(gdp = sum(gdp_per_capita), 
            gdp_per_1000k = gdp / 100000) %>%
  ggplot(aes(x = year, y = gdp_per_1000k)) + 
  geom_line(col = "deepskyblue3", size = 1) + 
  geom_point(col = "deepskyblue3", size = 2) + 
  labs(title = "Global GDP (per million)",
       x = "Year", 
       y = "GDP per million") + 
  scale_x_continuous(breaks = seq(1985, 2015, 2)) + 
  scale_y_continuous(breaks = seq(10, 20))


#***Insights***
  

#  * Peak suicide rate was **15.3** deaths per 100k in **1995**
#  * Decreased steadily, to **11.5** per 100k in **2015** (**~25% decrease**)
#* Rates are only now returning to their pre-90's rates
#* **Limited data in the 1980's**, so it's hard to say if rate then was truly representative of the global population

#-----------------全球性別比--------------------------------#
sex_num = xtabs(suicides_no~sex, data=datalist) 
sex_num = as.data.frame(sex_num)
ggplot(sex_num, aes(x = sex,y = Freq)) + geom_bar(stat = "identity") #男女總自殺人數比

sex_list = xtabs(suicides_no~year+sex, data=datalist) 
sex_num_list = xtabs(population~year+sex, data=datalist) 
sex_list_table = (sex_list / sex_num_list) *1000000
sex_list_table = as.data.frame(sex_list_table)
female_num = sex_list_table[1:31,]
male_num = sex_list_table[32:62,]
ggplot(data = female_num, aes(x = year, y = Freq)) + geom_line(col ="red",group = 1,size = 1)+ geom_point(col = "red",size = 2)
ggplot(data = male_num, aes(x = year, y = Freq)) + geom_line(col ="deepskyblue3",group = 1,size = 1)+ geom_point(col = "deepskyblue3",size = 2)

## **By Sex**
#```{r}
sex_plot <- data %>%
  group_by(sex) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
  ggplot(aes(x = sex, y = suicide_per_100k, fill = sex)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Global suicides (per 100k), by Sex",
       x = "Sex", 
       y = "Suicides per 100k") +
  theme(legend.position = "none") + 
  scale_y_continuous(breaks = seq(0, 25), minor_breaks = F)

### with time
sex_time_plot <- data %>%
  group_by(year, sex) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
  ggplot(aes(x = year, y = suicide_per_100k, col = factor(sex))) + 
  facet_grid(sex ~ ., scales = "free_y") + 
  geom_line() + 
  geom_point() + 
  labs(title = "Trends Over Time, by Sex", 
       x = "Year", 
       y = "Suicides per 100k", 
       color = "Sex") + 
  theme(legend.position = "none") + 
  scale_x_continuous(breaks = seq(1985, 2015, 5), minor_breaks = F)

grid.arrange(sex_plot, sex_time_plot, ncol = 2)
#```


#-----------------全球自殺年齡層分布-------------------------------#
age_num = xtabs(suicides_no~age, data=datalist) 
age_num = as.data.frame(age_num)

ggplot(age_num, aes(x = age,y = Freq)) + geom_bar(stat = "identity") 


## **By Age**

#```{r}
age_plot <- data %>%
  group_by(age) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
  ggplot(aes(x = age, y = suicide_per_100k, fill = age)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Global suicides per 100k, by Age",
       x = "Age", 
       y = "Suicides per 100k") +
  theme(legend.position = "none") + 
  scale_y_continuous(breaks = seq(0, 30, 1), minor_breaks = F)

### with time
age_time_plot <- data %>%
  group_by(year, age) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
  ggplot(aes(x = year, y = suicide_per_100k, col = age)) + 
  facet_grid(age ~ ., scales = "free_y") + 
  geom_line() + 
  geom_point() + 
  labs(title = "Trends Over Time, by Age", 
       x = "Year", 
       y = "Suicides per 100k", 
       color = "Age") + 
  theme(legend.position = "none") + 
  scale_x_continuous(breaks = seq(1985, 2015, 5), minor_breaks = F)


grid.arrange(age_plot, age_time_plot, ncol = 2)
#```

#-----------------全球各國家自殺率-------------------------------#
country_list = xtabs(suicides_no~country, data=data)
pop_list = xtabs(population~country, data=data)
country_list = (country_list / pop_list) *100000
country_list = as.data.frame(country_list)
country_list = country_list %>% arrange(desc(Freq))
ggplot(country_list, aes(x = country,y = Freq)) + geom_bar(stat = "identity") + coord_flip() +  
  labs(title = "Global suicides per 100k, by Country",x = "Country",y = "Suicides per 100k")
head(country_list,10) #前十名
# country     Freq   
# 1           Lithuania 4.146410
# 2  Russian Federation 3.277721
# 3           Sri Lanka 3.048394
# 4             Belarus 3.034468
# 5             Hungary 3.002241
# 6              Latvia 2.847101
# 7          Kazakhstan 2.689861
# 8            Slovenia 2.636048
# 9             Estonia 2.596452
# 10            Ukraine 2.487040

#-----------------全球各國家自殺率-------------------------------#
## **By Country**

### **Overall**
#```{r fig.width = 7, fig.height = 12}
country <- data %>%
  group_by(country, continent) %>%
  summarize(n = n(), 
            suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
  arrange(desc(suicide_per_100k))

country$country <- factor(country$country, 
                          ordered = T, 
                          levels = rev(country$country))

ggplot(country, aes(x = country, y = suicide_per_100k, fill = continent)) + 
  geom_bar(stat = "identity") + 
  geom_hline(yintercept = global_average, linetype = 2, color = "grey35", size = 1) +
  labs(title = "Global suicides per 100k, by Country",
       x = "Country", 
       y = "Suicides per 100k", 
       fill = "Continent") +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 45, 2)) + 
  theme(legend.position = "bottom")
#```


## **By Continent**
#```{r}
continent <- data %>%
  group_by(continent) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
  arrange(suicide_per_100k)

continent$continent <- factor(continent$continent, ordered = T, levels = continent$continent)

continent_plot <- ggplot(continent, aes(x = continent, y = suicide_per_100k, fill = continent)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Global Suicides (per 100k), by Continent",
  x = "Continent", 
  y = "Suicides per 100k", 
  fill = "Continent") +
  theme(legend.position = "none", title = element_text(size = 10)) + 
  scale_y_continuous(breaks = seq(0, 20, 1), minor_breaks = F)


continent_time <- data %>%
  group_by(year, continent) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000)

continent_time$continent <- factor(continent_time$continent, ordered = T, levels = continent$continent)

continent_time_plot <- ggplot(continent_time, aes(x = year, y = suicide_per_100k, col = factor(continent))) + 
  facet_grid(continent ~ ., scales = "free_y") + 
  geom_line() + 
  geom_point() + 
  labs(title = "Trends Over Time, by Continent", 
       x = "Year", 
       y = "Suicides per 100k", 
       color = "Continent") + 
  theme(legend.position = "none", title = element_text(size = 10)) + 
  scale_x_continuous(breaks = seq(1985, 2015, 5), minor_breaks = F)

grid.arrange(continent_plot, continent_time_plot, ncol = 2)
#```

# ```{r}
country_year_gdp <- data %>%
  group_by(country, year) %>%
  summarize(gdp_per_capita = mean(gdp_per_capita))

country_year_gdp_corr <- country_year_gdp %>%
  ungroup() %>%
  group_by(country) %>%
  summarize(year_gdp_correlation = cor(year, gdp_per_capita))
# ```


# ```{r}
country_mean_gdp <- data %>%
  group_by(country, continent) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000, 
            gdp_per_capita = mean(gdp_per_capita))

ggplot(country_mean_gdp, aes(x = gdp_per_capita, y = suicide_per_100k, col = continent)) + 
  geom_point() + 
  scale_x_continuous(labels=scales::dollar_format(prefix="$"), breaks = seq(0, 70000, 10000)) + 
  labs(title = "Correlation between GDP and Suicides num", 
       x = "GDP per capita", 
       y = "Suicides per 100k", 
       col = "Continent") 
# ```
# 
# There are quite a few high leverage & residual countries that could have a significant impact on the fit of my regression line (e.g. Lithuania, top left). I'll identify and exclude these using Cooks Distance, excluding those countries with a CooksD value of greater than 4/n.
# 
# I assess the statistics of this model (with outliers removed) below.
# 
# ```{r}

#-----------------------------線性模型-----------------------------------
model1 <- lm(suicide_per_100k ~ gdp_per_capita, data = country_mean_gdp)

#講解outliers
gdp_suicide_no_outliers <- model1 %>%
  augment() %>%
  arrange(desc(.cooksd)) %>%
  filter(.cooksd < 4/nrow(.)) %>% # removes 5/93 countries
  inner_join(country_mean_gdp, by = c("suicide_per_100k", "gdp_per_capita")) %>%
  select(country, continent, gdp_per_capita, suicide_per_100k)

model2 <- lm(suicide_per_100k ~ gdp_per_capita, data = gdp_suicide_no_outliers)

summary(model2)
#-------------------------------線性模型-----------------------------------

ggplot(gdp_suicide_no_outliers, aes(x = gdp_per_capita, y = suicide_per_100k, col = continent)) + 
  geom_point() + 
  geom_smooth(method = "lm", aes(group = 1)) + 
  scale_x_continuous(labels=scales::dollar_format(prefix="$"), breaks = seq(0, 70000, 10000)) + 
  labs(title = "Correlation between GDP and Suicides num", 
       x = "GDP (per capita)", 
       y = "Suicides per 100k", 
       col = "Continent") + 
  theme(legend.position = "none")

#-------------------------------線性模型-----------------------------------
cor(data$year,data$suicides_no,method = "pearson")
cor(data$year,data$suicides_no,method = "spearman")
cor(data$year,data$suicides_no,method = "kendall")

wilcox.test(data$year,data$suicides_no,alternative = "two.sided",extract = FALSE,correct = FALSE)
# Wilcoxon rank sum test
# 
# data:  data$year and data$suicides_no
# W = 736983018, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0
#--------------------------------線性模型-----------------------------------

#--------------------------------聚類模型----------------------------------
count <- table(data$year)

distance <- dist(count)

c.hc <-hclust(distance)

plot(c.hc,hang = -1)

c.hc <- hclust(distance,method ="ward.D")

r <- rect.hclust(c.hc,k= 3)
c.hc <- hclust(distance,method ="ward.D")
r <- rect.hclust(c.hc,k= 3)
#--------------------------------聚類模型----------------------------------
library(tree)
temp = data
temp$age <- ifelse(temp$age == "5-14", as.integer(0), ifelse(temp$age == "15-24",  as.integer(1), ifelse(temp$age == "25-34",  as.integer(2), ifelse(temp$age == "35-54",  as.integer(3), ifelse(temp$age == "55-74",  as.integer(4),  as.integer(5))))));
temp$suicides_100k = cut(temp$suicides_100k, breaks = c(-1, 3, 6, 12, 15, 50, 225), c("< 3", "3-6", "6-12", "12-15", "15-50", "50-225") )
temp$country <- as.character(temp$country)
temp$sex <- as.character(temp$sex)

suicide.tree = tree(suicides_100k ~. ,data=temp);    suicide.tree 
plot(suicide.tree);     text(suicide.tree) 



#----------------------------------講義----------------------------------------
library(rpart);   library(rpart.plot)

temp = data
temp$age <- ifelse(temp$age == "5-14", as.integer(0), ifelse(temp$age == "15-24",  as.integer(1), ifelse(temp$age == "25-34",  as.integer(2), ifelse(temp$age == "35-54",  as.integer(3), ifelse(temp$age == "55-74",  as.integer(4),  as.integer(5))))));
temp$suicides_100k = cut(temp$suicides_100k, breaks = c(-1, 3, 6, 12, 15, 50, 225), c("< 3", "3-6", "6-12", "12-15", "15-50", "50-225") )
temp$country <- as.character(temp$country)
temp$sex <- as.character(temp$sex)


set.seed(1)
train.index <- sample(x=1:nrow(temp), size=ceiling(0.8*nrow(temp) ))
train <- temp[train.index, ]
test <- temp[-train.index, ]

suicide.rpart <- rpart(suicides_100k ~ gdp_per_capita+age+sex+continent , data=train);   print(suicide.rpart)
rpart.plot(suicide.rpart)


summary(suicide.rpart)

result <- predict(suicide.rpart, newdata = test, type = "class")
#(6)建立混淆矩陣(confusion matrix)觀察模型表現
cm <- table(test$suicides_100k, result, dnn = c("實際", "預測"))
cm
accuracy <- sum(diag(cm)) / sum(cm)
#--------------------------------------------------------

