library(ggplot2)
library(dplyr)
library(stringr)
# ctrl + shift + enter to run

############################################################
# REFERENCES:
# 1. Exploratory data analysis in R (pt.1, pt.2) 
# https://stackoverflow.com/questions/27125672/what-does-function-mean-in-r
# https://r-charts.com/correlation/heat-map-ggplot2/
# https://dplyr.tidyverse.org/reference/summarise.html
###########################################################


# %>% is literally like pipe in unix. Oh thats so useful


# Part 1.
# 1.
ggplot(data = mpg) + geom_point(mapping = aes(x = hwy, y = cty, color = drv)) + geom_smooth(mapping = aes(x = hwy, y = cty)) #a.
ggplot(data = mpg, aes(x = hwy)) + geom_bar(width = 1) #b.
# 2.
ggplot(data = diamonds, aes(x = carat, y = price)) + geom_point() + facet_wrap(. ~ cut) #a.
ggplot(data = diamonds, aes(x = carat, y = price)) + geom_point() + facet_wrap(. ~ cut, scales="free") #b.
# Changes: all data points in every category appear more spread out, and less clustered, 
# specifically near the top right of the scatter plots

# 3.
count_color_cut <- diamonds %>% group_by(color, cut) %>% summarize(count = n()) #a.
#print(count_color_cut)
ggplot(count_color_cut, aes(x = cut, y = color, fill = count)) +
  geom_tile() + labs(x = "cut", y = "color", fill = "n") #b.


# 4.
ggplot(mpg) + geom_point(aes(displ,hwy)) #a.
ggplot(mpg,aes(displ,hwy)) + 
  geom_point(aes(color = class)) + geom_smooth() #b.

# Part 2.

# 5.
#unusual <- filter(diamonds, y < 3 | y > 20)
#unusual <- arrange(unusual, y)
#print(unusual)
diamonds %>% filter(y < 3 | y > 20) %>% arrange(y) #a.

#smaller <- filter(diamonds, carat < 3)
#ggplot(smaller, aes(carat)) +
  #geom_histogram(binwidth = 0.1)

diamonds %>% filter(carat < 3) %>%
  ggplot(aes(carat)) +
  geom_histogram(binwidth = 0.1) #b.
                
# Part 3.
# 6. 
review_10k <- readRDS("review_10k.rds") #a.
review_10k <- review_10k %>% mutate(pizza = str_detect(tolower(text), "pizza")) #b.
review_10k <- review_10k %>% mutate(n_love = str_count(tolower(text), "love"))  #c.
said_love <- review_10k %>% filter(n_love >= 1) #d. 
#head(said_love)
pizza_love <- review_10k %>% filter(str_detect(tolower(text), "pizza") & n_love >= 1) #e. 
#head(pizza_love)
#head(review_10k)
# F. in this format, the function is used to count explicitly the number of spaces, which means that any time there is
# multiple spaces between words, the function would count one more word than there actually is


