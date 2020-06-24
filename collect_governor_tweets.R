# governor tweets
library(tidyverse)

### Parse handles
# data taken from: https://www.nclc.org/images/Governors-Twitter-Handles-2020.pdf

govhandles <- read_csv("governor_handles.csv") %>% 
  mutate(gender = ifelse(is.na(gender),"M","F"),
         handle = str_extract(raw,"@(.*)") %>% str_remove("@"),
         state = str_sub(raw,1,str_locate(raw," ")[,1]-1),
         state = ifelse(state %in% c("North","South","West","New"),str_extract(raw,"(North|South|West|New) ([A-z]*) ") %>% str_trim,state),
         party = case_when(str_detect(raw," R @") ~ "R",
                           str_detect(raw," D @") ~ "D",
                           str_detect(raw," NPP @") ~ "NPP"),
         name = str_remove_all(raw,str_c(c(handle,state,"@",str_c(" ",party," "),"\\(mayor\\)"),collapse = "|")) %>% str_trim()) %>% 
  filter(!is.na(raw))

govhandles %>% select(-raw) %>% write_csv("governor_handles_clean.csv")


### collect tweets ###
library(rtweet)

### tweets by the governors ###
#govTweets <- get_timeline(govhandles$handle,n = 3200,check = F) #this will take some time
#write_as_csv(govTweets,"govTweets.csv")
govUsers <- lookup_users(govhandles$handle)

### replies to tweets by the governors ###

govTweets %>% 
  group_by(screen_name) %>% 
  summarise(n = n(),first_date = min(created_at)) %>% 
  arrange(first_date)



#lme4::lmer(retweet_count ~ (1|screen_name) + covidTweet+gender + followers_count,data = govTweets %>% filter(created_at > "1/1/2020")) %>% summary
rt_nGovFE <- lm(retweet_count ~ followers_count + covidTweet*gender + factor(date),data = govTweets %>% filter(created_at > "2020-02-01")) 
rt_yGovFE <- lm(retweet_count ~ screen_name + covidTweet*gender + factor(date),data = govTweets %>% filter(created_at > "2020-02-01")) 

fv_nGovFE <- lm(favorite_count ~ followers_count + covidTweet*gender + factor(date),data = govTweets %>% filter(created_at > "2020-02-01")) 
fv_yGovFE <- lm(favorite_count ~ screen_name + factor(date) + covidTweet*gender ,data = govTweets %>% filter(created_at > "2020-02-01")) 


stargazer::stargazer(rt_nGovFE,rt_yGovFE,fv_nGovFE,fv_yGovFE,
                     type = "text",omit = "screen_name|date",
                     dep.var.labels = c("Retweet Count","Favorite Count"),
                     add.lines = c("Governor Fixed Effects & N & Y & N & Y",
                                   "Day Fixed Effects & Y & Y & Y & Y"))

