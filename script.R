library(purrr)
library(rjstat)
library(tidyr)
library(httr)
library(ggplot2)
library(dplyr)
library(gganimate)

url <- paste0("https://data.ssb.no/api/v0/en/table/09253/")
query <- '{
  "query": [{"code": "Kjonn", "selection": { "filter": "item", "values": [ "0"] }}, 
            {"code": "StudieretningUtd", "selection": {"filter": "item", "values": ["00-99"]}},
            {"code": "FullforingVGO","selection": {"filter": "item", "values": ["5", "1", "2", "3", "4", "8", "9"]}},
            {"code": "ContentsCode", "selection": {"filter": "item", "values": ["EleverProsent"]}}
    ],
    "response": {
    "format": "json-stat"
    }
    }'

temp <- POST(url, body = query, encode = "json")
    
if (temp$status_code == 200) {
  dfnames <- fromJSONstat(content(temp, 'text'), naming = 'id')[[1]]
  idnames <- names(dfnames)
  rm(dfnames)
  df = fromJSONstat(content(temp, 'text'))[[1]]
  names(df) <- idnames
}

cats <- unique(df$FullforingVGO)
lvls <-  cats[order(nchar(cats),cats)]

df$FullforingVGO <- factor(df$FullforingVGO, levels = lvls)
  
df %>% 
  mutate(year = as.integer(substr(Tid, 1, 4))) %>% 
  filter(FullforingVGO != 'Total') %>% 
#  filter(year==2012) %>% 
  ggplot(aes(FullforingVGO)) +
  geom_bar(aes(weight=value),  fill ="#03cda3") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1), 
        plot.margin=unit(c(0.4,0.4,0.4,1),"cm")) +
  labs(title = 'Norwegian high school completion after 5 years, class of {frame_time}', x = 'Degree of completion', y = 'Percent of students') +
  transition_time(
    year
  ) +
  ease_aes('linear')
  
  
  
  
  
  
  
