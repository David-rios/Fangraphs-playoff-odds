## First need to know each opening day since 2016 and the last day of the season.
library("baseballr")
library("dplyr")
library("lubridate")

years_vector <- 2016:2022
res_list <- vector(mode = "list",length = length(years_vector))

for(i in seq_along(years_vector) ){
  
df_temp <- mlb_schedule(season = years_vector[i]) 
df_temp <- df_temp %>% 
  mutate(date = ymd(date)) %>% 
  filter(series_description == "Regular Season") %>% 
  summarise(min_date = min(date),
            max_date = max(date))

res_list[[i]] <- data.frame( dates = c(df_temp$min_date-1, seq(df_temp$min_date, df_temp$max_date, by = 7),df_temp$max_date +1  ),
            year = years_vector[i]
            )

}

df_years <- bind_rows(res_list)

# scrape projections using httr for each of the values in df_years 
require(httr)
require(glue)


res_list <- vector(mode = "list",length = length(df_years$dates))

for( i in seq_along(res_list)){
  try({
url = glue('https://www.fangraphs.com/api/playoff-odds/odds?dateEnd={df_years$dates[i]}&dateDelta=&projectionMode=2&standingsType=div')
res <-httr::GET(url = url)
resp <- res %>% 
  httr::content(as = "text",encoding = "UTF-8")
temp_df  <-jsonlite::fromJSON(resp) %>% janitor::clean_names()

res_list[[i]] <- data.frame(team = temp_df$abb_name,
           exp_w = temp_df$end_data$ExpW, 
           act_w = temp_df$w, 
           playoff_odds = temp_df$end_data$poffTitle,
           date= df_years$dates[i])

})

}

df_playoff_odds <- bind_rows(res_list)

require(ggplot2)

# Need to create the following variables 
# End of season wins (max for each team each year)
# Did the team make the playoffs ( max of playoff odds for each team each year)
df_playoff_odds1 <- df_playoff_odds %>% 
  mutate(year = year(date)) %>% 
  group_by(year, team) %>% 
  mutate(end_W = last(act_w),
         end_Playoff = last(playoff_odds)) %>% ungroup() 

df_playoff_odds2 <- df_playoff_odds1 %>% ungroup() %>% 
  mutate(error_W = exp_w - end_W) %>% 
  mutate(playoff_cor =   round(playoff_odds) == end_Playoff)

## Now want to create an estimate for each week of the season of the error_W and playoff_cor 

df_years1 <-df_years %>% 
  group_by(year) %>% 
  mutate(week = 1:n()) %>% ungroup() %>% 
  rename(date =dates )

df_playoff_odds3 <- df_playoff_odds2 %>% 
  left_join(df_years1)

df_playoff_odds3 %>% 
  group_by(year, week) %>% 
  summarise(med_werror = median(error_W),
            playoff_cor = sum(playoff_cor)) %>% 
  ggplot(aes(x = week-1, y = med_werror)) +
  geom_point(aes(group = year)) +
  geom_line(aes(group = year, color = year)) +
  theme_bw() +
  labs(x = "Week", y = "Median error in win projections")

df_playoff_odds3 %>% 
  group_by( week) %>% 
  summarise(med_werror =  median(error_W),
            playoff_cor = sum(playoff_cor)/length(2016:2022)) %>% 
  print(n = 27)
