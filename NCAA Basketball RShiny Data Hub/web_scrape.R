### Get the link of conference in division I
library(rvest)
library(tidyverse)
conference_link=read_html("https://www.sports-reference.com/cbb/seasons/2022.html")%>%
  html_elements("td.left")%>%
  html_elements("a")%>%
  html_attr(name="href")%>%
  paste0("https://www.sports-reference.com",.)%>%
  as_tibble()%>%
  filter(str_detect(value,"conferences"))%>%
  head(32)
### Add conference name 
conference_list=read.csv("conference_list.csv")
conference_list=conference_list[,2]
### Get the link of school in each conference
school_link=tibble()
for (i in 1:nrow(conference_link)){
  url=conference_link[i,]$value
  school_link_in_one_conference=read_html(url)%>%
    html_elements("td.left")%>%
    html_elements("a")%>%
    html_attr(name="href")%>%
    paste0("https://www.sports-reference.com",.)%>%
    as_tibble()%>%
    unique()%>%
    mutate(conference=conference_list[i])%>%
    filter(str_detect(value,"html"))
  Sys.sleep(3)##Sleep 3s to avoid to many requests per minitue
  school_link=rbind(school_link,school_link_in_one_conference)
}
### get the image of school logo
image_link=tibble()
for (i in 1:nrow(school_link)){
  url=school_link[i,1]$value
  image_link_in_one_school=read_html(url)%>%
    html_elements("img.teamlogo")%>%
    html_attr("src")
  Sys.sleep(3)
  image_link=rbind(image_link,image_link_in_one_school)
}
current_school_table=cbind(school_link,image_link)
colnames(current_school_table)=c("school_link","conferences","image_link")
### get the information for each school
data_school_list=tibble()
for (i in 1:nrow(school_link)){
  url=school_link[i,1]$value
  information=read_html(url)%>%
    html_elements("#season-total_per_game")%>%
    html_table()
  Sys.sleep(3)
  data_school=as.data.frame(information[[1]][1,])%>%select(-c(1))
  data_school_list=rbind(data_school_list,data_school)
}
final.school.df=cbind(current_school_table,data_school_list)
# write.csv(final.school.df,"C:/Users/user/Desktop/Duke Y1/Stat 523/project_AAAAA-main/project_AAAAA-main/final_table.csv")
### get the information of players in each school
data_player_list=tibble()
for (i in 1:nrow(final.school.df)){
  url=final.school.df[i,1]
  information=read_html(url)%>%
    html_elements("#per_game")%>%
    html_table()
  Sys.sleep(3)
  data_player=as.data.frame(information[[1]])%>%select(-c(1))%>%mutate(school=final.school.df[i,1])
  data_player_list=rbind(data_player_list,data_player)
}
# write.csv(data_player_list,"C:/Users/user/Desktop/Duke Y1/Stat 523/project_AAAAA-main/project_AAAAA-main/player_data.csv")
