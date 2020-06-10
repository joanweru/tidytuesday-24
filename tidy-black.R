library(tidyverse)
library(stringr)
library(ggplot2)
library(ggthemes)
library(magick)
library(png)
library(grid)
library(scales)

setwd("~/R- CLASS DATASETS")
black<-readPNG("images-5.png")
g<-rasterGrob(black,interpolate = TRUE,width = unit(3,"cm"),x=unit(1,"npc"),hjust=1,vjust=-1,just = "bottom",)
science<-read.csv("science.csv")
first<-read_csv("first.csv")

genderblack<-first%>%group_by(category,gender)%>%count(category,gender)%>%
  ungroup()%>%mutate(category=fct_reorder(category,n))%>%
  mutate(gender=case_when(str_detect(gender,"African-American Firsts")~"Male",TRUE~"Female"))%>%
  ggplot(aes(x=category,y=n,fill=gender))+geom_col(position = "dodge")+
  theme_fivethirtyeight()+scale_fill_manual(values = c("salmon3","saddlebrown"))+
  coord_flip()+theme(legend.position = "right",legend.direction = "vertical")+
  labs(title="FIRST ACHIEVEMENTS BY AFRICAN AMERICANS BY GENDER AND CATEGORY")+annotation_custom(g,xmin = -4,ymin=12)

first%>%mutate(personal=paste(person,accomplishment,sep = "----"))%>%add_count(category)%>%
  ggplot(aes(x=year,y=n,color=category))+geom_point(size=3)+theme_fivethirtyeight()+
  scale_color_manual(values = c("chocolate2","sienna4","tomato4","tomato3","salmon4","black","red4","firebrick"))+
  theme(legend.position = "right",legend.direction = "vertical",plot.title = element_text(hjust = 0.7))+
  scale_x_continuous(breaks = seq(1738,2050,30))+ggtitle(mytitle)+annotation_custom(g,xmin = -0.5)

attach(science)
science$field=case_when(str_detect(occupation_s,regex("computer",ignore_case = TRUE))~"Computing",
                         str_detect(occupation_s,regex("engineer|elec|robo|acous",ignore_case = TRUE))~"Engineering",
                         str_detect(occupation_s,regex("math|stat|prob",ignore_case = TRUE))~"Mathematics",
                         str_detect(occupation_s,regex("surgeon|med|nurse|dent|ophtha|psycho|surg|ortho",ignore_case=TRUE))~"Health science",
                         str_detect(occupation_s,regex("astr|atmos",ignore_case = TRUE))~"Space and geography",
                         str_detect(occupation_s,regex("chemi",ignore_case = TRUE))~"Chemistry",
                         str_detect(occupation_s,regex("econ|finan",ignore_case = TRUE))~"Economics",
                         str_detect(occupation_s,regex("physic",ignore_case = TRUE))~"Physics",
                         str_detect(occupation_s,regex("bio|gene|botan|epidem|zoo",ignore_case = TRUE))~"Biology",
                         str_detect(occupation_s,regex("resear",ignore_case = TRUE))~"Research",
                         str_detect(occupation_s,regex("farmer",ignore_case = TRUE))~"Agriculture",
                         str_detect(occupation_s,regex("art|draft|archi",ignore_case = TRUE))~"Design",
                         str_detect(occupation_s,regex("ling|anthro",ignore_case = TRUE))~"Language and Culture",
                         str_detect(occupation_s,regex("inventor|NA",ignore_case = TRUE))~"Inventor",
                        
                      )
#I noticed two values didnt get matched both of them were inventors so i just replaced the NA
science$field<-science$field%>%replace_na("Inventor")

science1<-science%>%mutate(decade=birth-birth%%10)%>%group_by(decade)%>%arrange(field)%>%
  mutate(index=row_number())%>%ungroup()

science1%>%ggplot(aes(x=decade,y=n,fill=field))+geom_bar(stat="identity",width=6.5,size=1.3)+
  scale_x_continuous(breaks=seq(1730,2010,by=10),labels = paste0(seq(1730,2010,by=10),"s"))+
  scale_y_continuous(breaks = seq(0,500,by=30))+theme_fivethirtyeight()+
  theme(legend.direction = "vertical",legend.position = "right",plot.title = element_text(size = 15))+
  annotation_custom(g,xmin = 0.5)+coord_cartesian(clip="off")+
  labs(caption = "Github:@joanweru",
       title="NOTABLE AFRICAN AMERICAN INVENTIONS AND CONTRIBUTIONS IN VARIOUS FIELDS OF SCIENCE THROUGHOUT THE DECADES")

View(science)
