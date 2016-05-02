library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(readr)
library(countrycode)
library(stringr)
library(rvest)

##Loading Data from ISU Website###

# Part 1. Loading Season Best Score (SBS)
#create a data frame that contains a list of html for season best score
#to create a list of website to scrape data from
#season_start_year <- c(2008,2009,2010,2011,2012,2013,2014,2015)
program_type <- c("to", "sp", "fs","fd","sd")
category <- c("m","l","p","d")
combination <- expand.grid( season_start_year=seq(2008,2015,by=1), program_type=c("to"), category = category)
combination <-bind_rows(combination, expand.grid( season_start_year=seq(2011,2015,by=1), program_type=c( "sp", "fs"), category = c("m","l","p")))
combination <-bind_rows(combination, expand.grid( season_start_year=seq(2011,2015,by=1), program_type=c ("fd","sd"), category = "d"))
#"http://www.isuresults.com/isujsstat/sb2015-16/sbtsmto.htm"
html <- paste0( "http://www.isuresults.com/isujsstat/sb",as.character(combination$season_start_year))
html <- paste0(html,"-")
html <- paste0(html,str_sub(as.character(combination$season_start_year+1), start = -2))
html <- paste0(html, "/sbts")
html <- paste0(html, combination$category)
html <- paste0(html, combination$program_type)
html <- paste0(html, ".htm")
html <-data.frame(html)

combination <-bind_cols(combination,html)

#only have data til 2011 for short program and free skating

#We create the helper function fix_name_column to format the names correctly. Because there is only 1 name column, we will want to split to first_name and last_name column. Also, for pair and ice dance, they name column contains both skaters' names, so this can help to split them to name_skater1 and name_skater_2, then we can split them into first_name and last_name column again.
fix_name_column <-function(name_list, expr){

g <- gregexpr(expr, name_list, fixed=TRUE)
last_name_pos = list()

for(i in 1:length(g))
{
  loc <- g[[i]]
  last_name_pos[length(last_name_pos)+1] <- loc[length(loc)] +1
  
}

last_name_pos.df <-do.call(rbind.data.frame, last_name_pos)

name.df <- data.frame(name_list)
colnames(last_name_pos.df) <- c("last_name_pos")
colnames(name.df) <- c("name")
name.df <-bind_cols(name.df,last_name_pos.df)

name.df <-
name.df %>%
mutate(first_name = str_sub(name,start= 1, end = last_name_pos-2),last_name=str_sub(name,last_name_pos )) %>%
select(first_name, last_name)

return (name.df)
}

#We created the helper function get_table_from_html to get a data frame from a table in a website.

get_table_from_html <- function(html.str, tableid){
ISU.html <- read_html(html.str)
raw.df <- 
ISU.html  %>%
html_nodes("table") %>%
.[[tableid]] %>%
html_table(fill= TRUE)
return(raw.df)
}

#We created the function get_season_best to get a data frame on SBS from a website.
#The data frame returned are in appropriate format.
#Season Best raw data
get_season_best <- function(ssy, c, st, html.str){

raw.df <-get_table_from_html(html.str, 1)
#clean up data
#1. only include columns with data
#2. exclude irrevelatent rows (ex. timestampe) in the table
cleaned.df <- 
raw.df[,1:6] %>% 
filter(!is.na(X3)) 

colnames(cleaned.df) <-c("rank","score","name","country","event","date")

if (c =="m"|c=="l"){
cleaned.df <-bind_cols(cleaned.df,fix_name_column(cleaned.df$name, " "))
}
else {
cleaned.df <-
cleaned.df %>%
mutate(name = str_replace(name," / ", "."))
skater.df <- fix_name_column(cleaned.df$name, ".")
colnames(skater.df) <-c("skater1", "skater2")
skater1.df  <- fix_name_column(skater.df$skater1, " ")
colnames(skater1.df) <-c("first_name_1", "last_name_1")
skater2.df  <- fix_name_column(skater.df$skater2, " ")
colnames(skater2.df) <-c("first_name_2", "last_name_2")
cleaned.df <-bind_cols(cleaned.df,skater1.df)
cleaned.df <-bind_cols(cleaned.df,skater2.df)

}

#fix date
cleaned.df <-
cleaned.df %>%
mutate (date = as.Date(date, format="%d.%m.%Y"))  %>%
mutate (season_start_year = ssy, 
season_end_year = ssy+1, 
category = c,
program_type = st,
rank = as.integer(as.character(rank))
)
#select relevent columns
if (c =="m"|c=="l"){
cleaned.df <-
cleaned.df %>%
select (
rank,
score ,
first_name,
last_name ,
country ,
event ,
date,
season_start_year,
season_end_year,
category,
program_type
) 
}
else{
cleaned.df <-
cleaned.df %>%
select (
rank,
score ,
first_name_1,
last_name_1,
first_name_2,
last_name_2,
country ,
event ,
date,
season_start_year,
season_end_year,
category,
program_type
) 


}
return (cleaned.df)
}

#Create an empty data frame and load the SBS data into the data frame by looping website.
SeasonBestScoreSingle.df <- 
data.frame(rank= integer(),
score = double(),
first_name= character(),
last_name = character(),
country = character(),
event = character(),
date=as.Date(character()),
season_start_year= integer(),
season_end_year= integer(),
category = character(),
program_type= character()
) 
SeasonBestScoreDouble.df <- 
data.frame(rank= integer(),
score = double(),
first_name_1= character(),
last_name_1 = character(),
first_name_2= character(),
last_name_2 = character(),
country = character(),
event = character(),
date=as.Date(character()),
season_start_year= integer(),
season_end_year= integer(),
category = character(),
program_type= character()
) 
for(i in 1:nrow(combination)){
category <- as.character(combination[i,]$category)
ssy <- as.integer(combination[i,]$season_start_year)
program_type <- as.character(combination[i,]$program_type)
html.str <-as.character(combination[i,]$html)

new.df<-get_season_best(
ssy, 
category, 
program_type,
html.str
)
if (category =="m"|category =="l")
{
  SeasonBestScoreSingle.df <- bind_rows(SeasonBestScoreSingle.df,new.df)
}
else{

SeasonBestScoreDouble.df <- bind_rows(SeasonBestScoreDouble.df,new.df)
}

}

head(SeasonBestScoreSingle.df)
head(SeasonBestScoreDouble.df)

#Part 2. Loading Event Result (ER)

#Create a data frame that contains a list of html for event result 
#to create a list of website to scrape data from

event_code <- c("fc","wc","ec","gpf")
#after reform of ice dance

#event_description <-"
#fc  Four Continents Championships
#"
#event_description <- read_csv(event_description, trim_ws = TRUE)

combination_event_result.df <- expand.grid( season_start_year= seq(2005,2015,by=1), event_code= c("fc","wc","ec"), program_id = seq(1,6,by=1))

combination_event_result.df <- bind_rows(combination_event_result.df,expand.grid( season_start_year= seq(2005,2014,by=1), event_code= c("gpf"), program_id = seq(1,6,by=1)))

#combination_event_result.df <-bind_rows(combination_event_result.df, expand.grid( season_start_year= seq(2005,2010,by=1), event_code= event_code,program_id = seq(1,9,by=1)))

#"http://www.isuresults.com/results/wc2004/SEG001.HTM"
combination_event_result.df <-
combination_event_result.df %>%
mutate (html = paste0( "http://www.isuresults.com/results/",event_code) ) %>%
#mutate (html = paste0(html,"/")   ) %>%
mutate (html = ifelse(event_code == "gpf",  paste0(html,paste0(
str_sub(as.character(season_start_year), start = -2),
str_sub(as.character(season_start_year+1), start = -2)
)) ,  
paste0(html,as.character(season_start_year)) )   ) %>%
mutate (html = paste0(html,"/SEG00")   ) %>%
mutate (html = paste0(html,as.character(program_id))   ) %>%
mutate (html = paste0(html,".HTM")   ) 


#We created a function to load the event result table from a html.
#Please note that the web page format has changed over time. So I create different loading caseto account for that.

#event result raw data
#ssy = 2011
# ec= "fc"
get_event_result <- function(ssy, ec, html.str){
#i <- 1
#html.str <- as.character(combination_event_result.df[i,]$html)

ISU.html <- read_html(html.str)

#load data
program_description <-  ISU.html  %>%
html_nodes(".caption2") %>%
html_text()
c <- strsplit(program_description, " - ")[[1]]  [1]
program_type <- strsplit(program_description, " - ")[[1]]  [2]

raw.df <- 
ISU.html  %>%
html_nodes("table") %>%
.[[3]] %>%
html_table(fill= TRUE)

#Clean up data
cleaned.df <- raw.df
if (ncol(cleaned.df) ==15) {
colnames(cleaned.df) <- c("PI","Q","name","country","TSS","TES", "emptyCol", "PCS","SS","TR","PE","CH","IN","Ded","StN")
}
else if (ncol(cleaned.df) ==14) {
colnames(cleaned.df) <- c("PI","name","country","TSS","TES", "emptyCol", "PCS","SS","TR","PE","CH","IN","Ded","StN")
cleaned.df <- 
cleaned.df %>%
mutate(Q=NA)
}

cleaned.df <-
cleaned.df %>%
select(-emptyCol) %>%
mutate(category = c, program_type = program_type, season_start_year = ssy, season_end_year = ssy +1, event_code = ec )

if (c =="Men"|c=="Ladies"|c =="Junior Men"|c=="Junior Ladies"){
cleaned.df <-bind_cols(cleaned.df,fix_name_column(cleaned.df$name, " "))
}
else {
cleaned.df <-
cleaned.df %>%
mutate(name = str_replace(name," / ", "."))
skater.df <- fix_name_column(cleaned.df$name, ".")
colnames(skater.df) <-c("skater1", "skater2")
skater1.df  <- fix_name_column(skater.df$skater1, " ")
colnames(skater1.df) <-c("first_name_1", "last_name_1")
skater2.df  <- fix_name_column(skater.df$skater2, " ")
colnames(skater2.df) <-c("first_name_2", "last_name_2")
cleaned.df <-bind_cols(cleaned.df,skater1.df)
cleaned.df <-bind_cols(cleaned.df,skater2.df)

}

cleaned.df <-
cleaned.df %>%
mutate(
PI = as.integer(PI) ,
TSS = as.double(TSS) ,
TES = as.double(TES) ,
PCS = as.double(PCS) ,
SS = as.double(SS) ,
TR = as.double(TR) ,
PE = as.double(PE) ,
CH = as.double(CH) ,
IN = as.double(IN) ,
Ded = as.double(Ded) 

) 

#select relevent columns
if (c =="Men"|c=="Ladies"|c =="Junior Men"|c=="Junior Ladies"){
cleaned.df <-
cleaned.df %>%
select (
PI ,
first_name,
last_name ,
country ,
TSS,
TES,
PCS,
SS,
TR,
PE,
CH,
IN,
Ded,
StN ,
Q,
event_code,
season_start_year,
season_end_year,
category,
program_type

) 
}
else{
cleaned.df <-
cleaned.df %>%
select (
PI ,
first_name_1,
last_name_1 ,
first_name_2,
last_name_2 ,
country ,
TSS,
TES,
PCS,
SS,
TR,
PE,
CH,
IN,
Ded,
StN ,
Q,
event_code,
season_start_year,
season_end_year,
category,
program_type

)     

}
return(cleaned.df)
}

#Create an empty data frame and load the event result data into the data frame by looping website.
EventResultSingle.df <- 
data.frame(PI= integer(),
first_name = character(),
last_name = character(),
country = character(),
TSS = double(),
TES = double(),
PCS = double(),
SS = double(),
TR = double(),
PE = double(),
CH = double(),
IN = double(),
Ded = double(),
StN = character(),
Q = character(),
event_code = character(),
season_start_year= integer(),
season_end_year= integer(),
category = character(),
program_type= character()
) 
EventResultDouble.df <- 
data.frame(PI= integer(),
first_name_1 = character(),
last_name_1 = character(),
first_name_2 = character(),
last_name_2 = character(),
country = character(),
TSS = double(),
TES = double(),
PCS = double(),
SS = double(),
TR = double(),
PE = double(),
CH = double(),
IN = double(),
Ded = double(),
StN = character(),
Q = character(),
event_code = character(),
season_start_year= integer(),
season_end_year= integer(),
category = character(),
program_type= character()
) 
for(i in 1:nrow(combination_event_result.df)){

html.str <- as.character(combination_event_result.df[i,]$html)
ssy <-  as.integer(combination_event_result.df[i,]$season_start_year)
ec <-as.character(combination_event_result.df[i,]$event_code)

new.df<-get_event_result(
ssy , ec, html.str
)
if (as.integer(combination_event_result.df[i,]$program_id)<5){
EventResultSingle.df <- bind_rows(EventResultSingle.df,new.df)    
}
else {
EventResultDouble.df <- bind_rows(EventResultDouble.df,new.df)
}
}

#Part 3. Loading Skater Biography Data
#We loaded skater biography data into data frame skater.df.
#Please note that we only load bio data of men and ladies. We did not load biography data on pair and ice dance. Because we decide that we will not do prediction for pair and ice dance. 
#There are 2 different formats for the bio data.
#Due to a long time to load the data, I store the loaded data into a skater.txt file.


#We created a function get_value_from_html_by_id to get text from a id in html.
#We created a function get_value_from_html_by_class to get text from a class in html.
#We used different version for different websites.

get_value_from_html_by_id <- function(ISU.html, id.str){
#id.str <- "FormView1_person_media_information_high_season_practice_placeLabel"
path <-'//*[@id="'
            path <-paste0(path,id.str)
            path <-paste0(path,'"]')
value <- ISU.html%>% 
html_nodes(xpath = path) %>%
html_text()
value <-as.character(value)
value <- str_trim(value, side = c("both"))
if (length(value) == 0){
value <-NA
}
return(value)
}
get_value_from_html_by_class <- function(ISU.html, class.str){

criteria <-paste0(".",class.str)
value <- ISU.html  %>%
html_nodes(criteria) %>%
html_text()
return(value)
}

#create a function to load skater bio into an empty data frame.

skaterInfoHTMLformat1.df <- read_csv("SkaterInfoHTMLformat1.csv", col_names=TRUE)
#skaterInfoHTMLformat2.df <- read_csv("SkaterInfoHTMLformat2.csv", col_names=TRUE)

#for format2
skaterInfoHTMLformat2.df <- "
ColumnName
dob
pob
height
hometown
occupation
hobbies
start_career
club_name
practice_on_ice_low_season
low_season_practice_place
practice_on_ice_high_season
high_season_practice_place
coach
choreographer
former_coach
"
skaterInfoHTMLformat2.df <- read_csv(skaterInfoHTMLformat2.df, trim_ws = TRUE, skip = 1)

skaterInfoHTMLformat2htmlName.df <- "
htmlName
Date of birth:
Place of birth:
Height:
Home town:
Profession:
Hobbies:
Start sk. / Club:
/
Practice low season:
Practice low season:
Practice high season:
Practice high season:
Coach:
Choreographer:
Former coach:
"
skaterInfoHTMLformat2htmlName.df <- read_csv(skaterInfoHTMLformat2htmlName.df, trim_ws = TRUE, skip = 1)
skaterInfoHTMLformat2.df <-bind_cols(skaterInfoHTMLformat2.df ,skaterInfoHTMLformat2htmlName.df )
rm(skaterInfoHTMLformat2htmlName.df)
skaterInfoHTMLformat2.df <-skaterInfoHTMLformat2.df %>%
mutate(pos= ifelse(ColumnName%in%c("coach","choreographer","former_coach","occupation","hometown","pob"), -1, 1)) %>%
mutate(pos= ifelse(ColumnName%in%c("practice_on_ice_low_season","practice_on_ice_high_season"), -3, pos))  %>%
mutate(pos= ifelse(ColumnName%in%c("low_season_practice_place","high_season_practice_place","start_career","club_name" ), -2, pos)) 


Skater.df <- 
data.frame(
name = character(),
country = character(),
dob = as.Date(character()),
pob = character(),
height = integer(),
hometown = character(),
occupation = character(),
hobbies = character(),
start_career = integer(),
club_name = character(),
coach = character(),
choreographer = character(),
former_coach = character(),
practice_on_ice_low_season = double(),
low_season_practice_place = character(),
practice_on_ice_high_season = double(),
high_season_practice_place = character(),
html = character()

)

#load data of skater
load_skater_info<-function(Skater.df){
skaterMainHTML <- c("http://www.isuresults.com/bios/fsbiosmen.htm","http://www.isuresults.com/bios/fsbiosladies.htm")

for (j in 1:length(skaterMainHTML)){
#html.str<-"http://www.isuresults.com/bios/fsbiosladies.htm"
#j<-1
htmlMain.str<-skaterMainHTML[j]
#raw.df <-get_table_from_html(html.str, 1)
ISUMain.html <- read_html(htmlMain.str)

url.list <- ISUMain.html  %>% 
html_nodes("a") %>% 
html_attr("href")
url.list <-  url.list[1:(length(url.list)-4)]
url.list <-paste0("http://www.isuresults.com",url.list)  

for( i in 1: length(url.list)){
#i <-1
html.str<-url.list[i]
#format 1
#html.str <- "http://www.isuresults.com/bios/isufs00013644.htm"
#format 2
#html.str<-"http://www.isuresults.com/bios/isufs00005472.htm"
ISU.html <- read_html(html.str)

title <- ISU.html  %>% 
html_nodes("title") %>% 
html_text()
# m<-1
#m = m+1
#get_value_from_html_by_id( ISU.html, "FormView1_person_media_information_former_coachLabel")
#mapply(get_value_from_html_by_id, rep(ISU.html,nrow(new.df)), new.df$ID)

#format 1
if (title != "Crystal Report Viewer") {
new.df <- skaterInfoHTMLformat1.df 


new.df <- data.frame(
ColumnName = character(),
data = character()
)

for (k in 1:nrow(skaterInfoHTMLformat1.df)){

ColumnName <- skaterInfoHTMLformat1.df$ColumnName[k]
data <- get_value_from_html_by_id(ISU.html,skaterInfoHTMLformat1.df$ID[k])
new.df <- bind_rows(new.df, data.frame(ColumnName,data))
}        
#new.df <- new.df %>%
#mutate(data = mapply(get_value_from_html_by_id, ISU.html, ID)) %>%
#select (ColumnName, data) %>%
#mutate(data= ifelse(data == "character(0)", NA, data))

new.df <- new.df %>%
mutate(data= ifelse(data == "", NA, data)) 
}else{
info.list <- ISU.html  %>%
html_nodes("div") %>%
html_text()

new.df <- data.frame(
ColumnName = character(),
data = character()
)
ColumnName <-"name"
data <-as.character(get_value_from_html_by_class( ISU.html, "fc1-3")[1])
new.df <- bind_rows(new.df, data.frame(ColumnName,data))
ColumnName <-"country"
data <- as.character(get_value_from_html_by_class( ISU.html, "fc1-3")[2])
new.df <- bind_rows(new.df, data.frame(ColumnName,data))

for (k in 1:nrow(skaterInfoHTMLformat2.df)){
#k<-1
#k<-k+1
ColumnName <- skaterInfoHTMLformat2.df$ColumnName[k]
htmlColName <- skaterInfoHTMLformat2.df$htmlName[k]
#the order of value for each field is at different relative position to the field name
pos<-as.integer(skaterInfoHTMLformat2.df$pos[k])
data<-info.list[match(htmlColName,info.list) + pos]
#the relative position may not be correct if we have null field for some of them
if (!is.na(match(data,skaterInfoHTMLformat2.df$htmlName))){
data <-NA
}else if (data %in%c("h / week ","cm")){
data <-NA
}else if(!ColumnName%in%c("practice_on_ice_low_season","practice_on_ice_high_season","start_career","height")&!is.na(as.integer(data))){
data <-NA
}


new.df <- bind_rows(new.df, data.frame(ColumnName,data))
}
}

n <- new.df$ColumnName
new.df <-as.data.frame(t(new.df[,-1]))
colnames(new.df) <- n

new.df <-
new.df %>%
mutate(            

name = as.character(name),
country = as.character(country),
dob =  as.Date(as.character(dob), format="%d.%m.%Y"),
pob = as.character(pob),
height = as.integer(as.character(height)),
hometown = as.character(hometown),
occupation = as.character(occupation),
hobbies = as.character(hobbies),
start_career = as.integer(as.character(start_career)),
club_name = as.character(club_name),
coach = as.character(coach),
choreographer = as.character(choreographer),
former_coach = as.character(former_coach),
practice_on_ice_low_season = as.double(as.character(practice_on_ice_low_season)),
low_season_practice_place = str_replace(as.character(low_season_practice_place) ,"at ", ""),
practice_on_ice_high_season = as.double(as.character(practice_on_ice_high_season)),
high_season_practice_place = str_replace(as.character(high_season_practice_place) ,"at ", ""),
html = html.str

) %>%
select(

name ,
country ,
dob ,
pob,
height ,
hometown ,
occupation ,
hobbies,
start_career ,
club_name,
coach,
choreographer ,
former_coach ,
practice_on_ice_low_season ,
low_season_practice_place ,
practice_on_ice_high_season ,
high_season_practice_place ,
html         
)

Skater.df<- bind_rows(Skater.df,new.df)
}

}
return(Skater.df)
}


#Skater.df <-load_skater_info(Skater.df)
#Skater.df <-bind_cols(Skater.df,fix_name_column(Skater.df$name, " "))

#write.table(Skater.df, "Skater.txt", quote =FALSE, sep = "\t", row.names = FALSE ,col.names = TRUE)

Skater.df  <- read_delim(file="Skater.txt",delim="\t", col_names=TRUE)

#create a category file to link each skater to men or ladies.

rm(new.df)
rm(html)

SkaterCategory.df <- 
data.frame(
html = character(),
category = character()
)
load_skater_category<-function(SkaterCategory.df){
skaterMainHTML <- c("http://www.isuresults.com/bios/fsbiosmen.htm","http://www.isuresults.com/bios/fsbiosladies.htm")
category<- c("m","l")

for (j in 1:length(skaterMainHTML)){
#html.str<-"http://www.isuresults.com/bios/fsbiosladies.htm"
#j<-1
htmlMain.str<-skaterMainHTML[j]
ISUMain.html <- read_html(htmlMain.str)
c <- category[j]
url.list <- ISUMain.html  %>% 
html_nodes("a") %>% 
html_attr("href")
url.list <-  url.list[1:(length(url.list)-4)]
url.list <-paste0("http://www.isuresults.com",url.list)  

for( i in 1: length(url.list)){
#i <-1
html.str<-url.list[i]
new.df <- data.frame(html = html.str, category = c)
SkaterCategory.df<- bind_rows(SkaterCategory.df,new.df)

}
}

return(SkaterCategory.df)
}

#SkaterCategory.df <-load_skater_info(SkaterCategory.df)
#write.table(SkaterCategory.df, "SkaterCategory.txt", quote =FALSE, sep = "\t", row.names = FALSE ,col.names = TRUE)

SkaterCategory.df  <- read_delim(file="SkaterCategory.txt",delim="\t", col_names=TRUE)


## DATA WRANGLING ###

#Part 1 Creating Bio dataframe

load("/Users/Isabella/Bio260/GroupProject/FinalProject.RData")

#Adding category(Men/Ladies) in SkaterCategory.df to Skater.df 
SkaterCategory.df <- read_delim(file="SkaterCategory.txt",delim="\t", col_names=TRUE)
Skater.df<- left_join(Skater.df,SkaterCategory.df,by="html")

#Cleaning process 
dim(Skater.df)
str(unique(Skater.df$name))
Skater.df$name[duplicated(Skater.df$name)]

Skater.df %>% filter(duplicated(name)==TRUE)
Skater.df %>% filter(name=="Yea-Ji SHIN")
#Two korean skaters who have the same name "Yea-Ji SHIN" ##
Skater.df %>% filter(name=="Zeljka KRIZMANIC") 
#Real duplicated skater "Zeljka KRIZMANIC" 
Skater.df %>% filter(name=="h / week ")
# Nonsense name 
bio<- Skater.df %>% filter(name!="h / week ") %>% filter(name!="Zeljka KRIZMANIC")
dup<- Skater.df %>% filter(name=="Zeljka KRIZMANIC") %>% head(n=1)
bio<- rbind(bio,dup)

dim(bio)
#2012 single skaters in total 

#Create numofskater(number of gender-specific skaters in each country)
bio<- bio %>% group_by(country,category) %>% mutate(numofskater=n()) %>% ungroup()
#Cut numofstaker into quintiles 
labels<- c("0~20%","20~40%","40~60%","60~80%","80~100%")
bio<- bio %>% group_by(category) %>% 
  mutate(num_quintile=cut(numofskater,
                          breaks=quantile(numofskater,prob=seq(0,1,0.2)),
                          include.lowest=TRUE,
                          labels=labels)) %>% 
  ungroup() 

bio<- bio %>% select(c(name,category,country,dob,height,start_career,
                       numofskater,num_quintile,first_name,last_name,html))
#Correct an outiler height based on information from Internet
bio<- bio%>% mutate(height=ifelse(height==16,160,height)) 
#Create continent variable
bio<- bio %>% mutate(continent=countrycode(country,"ioc","continent"))%>%
  mutate(continent=ifelse(country=="TPE","Asia",continent))

##Part 2: Wrangling EventResultsSingle Data & Creating Single Events Data w/o Bio##
EventResultSingle_wr<-EventResultSingle.df%>%
  mutate(full_name=paste(last_name,',',first_name), 
         event_year=paste(event_code,',',season_start_year))%>%
  select(-c(first_name_1,first_name_2,last_name_1,last_name_2))%>%
  filter(program_type%in%c('Free Skating','Short Program') & 
           category%in%c('Men','Ladies')&!is.na(last_name)&!is.na(first_name))
#Deleted empty name columns and created a full name column
#Added event_year column
#Also filtered for only Free Skating and Short Program, and Men and Ladies
#dim(EventResultSingle.df) #3868 rows, 24 Columns before wrangling

SP<-EventResultSingle_wr%>%filter(program_type=='Short Program')%>%
  rename(TSS_SP=TSS,TES_SP=TES,PCS_SP=PCS,SS_SP=SS,TR_SP=TR,
         PE_SP=PE,CH_SP=CH,IN_SP=IN,Ded_SP=Ded)%>%
  select(-c(program_type,Q,season_end_year,StN))

FS<-EventResultSingle_wr%>%filter(program_type=='Free Skating')%>%
  rename(TSS_FS=TSS,TES_FS=TES,PCS_FS=PCS,SS_FS=SS,TR_FS=TR,
         PE_FS=PE,CH_FS=CH,IN_FS=IN,Ded_FS=Ded)%>%
  select(full_name,event_year,TSS_FS,TES_FS,PCS_FS,SS_FS,TR_FS,PE_FS,CH_FS,IN_FS,Ded_FS)

single_event<-left_join(FS,SP,by=c("full_name","event_year"))%>%
  mutate(total_score=TSS_SP+TSS_FS)

##Part 3: Combine "bio" with "EventResultsSingle_wr" and "SeasonBestSingle.df"
#Get rid of duplicated name, Choose the one based on competition season 
SHIN<- bio %>% filter(name=="Yea-Ji SHIN" & year(dob)==1988)
bio2<- bio %>% filter(name!="Yea-Ji SHIN") 
bio2<- rbind(bio2,SHIN) 

bio_single<- bio2 %>% select(-c(category))
seasonbestsingle_bio<- 
  left_join(SeasonBestScoreSingle.df,bio2,by=c("first_name","last_name","country","category"))
#Create age (year difference between date of birth to event date) and 
#career_length (year difference between event date and start_career year)
seasonbestsingle_bio<- seasonbestsingle_bio %>% 
  mutate(span=interval(dob,date),age=year(as.period(span,unite="year")),
         career_length=round(year(date)-start_career)) %>%
  select(-span)

eventsingle_bio<- 
  left_join(EventResultSingle_wr,bio_single,by=c("first_name","last_name","country"))
eventsingle_bio<-eventsingle_bio %>% 
  mutate(year=(season_start_year+season_end_year)/2) %>% 
  mutate(age=round(year-year(dob)),career_length=round(year-start_career)) %>%
  select(-year) 

##Part 4: Wrangle eventresult_bio

tab<- eventsingle_bio %>% filter(program_type=="Short Program") %>% 
  mutate(TSS_SP=TSS,TES_SP=TES,PCS_SP=PCS,SS_SP=SS,TR_SP=TR,
         PE_SP=PE,CH_SP=CH,IN_SP=IN,Ded_SP=Ded) %>%
  select(c(first_name,last_name,TSS_SP,TES_SP,PCS_SP,SS_SP,TR_SP,PE_SP,CH_SP,IN_SP,
           Ded_SP,season_start_year,season_end_year,category,event_code))

eventsingle_bio<- eventsingle_bio %>% filter(program_type=="Free Skating")

single_event_bio<- 
  inner_join(tab,eventsingle_bio,
             by=c("first_name","last_name","season_start_year",
                  "season_end_year","category","event_code")) %>% 
  mutate(total=TSS_SP+TSS) %>% select(-c(program_type,Q)) %>% 
  group_by(season_start_year,category,event_code) %>% arrange(desc(total)) %>% 
  mutate(ranking=row_number()) %>% ungroup() %>% distinct()

single_event_bio<-single_event_bio %>% mutate(event_year=paste(event_code,',',season_start_year), full_name=paste(last_name,',',first_name)) %>%
  group_by(category) %>%
  mutate(country_rank=ntile(numofskater,5)) %>% ungroup()

#Final data sets to Use for analysis: EventResultSingle_wr, single_event_bio, seasonbestsingle_bio, single_event
#Saved the final data sets as 'Final Datasets.RData'to be used for analysis


