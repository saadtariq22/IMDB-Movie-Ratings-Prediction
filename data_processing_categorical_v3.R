
library(dplyr)
#######################################################
# Actor, editor, producer, production company mapping
#######################################################
# Read in data
IMDB_data = read.csv('films_fall_2021_training_data - films_fall_2021.csv')
IMDB_data$genre_count = IMDB_data$genre_action+IMDB_data$genre_adventure+
  IMDB_data$genre_animation+IMDB_data$genre_biography+IMDB_data$genre_comedy+IMDB_data$genre_crime+
  IMDB_data$genre_documentary+IMDB_data$genre_drama+IMDB_data$genre_family+IMDB_data$genre_fantasy+
  IMDB_data$genre_filmnoir+
  IMDB_data$genre_history+IMDB_data$genre_horror+IMDB_data$genre_music+IMDB_data$genre_musical+
  IMDB_data$genre_mystery+IMDB_data$genre_realitytv+
  IMDB_data$genre_romance+IMDB_data$genre_scifi+IMDB_data$genre_shortfilm+IMDB_data$genre_sport+
  IMDB_data$genre_thriller+IMDB_data$genre_war+IMDB_data$genre_western

# Assemble consolidated list of actors with ratings of movies they acted in
ActorData1 = IMDB_data[,c('main_actor1_name','imdb_score')]
colnames(ActorData1) <- c('name','score')
ActorData2 = IMDB_data[,c('main_actor2_name','imdb_score')]
colnames(ActorData2) <- c('name','score')
ActorData3 = IMDB_data[,c('main_actor3_name','imdb_score')]
colnames(ActorData3) <- c('name','score')
ActorScore = union_all(union_all(ActorData1,ActorData2),ActorData3)

actor_score = ActorScore %>%
  group_by(name) %>%
  summarise(avg_score = mean(score),n = n())

actor_score$tier = ifelse(actor_score$avg_score>=8,'Top',
                                    ifelse(actor_score$avg_score<=4,'Poor',
                                           ifelse(actor_score$avg_score>4&actor_score$avg_score<6,'Lower',
                                                  'Medium')))
actor_score$role = 'Actor'

# Assemble similar consolidated list of directors with average ratings of movies
Director = IMDB_data[,c('main_director_name','imdb_score')]
colnames(Director) <- c('name','score')

director_avg_score = Director %>%
  group_by(name) %>%
  summarise(avg_score = mean(score),n = n())

director_avg_score$tier = ifelse(director_avg_score$avg_score>=8,'Top',
                                          ifelse(director_avg_score$avg_score<=4,'Poor',
                                                 ifelse(director_avg_score$avg_score>4&director_avg_score$avg_score<6,'Lower',
                                                        'Medium')))
director_avg_score$role = 'Director'

# Assemble similar consolidated list of editors with average ratings of movies
Editor = IMDB_data[,c('editor_name','imdb_score')]
colnames(Editor) <- c('name','score')

editor_avg_score = Editor %>%
  group_by(name) %>%
  summarise(avg_score = mean(score),n = n())


editor_avg_score$tier = ifelse(editor_avg_score$avg_score>=8,'Top',
                                      ifelse(editor_avg_score$avg_score<=4,'Poor',
                                             ifelse(editor_avg_score$avg_score>4&editor_avg_score$avg_score<6,'Lower',
                                                    'Medium')))
editor_avg_score$role = 'Editor'

# Assemble similar consolidated list of producers with average ratings of movies
Producer = IMDB_data[,c('main_producer_name','imdb_score')]
colnames(Producer) <- c('name','score')

producer_avg_score = Producer %>%
  group_by(name) %>%
  summarise(avg_score = mean(score),n = n())

producer_avg_score$tier = ifelse(producer_avg_score$avg_score>=8,'Top',
                                 ifelse(producer_avg_score$avg_score<=4,'Poor',
                                        ifelse(producer_avg_score$avg_score>4&producer_avg_score$avg_score<6,'Lower',
                                                        'Medium')))
producer_avg_score$role = 'Producer'

# Assemble similar consolidated list of production companies with average ratings of movies
Prod_company = IMDB_data[,c('main_production_company','imdb_score')]
colnames(Prod_company) <- c('name','score')

prod_company = Prod_company %>%
  group_by(name) %>%
  summarise(avg_score = mean(score),n = n())

prod_company$tier = ifelse(prod_company$avg_score>=8,'Top',
                                        ifelse(prod_company$avg_score<=4,'Poor',
                                               ifelse(prod_company$avg_score>4&prod_company$avg_score<6,'Lower',
                                                      'Medium')))
prod_company$role = 'Production Company'

# Consolidated all tier information into one table for export
consolidated_lookup = union_all(union_all(union_all(union_all(actor_score,director_avg_score),
                                editor_avg_score),producer_avg_score),prod_company)
View(consolidated_lookup)
write.csv(consolidated_lookup,"IMDB_tier_lookup.csv",
          row.names = FALSE)


#Map back to the main df
IMDB_mapped1 = merge(IMDB_data,actor_score,by.x="main_actor1_name",by.y = "name")
names(IMDB_mapped1)[names(IMDB_mapped1) == 'tier'] <- 'actor1_tier'
names(IMDB_mapped1)[names(IMDB_mapped1) == 'avg_score'] <- 'actor1_avg_score'
IMDB_mapped1
dropList <- c("name","role",'n')
IMDB_mapped1 = IMDB_mapped1[, !colnames(IMDB_mapped1) %in% dropList]

IMDB_mapped2 = merge(IMDB_mapped1,actor_score,by.x="main_actor2_name",by.y = "name")
names(IMDB_mapped2)[names(IMDB_mapped2) == 'tier'] <- 'actor2_tier'
names(IMDB_mapped2)[names(IMDB_mapped2) == 'avg_score'] <- 'actor2_avg_score'
dropList <- c("name","role",'n')
IMDB_mapped2 = IMDB_mapped2[, !colnames(IMDB_mapped2) %in% dropList]

IMDB_mapped3 = merge(IMDB_mapped2,actor_score,by.x="main_actor3_name",by.y = "name")
names(IMDB_mapped3)[names(IMDB_mapped3) == 'tier'] <- 'actor3_tier'
names(IMDB_mapped3)[names(IMDB_mapped3) == 'avg_score'] <- 'actor3_avg_score'
dropList <- c("name","role",'n')
IMDB_mapped3 = IMDB_mapped3[, !colnames(IMDB_mapped3) %in% dropList]

IMDB_mapped4 = merge(IMDB_mapped3,director_avg_score,by.x="main_director_name",by.y = "name")
names(IMDB_mapped4)[names(IMDB_mapped4) == 'tier'] <- 'director_tier'
names(IMDB_mapped4)[names(IMDB_mapped4) == 'avg_score'] <- 'director_avg_score'
dropList <- c("name","role",'n')
IMDB_mapped4 = IMDB_mapped4[, !colnames(IMDB_mapped4) %in% dropList]

IMDB_mapped5 = merge(IMDB_mapped4,editor_avg_score,by.x="editor_name",by.y = "name")
names(IMDB_mapped5)[names(IMDB_mapped5) == 'tier'] <- 'editor_tier'
names(IMDB_mapped5)[names(IMDB_mapped5) == 'avg_score'] <- 'editor_avg_score'
dropList <- c("name","role",'n')
IMDB_mapped5 = IMDB_mapped5[, !colnames(IMDB_mapped5) %in% dropList]

IMDB_mapped6 = merge(IMDB_mapped5,producer_avg_score,by.x="main_producer_name",by.y = "name")
names(IMDB_mapped6)[names(IMDB_mapped6) == 'tier'] <- 'producer_tier'
names(IMDB_mapped6)[names(IMDB_mapped6) == 'avg_score'] <- 'producer_avg_score'
dropList <- c("name","role",'n')
IMDB_mapped6 = IMDB_mapped6[, !colnames(IMDB_mapped6) %in% dropList]

IMDB_mapped7 = merge(IMDB_mapped6,prod_company,by.x="main_production_company",by.y = "name")
names(IMDB_mapped7)[names(IMDB_mapped7) == 'tier'] <- 'production_company_tier'
names(IMDB_mapped7)[names(IMDB_mapped7) == 'avg_score'] <- 'production_company_avg_score'
dropList <- c("name","role",'n')
IMDB_mapped7 = IMDB_mapped7[, !colnames(IMDB_mapped7) %in% dropList]
IMDB_mapped7

#######################################################
# Language Mapping
#######################################################
# Frequency table for each language appeared in IMDB data
my_summary <- IMDB_mapped7 %>%
  count(main_lang, sort = TRUE) 
my_summary
# We will keep 4 most frequent languages, no language, and map all others as 'Others'
IMDB_mapped7$main_lang_category <- "Others"
IMDB_mapped7$main_lang_category[IMDB_mapped7$main_lang == "English"] <- "English"
IMDB_mapped7$main_lang_category[IMDB_mapped7$main_lang == "Français"] <- "Français"
IMDB_mapped7$main_lang_category[IMDB_mapped7$main_lang == "Deutsch"] <- "Deutsch"
IMDB_mapped7$main_lang_category[IMDB_mapped7$main_lang == "Español"] <- "Español"
IMDB_mapped7$main_lang_category[IMDB_mapped7$main_lang == "No Language"] <- "No Language"
#IMDB_mapped7$main_lang_category=as.factor(IMDB_mapped7$main_lang_category)

#######################################################
# Season Mapping
#######################################################
#Converting months into seasons
IMDB_mapped7$seasons <- "Winter"
IMDB_mapped7$seasons[is.element(IMDB_mapped7$month_of_release, c(3,4,5))] <- "Spring"
IMDB_mapped7$seasons[is.element(IMDB_mapped7$month_of_release, c(6,7,8))] <- "Summer"
IMDB_mapped7$seasons[is.element(IMDB_mapped7$month_of_release, c(9,10,11))] <- "Fall"
IMDB_mapped7

#######################################################
# Country Mapping
#######################################################
table(IMDB_mapped7$main_production_country)
unique(IMDB_mapped7$main_production_country)
continent_info = read.csv('countries and continents.csv', na.strings = "NNN")
IMDB_mapped8 = merge(IMDB_mapped7,continent_info,by.x="main_production_country",by.y = "name")
colnames(IMDB_mapped8)

View(IMDB_mapped8)
# Save mapped IMDB data 
write.csv(IMDB_mapped8,file="IMDB_export_complete.csv",
          row.names = FALSE)




