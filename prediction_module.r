### Prediction Module###
########################

# read in data here
# source data as an example
IMDBdata = read.csv('films_fall_2021_training_data - films_fall_2021.csv')

# Load look up table
# Note that we do not want R to read in 'NA' (North America) as Null values
continent = read.csv('countries and continents.csv', na.strings = "NNN")
imdb_stuff_tier = read.csv('IMDB_tier_lookup.csv')

# Handling NA if there's any -> for actors/producers/directors/editors/production companies mapping
# Transform main language
IMDBdata$main_lang_category <- "Others"
IMDBdata$main_lang_category[IMDBdata$main_lang == "English"] <- "English"
IMDBdata$main_lang_category[IMDBdata$main_lang == "Français"] <- "Français"
IMDBdata$main_lang_category[IMDBdata$main_lang == "Deutsch"] <- "Deutsch"
IMDBdata$main_lang_category[IMDBdata$main_lang == "Español"] <- "Español"
IMDBdata$main_lang_category[IMDBdata$main_lang == "No Language"] <- "No Language"

# Transform main production country
IMDBdata = merge(IMDBdata,continent,by.x="main_production_country",by.y = "name")

# Transform actors
dropList <- c('avg_score',"name","role",'n')
actors_tier = imdb_stuff_tier[imdb_stuff_tier$role == 'Actor',]
debut_actor_avg_score = mean(actors_tier[actors_tier$n == 1,]$avg_score)
debut_actor_tier = ifelse(debut_actor_avg_score>=8,'Top',
                          ifelse(debut_actor_avg_score<=4,'Poor',
                                 ifelse(debut_actor_avg_score>4&debut_actor_avg_score<6,'Lower',
                                        'Medium')))

IMDBdata1 = merge(IMDBdata,actors_tier,by.x="main_actor1_name",by.y = "name")
names(IMDBdata1)[names(IMDBdata1) == 'tier'] <- 'actor1_tier'
IMDBdata1 = IMDBdata1[, !colnames(IMDBdata1) %in% dropList]
debut_actor_avg_score
IMDBdata1[is.na(IMDBdata1$actor1_tier)] <- debut_actor_tier

IMDBdata2= merge(IMDBdata1,actors_tier,by.x="main_actor2_name",by.y = "name")
names(IMDBdata2)[names(IMDBdata2) == 'tier'] <- 'actor2_tier'
IMDBdata2 = IMDBdata2[, !colnames(IMDBdata2) %in% dropList]
IMDBdata2[is.na(IMDBdata2$actor2_tier)] <- debut_actor_tier 

IMDBdata3 = merge(IMDBdata2,actors_tier,by.x="main_actor3_name",by.y = "name")
names(IMDBdata3)[names(IMDBdata3) == 'tier'] <- 'actor3_tier'
IMDBdata3 = IMDBdata3[, !colnames(IMDBdata3) %in% dropList]
IMDBdata3[is.na(IMDBdata3$actor3_tier)] <- debut_actor_tier

# Transform directors
directors_tier = imdb_stuff_tier[imdb_stuff_tier$role == 'Director',]
debut_director_avg_score = mean(directors_tier[directors_tier$n == 1,]$avg_score)
debut_director_tier = ifelse(debut_director_avg_score>=8,'Top',
                          ifelse(debut_director_avg_score<=4,'Poor',
                                 ifelse(debut_director_avg_score>4&debut_director_avg_score<6,'Lower',
                                        'Medium')))

IMDBdata4 = merge(IMDBdata3,directors_tier,by.x="main_director_name",by.y = "name")
names(IMDBdata4)[names(IMDBdata4) == 'tier'] <- 'director_tier'
IMDBdata4 = IMDBdata4[, !colnames(IMDBdata4) %in% dropList]
IMDBdata4[is.na(IMDBdata4$director_tier)] <- debut_director_tier

# Transform editors
editors_tier = imdb_stuff_tier[imdb_stuff_tier$role == 'Editor',]
debut_editor_avg_score = mean(directors_tier[directors_tier$n == 1,]$avg_score)
debut_editor_tier = ifelse(debut_editor_avg_score>=8,'Top',
                             ifelse(debut_editor_avg_score<=4,'Poor',
                                    ifelse(debut_editor_avg_score>4&debut_editor_avg_score<6,'Lower',
                                           'Medium')))

IMDBdata5 = merge(IMDBdata4,editors_tier,by.x="editor_name",by.y = "name")
names(IMDBdata5)[names(IMDBdata5) == 'tier'] <- 'editor_tier'
IMDBdata5 = IMDBdata5[, !colnames(IMDBdata5) %in% dropList]
IMDBdata5[is.na(IMDBdata5$editor_tier)] <- debut_editor_tier

# Transform producers
producers_tier = imdb_stuff_tier[imdb_stuff_tier$role == 'Producer',]
debut_producer_avg_score = mean(producers_tier[producers_tier$n == 1,]$avg_score)
debut_producer_tier = ifelse(debut_producer_avg_score>=8,'Top',
                           ifelse(debut_producer_avg_score<=4,'Poor',
                                  ifelse(debut_producer_avg_score>4&debut_producer_avg_score<6,'Lower',
                                         'Medium')))

IMDBdata6 = merge(IMDBdata5,producers_tier,by.x="main_producer_name",by.y = "name")
names(IMDBdata6)[names(IMDBdata6) == 'tier'] <- 'producer_tier'
IMDBdata6 = IMDBdata6[, !colnames(IMDBdata6) %in% dropList]
IMDBdata6[is.na(IMDBdata6$producer_tier)] <- debut_producer_tier

# Transform production_company
prod_companies_tier = imdb_stuff_tier[imdb_stuff_tier$role == 'Production Company',]
debut_prod_company_avg_score = mean(prod_companies_tier[prod_companies_tier$n == 1,]$avg_score)
debut_prod_company_tier = ifelse(debut_prod_company_avg_score>=8,'Top',
                             ifelse(debut_prod_company_avg_score<=4,'Poor',
                                    ifelse(debut_prod_company_avg_score>4&debut_prod_company_avg_score<6,'Lower',
                                           'Medium')))

IMDBdata7 = merge(IMDBdata6,prod_companies_tier,by.x="main_production_company",by.y = "name")
names(IMDBdata7)[names(IMDBdata7) == 'tier'] <- 'production_company_tier'
IMDBdata7 = IMDBdata7[, !colnames(IMDBdata7) %in% dropList]
IMDBdata7[is.na(IMDBdata7$producer_tier)] <- debut_prod_company_tier

IMDBdata_r = IMDBdata7

factor_cols = c('main_lang', "genre_action", "genre_adventure", "genre_animation",
                "genre_biography", "genre_comedy", "genre_crime", "genre_documentary",
                "genre_drama","genre_family","genre_fantasy","genre_filmnoir",
                "genre_history","genre_horror","genre_music","genre_musical",
                "genre_mystery","genre_realitytv","genre_romance","genre_scifi",
                "genre_shortfilm","genre_sport","genre_thriller","genre_war",
                "genre_western","main_actor1_is_female","main_actor2_is_female",
                "main_actor3_is_female","main_director_is_female","main_production_country",
                "actor1_tier","actor2_tier","actor3_tier","director_tier","editor_tier",
                "producer_tier","production_company_tier","main_lang_category",'Continent')
IMDBdata_r[factor_cols] <- lapply(IMDBdata_r[factor_cols], factor)

cont_cols = c("budget_in_millions","month_of_release",
              "year_of_release","duration_in_hours",
              "total_number_languages","total_number_of_actors",
              "total_number_of_directors",
              "total_number_of_producers",
              "total_number_of_production_companies",
              "total_number_of_production_countries"
)
IMDBdata_pred = IMDBdata_r[,c('imdb_score',factor_cols,cont_cols)]

# Load saved model and predict
pred_model <- readRDS('model.rds')
pred_result = predict(pred_model,IMDBdata_pred)
print(pred_result)
