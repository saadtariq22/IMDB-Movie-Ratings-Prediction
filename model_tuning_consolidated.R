##########Load Libraries###############
library(Hmisc)
library(dplyr)
library(ggplot2)
library(ggformula)
library(splines)
library(corrplot)
library(e1071)
library(boot)
require(methods)
library(mgcv)
require(caTools)
library(gridExtra)
library(stargazer)
library(car)
##########Load Data####################
IMDB_modified_data = read.csv('IMDB_export_complete.csv', na.strings = "NNN")
factor_cols = c('main_lang', "genre_action", "genre_adventure", "genre_animation",
                "genre_biography", "genre_comedy", "genre_crime", "genre_documentary",
                "genre_drama","genre_family","genre_fantasy","genre_filmnoir",
                "genre_history","genre_horror","genre_music","genre_musical",
                "genre_mystery","genre_realitytv","genre_romance","genre_scifi",
                "genre_shortfilm","genre_sport","genre_thriller","genre_war",
                "genre_western","main_actor1_is_female","main_actor2_is_female",
                "main_actor3_is_female","main_director_is_female","main_production_country",
                "actor1_tier","actor2_tier","actor3_tier","director_tier","editor_tier",
                "producer_tier","production_company_tier","main_lang_category","seasons",'Continent')
IMDB_modified_data[factor_cols] <- lapply(IMDB_modified_data[factor_cols], factor)
table(apply(IMDB_modified_data[factor_cols], 2, function(a) length(unique(a))==1))
attach(IMDB_modified_data)
cont_cols = c("budget_in_millions","month_of_release",
              "year_of_release","duration_in_hours",
              "genre_count",
              "total_number_languages","total_number_of_actors",
              "total_number_of_directors",
              "total_number_of_producers",
              "total_number_of_production_companies",
              "total_number_of_production_countries"
)
numeric_predictors <- IMDB_modified_data[,cont_cols]

##########Helper Function###############
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

report <- function(feature){
  print("Plot the feature vs target variable")
  plot(feature,imdb_score)
  print("-------------------------------")
  print("Skewness")
  print(skewness(feature))
  print("Median Mean Min Max")
  print(summary(feature))
  print("-------------------------------")
  print("Stan Dev")
  print(sd(feature))
}

evalModel <- function(gmodel,lmodel,model_type){
  DF = data.frame(model=character(),mse=double(), R2=double(), AdjR2=double())
  mseCur = cv.glm(IMDB_modified_data,gmodel,K=10)$delta[1]
  DF[nrow(DF) + 1,] = list(name=model_type, mse=mseCur, R2=summary(lmodel)$r.squared, AdjR2=summary(lmodel)$adj.r.squared)
  return(DF)
}

splineFit <- function(d, feature){
  splineDF = data.frame(model=character(),knots=character(), R2=double(), AdjR2=double())
  for(i in 1:d){
    k1 = quantile(feature, 0.5)
    spl1 = lm(imdb_score~bs(feature, knots=c(k1), degree=i))
    splineDF[nrow(splineDF) + 1,] = list(model=paste("Degree",i,sep=""),knots=paste("Knots:",k1,sep=" "),R2=summary(spl1)$r.squared,AdjR2=summary(spl1)$adj.r.squared)
    k1 = quantile(feature, 0.33)
    k2 = quantile(feature, 0.66)
    spl2 = lm(imdb_score~bs(feature, knots=c(k1,k2), degree=i))
    splineDF[nrow(splineDF) + 1,] = list(model=paste("Degree",i,sep=""),knots=paste("Knots:",k1,k2,sep=" "),R2=summary(spl2)$r.squared,AdjR2=summary(spl2)$adj.r.squared)
    k1 = quantile(feature, 0.25)
    k2 = quantile(feature, 0.5)
    k3 = quantile(feature, 0.75)
    spl3 = lm(imdb_score~bs(feature, knots=c(k1,k2,k3), degree=i))
    splineDF[nrow(splineDF) + 1,] = list(model=paste("Degree",i,sep=""),knots=paste("Knots:",k1,k2,k3,sep=" "),R2=summary(spl3)$r.squared,AdjR2=summary(spl3)$adj.r.squared)
    k1 = quantile(feature, 0.2)
    k2 = quantile(feature, 0.4)
    k3 = quantile(feature, 0.6)
    k4 = quantile(feature, 0.8)
    spl4 = lm(imdb_score~bs(feature, knots=c(k1,k2,k3,k4), degree=i))
    splineDF[nrow(splineDF) + 1,] = list(model=paste("Degree",i,sep=""),knots=paste("Knots:",k1,k2,k3,k4,sep=" "),R2=summary(spl4)$r.squared,AdjR2=summary(spl4)$adj.r.squared) 
    print(paste("Degree ",i,sep=""))
    print(anova(spl1,spl2,spl3,spl4))
  }
  return(splineDF)
}
##########Correlation Analysis##########
corr_numeric <- rcorr(as.matrix(numeric_predictors))
t1 <- flattenCorrMatrix(corr_numeric$r, corr_numeric$P)
Highly_Correlated = subset(t1, abs(cor)>0.8)
corrplot(cor(numeric_predictors), type="upper")
Highly_Correlated
# No continous predictors have high correlation

##########Predictor Discovery###########
## budget_in_millions
report(budget_in_millions)
# Shows: 1. non-constant variance  2. polynomial characteristics
lm_1_l = lm(imdb_score~budget_in_millions)
lm_1_g = glm(imdb_score~budget_in_millions)
model_performance = evalModel(lm_1_g,lm_1_l,paste('Degree:',1,sep=' '))
degrees = c(2,3,4,5,6)
model_list = c()
for (i in degrees) {
  print(paste('Poly Spline Degree:',i,sep=' '))
  model_l = lm(imdb_score~poly(budget_in_millions, degree = i))
  model_g = glm(imdb_score~poly(budget_in_millions, degree = i))
  print(evalModel(model_g,model_l,paste('Poly Degree:',i,sep=' ')))
  result = evalModel(model_g,model_l,paste('Poly Degree:',i,sep=' '))
  model_performance = union_all(model_performance,result)
  model_list = c(model_list, model_l)
}
model_performance

# This shows:
# As degree goes up, MSE decreases while R2 increases.
# MSE stops decreasing at degree = 4
# we will test this predictor with polynomial with degree 2, 3, 4 in the main model

# ANOVA test
poly1 = lm(imdb_score~budget_in_millions)
poly2 = lm(imdb_score~poly(budget_in_millions, degree = 2))
poly3 = lm(imdb_score~poly(budget_in_millions, degree = 3))
poly4 = lm(imdb_score~poly(budget_in_millions, degree = 4))
anova(poly1,poly2,poly3,poly4)
# degree 3 or 4

## month_of_release
report(month_of_release)
plot(month_of_release,imdb_score)
smooth_1_month = smooth.spline(month_of_release,imdb_score,spar=0.001)
smooth_2_month = smooth.spline(month_of_release,imdb_score,spar=0.5)
smooth_3_month = smooth.spline(month_of_release,imdb_score,spar=5)
ggplot(IMDB_modified_data, aes(y=imdb_score, x=month_of_release)) +
  geom_point() + geom_spline(spar=0.0001, col='blue')
# Seasonality: roughly every 3-4 months, thus we should consider 2-3 knots
# knots position via intuition
knots_1 = 8
knots_2 = c(2,8)
knots_3 = c(2,5,8)
degrees = c(1,2,3)

bspline1_1 = lm(imdb_score~bs(month_of_release, knots = knots_1, degree = 1))
bspline1_2 = lm(imdb_score~bs(month_of_release, knots = knots_1, degree = 2))
bspline1_3 = lm(imdb_score~bs(month_of_release, knots = knots_1, degree = 3))
summary(bspline1_1)
summary(bspline1_2)
summary(bspline1_3)
bspline2_1 = lm(imdb_score~bs(month_of_release, knots = knots_2, degree = 1))
bspline2_2 = lm(imdb_score~bs(month_of_release, knots = knots_2, degree = 2))
bspline2_3 = lm(imdb_score~bs(month_of_release, knots = knots_2, degree = 3))
summary(bspline2_1)
summary(bspline2_2)
summary(bspline2_3)
bspline3_1 = lm(imdb_score~bs(month_of_release, knots = knots_3, degree = 1))
bspline3_2 = lm(imdb_score~bs(month_of_release, knots = knots_3, degree = 2))
bspline3_3 = lm(imdb_score~bs(month_of_release, knots = knots_3, degree = 3))
summary(bspline3_1)
summary(bspline3_2)
summary(bspline3_3)
# So far 3 knots with degree 1 has the highest R^2
# Visualization
ggplot(IMDB_modified_data, aes(y=imdb_score, x=month_of_release)) +
  geom_point() + geom_spline(spar=0.0001, col='blue') +
  geom_smooth(method = 'lm',formula = y~bs(x, knots=knots_3, degree=3),col="red") +
  geom_vline(xintercept=knots_1,lty=2) 
# We have a categorical variable seasons that breaks months into different season,
# In model tuning, we will compare MSE and R^2 to choose which to include

## year_of_release
report(year_of_release)
# We see similar problem with budget
lm_3_l = lm(imdb_score~year_of_release)
lm_3_g = glm(imdb_score~year_of_release)
model_performance = evalModel(lm_3_g,lm_3_l,paste('Degree:',1,sep=' '))
degrees = c(2,3,4)
for (i in degrees) {
  print(paste('Poly Spline Degree:',i,sep=' '))
  model_l = lm(imdb_score~poly(year_of_release, degree = i))
  model_g = glm(imdb_score~poly(year_of_release, degree = i))
  print(evalModel(model_g,model_l,paste('Poly Degree:',i,sep=' ')))
  result = evalModel(model_g,model_l,paste('Poly Degree:',i,sep=' '))
  model_performance = union_all(model_performance,result)
}
model_performance

# This shows:
# As degree goes up, MSE decreases while R2 increases
# we will test this predictor with polynomial with degree 2 or 3 in the main model

# ANOVA test
poly1 = lm(imdb_score~year_of_release)
poly2 = lm(imdb_score~poly(year_of_release, degree = 2))
poly3 = lm(imdb_score~poly(year_of_release, degree = 3))
poly4 = lm(imdb_score~poly(year_of_release, degree = 4))
anova(poly1,poly2,poly3,poly4)
# Indicating degree 2 or 3

## duration_in_hours
report(duration_in_hours)
# This predictor shows polynomial characteristics and non-constant variance
lm_4_l = lm(imdb_score~duration_in_hours)
lm_4_g = glm(imdb_score~duration_in_hours)
model_performance = evalModel(lm_4_g,lm_4_l,paste('Degree:',1,sep=' '))
degrees = c(2,3,4,5)
for (i in degrees) {
  print(paste('Poly Spline Degree:',i,sep=' '))
  model_l = lm(imdb_score~poly(duration_in_hours, degree = i))
  model_g = glm(imdb_score~poly(duration_in_hours, degree = i))
  print(evalModel(model_g,model_l,paste('Poly Degree:',i,sep=' ')))
  result = evalModel(model_g,model_l,paste('Poly Degree:',i,sep=' '))
  model_performance = union_all(model_performance,result)
}
model_performance
# ANOVA test so we don't overfit
poly1 = lm(imdb_score~duration_in_hours)
poly2 = lm(imdb_score~poly(duration_in_hours, degree = 2))
poly3 = lm(imdb_score~poly(duration_in_hours, degree = 3))
poly4 = lm(imdb_score~poly(duration_in_hours, degree = 4))
poly5 = lm(imdb_score~poly(duration_in_hours, degree = 5))
anova(poly1,poly2,poly3,poly4,poly5)
# Indicating degree 4

## genre_count
report(genre_count)
# Most of the movies have 3 listed genres
lm_11_l = lm(imdb_score~genre_count)
lm_11_g = glm(imdb_score~genre_count)
model_performance = evalModel(lm_11_g,lm_11_l,paste('Degree:',1,sep=' '))
degrees = c(2)
for (i in degrees) {
  print(paste('Poly Spline Degree:',i,sep=' '))
  model_l = lm(imdb_score~poly(genre_count, degree = i))
  model_g = glm(imdb_score~poly(genre_count, degree = i))
  print(evalModel(model_g,model_l,paste('Poly Degree:',i,sep=' ')))
  result = evalModel(model_g,model_l,paste('Poly Degree:',i,sep=' '))
  model_performance = union_all(model_performance,result)
}
model_performance

# ANOVA
poly1 = lm(imdb_score~genre_count)
poly2 = lm(imdb_score~poly(genre_count, degree = 2))
anova(poly1, poly2)
# linear 

# Try splines
spline_result = splineFit(5,genre_count)
# Indicating spline with knot at 3, degree 1

## total_number_languages
report(total_number_languages)
# Most of the movie mainly has 1 language
# Showing polynomial characteristics
lm_5_l = lm(imdb_score~total_number_languages)
lm_5_g = glm(imdb_score~total_number_languages)
model_performance = evalModel(lm_5_g,lm_5_l,paste('Degree:',1,sep=' '))
degrees = c(2,3,4)
for (i in degrees) {
  print(paste('Poly Spline Degree:',i,sep=' '))
  model_l = lm(imdb_score~poly(total_number_languages, degree = i))
  model_g = glm(imdb_score~poly(total_number_languages, degree = i))
  print(evalModel(model_g,model_l,paste('Poly Degree:',i,sep=' ')))
  result = evalModel(model_g,model_l,paste('Poly Degree:',i,sep=' '))
  model_performance = union_all(model_performance,result)
}
model_performance
# ANOVA test so we don't overfit
poly1 = lm(imdb_score~total_number_languages)
poly2 = lm(imdb_score~poly(total_number_languages, degree = 2))
poly3 = lm(imdb_score~poly(total_number_languages, degree = 3))
poly4 = lm(imdb_score~poly(total_number_languages, degree = 4))
anova(poly1,poly2,poly3,poly4)
# Indicating there's no point add degree on this. 
# However this would have some connections with main language of one movie

# Try splines
spline_result = splineFit(5,total_number_languages)
View(spline_result)
# Indicating spline with knot at 1 and 2, degree 3


## total_number_of_actors
report(total_number_of_actors)
# Note that number of actors clustered around 0-50, regression itself might be
# hard to capture its behaviours correctly
lm_6_l = lm(imdb_score~total_number_of_actors)
lm_6_g = glm(imdb_score~total_number_of_actors)
model_performance = evalModel(lm_6_g,lm_6_l,paste('Degree:',1,sep=' '))
degrees = c(2,3,4)
for (i in degrees) {
  print(paste('Poly Spline Degree:',i,sep=' '))
  model_l = lm(imdb_score~poly(total_number_of_actors, degree = i))
  model_g = glm(imdb_score~poly(total_number_of_actors, degree = i))
  print(evalModel(model_g,model_l,paste('Poly Degree:',i,sep=' ')))
  result = evalModel(model_g,model_l,paste('Poly Degree:',i,sep=' '))
  model_performance = union_all(model_performance,result)
}
model_performance
# MSE frist decreased then increased as degree go up
# ANOVA test
poly1 = lm(imdb_score~total_number_of_actors)
poly2 = lm(imdb_score~poly(total_number_of_actors, degree = 2))
poly3 = lm(imdb_score~poly(total_number_of_actors, degree = 3))
poly4 = lm(imdb_score~poly(total_number_of_actors, degree = 4))
anova(poly1,poly2,poly3,poly4)
# Indicating linear model, some improvement in degree 2

## total_number_of_directors
report(total_number_of_directors)
# From the plot:
# 1. we have max 3 directors for one movie
# 2. most movies have 1 director
# 3. since data clustered at 1 director, fitting regression might not be the best choice
# We have only 3 unique points, thus we will try regression up to 2 degree
lm_7_l = lm(imdb_score~total_number_of_directors)
lm_7_g = glm(imdb_score~total_number_of_directors)
model_performance = evalModel(lm_7_g,lm_7_l,paste('Degree:',1,sep=' '))
degrees = c(2)
for (i in degrees) {
  print(paste('Poly Spline Degree:',i,sep=' '))
  model_l = lm(imdb_score~poly(total_number_of_directors, degree = i))
  model_g = glm(imdb_score~poly(total_number_of_directors, degree = i))
  print(evalModel(model_g,model_l,paste('Poly Degree:',i,sep=' ')))
  result = evalModel(model_g,model_l,paste('Poly Degree:',i,sep=' '))
  model_performance = union_all(model_performance,result)
}
model_performance
# Increase in degree does not decrease mse too much
# R2 is very low

# ANOVA test
poly1 = lm(imdb_score~total_number_of_directors)
poly2 = lm(imdb_score~poly(total_number_of_directors, degree = 2))
anova(poly1,poly2)
# Indicating linear model

# try splines
spline_result = splineFit(5,total_number_of_directors)
View(spline_result)
# Also indicating linear

## total_number_of_producers
report(total_number_of_producers)
# very similar to actors
# cluster between 0-4
lm_8_l = lm(imdb_score~total_number_of_producers)
lm_8_g = glm(imdb_score~total_number_of_producers)
model_performance = evalModel(lm_8_g,lm_8_l,paste('Degree:',1,sep=' '))
degrees = c(2,3,4,5)
for (i in degrees) {
  print(paste('Poly Spline Degree:',i,sep=' '))
  model_l = lm(imdb_score~poly(total_number_of_producers, degree = i))
  model_g = glm(imdb_score~poly(total_number_of_producers, degree = i))
  print(evalModel(model_g,model_l,paste('Poly Degree:',i,sep=' ')))
  result = evalModel(model_g,model_l,paste('Poly Degree:',i,sep=' '))
  model_performance = union_all(model_performance,result)
}
model_performance
# Higher degree does not improve performance too much

# ANOVA test
poly1 = lm(imdb_score~total_number_of_producers)
poly2 = lm(imdb_score~poly(total_number_of_producers, degree = 2))
poly3 = lm(imdb_score~poly(total_number_of_producers, degree = 3))
poly4 = lm(imdb_score~poly(total_number_of_producers, degree = 4))
anova(poly1,poly2,poly3,poly4)
anova(poly1,poly4)
# Indicating linear

# try splines
spline_result = splineFit(6,total_number_of_producers)
View(spline_result)
# Knots at 1, 2, 3, 4; degree in c(1,2,3,6)

## total_number_of_production_companies
report(total_number_of_production_companies)
# Similar to number of actors
lm_9_l = lm(imdb_score~total_number_of_production_companies)
lm_9_g = glm(imdb_score~total_number_of_production_companies)
model_performance = evalModel(lm_9_g,lm_9_l,paste('Degree:',1,sep=' '))
degrees = c(2,3,4,5)
for (i in degrees) {
  print(paste('Poly Spline Degree:',i,sep=' '))
  model_l = lm(imdb_score~poly(total_number_of_production_companies, degree = i))
  model_g = glm(imdb_score~poly(total_number_of_production_companies, degree = i))
  print(evalModel(model_g,model_l,paste('Poly Degree:',i,sep=' ')))
  result = evalModel(model_g,model_l,paste('Poly Degree:',i,sep=' '))
  model_performance = union_all(model_performance,result)
}
model_performance
# MSE decreases at degree = 2, increases with higher degrees

# ANOVA test
poly1 = lm(imdb_score~total_number_of_production_companies)
poly2 = lm(imdb_score~poly(total_number_of_production_companies, degree = 2))
poly3 = lm(imdb_score~poly(total_number_of_production_companies, degree = 3))
poly4 = lm(imdb_score~poly(total_number_of_production_companies, degree = 4))
anova(poly1,poly2,poly3,poly4)
# Linear/quadratic 

# Try splines
spline_result = splineFit(6,total_number_of_production_companies)
View(spline_result)
# Result is worse

## total_number_of_production_countries
report(total_number_of_production_countries)

lm_10_l = lm(imdb_score~total_number_of_production_countries)
lm_10_g = glm(imdb_score~total_number_of_production_countries)
model_performance = evalModel(lm_10_g,lm_10_l,paste('Degree:',1,sep=' '))
degrees = c(2,3,4,5)
for (i in degrees) {
  print(paste('Poly Spline Degree:',i,sep=' '))
  model_l = lm(imdb_score~poly(total_number_of_production_countries, degree = i))
  model_g = glm(imdb_score~poly(total_number_of_production_countries, degree = i))
  print(evalModel(model_g,model_l,paste('Poly Degree:',i,sep=' ')))
  result = evalModel(model_g,model_l,paste('Poly Degree:',i,sep=' '))
  model_performance = union_all(model_performance,result)
}
model_performance
# MSE decrease at degree = 2, increases as degree increases

# ANOVA test
poly1 = lm(imdb_score~total_number_of_production_countries)
poly2 = lm(imdb_score~poly(total_number_of_production_countries, degree = 2))
poly3 = lm(imdb_score~poly(total_number_of_production_countries, degree = 3))
poly4 = lm(imdb_score~poly(total_number_of_production_countries, degree = 4))
anova(poly1,poly2,poly3,poly4)
anova(poly1,poly4)
# There is some improvement in degree = 4
# Try splines
spline_result = splineFit(6,total_number_of_production_countries)
View(spline_result)
# Has better R^2

##########Model Tuning##################
## hyper parameters
# budget_in_millions:
# polynomial
degree_budget = c(2, 3, 4)

# month of release: 
# polynomial spline
knots_2 = c(2,8)
knots_3 = c(2,4,8)
knots_list = list(knots_2,knots_3)
degree_list = c(1,2,3)

# year_of_release:
# polynomial
degree_year = c(2, 3, 4)

# duration_in_hours
# polynomial
degree_duration = c(2,3,4,5)

# genre_count
# linear 
# or
# polynomial spline
knot_genre = 3
degree_genre = c(1,2,3)

# total_number_languages
# linear 
# or
# polynomial spline
knot_total_number_languages = c(1,2)
degree_total_number_languages = c(2,3,4)
# Indicating spline with knot at 1 and 2, degree 3

# total_number_of_actors
degree_total_number_of_actors = c(1,2,3)

# total_number_of_directors
# Linear

# total_number_of_producers
# linear 
# or
# polynomial spline
knot_producer = c(1,2,3,4)
degree_producer = c(3,4,5,6)
# Knots at 1, 2, 3, 4; degree in c(1,2,3,6)

# total_number_of_production_companies
# polynomial
degree_total_number_of_production_companies = c(1,2,3)

# total_number_of_production_countries
# polynomial
# or
# polynomial spline
knot_total_number_of_production_countries = c(2,3)
degree_total_number_of_production_countries = c(3,4,5,6)


## Test
data_r = IMDB_modified_data[,c('imdb_score',factor_cols,cont_cols)]
data_r1 = data_r[,!colnames(data_r) %in% c('main_lang','main_production_country','seasons')]
data_r2 = data_r[,!colnames(data_r) %in% c('main_lang','main_production_country','month_of_release')]

# Drop genre_filmnoir, genre_realitytv, genre_shortfilm, as they only have one value
#Test_results = data.frame(model=character(),mse=double(), R2=double(), AdjR2=double())
name_list = c()
R2_list = c()
adj_R2_list = c()
print(Sys.time())
print('One knot')
for (i in degree_budget) {
  # i: budget degree control
  print(paste('i: ',i))
  for (j in degree_list) {
    # j: month of release degree control
    print(paste('j: ',j))
    for (k in degree_year) {
      # k: year of release degree control
      for (l in degree_duration) {
        # l: duration degree control
        for (m in degree_total_number_of_actors) {
          # m: number of actors degree control
          for (n in degree_total_number_of_production_countries) {
            # n: number of production countries degree control
            for (o in knots_list) {
              print(str(o))
              print(Sys.time())
              model_name1 = paste("i=",i,", j=",j,", k:",k, ", l:",l,
                                 ", m=",m, ", n=",n ,", o=",paste(unlist(o), collapse='|'), ', with month of release')
              print(model_name1)
              #print('with month of release')
              model_l1 <- lm(imdb_score~poly(budget_in_millions, degree=i)+
                               bs(month_of_release,knots=o,degree=j)+
                               poly(year_of_release,degree=k)+
                               poly(duration_in_hours,degree=l)+
                               total_number_languages+genre_action+genre_adventure+genre_animation+
                               genre_biography+genre_comedy+genre_crime+genre_documentary+
                               genre_drama+genre_family+genre_fantasy+genre_history+
                               genre_horror+genre_music+genre_musical+genre_mystery+
                               genre_romance+genre_scifi+genre_sport+genre_thriller+
                               genre_war+genre_western+main_actor1_is_female+main_actor2_is_female+
                               main_actor3_is_female+poly(total_number_of_actors, degree=m)+
                               main_director_is_female+total_number_of_directors+total_number_of_producers+
                               total_number_of_production_companies+poly(total_number_of_production_countries,degree=n)+
                               genre_count+actor1_tier+actor2_tier+actor3_tier+editor_tier+
                               producer_tier+production_company_tier+main_lang_category+Continent)
              name_list = c(name_list,model_name1)
              R2=summary(model_l1)$r.squared
              AdjR2=summary(model_l1)$adj.r.squared
              R2_list=c(R2_list,R2)
              adj_R2_list=c(adj_R2_list,AdjR2)
              
              print(Sys.time())
              model_name2 = paste("i=",i,", j=",j,", k:",k, ", l:",l,
                                  ", m=",m, ", n=",n ,", o=",paste(unlist(o), collapse='|'), ', with season of release')
              print(model_name2)
              print('with season of release')
              model_l2 <- lm(imdb_score~poly(budget_in_millions, degree=i)+
                               poly(year_of_release,degree=k)+
                               poly(duration_in_hours,degree=l)+
                               total_number_languages+genre_action+genre_adventure+genre_animation+
                               genre_biography+genre_comedy+genre_crime+genre_documentary+
                               genre_drama+genre_family+genre_fantasy+genre_history+
                               genre_horror+genre_music+genre_musical+genre_mystery+
                               genre_romance+genre_scifi+genre_sport+genre_thriller+
                               genre_war+genre_western+main_actor1_is_female+main_actor2_is_female+
                               main_actor3_is_female+poly(total_number_of_actors, degree=m)+
                               main_director_is_female+total_number_of_directors+total_number_of_producers+
                               total_number_of_production_companies+poly(total_number_of_production_countries,degree=n)+
                               genre_count+actor1_tier+actor2_tier+actor3_tier+editor_tier+
                               producer_tier+production_company_tier+main_lang_category+Continent+seasons)
              name_list = c(name_list,model_name2)
              R2=summary(model_l2)$r.squared
              AdjR2=summary(model_l2)$adj.r.squared
              R2_list=c(R2_list,R2)
              adj_R2_list=c(adj_R2_list,AdjR2)
            }
          }
        }
      }
    }
  }
}
print(Sys.time())
Test_results1 = data.frame(name_list,R2_list,adj_R2_list)   
View(Test_results1)


# Optimal model with highest R^2, with some of the continuous predictors being linear:
# i= 4 , j= 1 , k: 3 , l: 4 , m= 3 , n= 4 , o= 2|8 , with month of release

# To gain an idea of which predictors are important
optimal_r2 <- lm(imdb_score~poly(budget_in_millions, degree=4)+
                    bs(month_of_release,knots=c(2,8),degree=1)+
                    poly(year_of_release,degree=3)+
                    poly(duration_in_hours,degree=4)+
                    total_number_languages+genre_action+genre_adventure+genre_animation+
                    genre_biography+genre_comedy+genre_crime+genre_documentary+
                    genre_drama+genre_family+genre_fantasy+genre_history+
                    genre_horror+genre_music+genre_musical+genre_mystery+
                    genre_romance+genre_scifi+genre_sport+genre_thriller+
                    genre_war+genre_western+main_actor1_is_female+main_actor2_is_female+
                    main_actor3_is_female+poly(total_number_of_actors, degree=3)+
                    main_director_is_female+total_number_of_directors+total_number_of_producers+
                    total_number_of_production_companies+poly(total_number_of_production_countries,degree=4)+
                    genre_count+actor1_tier+actor2_tier+actor3_tier+editor_tier+
                    producer_tier+production_company_tier+main_lang_category+Continent,
                  data = data_r)  

summary(optimal_r2)
# Multiple R-squared:  0.7159,	Adjusted R-squared:  0.6748

# Important predictors with at least 0.1 p-value:
# budget_in_millions,duration_in_hours,total_number_of_actors,main_actor1_is_female,total_number_of_producers,
# total_number_of_production_countries,actor1_tier,actor2_tier,
# actor3_tier, editor_tier, producer_tier,production_company_tier,main_actor1_is_female1
# genre_action, genre_adventure, genre_comedy, genre_history, genre_mystery, genre_war,genre_biography
# main_lang_category, Continent

# predictors we keep because we will remove other related predictors
# genre_count,month_of_release

# Splines - More complicated set up
name_list = c()
R2_list = c()
adj_R2_list = c()
print(Sys.time())
print('One knot')
for (i in degree_budget) {
  # i: budget degree control
  for (j in degree_list) {
    # j: month of release degree control
    for (k in degree_year) {
      # k: year of release degree control
      for (l in degree_duration) {
        # l: duration degree control
        for (m in degree_total_number_of_actors) {
          # m: number of actors degree control
          for (n in degree_total_number_of_production_countries) {
            # n: number of production countries degree control
            for (o in knots_list) {
              # o: knots for month
              for (a in degree_genre) {
                # a: genre count
                for (b in degree_total_number_languages) {
                  # b: total_number_languages
                  for (c in degree_producer) {
                    # c: total_number_of_producers
                    for (d in degree_total_number_of_production_companies) { 
                      # d: total_number_of_production_companies
                      print(str(o))
                      print(Sys.time())
                      model_name1 = paste("i=",i,", j=",j,", k:",k, ", l:",l,
                                          ", m=",m, ", n=",n ,", a:",a, ", b:",b,
                                          ", c:",c, ", d:",d,
                                          ", o=",paste(unlist(o), collapse='|'), ', with month of release')
                      print(model_name1)
                      print('with month of release')
                      
                      model_l1 <- lm(imdb_score~poly(budget_in_millions, degree=i)+
                                       bs(month_of_release,knots=o,degree=j)+
                                       poly(year_of_release,degree=k)+
                                       poly(duration_in_hours,degree=l)+
                                       bs(genre_count,knots = 3, degree = a)+
                                       bs(total_number_languages,knots = c(1,2),degree = b)+
                                       genre_action+genre_adventure+genre_animation+
                                       genre_biography+genre_comedy+genre_crime+genre_documentary+
                                       genre_drama+genre_family+genre_fantasy+genre_history+
                                       genre_horror+genre_music+genre_musical+genre_mystery+
                                       genre_romance+genre_scifi+genre_sport+genre_thriller+
                                       genre_war+genre_western+main_actor1_is_female+main_actor2_is_female+
                                       main_actor3_is_female+poly(total_number_of_actors, degree=m)+
                                       main_director_is_female+total_number_of_directors+
                                       bs(total_number_of_producers,knots=c(1,2,3,4),degree=c)+
                                       poly(total_number_of_production_companies,degree = d)+
                                       bs(total_number_of_production_countries,knots=c(2,3),degree=n)+
                                       actor1_tier+actor2_tier+actor3_tier+editor_tier+
                                       producer_tier+production_company_tier+main_lang_category+Continent,
                                     data = data_r)
                      name_list = c(name_list,model_name1)
                      R2=summary(model_l1)$r.squared
                      AdjR2=summary(model_l1)$adj.r.squared
                      R2_list=c(R2_list,R2)
                      adj_R2_list=c(adj_R2_list,AdjR2)
                      
                      print(Sys.time())
                      model_name2 = paste("i=",i,", j=",j,", k:",k, ", l:",l,
                                          ", m=",m, ", n=",n ,", a:",a, ", b:",b,
                                          ", c:",c, ", d:",d,
                                          ", o=",paste(unlist(o), collapse='|'), ', with season of release')
                      print(model_name2)
                      #print('with season of release')
                      model_l2 <- lm(imdb_score~poly(budget_in_millions, degree=i)+
                                       poly(year_of_release,degree=k)+
                                       poly(duration_in_hours,degree=l)+
                                       bs(genre_count,knots = 3, degree = a)+
                                       bs(total_number_languages,knots = c(1,2),degree = b)+
                                       genre_action+genre_adventure+genre_animation+
                                       genre_biography+genre_comedy+genre_crime+genre_documentary+
                                       genre_drama+genre_family+genre_fantasy+genre_history+
                                       genre_horror+genre_music+genre_musical+genre_mystery+
                                       genre_romance+genre_scifi+genre_sport+genre_thriller+
                                       genre_war+genre_western+main_actor1_is_female+main_actor2_is_female+
                                       main_actor3_is_female+poly(total_number_of_actors, degree=m)+
                                       main_director_is_female+total_number_of_directors+
                                       bs(total_number_of_producers,knots=c(1,2,3,4),degree=c)+
                                       poly(total_number_of_production_companies,degree = d)+
                                       bs(total_number_of_production_countries,knots=c(2,3),degree=n)+
                                       actor1_tier+actor2_tier+actor3_tier+editor_tier+
                                       producer_tier+production_company_tier+main_lang_category+Continent+seasons)
                      name_list = c(name_list,model_name2)
                      R2=summary(model_l2)$r.squared
                      AdjR2=summary(model_l2)$adj.r.squared
                      R2_list=c(R2_list,R2)
                      adj_R2_list=c(adj_R2_list,AdjR2)
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
print(Sys.time())
Test_results1_2 = data.frame(name_list,R2_list,adj_R2_list)   
View(Test_results1_2)

# # Optimal model with highest R^2, with some of the continuous predictors being splines:
# i= 4 , j= 1 , k: 3 , l: 4 , m= 1 , n= 5 , a: 2 , b: 2 , c: 3 , d: 1 , o= 2|8 , with month of release
# These models are giving us higher R^2.
# Note that in both scenarios, we notice models with month of release performs better

optimal_r2_splines = lm(imdb_score~poly(budget_in_millions, degree=4)+
                          bs(month_of_release,knots=c(2,8),degree=1)+
                          poly(year_of_release,degree=3)+
                          poly(duration_in_hours,degree=4)+
                          bs(genre_count,knots = 3, degree = 2)+
                          bs(total_number_languages,knots = c(1,2),degree = 2)+
                          genre_action+genre_adventure+genre_animation+
                          genre_biography+genre_comedy+genre_crime+genre_documentary+
                          genre_drama+genre_family+genre_fantasy+genre_history+
                          genre_horror+genre_music+genre_musical+genre_mystery+
                          genre_romance+genre_scifi+genre_sport+genre_thriller+
                          genre_war+genre_western+main_actor1_is_female+main_actor2_is_female+
                          main_actor3_is_female+poly(total_number_of_actors, degree=1)+
                          main_director_is_female+total_number_of_directors+
                          bs(total_number_of_producers,knots=c(1,2,3,4),degree=3)+
                          poly(total_number_of_production_companies,degree = 1)+
                          bs(total_number_of_production_countries,knots=c(2,3),degree=5)+
                          actor1_tier+actor2_tier+actor3_tier+editor_tier+
                          producer_tier+production_company_tier+main_lang_category+Continent,
                        data = data_r)

summary(optimal_r2_splines)
# Multiple R-squared:  0.7224,	Adjusted R-squared:  0.6753 
# Higher R^2, but similar adjusted R^2

# Important predictors with at least 0.1 p-value:
# budget_in_millions,duration_in_hours,total_number_of_actors,actor1_tier,actor2_tier,main_actor1_is_female1,
# total_number_of_production_countries,main_lang_category
# actor3_tier, editor_tier, producer_tier,production_company_tier,Continent

# predictors we keep because we will remove other related predictors
# genre_count,month_of_release

## Revised both optimal models: remove predictors with low p-values
optimal_r2_revised <- lm(imdb_score~poly(budget_in_millions, degree=4)+
                           bs(month_of_release,knots=c(2,8),degree=1)+
                           poly(duration_in_hours,degree=4)+
                           genre_action+genre_adventure+genre_comedy+genre_biography+
                           genre_history+genre_mystery+genre_war+
                           main_actor1_is_female+
                           poly(total_number_of_actors, degree=3)+
                           total_number_of_producers+
                           poly(total_number_of_production_countries,degree=4)+
                           actor1_tier+actor2_tier+actor3_tier+editor_tier+
                           producer_tier+production_company_tier+main_lang_category+Continent,
                         data = data_r)  

summary(optimal_r2_revised)

optimal_r2_splines_revised = lm(imdb_score~poly(budget_in_millions, degree=4)+
                                  bs(month_of_release,knots=c(2,8),degree=1)+
                                  poly(duration_in_hours,degree=4)+
                                  bs(genre_count,knots = 3, degree = 2)+
                                  main_actor1_is_female+total_number_of_actors+
                                  bs(total_number_of_production_countries,knots=c(2,3),degree=5)+
                                  actor1_tier+actor2_tier+actor3_tier+editor_tier+
                                  producer_tier+production_company_tier+main_lang_category+Continent,
                                data = data_r)

summary(optimal_r2_splines_revised)
stargazer(optimal_r2_revised, optimal_r2_splines_revised, type = 'text')


## hyper parameters expended
# budget_in_millions:
# polynomial
degree_budget = c(2, 3, 4)

# month of release: 
# polynomial spline
knots_2 = c(2,8)
knots_3 = c(2,4,8)
knots_list = list(knots_2,knots_3)
degree_list = c(1,2,3)

# year_of_release:
# polynomial
degree_year = c(2, 3, 4)

# duration_in_hours
degree_duration = c(2,3,4,5)

# genre_count
knot_genre = 3
degree_genre = c(1,2,3)

# total_number_languages
knot_total_number_languages = c(1,2)
degree_total_number_languages = c(2,3,4)
# Indicating spline with knot at 1 and 2, degree 3

# total_number_of_actors
degree_total_number_of_actors = c(1,2,3)

# total_number_of_directors
# Linear

# total_number_of_producers
knot_producer = c(1,2,3,4)
degree_producer = c(1,2,3,4,5,6,7)
# Knots at 1, 2, 3, 4; degree in c(1,2,3,6)

# total_number_of_production_companies
degree_total_number_of_production_companies = c(1,2,3)

# total_number_of_production_countries
knot_total_number_of_production_countries = c(2,3)
degree_total_number_of_production_countries = c(3,4,5,6,7)

# Fit again with selected predictors: polynomial with genres
name_list = c()
R2_list = c()
adj_R2_list = c()
mse_list = c()
print(Sys.time())
for (i in degree_budget) {
  # i: budget degree control
  for (j in degree_list) {
    # j: month of release degree control
    for (l in degree_duration) {
        # l: duration degree control
      for (m in degree_total_number_of_actors) {
          # m: number of actors degree control
        for (o in knots_list) {
          #print(str(o))
          print(Sys.time())
          model_name1 = paste("i=",i,", j=",j, ", l:",l,
                              ", m=",m,", o=",paste(unlist(o), collapse='|'), ', with month of release')
          print(model_name1)
          print('with month of release')
          mse_degree_j = rep(NA,10)
          for (n in 1:10) {#10 samples
            print(paste('trail',n))
            sample = sample.split(data_r1, SplitRatio = 0.5)
            train = subset(data_r1, sample==TRUE)
            test = subset(data_r1, sample==FALSE)
            model_l1 <- lm(imdb_score~poly(budget_in_millions, degree=i)+
                            bs(month_of_release,knots=o,degree=j)+
                            poly(duration_in_hours,degree=l)+
                            genre_action+genre_adventure+genre_comedy+genre_biography+
                            genre_history+genre_mystery+genre_war+
                            main_actor1_is_female+
                            poly(total_number_of_actors, degree=m)+total_number_of_producers+
                            actor1_tier+actor2_tier+actor3_tier+editor_tier+
                            producer_tier+production_company_tier+main_lang_category+Continent,
                            data = data_r)
            
            #predict
            pred = predict(model_l1, test[,!colnames(test) %in% c('imdb_score')])
            test$res = (test$imdb_score - pred)
            test$res_sq = (test$res)^2
            mse_degree_j[n] = mean(test$res_sq)
          }
          mse_ = mean(mse_degree_j)
          name_list = c(name_list,model_name1)
          R2=summary(model_l1)$r.squared
          AdjR2=summary(model_l1)$adj.r.squared
          R2_list=c(R2_list,R2)
          adj_R2_list=c(adj_R2_list,AdjR2)  
          mse_list = c(mse_list,mse_)
        }
      }
    }
  }
}
print(Sys.time())
Test_results2 = data.frame(name_list,mse_list,R2_list,adj_R2_list)   
View(Test_results2)

# This gives following optimal models, ordered by decreasing mse
# i= 3 , j= 2 , l: 4 , m= 3 , o= 2|4|8 , with month of release
# Best MSE = 0.2481625
# i= 4 , j= 2 , l: 4 , m= 3 , o= 2|8 , with month of release
# i= 4 , j= 1 , l: 3 , m= 3 , o= 2|4|8 , with month of release

# Fit again with selected predictors: splines
name_list = c()
R2_list = c()
adj_R2_list = c()
mse_list = c()
print(Sys.time())
for (i in degree_budget) {
  for (j in degree_list) {
    for (o in knots_list) {
      for (k in degree_duration) {
        for (l in degree_genre) {
          for (m in degree_total_number_of_actors) {
            for (n in degree_total_number_of_production_countries) {
              model_name1 = paste("i=",i,", j=",j, ", k:",k, ", l:",l,
                                  ", m=",m,", n=",n,", o=",paste(unlist(o), collapse='|'), ', with month of release')
              print(model_name1)
              mse_degree_j = rep(NA,10)
              for (n in 1:10) {#10 samples
                print(paste('trail',n))
                sample = sample.split(data_r1, SplitRatio = 0.5)
                train = subset(data_r1, sample==TRUE)
                test = subset(data_r1, sample==FALSE)
                model_l1 <- lm(imdb_score~poly(budget_in_millions, degree=i)+
                                 bs(month_of_release,knots=o,degree=j)+
                                 poly(duration_in_hours,degree=k)+
                                 bs(genre_count,knots = 3, degree = l)+
                                 main_actor1_is_female+poly(total_number_of_actors,degree = m)+
                                 bs(total_number_of_production_countries,knots=c(2,3),degree=n)+
                                 actor1_tier+actor2_tier+actor3_tier+editor_tier+
                                 producer_tier+production_company_tier+main_lang_category+Continent,
                               data = data_r)
                
                #predict
                pred = predict(model_l1, test[,!colnames(test) %in% c('imdb_score')])
                test$res = (test$imdb_score - pred)
                test$res_sq = (test$res)^2
                mse_degree_j[n] = mean(test$res_sq)
              }
              mse_ = mean(mse_degree_j)
              name_list = c(name_list,model_name1)
              R2=summary(model_l1)$r.squared
              AdjR2=summary(model_l1)$adj.r.squared
              R2_list=c(R2_list,R2)
              adj_R2_list=c(adj_R2_list,AdjR2)  
              mse_list = c(mse_list,mse_)
            }
          }
        }
      }
    }
  }
}
print(Sys.time())
Test_results2_2 = data.frame(name_list,mse_list,R2_list,adj_R2_list)   
View(Test_results2_2)

# This gives following optimal models, order by decreasing mse
# i= 3 , j= 1 , k: 5 , l: 1 , m= 1 , n= 6 , o= 2|8 , with month of release
# MSE = 0.2535157
# i= 4 , j= 1 , k: 5 , l: 1 , m= 3 , n= 7 , o= 2|8 , with month of release
# i= 4 , j= 2 , k: 5 , l: 2 , m= 3 , n= 7 , o= 2|8 , with month of release


# Before further comparison, we will take a look at the importance of the predictors
# i= 3 , j= 2 , l: 4 , m= 3 , o= 2|4|8 , with month of release
optimal_m1 = lm(imdb_score~poly(budget_in_millions, degree=3)+
                  bs(month_of_release,knots=c(2,4,8),degree=2)+
                  poly(duration_in_hours,degree=4)+
                  genre_action+genre_adventure+genre_comedy+genre_biography+
                  genre_history+genre_mystery+genre_war+
                  main_actor1_is_female+
                  poly(total_number_of_actors, degree=3)+total_number_of_producers+
                  actor1_tier+actor2_tier+actor3_tier+editor_tier+
                  producer_tier+production_company_tier+main_lang_category+Continent,
                data = data_r)
summary(optimal_m1)
# i= 3 , j= 1 , k: 5 , l: 1 , m= 1 , n= 6 , o= 2|8 , with month of release
optimal_m2 = lm(imdb_score~poly(budget_in_millions, degree=3)+
                  bs(month_of_release,knots=c(2,8),degree=1)+
                  poly(duration_in_hours,degree=5)+
                  bs(genre_count,knots = 3, degree = 1)+
                  main_actor1_is_female+poly(total_number_of_actors,degree = 1)+
                  bs(total_number_of_production_countries,knots=c(2,3),degree=6)+
                  actor1_tier+actor2_tier+actor3_tier+editor_tier+
                  producer_tier+production_company_tier+main_lang_category+Continent,
                data = data_r)
summary(optimal_m2)
stargazer(optimal_m1,optimal_m2,type = 'text')
# Optimal model 1 has higher adjusted R^2 and lower MSE, which is the winner model at this stage

# Taking away month_of_release
# Tune again
name_list = c()
R2_list = c()
adj_R2_list = c()
mse_list = c()
print(Sys.time())
for (i in degree_budget) {
  for (l in degree_genre) {
    for (m in degree_total_number_of_actors) {
      model_name1 = paste("i=",i, ", l:",l,
                          ", m=",m)
      print(model_name1)
      mse_degree_j = rep(NA,10)
      for (n in 1:10) {#10 samples
        print(paste('trail',n))
        sample = sample.split(data_r1, SplitRatio = 0.5)
        train = subset(data_r1, sample==TRUE)
        test = subset(data_r1, sample==FALSE)
        model_l1 <- lm(imdb_score~poly(budget_in_millions, degree=i)+
                         poly(duration_in_hours,degree=l)+
                         genre_action+genre_adventure+genre_comedy+genre_biography+
                         genre_history+genre_mystery+genre_war+
                         main_actor1_is_female+
                         poly(total_number_of_actors, degree=m)+total_number_of_producers+
                         actor1_tier+actor2_tier+actor3_tier+editor_tier+
                         producer_tier+production_company_tier+main_lang_category+Continent,
                       data = data_r)
        
        #predict
        pred = predict(model_l1, test[,!colnames(test) %in% c('imdb_score')])
        test$res = (test$imdb_score - pred)
        test$res_sq = (test$res)^2
        mse_degree_j[n] = mean(test$res_sq)
      }
      mse_ = mean(mse_degree_j)
      name_list = c(name_list,model_name1)
      R2=summary(model_l1)$r.squared
      AdjR2=summary(model_l1)$adj.r.squared
      R2_list=c(R2_list,R2)
      adj_R2_list=c(adj_R2_list,AdjR2)  
      mse_list = c(mse_list,mse_)
    }
  }
}
print(Sys.time())
Test_results3 = data.frame(name_list,mse_list,R2_list,adj_R2_list)   
View(Test_results3)
# previously:
# i= 3 , j= 2 , l: 4 , m= 3 , o= 2|4|8 , with month of release
# MSE: 0.2481625
# Optimal models, order by MSE decreasing
# MSE = 0.2559102
# i= 3 , l: 3 , m= 3
# i= 3 , l: 3 , m= 1
# i= 4 , l: 3 , m= 2

# Without month of release, tuning gives relatively close results, however for total
# number of actors, this varies a lot

# Do we need higher power?
optimal_3_1 = lm(imdb_score~poly(budget_in_millions, degree=3)+
                   poly(duration_in_hours,degree=3)+
                   genre_action+genre_adventure+genre_comedy+genre_biography+
                   genre_history+genre_mystery+genre_war+
                   main_actor1_is_female+
                   total_number_of_actors+total_number_of_producers+
                   actor1_tier+actor2_tier+actor3_tier+editor_tier+
                   producer_tier+production_company_tier+main_lang_category+Continent,
                 data = data_r)
optimal_3_2 = lm(imdb_score~poly(budget_in_millions, degree=4)+
                   poly(duration_in_hours,degree=3)+
                   genre_action+genre_adventure+genre_comedy+genre_biography+
                   genre_history+genre_mystery+genre_war+
                   main_actor1_is_female+
                   total_number_of_actors+total_number_of_producers+
                   actor1_tier+actor2_tier+actor3_tier+editor_tier+
                   producer_tier+production_company_tier+main_lang_category+Continent,
                 data = data_r)
optimal_3_3 = lm(imdb_score~poly(budget_in_millions, degree=3)+
                   poly(duration_in_hours,degree=3)+
                   genre_action+genre_adventure+genre_comedy+genre_biography+
                   genre_history+genre_mystery+genre_war+
                   main_actor1_is_female+
                   poly(total_number_of_actors, degree = 2)+total_number_of_producers+
                   actor1_tier+actor2_tier+actor3_tier+editor_tier+
                   producer_tier+production_company_tier+main_lang_category+Continent,
                 data = data_r)
optimal_3_4 = lm(imdb_score~poly(budget_in_millions, degree=4)+
                   poly(duration_in_hours,degree=3)+
                   genre_action+genre_adventure+genre_comedy+genre_biography+
                   genre_history+genre_mystery+genre_war+
                   main_actor1_is_female+
                   poly(total_number_of_actors, degree = 2)+total_number_of_producers+
                   actor1_tier+actor2_tier+actor3_tier+editor_tier+
                   producer_tier+production_company_tier+main_lang_category+Continent,
                 data = data_r)
optimal_3_5 = lm(imdb_score~poly(budget_in_millions, degree=3)+
                   poly(duration_in_hours,degree=3)+
                   genre_action+genre_adventure+genre_comedy+genre_biography+
                   genre_history+genre_mystery+genre_war+
                   main_actor1_is_female+
                   poly(total_number_of_actors, degree = 3)+total_number_of_producers+
                   actor1_tier+actor2_tier+actor3_tier+editor_tier+
                   producer_tier+production_company_tier+main_lang_category+Continent,
                 data = data_r)
optimal_3_6 = lm(imdb_score~poly(budget_in_millions, degree=4)+
                   poly(duration_in_hours,degree=3)+
                   genre_action+genre_adventure+genre_comedy+genre_biography+
                   genre_history+genre_mystery+genre_war+
                   main_actor1_is_female+
                   poly(total_number_of_actors, degree = 3)+total_number_of_producers+
                   actor1_tier+actor2_tier+actor3_tier+editor_tier+
                   producer_tier+production_company_tier+main_lang_category+Continent,
                 data = data_r)
anova(optimal_3_1,optimal_3_2,optimal_3_3,optimal_3_4,optimal_3_5,optimal_3_6)
stargazer(optimal_3_1,optimal_3_2,optimal_3_3,optimal_3_4,optimal_3_5,optimal_3_6, type="text")
anova(optimal_3_1,optimal_3_2,optimal_3_4,optimal_3_6)
# increase in degrees does not help too much. We are potentially facing overfitting issue
# Significant model improvement stopped at optimal_3_2

# Here we further look at budget and duration again
# budget
plot = ggplot(data_r, aes(y=imdb_score,x=budget_in_millions))
scatter=geom_point(color="grey")
line_poly1=geom_smooth(method='lm', formula = y~x, aes(colour="degree=1"))
line_poly2=geom_smooth(method='lm', formula = y~poly(x,3), aes(colour="degree=3"))
line_poly3=geom_smooth(method='lm', formula = y~poly(x,4), aes(colour="degree=4"))
line_poly4=geom_smooth(method='lm', formula = y~poly(x,5), aes(colour="degree=5"))
line_color=scale_colour_manual(values = "green", name = "model")
add_legend=theme(legend.justification = c(1, 1), legend.position = c(1, 1),
                 legend.title = element_text(size = 6), 
                 legend.text = element_text(size = 6),
                 legend.key.size = unit(0.5, 'cm')) 
degree1=plot+scatter+line_poly1+line_color+add_legend
degree2=plot+scatter+line_poly2+line_color+add_legend
degree3=plot+scatter+line_poly3+line_color+add_legend
degree4=plot+scatter+line_poly4+line_color+add_legend
grid.arrange(degree1,degree2,degree3,degree4)
# We might be overfitting with degree 5 at high budget movies, 
# then in this case we stick with d=4

# Outliers test
poly_budget = lm(imdb_score~poly(budget_in_millions, degree=4))
qqPlot(poly_budget)
outlierTest(poly_budget)
# Indicating record 607

# duration
plot = ggplot(data_r, aes(y=imdb_score,x=duration_in_hours))
scatter=geom_point(color="grey")
line_poly1=geom_smooth(method='lm', formula = y~x, aes(colour="degree=1"))
line_poly2=geom_smooth(method='lm', formula = y~poly(x,2), aes(colour="degree=2"))
line_poly3=geom_smooth(method='lm', formula = y~poly(x,3), aes(colour="degree=3"))
line_poly4=geom_smooth(method='lm', formula = y~poly(x,4), aes(colour="degree=4"))
line_color=scale_colour_manual(values = "green", name = "model")
add_legend=theme(legend.justification = c(1, 1), legend.position = c(1, 0),
                 legend.title = element_text(size = 6), 
                 legend.text = element_text(size = 6),
                 legend.key.size = unit(0.5, 'cm')) 

degree1=plot+scatter+line_poly1+line_color+add_legend
degree2=plot+scatter+line_poly2+line_color+add_legend
degree3=plot+scatter+line_poly3+line_color+add_legend
degree4=plot+scatter+line_poly4+line_color+add_legend
grid.arrange(degree1,degree2,degree3,degree4)
# We noticed that degree 4 has overfitting issues with very short/long durations 
# (extreme values), in this case stick with degree = 3 (capture a good sense of trend)

# Outliers test
poly_duration = lm(imdb_score~poly(duration_in_hours, degree=3))
qqPlot(poly_duration)
outlierTest(poly_duration)
# Record 480 and 607

# number of actors
plot = ggplot(data_r, aes(y=imdb_score,x=total_number_of_actors))
scatter=geom_point(color="grey")
line_poly1=geom_smooth(method='lm', formula = y~x, aes(colour="degree=1"))
line_poly2=geom_smooth(method='lm', formula = y~poly(x,2), aes(colour="degree=2"))
line_poly3=geom_smooth(method='lm', formula = y~poly(x,3), aes(colour="degree=3"))
line_color=scale_colour_manual(values = "green", name = "model")
add_legend=theme(legend.justification = c(1, 1), legend.position = c(1, 0),
                 legend.title = element_text(size = 6), 
                 legend.text = element_text(size = 6),
                 legend.key.size = unit(0.5, 'cm')) 

degree1=plot+scatter+line_poly1+line_color+add_legend
degree2=plot+scatter+line_poly2+line_color+add_legend
degree3=plot+scatter+line_poly3+line_color+add_legend
grid.arrange(degree1,degree2,degree3)
# Linear makes sense, as higher power (d=3) fits in a very similar way

# Outliers test
poly_n_actors = lm(imdb_score~total_number_of_actors)
qqPlot(poly_n_actors)
outlierTest(poly_n_actors)
# Record 607

# Take out outliers we observed from previous analysis
data_r_revised = data_r[-c(480,607)]

# Thus our final model without interactions is:
model_fin_without_intr = lm(imdb_score~poly(budget_in_millions, degree=4)+
                              poly(duration_in_hours,degree=3)+
                              genre_action+genre_adventure+genre_comedy+genre_biography+
                              genre_history+genre_mystery+genre_war+
                              main_actor1_is_female+
                              total_number_of_actors+total_number_of_producers+
                              actor1_tier+actor2_tier+actor3_tier+editor_tier+
                              producer_tier+production_company_tier+main_lang_category+Continent,
                            data = data_r_revised)
# MSE: 0.2579126, which is the second optimal model in the tuning session without month of release
summary(model_fin_without_intr)

## Possible intersections - via intuition
# main_lang_category * total_number_languages
model_fin_with_com1 = lm(imdb_score~poly(budget_in_millions, degree=4)+
                           poly(duration_in_hours,degree=3)+
                           genre_action+genre_adventure+genre_comedy+genre_biography+
                           genre_history+genre_mystery+genre_war+
                           main_actor1_is_female+
                           total_number_of_actors+total_number_of_producers+
                           actor1_tier+actor2_tier+actor3_tier+editor_tier+
                           producer_tier+production_company_tier+main_lang_category+Continent+
                          main_lang_category * total_number_languages,
                          data = data_r_revised)
summary(model_fin_with_com1)
anova(model_fin_with_com1)
# This is not adding prediction power to our model

# genre_animation * total_number_of_directors
model_fin_with_com2 = lm(imdb_score~poly(budget_in_millions, degree=4)+
                           poly(duration_in_hours,degree=3)+
                           genre_action+genre_adventure+genre_comedy+genre_biography+
                           genre_history+genre_mystery+genre_war+
                           main_actor1_is_female+
                           total_number_of_actors+total_number_of_producers+
                           actor1_tier+actor2_tier+actor3_tier+editor_tier+
                           producer_tier+production_company_tier+main_lang_category+Continent+
                           genre_animation * total_number_of_directors,
                         data = data_r_revised)
summary(model_fin_with_com2)
anova(model_fin_with_com2)
# Again we should not consider add it to the model

# genre_animation * budget_in_millions
model_fin_with_com3 = lm(imdb_score~poly(budget_in_millions, degree=4)+
                           poly(duration_in_hours,degree=3)+
                           genre_action+genre_adventure+genre_comedy+genre_biography+
                           genre_history+genre_mystery+genre_war+
                           main_actor1_is_female+
                           total_number_of_actors+total_number_of_producers+
                           actor1_tier+actor2_tier+actor3_tier+editor_tier+
                           producer_tier+production_company_tier+main_lang_category+Continent+
                           genre_animation * budget_in_millions,
                         data = data_r_revised)
summary(model_fin_with_com3)
anova(model_fin_with_com3)
# No importance, not adding this to the main model

# genre_scifi * budget_in_millions
model_fin_with_com4 = lm(imdb_score~poly(budget_in_millions, degree=4)+
                           poly(duration_in_hours,degree=3)+
                           genre_action+genre_adventure+genre_comedy+genre_biography+
                           genre_history+genre_mystery+genre_war+
                           main_actor1_is_female+
                           total_number_of_actors+total_number_of_producers+
                           actor1_tier+actor2_tier+actor3_tier+editor_tier+
                           producer_tier+production_company_tier+main_lang_category+Continent+
                           genre_scifi * budget_in_millions,
                         data = data_r_revised)
summary(model_fin_with_com4)
anova(model_fin_with_com4)
# No importance, not adding this to the main model

# total_number_of_actors * budget_in_millions
model_fin_with_com5 = lm(imdb_score~poly(budget_in_millions, degree=4)+
                           poly(duration_in_hours,degree=3)+
                           genre_action+genre_adventure+genre_comedy+genre_biography+
                           genre_history+genre_mystery+genre_war+
                           main_actor1_is_female+
                           total_number_of_actors+total_number_of_producers+
                           actor1_tier+actor2_tier+actor3_tier+editor_tier+
                           producer_tier+production_company_tier+main_lang_category+Continent+
                           genre_scifi * budget_in_millions,
                         data = data_r_revised)
summary(model_fin_with_com5)
anova(model_fin_with_com5)
# No importance, not adding this to the main model

# total_number_of_directors * budget_in_millions
model_fin_with_com6 = lm(imdb_score~poly(budget_in_millions, degree=4)+
                           poly(duration_in_hours,degree=3)+
                           genre_action+genre_adventure+genre_comedy+genre_biography+
                           genre_history+genre_mystery+genre_war+
                           main_actor1_is_female+
                           total_number_of_actors+total_number_of_producers+
                           actor1_tier+actor2_tier+actor3_tier+editor_tier+
                           producer_tier+production_company_tier+main_lang_category+Continent+
                           total_number_of_directors * budget_in_millions,
                         data = data_r_revised)
summary(model_fin_with_com6)
anova(model_fin_with_com6)
# No importance, not adding this to the main model

# genre_action*genre_drama
model_fin_with_com7 = lm(imdb_score~poly(budget_in_millions, degree=4)+
                           poly(duration_in_hours,degree=3)+
                           genre_action+genre_adventure+genre_comedy+genre_biography+
                           genre_history+genre_mystery+genre_war+
                           main_actor1_is_female+
                           total_number_of_actors+total_number_of_producers+
                           actor1_tier+actor2_tier+actor3_tier+editor_tier+
                           producer_tier+production_company_tier+main_lang_category+Continent+
                           genre_action*genre_drama,
                         data = data_r_revised)
summary(model_fin_with_com7)
anova(model_fin_with_com7)

#genre_action*genre_comedy
model_fin_with_com8 = lm(imdb_score~poly(budget_in_millions, degree=4)+
                           poly(duration_in_hours,degree=3)+
                           genre_action+genre_adventure+genre_comedy+genre_biography+
                           genre_history+genre_mystery+genre_war+
                           main_actor1_is_female+
                           total_number_of_actors+total_number_of_producers+
                           actor1_tier+actor2_tier+actor3_tier+editor_tier+
                           producer_tier+production_company_tier+main_lang_category+Continent+
                           genre_action*genre_drama + genre_action*genre_comedy,
                         data = data_r_revised)
summary(model_fin_with_com8)
anova(model_fin_with_com8)
# No importance, not adding this to the main model

#genre_history*duration_in_hours
model_fin_with_com9 = lm(imdb_score~poly(budget_in_millions, degree=4)+
                           poly(duration_in_hours,degree=3)+
                           genre_action+genre_adventure+genre_comedy+genre_biography+
                           genre_history+genre_mystery+genre_war+
                           main_actor1_is_female+
                           total_number_of_actors+total_number_of_producers+
                           actor1_tier+actor2_tier+actor3_tier+editor_tier+
                           producer_tier+production_company_tier+main_lang_category+Continent+
                           genre_action*genre_drama + genre_history*poly(duration_in_hours,degree=3),
                         data = data_r_revised)
summary(model_fin_with_com9)
anova(model_fin_with_com9)
# No importance, not adding this to the main model

# genre_documentary*genre_action
model_fin_with_com10 = lm(imdb_score~poly(budget_in_millions, degree=4)+
                           poly(duration_in_hours,degree=3)+
                           genre_action+genre_adventure+genre_comedy+genre_biography+
                           genre_history+genre_mystery+genre_war+
                           main_actor1_is_female+
                           total_number_of_actors+total_number_of_producers+
                           actor1_tier+actor2_tier+actor3_tier+editor_tier+
                           producer_tier+production_company_tier+main_lang_category+Continent+
                           genre_action*genre_drama + 
                           genre_documentary*genre_action,
                         data = data_r_revised)
summary(model_fin_with_com10)
anova(model_fin_with_com10)
# No importance, not adding this to the main model

# genre_western*genre_action
model_fin_with_com11 = lm(imdb_score~poly(budget_in_millions, degree=4)+
                            poly(duration_in_hours,degree=3)+
                            genre_action+genre_adventure+genre_comedy+genre_biography+
                            genre_history+genre_mystery+genre_war+
                            main_actor1_is_female+
                            total_number_of_actors+total_number_of_producers+
                            actor1_tier+actor2_tier+actor3_tier+editor_tier+
                            producer_tier+production_company_tier+main_lang_category+Continent+
                            genre_action*genre_drama + 
                            genre_western*genre_action,
                          data = data_r_revised)
summary(model_fin_with_com11)
anova(model_fin_with_com11)
# No importance, not adding this to the main model

# main_actor1_is_female*main_actor2_is_female*main_actor3_is_female
model_fin_with_com12 = lm(imdb_score~poly(budget_in_millions, degree=4)+
                            poly(duration_in_hours,degree=3)+
                            genre_action+genre_adventure+genre_comedy+genre_biography+
                            genre_history+genre_mystery+genre_war+
                            main_actor1_is_female+
                            total_number_of_actors+total_number_of_producers+
                            actor1_tier+actor2_tier+actor3_tier+editor_tier+
                            producer_tier+production_company_tier+main_lang_category+Continent+
                            genre_action*genre_drama + 
                            main_actor1_is_female*main_actor2_is_female*main_actor3_is_female,
                          data = data_r_revised)
summary(model_fin_with_com12)
anova(model_fin_with_com12)
# No importance, not adding this to the main model

# total_number_of_directors*genre_animation
model_fin_with_com13 = lm(imdb_score~poly(budget_in_millions, degree=4)+
                            poly(duration_in_hours,degree=3)+
                            genre_action+genre_adventure+genre_comedy+genre_biography+
                            genre_history+genre_mystery+genre_war+
                            main_actor1_is_female+
                            total_number_of_actors+total_number_of_producers+
                            actor1_tier+actor2_tier+actor3_tier+editor_tier+
                            producer_tier+production_company_tier+main_lang_category+Continent+
                            genre_action*genre_drama + 
                            main_actor1_is_female*main_actor2_is_female*main_actor3_is_female+
                            total_number_of_directors*genre_animation,
                          data = data_r_revised)
summary(model_fin_with_com13)
anova(model_fin_with_com13)
# No importance, not adding this to the main model

# poly(budget_in_millions,3)*year_of_release*genre_action
model_fin_with_com14 = lm(imdb_score~poly(budget_in_millions, degree=4)+
                            poly(duration_in_hours,degree=3)+
                            genre_action+genre_adventure+genre_comedy+genre_biography+
                            genre_history+genre_mystery+genre_war+
                            main_actor1_is_female+
                            total_number_of_actors+total_number_of_producers+
                            actor1_tier+actor2_tier+actor3_tier+editor_tier+
                            producer_tier+production_company_tier+main_lang_category+Continent+
                            genre_action*genre_drama +
                            main_actor1_is_female*main_actor2_is_female*main_actor3_is_female+
                            poly(budget_in_millions, degree=4)*year_of_release*genre_action,
                          data = data_r_revised)
summary(model_fin_with_com14)
anova(model_fin_with_com14)
# No importance, not adding this to the main model

model_fin_with_intr = lm(imdb_score~poly(budget_in_millions, degree=4)+
                           poly(duration_in_hours,degree=3)+
                           genre_action+genre_adventure+genre_comedy+genre_biography+
                           genre_history+genre_mystery+genre_war+
                           main_actor1_is_female+
                           total_number_of_actors+total_number_of_producers+
                           actor1_tier+actor2_tier+actor3_tier+editor_tier+
                           producer_tier+production_company_tier+main_lang_category+Continent+
                           main_actor1_is_female*main_actor2_is_female*main_actor3_is_female,
                         data = data_r_revised)
summary(model_fin_with_intr)

# Heteroskedasticity
plot(predict(model_fin_with_intr),residuals(model_fin_with_intr),col="red")
abline(0,0,lty=2)
ncvTest(model_fin_with_intr) 

# Predictor Significance
anova(model_fin_with_intr)
# Note that here in this model, main_lang_category is  not important F-test wise.

### MSE with model fit on data without outliers
mse_lst = c()
for (n in 1:30) {#30 samples
  print(paste('trail',n))
  sample = sample.split(data_r_revised, SplitRatio = 0.5)
  train = subset(data_r_revised, sample==TRUE)
  test = subset(data_r_revised, sample==FALSE)
  
  #predict
  pred1 = predict(model_fin_with_intr, test[,!colnames(test) %in% c('imdb_score')])
  test$res1 = (test$imdb_score - pred1)
  test$res_sq1 = (test$res1)^2
  mse_lst  = c(mse_lst , mean(test$res_sq1))
}
mean(mse_lst)
# MSE = 0.254
# We observe a higher MSE (this is expected as in source data we didn't remove outliers)

# Final Model
Fin_Model = lm(imdb_score~poly(budget_in_millions, degree=4)+
                 poly(duration_in_hours,degree=3)+
                 genre_action+genre_adventure+genre_comedy+genre_biography+
                 genre_history+genre_mystery+genre_war+
                 main_actor1_is_female+
                 total_number_of_actors+total_number_of_producers+
                 actor1_tier+actor2_tier+actor3_tier+editor_tier+
                 producer_tier+production_company_tier+main_lang_category+Continent+
                 main_actor1_is_female*main_actor2_is_female*main_actor3_is_female,
               data = data_r_revised)

##########Model Analysis################
# Heteroskedasticity
plot(predict(Fin_Model),residuals(Fin_Model),col="red")
abline(0,0,lty=2)
ncvTest(Fin_Model) 
# Our residual plot looks good (values within 2)
# Heteroskedasticity shows we have no evidence against constant variance


# Predictor Significance
anova(Fin_Model)

# Outliers test
outlierTest(Fin_Model)
# gives record 256 as an outlier
data_r_revised2 = data_r[-c(256,480,607)]
# remove these data points, fit the final model on data without outliers
Fin_Model_r = lm(imdb_score~poly(budget_in_millions, degree=4)+
                   poly(duration_in_hours,degree=3)+
                   genre_action+genre_adventure+genre_comedy+genre_biography+
                   genre_history+genre_mystery+genre_war+
                   main_actor1_is_female+
                   total_number_of_actors+total_number_of_producers+
                   actor1_tier+actor2_tier+actor3_tier+editor_tier+
                   producer_tier+production_company_tier+main_lang_category+Continent+
                   main_actor1_is_female*main_actor2_is_female*main_actor3_is_female,
               data = data_r_revised2)
summary(Fin_Model_r)
stargazer(Fin_Model_r,type = 'text')
anova(Fin_Model_r)
saveRDS(Fin_Model_r, "model.rds")
