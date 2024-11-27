#this API provides mostly a method to predict diabetes/non with a given set of inputs

#library loads
library(GGally)
library(leaflet)
library(tidyverse)
library(tidymodels)
library(plumber)

#read data in
api_data <- read_csv(file = 'diabetes_binary_health_indicators_BRFSS2015.csv')

#preprocessing
api_data <- api_data |> mutate(
  Diabetes_binary = factor(Diabetes_binary,labels=c("not diabetic","diabetic")),
  HighBP = factor(HighBP,labels=c("no high blood pressure","high blood pressure")),
  HighChol = factor(HighChol,labels=c("no high cholesterol","high cholesterol")),
  CholCheck = factor(CholCheck,labels=c("no recent cholesterol check","recent cholesterol check")),
  Smoker = factor(Smoker,labels=c("non-smoker","smoker")),
  Stroke = factor(Stroke,labels=c("no stroke","stroke")),
  HeartDiseaseorAttack = factor(HeartDiseaseorAttack,labels=c("no heart problems CHD/MI","heart problems CHD/MI")),
  PhysActivity = factor(PhysActivity,labels=c("no physical activity","physical activity")),
  Fruits = factor(Fruits,labels=c("no fruits","fruits")),
  Veggies = factor(Veggies,labels=c("no vegetables","vegetables")),
  HvyAlcoholConsump = factor(HvyAlcoholConsump,labels=c("no heavy alcohol","heavy alcohol")),
  AnyHealthcare = factor(AnyHealthcare,labels=c("no healthcare","healthcare")),
  NoDocbcCost = factor(NoDocbcCost,labels=c("not avoided doctor for cost","avoided doctor for cost")),
  DiffWalk = factor(DiffWalk,labels=c("no walking difficulty","walking difficulty")),
  Sex = factor(Sex,labels=c("female","male"))
)

#larger categories
api_data <- api_data |> mutate(
  GenHlth = factor(GenHlth,labels=c("excellent","very good","good","fair","poor")),
  Age = factor(Age,labels=c(
    "18-24",
    "25-29",
    "30-34",
    "35-39",
    "40-44",
    "45-49",
    "50-54",
    "55-59",
    "60-64",
    "65-69",
    "70-74",
    "75-79",
    "80+")),
  Education = factor(Education,labels=c(
    "Never attended school or only kindergarten",
    "Grades 1-8 (Elementary)",
    "Grades 9-11 (Some high school)",
    "Grades 12 or GED (High school graduate)",
    "College 1-3 years (Some college or technical school",
    "College 4+ years (College graduate)")),
  Income = factor(Income,labels=c(
    "[ - $10k)",
    "[$10k - $15k)",
    "[$15k - $20k)",
    "[$20k - $25k)",
    "[$25k - $35k)",
    "[$35k - $50k)",
    "[$50k - $75k)",
    "[$75k - ]"))
)

#convert doubles to ints
api_data <- api_data |> mutate(
  BMI = as.integer(BMI),
  MentHlth = as.integer(MentHlth),
  PhysHlth = as.integer(PhysHlth)
)

#read and fit model - randomforest was best
load("workflow_randomforest.rda")

#full first on data
final_fit <- workflow_randomforest |>
  fit(filter(api_data,row_number()<=1000))

#my stuff!
#info endpoint - reply with name and github pages url
#* @get /info
function() {
  #####################EDIT SITE LINK####
  #"My name is Andy Powers. You may access my Github pages site at: ****"
  #head(api_data)
  #workflow_classtree |> fit(filter(api_data,row_number()==1))
  predict(final_fit,head(api_data))
}
#localhost:1776/info

