#this API provides mostly a method to predict diabetes/non with a given set of inputs

#library loads
library(GGally)
library(leaflet)
library(tidyverse)
library(tidymodels)
library(plumber)
library(DescTools)

#set port 1776
options("plumber.port" = 1776)

#read raw data in
api_data <- read_csv(file = 'diabetes_binary_health_indicators_BRFSS2015.csv')
raw_data <- api_data #for getting means, modes as defaults

#run EDA to prep for model
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
) |> mutate(
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
) |> mutate(
  BMI = as.integer(BMI),
  MentHlth = as.integer(MentHlth),
  PhysHlth = as.integer(PhysHlth)
)

#find means and most prevalent values for use as defaults
defaults <- tibble(
  BMI = round(mean(raw_data$BMI),0),
  MentHlth = round(mean(raw_data$MentHlth),0),
  PhysHlth = round(mean(raw_data$PhysHlth),0),
  HighBP = Mode(raw_data$HighBP),
  HighChol = Mode(raw_data$HighChol),
  HeartDiseaseorAttack = Mode(raw_data$HeartDiseaseorAttack),
  Veggies = Mode(raw_data$Veggies),
  DiffWalk = Mode(raw_data$DiffWalk)
)

#load and fit model - randomforest was best
load("workflow_randomforest.rda")
final_fit <- workflow_randomforest |>
  #fit(filter(api_data,row_number()<=1000)) ########## EDIT LATER to full data
  fit(api_data)

#info endpoint - reply with name and github pages url
#* @get /info
function() {
  #predict(final_fit,head(data))
  "My name is Andy Powers. You may access my Github pages site at: https://apowers-ncsu.github.io/sst558-project3/"
}
#localhost:1776/info

#pred endpoint - take predictors and provide model prediction
#* @get /pred
#* @param xBMI positive integer
#* @param xMentHlth 0-30 days of poor mental health
#* @param xPhysHlth 0-30 days of poor physical health
#* @param xHighBP binary
#* @param xHighChol binary
#* @param xHeartDiseaseorAttack binary
#* @param xVeggies binary
#* @param xDiffWalk binary
function(
    xBMI = defaults$BMI,
    xMentHlth = defaults$MentHlth,
    xPhysHlth = defaults$PhysHlth,
    xHighBP = defaults$HighBP,
    xHighChol = defaults$HighChol,
    xHeartDiseaseorAttack = defaults$HeartDiseaseorAttack,
    xVeggies = defaults$Veggies,
    xDiffWalk = defaults$DiffWalk
    ) {




  #predict(final_fit,head(data))
  
  #first, run the EDA needed on the vars
  ####FIX THIS NEXT 
  #gotta see whether i need to do this, given that I can't quite FACTOR
  #do i need to send in the number or the label? probably number but...

  #bmi, mental, physical all are fine as numerics
  xBMI <- as.integer(xBMI)
  xMentHlth <- as.integer(xMentHlth)
  xPhysHlth <- as.integer(xPhysHlth)
  xHighBP <- if (xHighBP == 0) "no high blood pressure" else "high blood pressure"
  xHighChol <- if (xHighChol == 0) "no high cholesterol" else "high cholesterol"
  xHeartDiseaseorAttack <- if (xHeartDiseaseorAttack == 0) "no heart problems CHD/MI" else "heart problems CHD/MI"
  xVeggies <- if (xVeggies == 0) "no vegetables" else "vegetables"
  xDiffWalk <- if (xDiffWalk == 0) "no walking difficulty" else "walking difficulty"
  
  params <- add_row(api_data[0,]) |> 
    mutate(
      BMI = xBMI,
      MentHlth = xMentHlth,
      PhysHlth = xPhysHlth,
      HighBP = xHighBP,
      HighChol = xHighChol,
      HeartDiseaseorAttack = xHeartDiseaseorAttack,
      Veggies = xVeggies,
      DiffWalk = xDiffWalk
  )
  
  predict(final_fit,params)

}
#localhost:1776/pred
