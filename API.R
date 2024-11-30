#this API provides mostly a method to predict diabetes/non with a given set of inputs

#relevant codes for build, test, export, etc.
#BASH > gzip api_andypowers_sst558_project3.tar
#C:\stats>docker save -o api_andypowers_sst558_project3.tar api
#C:\stats>docker run --rm -p 1776:1776 api
#C:\stats>docker build -t api .


#library loads
library(GGally)
library(leaflet)
library(tidyverse)
library(tidymodels)
library(plumber)
library(DescTools)
library(ranger)

#set port 1776
options("plumber.port" = 1776)

#read raw data in
print(getwd())
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
  fit(api_data)
  #fit(filter(api_data,row_number()<=1000)) #for testing only!
api_data <- filter(api_data,row_number()<=1000)



#info endpoint - reply with name and github pages url
#* @get /info
function() {
  #predict(final_fit,head(data))
  "My name is Andy Powers. You may access my Github pages site at: https://apowers-ncsu.github.io/sst558-project3/"
}
#localhost:1776/info



#pred endpoint - take predictors and provide model prediction
#* @get /pred
#* @param BMI:int positive integer
#* @param MentHlth:int 0-30 days of poor mental health
#* @param PhysHlth:int 0-30 days of poor physical health
#* @param HighBP:int binary
#* @param HighChol:int binary
#* @param HeartDiseaseorAttack:int binary
#* @param Veggies:int binary
#* @param DiffWalk:int binary
function(
    BMI = defaults$BMI,
    MentHlth = defaults$MentHlth,
    PhysHlth = defaults$PhysHlth,
    HighBP = defaults$HighBP,
    HighChol = defaults$HighChol,
    HeartDiseaseorAttack = defaults$HeartDiseaseorAttack,
    Veggies = defaults$Veggies,
    DiffWalk = defaults$DiffWalk
    ) {

  #update to match EDA; adding 'x' because I can't deal with .env/.data yet
  xBMI <- as.integer(BMI)
  xMentHlth <- as.integer(MentHlth)
  xPhysHlth <- as.integer(PhysHlth)
  xHighBP <- if (HighBP == 0) "no high blood pressure" else "high blood pressure"
  xHighChol <- if (HighChol == 0) "no high cholesterol" else "high cholesterol"
  xHeartDiseaseorAttack <- if (HeartDiseaseorAttack == 0) "no heart problems CHD/MI" else "heart problems CHD/MI"
  xVeggies <- if (Veggies == 0) "no vegetables" else "vegetables"
  xDiffWalk <- if (DiffWalk == 0) "no walking difficulty" else "walking difficulty"
  
  #prep tibble to send for prediction
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
  
  #predict!
  predict(final_fit,params)

}
#localhost:1776/pred?BMI=40&MentHlth=20
#localhost:1776/pred?BMI=40&MentHlth=30&PhySHlth=30&HighBP=1&HighChol=1&HeartDiseaseorAttack=1&Veggies=1&DiffWalk=1
#localhost:1776/pred
#localhost:1776/pred?BMI=40&HighBP=1&HighChol=1&Veggies=0&DiffWalk=0



#confusion endpoint - visual of confusion matrix
#* @serializer png
#* @get /confusion
function() {
  #create confusion matrix
  truth_preds <- tibble(
    truth = api_data$Diabetes_binary,
    preds = predict(final_fit,api_data)
  )
  truth_preds <- truth_preds |> mutate(
    preds = as_factor(preds$.pred_class)
  )
  cm <- conf_mat(truth_preds,truth,preds)
  print(autoplot(cm, type = "heatmap"))
  
}
#localhost:1776/confusion