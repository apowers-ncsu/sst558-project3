#this API provides mostly a method to predict diabetes/non with a given set of inputs

#library loads
library(GGally)
library(leaflet)
library(tidyverse)
library(tidymodels)
library(plumber)

#read prepped data in
load("data.RData")

#load and fit model - randomforest was best
load("workflow_randomforest.rda")
final_fit <- workflow_randomforest |>
  fit(filter(data,row_number()<=1000)) ########## EDIT LATER to full data

#info endpoint - reply with name and github pages url
#* @get /info
function() {
  #predict(final_fit,head(data))
  "My name is Andy Powers. You may access my Github pages site at: https://apowers-ncsu.github.io/sst558-project3/"
}
#localhost:1776/info

