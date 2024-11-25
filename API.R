#myAPI.R 
library(GGally)
library(leaflet)

#Send a message
#* @get /readme
function(){
  "This is our basic API"
}

#http://localhost:PORT/readme


#* Find natural log of a number
#* @param num Number to find ln of
#* @get /ln
function(num){
  log(as.numeric(num))
}

#query with http://localhost:PORT/ln?num=1

#* Find multiple of two numbers
#* @param num1 1st number
#* @param num2 2nd number
#* @get /mult
function(num1, num2){
  as.numeric(num1)*as.numeric(num2)
}

#query with http://localhost:PORT/mult?num1=10&num2=20

#* Plot of iris data
#* @serializer png
#* @param type base or ggally
#* @param color TRUE or FALSE (only for ggally)
#* @get /plotiris
function(type = "base", color = FALSE){
  if(tolower(type) == "ggally"){
    if(color){
      a <- GGally::ggpairs(iris, aes(color = Species))
      print(a)
    } else {
      a <- GGally::ggpairs(iris)
      print(a)
    }
  } else {
    pairs(iris)
  }
}
#http://localhost:PORT/plotiris?type=ggally


#* Plotting widget
#* @serializer htmlwidget
#* @param lat latitude
#* @param lng longitude
#* @get /map
function(lng = 174.768, lat = -36.852){
  m <- leaflet::leaflet() |>
    addTiles() |>  # Add default OpenStreetMap map tiles
    addMarkers(as.numeric(lng), as.numeric(lat))
  m  # Print the map
}

#query with http://localhost:PORT/map?lng=174&lat=-36


# Choose a predictor
#* @param predictor
#* @get /pred
function(predictor) {
  data <- iris
  if (is.numeric(data[[predictor]])) {
    value <- mean(data[[predictor]])
    message <- paste("The mean of", predictor, "is", value)
    return(message)
  } else if (predictor == "Species") {
    table <- table(data[[predictor]])
    return(paste0(names(table), ": ", table))
  } else {
    stop("Invalid predictor.")
  }
}