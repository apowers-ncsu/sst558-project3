# start from the rstudio/plumber image
FROM rstudio/plumber

# install the linux libraries needed for plumber
RUN apt-get update -qq && apt-get install -y  libssl-dev  libcurl4-gnutls-dev  libpng-dev pandoc 
    
    
# install plumber, GGally
RUN R -e "install.packages(c('GGally', 'leaflet', 'plumber', 'tidyverse', 'tidymodels', 'DescTools', 'ranger'))"

# copy myAPI.R from the current directory into the container
COPY API.R API.R
COPY diabetes_binary_health_indicators_BRFSS2015.csv diabetes_binary_health_indicators_BRFSS2015.csv
COPY workflow_randomforest.rda workflow_randomforest.rda

# open port to traffic
EXPOSE 1776

# when the container starts, start the myAPI.R script
ENTRYPOINT ["R", "-e", \
    "pr <- plumber::plumb('API.R'); pr$run(host='0.0.0.0', port=1776)"]
