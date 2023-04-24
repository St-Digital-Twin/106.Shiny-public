FROM rocker/shiny:latest
#RUN sudo add-apt-repository -y ppa:cran/imagemagick
RUN apt-get update && apt-get install -y --no-install-recommends \
sudo \
    libssl-dev \
    libcurl4-gnutls-dev \
    libxml2-dev \
    libudunits2-dev \
    libgdal-dev \
    zlib1g-dev \
    libv8-dev \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \   
    libssh2-1-dev \
    libxml2-dev \
    libsodium-dev \
    libfreetype6-dev \
    libmagick++-dev \
    && rm -rf /var/lib/apt/lists/*
#, 'textshaping' , 'ragg'
# Install R packages
RUN R -e "install.packages(c('shiny', 'patchwork', 'thematic', 'shinyWidgets', 'plotly', 'ggplot2','bs4Dash', \
'lubridate', 'remotes', 'tidyverse','mapdeck',  'dplyr', 'magrittr', \
'shinythemes','rdrop2', 'textshaping', 'ragg', 'tidyverse', 'stringr', 'plotly', \
'cowplot','qs','data.table', 'DT', 'shinycssloaders', 'shinyjs','fresh', 'sodium'),  \
repos =  c('http://rstudio.org/_packages', 'http://cran.rstudio.com'))"
# Create app directory

COPY ./ ./srv/shiny-server/

#COPY ui.R /srv/shiny-server/
#COPY www /srv/shiny-server/www


EXPOSE 4010

CMD ["R", "-e", "shiny::runApp('./srv/shiny-server/',host = '0.0.0.0', port = 4010)"]
