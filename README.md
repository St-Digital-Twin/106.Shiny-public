# City Dtwin GUI

A GUI for the Dtwin City package made with RShiny. Check it out on ShinyApps.io [here](https://benyamindsmith.shinyapps.io/starBlissGUI/) or build the Docker Image with the [instructions below]


## Dependencies

This Shiny App depends on the following packages:

'shiny','shinyWidgets', 'plotly', 'ggplot2','bs4Dash', 'lubridate', 'remotes', 'tidyverse','mapdeck',  'dplyr', 'magrittr', 
'shinythemes','rdrop2', 'textshaping', 'ragg', 'tidyverse', 'stringr', 'plotly', 'cowplot','qs','data.table','shinycssloaders', 'shinyjs','fresh'


## Building the Docker Image

To build the docker image simply run:

```
docker build -t city .
```

To run docker container: 

```
docker run -p 4022:4022 city

```

