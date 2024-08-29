# Example shiny app docker file
# https://blog.sellorm.com/2021/04/25/shiny-app-in-docker/

# get shiny serveR and a version of R from the rocker project
FROM rocker/shiny:4.0.5

# system libraries
# Try to only install system libraries you actually need
# Package Manager is a good resource to help discover system deps
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    glpk-utils \
    libssl-dev \
    libxml2-dev \
    libgmp-dev
  

# install R packages required 
# Change the packages list to suit your needs
RUN R -e 'install.packages(c(\
              "shiny", \
              "shinydashboard", \
              "mongolite", \
              "dplyr", \
              "glue", \
              "reactable", \
              "DT", \
              "plotly", \
              "tidyr", \
              "shinyjs", \
              "shinyalert", \
              "shinyWidgets", \
              "ddpcr", \
              "scrypt", \
              "shinyBS", \
              "spsComps", \
              "jsonlite", \
              "spsComps", \
              "networkD3", \
              "devtools", \
              "remotes", \
              "shinythemes", \
              "visNetwork", \
              "rjson", \
              "ggplot2", \
              "plyr", \
              "shinycssloaders", \
              "htmltools" \
            ), \
            repos="https://packagemanager.rstudio.com/cran/__linux__/focal/2021-04-23"\
          )'

# RUN Rscript -e 'remotes::install_github("igraph/rigraph@master")'

RUN Rscript -e 'remotes::install_github("datastorm-open/shinymanager")'

RUN R -e "options(shiny.maxRequestSize = 32*1024^2)"

# copy the app directory into the image
COPY /InterDictBio /srv/shiny-server/

# Copy configuration files into the Docker image
COPY shiny-server.conf  /etc/shiny-server/shiny-server.conf

RUN rm /srv/shiny-server/index.html

# Make the ShinyApp available at port 80
EXPOSE 80

# Copy further configuration files into the Docker image
COPY shiny-server.sh /usr/bin/shiny-server.sh
RUN ["chmod", "+x", "/usr/bin/shiny-server.sh"]

# run app
CMD ["/usr/bin/shiny-server"]