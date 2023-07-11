FROM rocker/shiny:latest

LABEL Description="LC-IM-MS data visualization tool in R / Shiny "

## Get igraph dependency
RUN apt-get -y update && apt-get -y install libglpk-dev  libnetcdf-dev

RUN echo

## Stable dependencies
ADD install.R /tmp
RUN R -e "source('/tmp/install.R')"

## More rapidly changing dependencies
ADD install2.R /tmp
RUN R -e "source('/tmp/install2.R')"

COPY --chown=shiny:shiny app.R utils.R /srv/shiny-server/IonMobilityViz/
