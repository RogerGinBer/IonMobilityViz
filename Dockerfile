FROM rocker/shiny:latest

LABEL Description="LC-IM-MS data visualization tool in R / Shiny "

## Stable dependencies
ADD install.R /tmp
RUN R -e "source('/tmp/install.R')"

## More rapidly changing dependencies
ADD install2.R /tmp
RUN R -e "source('/tmp/install2.R')"

ADD app.R /srv/shiny-server/IonMobilityViz/

RUN R -e ""
