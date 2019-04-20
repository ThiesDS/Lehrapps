FROM rocker/shiny:latest

RUN echo "install.packages(c('mosaic','nycflights13','shinycssloaders','shinydashboard'))" > /tmp/packages.R

RUN Rscript /tmp/packages.R

EXPOSE 3838

CMD ["/usr/bin/shiny-server.sh"]