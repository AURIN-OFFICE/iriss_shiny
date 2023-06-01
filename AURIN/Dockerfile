
# FROM rocker/r-apt:bionic
#FROM rocker/r-rmd
FROM eddelbuettel/r2u:20.04

RUN apt-get update && apt-get install -y --no-install-recommends \
    sudo \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    && rm -rf /var/lib/apt/lists/*


RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    wget \
    graphviz \
    texlive-latex-extra \
    lmodern \
    perl




WORKDIR /AURIN/
COPY . .
RUN Rscript -e 'install.packages("shiny")' 
RUN Rscript -e 'install.packages("shinythemes")' 


EXPOSE 80
CMD ["Rscript", "-e", "shiny::runApp('inst/application/', port = 80, host = '0.0.0.0')"]


