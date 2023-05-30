FROM rocker/r-ver:latest

RUN apt-get update && apt-get install -y --no-install-recommends \
    sudo \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    && rm -rf /var/lib/apt/lists/*

RUN addgroup --system app \
    && adduser --system --ingroup app app
WORKDIR /AURIN/
COPY . .
RUN R -e "options(warn=2); install.packages('shiny')"


EXPOSE 4171
CMD ["R", "-e", "shiny::runApp('inst/application',port= 4171)"]


