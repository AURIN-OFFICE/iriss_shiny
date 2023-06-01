

# FROM rocker/r-apt:bionic
#FROM rocker/r-rmd
FROM eddelbuettel/r2u:20.04

# Set the locale


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


#WORKDIR /AURIN/
#COPY . .
#RUN Rscript -e 'install.packages("shiny")'
RUN Rscript -e 'install.packages("stringi")'
#RUN install.r
#EXPOSE 80

RUN Rscript -e 'install.packages("shiny")'
#CMD ["Rscript", "-e", "shiny::runApp('inst/application/', port = 80, host = '0.0.0.0')"]

RUN apt-get install -y locales locales-all
ENV LC_ALL en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US.UTF-8

## Install LaTeX
## Source: https://registry.hub.docker.com/u/rocker/hadleyverse/dockerfile/
RUN apt-get update \
  && apt-get install -y --no-install-recommends \
         ghostscript \
    imagemagick \
    lmodern \
    texlive-fonts-recommended \
    texlive-humanities \
    texlive-latex-extra \
    texinfo \
  && apt-get clean \
  && cd /usr/share/texlive/texmf-dist \
  && wget http://mirrors.ctan.org/install/fonts/inconsolata.tds.zip \
  && unzip inconsolata.tds.zip \
  && rm inconsolata.tds.zip \
  && echo "Map zi4.map" >> /usr/share/texlive/texmf-dist/web2c/updmap.cfg \
  && mktexlsr \
  && updmap-sys

## Install Pandoc
RUN mkdir -p /opt/pandoc \
  && wget --no-check-certificate -O /tmp/pandoc.zip https://s3.amazonaws.com/rstudio-buildtools/pandoc-1.13.1.zip \
  && unzip -j /tmp/pandoc.zip "pandoc-1.13.1/linux/debian/x86_64/pandoc" -d /opt/pandoc \
  && chmod +x /opt/pandoc/pandoc \
  && ln -s /opt/pandoc/pandoc /usr/local/bin \
  && unzip -j /tmp/pandoc.zip "pandoc-1.13.1/linux/debian/x86_64/pandoc-citeproc" -d /opt/pandoc \
  && chmod +x "/opt/pandoc/pandoc-citeproc" \
  && ln -s "/opt/pandoc/pandoc-citeproc" /usr/local/bin \
  && rm -f /tmp/pandoc.zip

## Install knitr and pander package


WORKDIR /AURIN/
COPY . .

#RUN Rscript -e 'install.packages("shiny")'
#RUN Rscript -e 'install.packages("stringi")'
RUN install.r
EXPOSE 80

#CMD ["Rscript", "-e", 'install.packages("shiny")']
CMD ["Rscript", "-e", "shiny::runApp('inst/application/', port = 80, host = '0.0.0.0')"]

