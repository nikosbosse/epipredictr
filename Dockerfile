## Start with the docker image
FROM rocker/tidyverse:latest

MAINTAINER "Nikos Bosse" nikosbosse@gmail.com

RUN apt-get install -y \
    texlive-latex-recommended \
    texlive-fonts-extra \
    texinfo \
    libqpdf-dev \
    && apt-get clean

ADD . /home/rstudio/

## Get packages
RUN Rscript -e "install.packages('rstan', dependencies = TRUE)"
## RUN Rscript -e "devtools::install_dev_deps()"