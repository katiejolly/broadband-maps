#FROM 202306134469.dkr.ecr.us-west-2.amazonaws.com/dataeng-datasci-ooklaverse-shiny:0.6.1
# get shiny serves plus tidyverse packages image
FROM rocker/shiny-verse:latest

ENV SHINY_LOG_STDERR 1

# Install additional system packages
RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    libgdal-dev \
    libmysqlclient-dev \
    libudunits2-dev \
  && rm -rf /var/lib/apt/lists/* \
  && rm -rf /srv/shiny-server

RUN install2.r --error \
    ggrepel \
    showtext \
    sf \
    ggtext \
    janitor \
    cowplot \
    shadowtext \
    tippy \
    ggspatial \
    raster \
    ragg \
    lwgeom \
    Cairo \
    ggnewscale \
    DBI \
    formattable

COPY app /srv/shiny-server/