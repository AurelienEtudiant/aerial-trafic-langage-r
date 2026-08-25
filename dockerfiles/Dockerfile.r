FROM rocker/r-ver:4

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
       libssl-dev \
       libsasl2-dev \
       libcurl4-openssl-dev \
       libicu-dev \
    && rm -rf /var/lib/apt/lists/*

RUN install2.r \
    --error \
    --skipinstalled \
    --ncpus -1 \
    dplyr \
    jsonlite \
    mongolite \
    stringr \
    tibble \
    && rm -rf /tmp/downloaded_packages

WORKDIR /workspace