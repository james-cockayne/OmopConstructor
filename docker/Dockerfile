FROM rocker/verse:latest

WORKDIR /code

RUN apt-get update && apt-get install -y \
        default-jdk \
        r-cran-rjava \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c('CDMConnector', 'DatabaseConnector', 'pak'))"
RUN R -e "pak::pkg_install('ohdsi/OmopConstructor')"

COPY docker/scripts .

ENTRYPOINT ["/bin/bash", "-c", "Rscript main.r"]
