FROM rocker/shiny:4.4.2

# ------------------------------------------------------------
# Install required system-level dependencies for R packages
# (SSL, XML, graphics, fonts, and common compilation needs)
# ------------------------------------------------------------
RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    curl\
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libglpk-dev \
    libgmp-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    cmake \
    pkg-config \
    libuv1-dev \
    libgit2-dev \
    && rm -rf /var/lib/apt/lists/*
    
# ------------------------------------------------------------
# Install core R helper packages required for dependency
# management and reproducible environments
# ------------------------------------------------------------

RUN R -e "install.packages(c('renv','remotes','BiocManager'), repos='https://cloud.r-project.org')"

# ------------------------------------------------------------
# Prepare app directory for Scheme A
# App will live at /srv/shiny-server/lipidanalyst
# ------------------------------------------------------------
RUN mkdir -p /srv/shiny-server/lipidanalyst
WORKDIR /srv/shiny-server/lipidanalyst

# ------------------------------------------------------------
# Copy lockfile and renv metadata first for better layer caching
# ------------------------------------------------------------
COPY renv.lock /srv/shiny-server/lipidanalyst/renv.lock
COPY renv/ /srv/shiny-server/lipidanalyst/renv/
          

# Set Bioconductor version
RUN R -e "options(repos = c(CRAN='https://cloud.r-project.org')); BiocManager::install(version='3.20', ask=FALSE, update=FALSE)"

# Install key Bioconductor dependencies explicitly
RUN R -e "options(repos = BiocManager::repositories()); BiocManager::install(c('DelayedArray','GenomicRanges','SummarizedExperiment','MultiAssayExperiment','MultiDataSet','ropls'), ask=FALSE, update=FALSE)"

# Install leidenAlg separately
RUN R -e "install.packages('remotes', repos='https://cloud.r-project.org'); \
          remotes::install_version('leidenAlg', version = '1.1.6', repos='https://cloud.r-project.org')"

# Restore remaining packages from renv.lock
RUN R -e "options(repos = BiocManager::repositories()); renv::restore(prompt = FALSE)"
# ------------------------------------------------------------
# Copy the full application source code into the container
# (executed after dependency restoration to optimize caching)
# ------------------------------------------------------------
       
COPY . /srv/shiny-server/lipidanalyst

# ------------------------------------------------------------
# Clean up and set permissions
# ------------------------------------------------------------
RUN rm -f /srv/shiny-server/lipidanalyst/.Rprofile && \
    chmod -R 755 /srv/shiny-server/lipidanalyst

# ------------------------------------------------------------
# Expose the default Shiny Server port
# ------------------------------------------------------------

EXPOSE 3838