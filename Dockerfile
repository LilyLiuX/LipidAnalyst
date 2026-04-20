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

RUN R -e "install.packages(c('renv','remotes'), repos='https://cloud.r-project.org')"

# ------------------------------------------------------------
# Install BiocManager to manage Bioconductor repositories
# ------------------------------------------------------------

RUN R -e "install.packages('BiocManager', repos='https://cloud.r-project.org')"

# ------------------------------------------------------------
# Prepare application directory and copy dependency lockfile
# Only the lockfile is copied at this stage to allow Docker
# layer caching when application code changes
# ------------------------------------------------------------

RUN mkdir -p /srv/shiny-server/lipidanalyst
COPY renv.lock /srv/shiny-server/lipidanalyst/renv.lock

WORKDIR /srv/shiny-server/lipidanalyst
          

# Set Bioconductor version
RUN R -e "options(repos = c(CRAN='https://cloud.r-project.org')); BiocManager::install(version='3.20', ask=FALSE, update=FALSE)"

# Install key Bioconductor dependencies explicitly
RUN R -e "options(repos = BiocManager::repositories()); BiocManager::install(c('DelayedArray','GenomicRanges','SummarizedExperiment','MultiAssayExperiment','MultiDataSet','ropls'), ask=FALSE, update=FALSE)"

# Install leidenAlg separately
RUN R -e "options(repos = c(CRAN='https://cloud.r-project.org')); install.packages('leidenAlg')"

# Restore remaining packages from renv.lock
RUN R -e "options(repos = BiocManager::repositories()); renv::restore(prompt = FALSE)"
# ------------------------------------------------------------
# Copy the full application source code into the container
# (executed after dependency restoration to optimize caching)
# ------------------------------------------------------------
       
COPY . /srv/shiny-server/lipidanalyst

RUN chmod -R 755 /srv/shiny-server/lipidanalyst
RUN rm -f /srv/shiny-server/lipidanalyst/.Rprofile

# ------------------------------------------------------------
# Expose the default Shiny Server port
# ------------------------------------------------------------

EXPOSE 3838

FROM rocker/shiny:4.4.2

# ------------------------------------------------------------
# Install required system-level dependencies for R packages
# (SSL, XML, graphics, fonts, and common compilation needs)
# ------------------------------------------------------------
RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    curl \
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

RUN R -e "install.packages(c('renv','remotes'), repos='https://cloud.r-project.org')"

# ------------------------------------------------------------
# Install BiocManager to manage Bioconductor repositories
# ------------------------------------------------------------

RUN R -e "install.packages('BiocManager', repos='https://cloud.r-project.org')"

# ------------------------------------------------------------
# Prepare application directory and copy dependency lockfile
# Only the lockfile is copied at this stage to allow Docker
# layer caching when application code changes
# ------------------------------------------------------------

WORKDIR /srv/shiny-server

COPY renv.lock renv.lock
COPY renv/ renv/
          

# Set Bioconductor version
RUN R -e "options(repos = c(CRAN='https://cloud.r-project.org')); BiocManager::install(version='3.20', ask=FALSE, update=FALSE)"

# Install key Bioconductor dependencies explicitly
RUN R -e "options(repos = BiocManager::repositories()); BiocManager::install(c('DelayedArray','GenomicRanges','SummarizedExperiment','MultiAssayExperiment','MultiDataSet','ropls'), ask=FALSE, update=FALSE)"

# Install leidenAlg separately
RUN R -e "options(repos = c(CRAN='https://cloud.r-project.org')); install.packages('leidenAlg')"

# Restore remaining packages from renv.lock
RUN R -e "options(repos = BiocManager::repositories()); renv::restore(prompt = FALSE)"
# ------------------------------------------------------------
# Copy the full application source code into the container
# (executed after dependency restoration to optimize caching)
# ------------------------------------------------------------
COPY . . 
RUN rm -f .Rprofile

# ------------------------------------------------------------
# Expose the default Shiny Server port
# ------------------------------------------------------------

EXPOSE 3838

WORKDIR /srv/shiny-server

COPY . /srv/shiny-server

