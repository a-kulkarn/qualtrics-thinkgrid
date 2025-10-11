FROM rocker/r-ver:latest

# Use Posit Package Manager for pre-compiled binaries (matches CRAN's binary packages)
RUN echo "options(repos = c(CRAN = 'https://packagemanager.posit.co/cran/__linux__/jammy/latest'))" >> /usr/local/lib/R/etc/Rprofile.site

# Install system dependencies for R packages
RUN apt-get update && apt-get install -y \
    libpng-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libjpeg-dev \
    libtiff-dev \
    cargo \
    cmake \
    texlive-latex-base \
    texlive-fonts-recommended \
    texlive-fonts-extra \
    texlive-latex-extra \
    qpdf \
    tidy \
    pandoc \
    && rm -rf /var/lib/apt/lists/*

# Install Python (CRAN servers have Python available)
RUN apt-get update && apt-get install -y \
    python3 \
    python3-pip \
    python3-venv \
    && rm -rf /var/lib/apt/lists/*

# Install R package dependencies in separate layers for better caching
# Core dependencies (least likely to change)
RUN Rscript -e 'install.packages(c("methods", "stats", "utils", "grid"))'

# Import dependencies
RUN Rscript -e 'install.packages(c("colorspace", "cowplot", "ggplot2", "gifski", "gridExtra", "scales"))'
RUN Rscript -e 'install.packages(c("reticulate"))'

# Suggest dependencies for testing
RUN Rscript -e 'install.packages(c("testthat", "vdiffr", "png"))'
RUN Rscript -e 'install.packages(c("knitr", "rmarkdown"))'
RUN Rscript -e 'install.packages(c("lme4", "sjPlot"))'

# # Set environment variable to ensure reticulate finds Python
# ENV RETICULATE_PYTHON=/usr/bin/python3

# Copy package tarball last - this layer changes most frequently
# All layers above will be cached and reused
COPY ThinkingGrid_0.1.0.tar.gz .

CMD ["R", "CMD", "check", "--as-cran", "ThinkingGrid_0.1.0.tar.gz"]
