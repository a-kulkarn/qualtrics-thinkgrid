FROM rocker/r-ver:latest

# Use Posit Package Manager for pre-compiled binaries (matches CRAN's binary packages)
RUN echo "options(repos = c(CRAN = 'https://packagemanager.posit.co/cran/__linux__/jammy/latest'))" >> /usr/local/lib/R/etc/Rprofile.site

# Install Python (CRAN servers have Python available)
# This layer will be cached as long as these dependencies don't change
RUN apt-get update && apt-get install -y \
    python3 \
    python3-pip \
    python3-venv \
    && rm -rf /var/lib/apt/lists/*

# Install R package dependencies
# This layer will be cached as long as the package list doesn't change
RUN Rscript -e 'install.packages(c("png", "vdiffr", "lme4", "knitr", "rmarkdown", "sjPlot", "colorspace", "cowplot", "gifski", "gridExtra", "reticulate", "testthat"))'

# Set environment variable to ensure reticulate finds Python
ENV RETICULATE_PYTHON=/usr/bin/python3

# Copy package tarball last - this layer changes most frequently
# All layers above will be cached and reused
COPY ThinkingGrid_0.1.0.tar.gz .

CMD ["R", "CMD", "check", "--as-cran", "ThinkingGrid_0.1.0.tar.gz"]
