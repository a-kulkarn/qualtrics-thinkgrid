FROM rocker/tidyverse

# Install python directly. Building through R is too slow for testing.
RUN apt install python3-venv python3-pip python3-dev

# Copy files.
COPY install.R /usr/local/src/install_test/install.R

# Run the installation tests.
WORKDIR /usr/local/src/install_test
# CMD ["Rscript", "install.R"]

