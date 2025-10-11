# FROM r-base
FROM rocker/tidyverse
COPY ThinkingGrid_0.1.0.tar.gz .

RUN Rscript -e 'install.packages(c("png", "vdiffr", "lme4", "knitr", "sjPlot", "colorspace", "cowplot", "gifski", "gridExtra", "reticulate"))'

CMD ["R", "CMD", "check", "ThinkingGrid_0.1.0.tar.gz"]
