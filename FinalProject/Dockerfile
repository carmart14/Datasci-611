FROM rocker/verse
# Install system dependencies for R packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    pandoc \
    wget \
    git \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c('dplyr','tidyverse','ggplot2','scales','factoextra','pheatmap','purrr','lme4','reshape2','broom', 'quarto'))"

# Install Quarto CLI
RUN wget https://quarto.org/download/latest/quarto-linux-amd64.deb \
    && apt install -y ./quarto-linux-amd64.deb \
    && rm quarto-linux-amd64.deb


# Set working directory
WORKDIR /project

# Copy project files into container
COPY . /project

# Default command
#CMD ["quarto", "render", "report.qmd", "--to", "html"]


CMD ["/usr/lib/rstudio-server/bin/rserver", "--server-daemonize=0"]








