# Base R image
FROM rocker/tidyverse:latest

# Install required packages
RUN R -e "install.packages(c('tidyverse', 'cluster', 'ggplot2', 'plotly'))"

# Set working directory
WORKDIR /project

# Copy all project files into container
COPY . /project

# Default command when container starts
CMD ["make", "all"]
