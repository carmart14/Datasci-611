---
title: "README"
output: html_document
---

## Requirements

-   **Docker** (for reproducible environment)
-   **R** (packages installed in the Dockerfile)
-   **RStudio Server** (optional, run via Docker at port 8787)
-   **Quarto** (for rendering the report)

## Running the Project

### Using Docker:

If you're comfortable with docker then boom:

1.  Build the container: \`\`\`bash docker build -t netflix_project .


If you're not comfortable with docker please install the desktop application first. 

Once you have that working, open the application and allow docker to interact with your WSL

-if you dont have wsl installed, please google it because I think that'll be more helpful than i would be-

once youve confirmed docker is working

-type this in your console: docker --version 
 if this prints a version, your golden.
 
Once you have thatdo back up to step one and build the container :) 


## running RStudio in Docker

docker run -d -p 8787:8787 -v \$(pwd):/home/rstudio/project --name netflix_rstudio netflix_project

Open RStudio in your browser: <http://localhost:8787>

Username: rstudio

Password: rstudio


here is my email if you need help: carmart@unc.edu

