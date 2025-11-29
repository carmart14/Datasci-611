# Makefile for BIOS611 clustering analysis
R = Rscript
RFLAGS = --vanilla

.PHONY: all run figures clean docker-build docker-run

# Default target
all: run figures

# Run both Task 1 and Task 2 (single R script)
run:
	$(R) $(RFLAGS) clustering.R

# Optional: message about where plots are stored
figures:
	@echo "Plots should be written by clustering.R into figures/"

# Clean up results
clean:
	rm -rf figures/* data/*.rds data/*.csv

# Docker convenience
DOCKER_IMAGE = bios611_clustering:latest

docker-build:
	docker build -t $(DOCKER_IMAGE) .

docker-run:
	docker run --rm -it -v "$(PWD)":/project --name bios611_container $(DOCKER_IMAGE) /bin/bash
