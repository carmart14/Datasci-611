#============================#
# 1. Define targets          #
#============================#

# Default target
all: report



#------------------------------------#
# Load and clean raw data
#------------------------------------#
data/processed/clean_user_data.rds data/processed/mov_show_data.rds: load_data.R \
data/raw/netflix_users.csv data/raw/Netflix_Movies_and_TV_Shows.csv
	Rscript load_data.R



#----------------------------#
# Analysis step
#----------------------------#

# Movies & Shows Analysis
data/processed/content_count.rds \
data/processed/avg_duration_summary.rds \
data/processed/genre_mat.RDS \
data/processed/country_type_summary.RDS \
data/processed/country_genre_summary.RDS: netflix_moviesandshow_analysis.R \
data/processed/mov_show_data.rds
	Rscript netflix_moviesandshow_analysis.R

# User Analysis
data/processed/age_genre_summary.csv \
data/processed/age_genre_anova.csv \
data/processed/age_genre_models.rds \
data/processed/watch_summary.csv \
data/processed/subscription_two_way_anova.csv \
data/processed/subscription_model.rds: netflix_user_analysis.R \
data/processed/clean_user_data.rds
	Rscript netflix_user_analysis.R



#----------------------------#
# Figures step
#----------------------------#
figures/age_genre_plot.png figures/watch_time_plot.png: figures.R outputs/cleaned_data.rds outputs/age_genre_models.rds outputs/subscription_model.rds
	Rscript figures.R

#----------------------------#
# Report step
#----------------------------#
report: figures/age_genre_plot.png figures/watch_time_plot.png report.Rmd
	Rscript -e "rmarkdown::render('report.Rmd', output_file='report.html')"

#----------------------------#
# Clean step
#----------------------------#
clean:
	rm -rf outputs/* figures/* report.html
