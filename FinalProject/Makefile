#============================#
# 1. Default Target          #
#============================#

all: report

#============================#
# Load and Clean Raw Data    #
#============================#
data/processed/clean_user_data.rds data/processed/mov_show_data.rds: \
	scripts/load_data.R \
	data/raw/netflix_users.csv \
	data/raw/Netflix_Movies_and_TV_Shows.csv
	Rscript scripts/load_data.R


#============================#
# Movies & Shows Analysis    #
#============================#
data/processed/content_count.rds \
data/processed/avg_duration_summary.rds \
data/processed/genre_mat.RDS \
data/processed/country_type_summary.RDS \
data/processed/country_genre_summary.RDS: \
        scripts/netflix_movieandshow_analysis.R \
        data/processed/mov_show_data.rds
	Rscript scripts/netflix_movieandshow_analysis.R

#============================#
# User Analysis              #
#============================#
data/processed/age_genre_summary.csv \
data/processed/age_genre_anova.csv \
data/processed/age_genre_models.rds \
data/processed/watch_summary.csv \
data/processed/subscription_two_way_anova.csv \
data/processed/subscription_model.rds: \
        scripts/netflix_user_analysis.R \
        data/processed/clean_user_data.rds
	Rscript scripts/netflix_user_analysis.R

#============================#
# Figures                    #
#============================#
figures/age_genre_by_country_plot.png \
figures/watch_time_by_subscrip_plot.png: \
        figures/plots.R \
        data/processed/clean_user_data.rds \
        data/processed/age_genre_models.rds \
        data/processed/subscription_model.rds
	Rscript figures/plots.R

#============================#
# Report                     #
#============================#
#----------------------------#
# Report step
#----------------------------#
report: figures/age_genre_by_country_plot.png figures/watch_time_by_subscrip_plot.png scripts/report.qmd
	quarto render scripts/report.qmd --to html --output report.html


#============================#
# Clean                      #
#============================#
clean:
	rm -rf data/processed/* figures/* report.html
