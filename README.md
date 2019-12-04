# Sixty-Six Wards Data

## Getting Started

This repository includes all of my scripts to clean and format historical election data. If you want to rerun the scripts or load the data, you will need R.

The eventual goal is to provide all of the necessary data to recreate my work on [Sixty-Six Wards](https://sixtysixwards.com).

## Just want to use the data?
All of the outputs live in processed_data/

`df_all_YYYY-MM-DD.Rds` contains results for all Philadelphia elections since 2002, [crosswalked](https://sixtysixwards.com/home/crosswalk-tutorial/) to present-day divisions (see below).

`df_major_YYYY-MM-DD.Rds` contains crosswalked results for "major" races, which excludes ballot questions, judicial retentions, and some other races. This is what you usually want.

`div_cats_YYYY-MM-DD.Rds` contains the [divisions' categories](https://sixtysixwards.com/home/a-note-on-names/) using historical correlations.

`candidates_YYYY-MM-DD.Rds` contains unique candidate ids, seeking to connect similar people across elections, when their name on the ballot may have changed. The column `candidate_id` is unique.

`candidates_to_races_YYYY-MM-DD.Rds` contains the mapping of a given race and name on the ballot to the cleaned `candidate_id`.

## Scripts
The scripts in prep_data create those processed outputs from raw_data.

`01_create_division_crosswalk.R` creates the crosswalk files to map data for historical division boundaries to current division boundaries.

`02_crosswalk_and_clean_results.R` uses those crosswalk files to process the historical election results, creating `df_all_*.Rds` and `df_major_*.Rds`.

`03_div_svd.R` uses my local svd-correlation library (forthcoming) to calculate the division categories, creating `div_cats_*.Rds`.

`04_candidates.R` attempts to create a list of unique *people* from the candidates in many races. The main challenge is combining people when they may have spelled their names differently on different ballots. A large part of this is manual; if you find an error, please let me know!

## Raw Data
I couldn't do this without the fantastic open data work of the City Commissioners' Office and [Open Data Philly](https://opendataphilly.org). The raw data I use here includes historic GIS boundaries for divisions, the raw election results from [philadelphiavotes.com](https://philadelphiavotes.com), and Census data downloaded from American Fact Finder.

## Questions? Comments?
This is **very much** a work in progress. If you try to use the data and encounter any frictions, please let me know!

If you use the data, please cite me, and this repo.
