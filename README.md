# Intermedial Agenda Setting in Election Campaigns: Examining the Influence of Candidates' Tweets on the Media Agenda

This repository contains all code and data to reproduce all results, tables and figures of the thesis

## R Scripts

* dataprep_media.R: data preparation for Media Data (print and TV)
* immi_tweet_selection.R: select immigration-related tweets with quanteda (see Important Remark)
* build_timeseries.R: building timeseries data necessary for estimating VAR models
* descriptive_statistics.R: plotting all figures in the descriptive statistics part of the thesis
* functions_model_creation.R: contains only functions for calculating and processing IRFs
* plot_irf.R: contains only functions to plot IRFs as can be seen in the results part
* model_specification.R: model specification, using functions from functions_model_creation.R and plot_irf.R

## Data
Folder contains all relevant data for the results in the thesis.
All data can be created from the data in the data/raw folder.

## Tables
Folder conatains all tables of the thesis in tex format. The suffix "impute" and "monday" indicate the different imputation approaches.


## Plots
Folder contains all descriptive plots as well as the IRFs from the results part. The suffix "impute" and "monday" indicate the different imputation approaches.

## IMPORTANT REMARK
Twitter does not allow sharing full text of Tweets. Therefore immi_tweets.rds and full_data_fin.rds do not contain any text but only the IDs of the Tweets, the author ID, whether the Tweet is a retweet and the time it was created. For this reason it is not possible to run immi_tweet_selection.R since it is relying on the text of the tweets. However, all relevant are present in the data folder.
