# Geometric and demographic effects explain contrasting fragmentation-biodiversity relationships across scales


<!-- badges: start -->
<!-- badges: end -->

*This repository contains the code and data to reproduce the analyses and figures in the paper: Geometric and demographic effects explain contrasting fragmentation-biodiversity relationships across scales*

## Authors

- Stav Gelber, Shane A. Blowes, Jonathan M. Chase, Andreas Huth, Frank M. Schurr, Britta Tietjen, Julian W. Zeller, Felix May

## Structure of the repository

-   `data-raw/`: Raw data from model simulations used to produce the figures. Each dataset includes the model parameters used to produce it.
-   `Model/`: Source code to run the model simulations as well as R sripts to run the model locally and on a cluster.
-   `R/`: R scripts to reproduce the figures in the paper from the raw data.

## How to reproduce the figures

1. Clone this repository to your computer.

2. Adjust the parameter file according to the parameters in the "...static_parameters.csv" file in the data-raw folder. 

3. Run the model either on a cluster using  `Model/cluster_model_run.R` or locally using `Model/run_model.R`.

4. Run the R scripts in the `R/` folder to reproduce the figures.