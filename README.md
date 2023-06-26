# Bayesian spatio-temporal analysis of COVID-19 in Catalonia

In this repository can be found all of the work related to my Fundamental Principles of Data Science Master's Thesis of the University of Barcelona (UB) supervised by Dr. Cristian Tebé and Dra. Laura Igual. 

## Abstract

In this study, we model the COVID-19 pandemic in the different basic health areas (ABS) in Catalonia, describing the spatial, temporal and spatio-temporal trends of reported COVID-19 cases and hospitalisations. As the ABS are small areas, spatio-temporal small area estimation methods have to be used, which allow us to borrow strength from neighbouring areas and time points. In particular, we use Bayesian hierarchical spatio-temporal models estimated with INLA, providing a very flexible and robust tool that allows the specification of a wide variety of different models, from which the best one is selected according to the model performance. Results are found to be heterogeneous and different hotspots and coldspots are identified both over the whole pandemic period and at different points in time. The analysis of the impact of some characteristics of the ABS shows that urban areas are at higher risk of COVID-19 cases and hospitalisations, while socio-economic deprivation of the area is a risk factor for hospitalisations. In addition, full vaccination coverage of an ABS is shown to have a protective effect on the risk of COVID-19 cases and hospitalisations in specific waves of the pandemic.

## Content of this repository

### Code

- *MakingData.R*: import and clean the data from the official open data catalogue of the Government of Catalonia (https://analisi.transparenciacatalunya.cat/en/).
- *SAE.R*: estimation of the models with INLA.

All of the code is done using _R_ software version 4.3.0. Mainly the _R_ packages _dplyr_ and _purrr_ have been used for data management and _R-INLA_ for estimating the models with INLA.

### Notebook

- *Analysis_Spatial_Temporal*: contains the description of the variables of the study and presents the results of the spatial and the spatio-temporal models, including also the ABS characteristics in the modely.
- *Analysis_Spatial_Temporal_Vaccination*: contains the description of the vaccination data and the results of the estimated spatio-temporal model including the vaccination effect.

For each notebook, the R-markdown file with the code for elaborating the notebook is included together with the resulting html notebook.




