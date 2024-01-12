# Bayesian spatio-temporal analysis of COVID-19 in Catalonia

In this repository can be found all of the work related to the manuscript under review "Bayesian spatio-temporal analysis of the COVID-19 pandemic in Catalonia", by Pau Satorra and Cristian Tebé.

## Content of this repository

### Code

- *MakingData.R*: import and clean the data from the official open data catalogue of the Government of Catalonia (https://analisi.transparenciacatalunya.cat/en/).
- *SAE.R*: estimation of the models with INLA.

All of the code is done using _R_ software version 4.3.0. Mainly, the _R_ packages _dplyr_ and _purrr_ have been used for data management and _R-INLA_ for estimating the models with INLA.

### Notebook

- *Analysis_Spatial_Temporal*: contains the description of the variables of the study and presents the results of the spatial and the spatio-temporal models, including also the ABS characteristics in the modely.
- *Analysis_Spatial_Temporal_Vaccination*: contains the description of the vaccination data and the results of the estimated spatio-temporal model including the vaccination effect.

For each notebook, the R-markdown file with the code for elaborating the notebook is included together with the resulting html notebook in the *Reports/* folder.

### Data

- *dat.Rda*: contains the evolution of the observed, expected, rate and standardised incidence ratios (SIR) of cases/hospitalisation/vaccination, for each ABS and week. It also has the information about the ABS characteristics. It is created in *SAE.R* and it contains all the information needed to run the different spatio-temporal models.

- *shapefileT.R*: contains the geography of the basic health areas (ABS) of Catalonia

- *0_Raw/*: this folder contains the external data needed, other than the data that is read through an API from the opendata portal of the Government of Catalonia.

- *1_Processed/*: this folder contains the processed data from *MakingData.R*

To have the data that would be inside the *2_Analysed/* folder, *SAE.R* has to be compiled. We have not uploaded them because of the big size of the files.

### Shiny application

In the *COVIDCAT_Evo/* folder we can find all of the code for developing the R shiny application (https://brui.shinyapps.io/covidcat_evo/), which visualizes the results of the study.




