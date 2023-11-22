# Bayesian spatio-temporal analysis of COVID-19 in Catalonia

In this folder is found all the code of the shiny application COVIDCAT-Evolution (https://brui.shinyapps.io/covidcat_evo/).

### Code

- *app.R*: code for building the app (ui and server)

### Modules

The app is organized in shiny modules:

- *outcomeModule.R*: code for building the contents of the *Cases* and *Hospitalisations* menus

- *VacModule.R*: code for building the content in the *Vaccination* tab

- *modModule.R*: code for building the information of the model that is presented in the right expandable box

### Data

- *dat_cat*: COVID-19 observed evolution of cases/hospitalisation/vaccination in Catalonia

- *shapefileT*: spatial geography of ABS in Catalonia

- *dat_covar*: information about ABS' characteristics (spatial covariates)

- *ndat_vac*: ABS' evolution of cumulative observed fully vaccination

- *res_model*: results of the fitted spatio-temporal model

- *dat_rr*: estimated total RR of the fitted spatio-temporal model for each ABS and week

- *dat_spatial*: estimated spatial RR of the fitted spatio-temporal model for each ABS 

- *dat_temp*: estimated temporal RR of the fitted spatio-temporal model for each week

- *dat_sptemp*: estimated spatio-temporal RR of the fitted spatio-temporal model for each ABS and week

### Other

In the *WWW/* folder we can find some images and the javascript code for inserting the logo.

In the *HTML/* folder we can find the CSS code for the style.