# Shimmer

Shiny Model Explorer (Shimmer) is an R Shiny Application for simulating from population pharmacokinetic (PK) models described by systems of ordinary differential equations (ODE) via the mrgsolve package. No code or technical knowledge is required to get started, with options to accommodate more advanced simulation scenarios. 

Users can select from a library of standard PK models for biologics that include variations of linear and non-linear elimination, multiple compartments, target mediated drug disposition, and absorption compartments. Alternatively, users can upload their own custom mrgsolve model files for performing simulations in the shiny application.

The graphical user interface (GUI) allows simplified specification of model fixed effects, random effects, dosing regimens, time intervals, population size, simulation replicates, and solver settings. Shimmer constructs full random effect matrices (in correlation form) by default for simulating correlations between any pair of random effect variables regardless of whether these parameters are explicitly defined in the model file.  

Additionally, Shimmer allows code-free specification of multiple, complex dosing regimens in a single simulation for simultaneous comparison of various dosing scenarios. All datasets generated within the shiny application are available for export and formatted to be compatible with NONMEM. 

Visualize simulation results and summary statistics directly in the shiny app with options for customizing plot aesthetics and adjusting calculation parameters. The simulation results can be downloaded as a simulation report (PDF) that includes all default and user defined settings from the current shiny app session. The "Documentation" tab features an integrated user guide with additional information about the mrgsolve package, built-in model library, specification of dosing regimens, simulation setup, and exporting results. 


## Required Software

 * R and RStudio IDE
 * [Rtools](#rtools-for-windows-users) (Windows users only)
 * [mrgsolve R package](https://github.com/metrumresearchgroup/mrgsolve/wiki/mrgsolve-Installation) (>= 1.0.0) 
 * LaTeX
 * Dependent R packages
 
 
### R and RStudio IDE

[Install R and RStudio desktop from posit website](https://posit.co/download/rstudio-desktop/)


### Rtools for Windows users

Windows users must install Rtools compatible with their current version of R. Please see `mrgsolve` [Rtools installation instructions](https://github.com/metrumresearchgroup/mrgsolve/wiki/mrgsolve-Installation#windows-users) for additional details.


### mrgsolve R package installation

```{r}
install.packages("mrgsolve")
```

[mrgsolve package documentation](https://mrgsolve.org/)

 
### LaTeX

LaTeX is required to export simulation reports (PDF) from the shiny app. We recommend installing TinyTeX (a LaTeX distribution) via the 'tinytex' R package:

```{r}
install.packages("tinytex")
tinytex::install_tinytex()
# to uninstall TinyTeX, run tinytex::uninstall_tinytex() 
```

[tinytex package documentation](https://yihui.org/tinytex/)


### Dependent R Packages

Shimmer requires local installation of the following R packages:

shiny, tablerDash, shinyWidgets, shinyFeedback, shinyBS, shinyjs, rhandsontable, reactable, htmlwidgets, plotly, bs4Dash, mrgsolve, dplyr, purrr, data.table, DT, ggplot2, flux, markdown, rmarkdown, ggpubr, matrixcalc, assertr, tidyr, tinytex, shinyjs, & reactable

For specific versions, see [session information log file](www/session_information.txt)


## Installation

Install Shiny application by cloning with HTTPS/SSH or via direct download of source code:
```{bash}
git clone https://github.com/AmadorBioscience/Shimmer.git
```


## Running the Application

Start app in a subdirectory called "Shimmer"
```{r}
shiny::runApp("Shimmer", launch.browser = TRUE)
```


If loaded into the `Shimmer` RStudio project via the "Shimmer.Rproj" file
```{r}
shiny::runApp(launch.browser = TRUE)
```


## Acknowledgments

Shimmer was based on the PMX_Simulations shiny application developed by PXM Solutions, under the AGPL-3.0 license:

 * https://github.com/michielve/PMX_Simulations
 * https://app.axiaal.com/PKSim/
 * https://www.pmxsolutions.com

