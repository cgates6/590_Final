590 Project Introduction
------------------------

This project will be a data analysis project which will be used to analyze contact angle data which was collected for my thesis work. In order to analyze this data, a consistent method of processing each trial must be determined and carried out. A script for analyzing this data will be written in R to ensure that the each trial is analyzed with the same consistency.

Background
----------

The script to be written will be able to analyze contact angle over time data from a sessile droplet profile. The dynamic nature of the sessile droplet placed on a porous surface such as a rock prevents an equilibrium contact angle from being determined, due to initial spreading of the droplet and imbibition of the droplet into the porous medium throughout the life of the droplet.

Upon contact, the base of the droplet spreads due to attractive forces on the surface. Eventually the base of the droplet will cease spreading and will come to a relatively constant value. After the droplet has reached this maximum base size, all changes in contact angle are assumed to be due to imbibition of the droplet into the rock. Previous studies attempting to determine the equilibrium contact angle of sessile droplets on porous surfaces have analyzed this spreading nature of the droplet to determine the time at which the droplet has reached its maximum base, and thus surface energies are in balance with the internal energies of the droplet. The mean of the left and right contact angles at this time of maximum spreading should thus be used as a quasi-equilibrium contact angle for the surface. In order to determine the quasi-equilbrium contact angle for these rock surfaces, several regression analyses must be conducted for each trial.

Data Collection and Analysis
----------------------------

The data to be used for this project has been collected at the Center for Nanophase Material Sciences (CNMS) building at Oak Ridge. Polished Carthage Marble samples were analyzed 12 times using a sessile droplet method. The data for each of these trials is saved in an individual excel file which will need to be opened and analyzed by the R script. To organize the data analysis into a productive flow, the centralized workflow will be used.

Deliverables
------------

The outcome of this project will be in the form of a data frame and data table which contains all of the parameters produced from the regression analyses, including the final estimated equilibrium contact angles for each trial, and a mean equilibrium contact angle for Carthage Marble.
