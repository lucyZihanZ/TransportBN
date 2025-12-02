# Bayesian Networks for Categorical Data Analysis With Applications to Transportation 

The file preprocessing_network_creation.R is primarily reading in the data, cleaning, and categorizing it. Also, the arcs for the Bayesian Network are defined therein.

The other files (roundabout.R, lighting_IMD.R, bike_accident_distribution.R) are querying the network to answer specific questions. First, roundabout.R assesses the effectiveness of roundabouts vs. T-Junctions in preventing fatalities. Second, lighting_IMD.R, examines if there is potentially any improvements needed to lighting infrastructure in deprived communities, or if the issue is perhaps more behavioural or related to other confounding factors. Last, bike_accident_distribution.R looks at the distribution of bike accidents across different IMD levels. As expected the least deprived community has the most biking accidents, but the gap between the most and least deprived communities in terms of bike fatalities is smaller than one would expect given how much more the least deprived communities bike comparatively, indicating an inequity in biking infrastructure.

Last, the confidence intervals are not available readily from bnlearn, and so to obtain them we bootstrap the data with replacement to introduce uncertainty in the parameter estimates. 
