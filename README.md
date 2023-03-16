# loessclust

A R Package for analyzing time series data using LOESS interpolation and hierarchical clustering.

## Example usage

Although the package is designed for time series data, it doesn't necessarily have to be time series, and it also doesn't have to be sampled at regular intervals. For instance, in this case, we have analyzed the effect of age on the expression level of 7,348 proteins in the plasma, using a sample size of 450, as in the figure below.

<p align="center">
  <img src="test_data/plasma_clustered.png" alt="Effect of Age on Plasma Protein Expression Level" width="400"/>
  <br>Effect of Age on Plasma Protein Expression Level
</p>

## Setup
### Use directly in R
The package hasn't been released on CRAN yet, so it has to be downloaded from Github.
1. Install the "devtools" package in R: 
```
install.packages("devtools")
```

2. Load the "devtools" package in R: 
```
library(devtools)
```

3. Install the package from Github (This step shouldn't take long): 
```
install_github("seanyoon777/loessclust")
```

4. Load the package. 
```
library(loessclust)
```

### View package source code
Clone the repository to your local directory:
```
git clone https://github.com/seanyoon777/loessclust.git
```

## Development
