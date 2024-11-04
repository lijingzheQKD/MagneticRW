# Random Walk Simulation for Magnetic Database and Well Analysis

## Overview

This project simulates a random walk to analyze magnetic fields and well locations in a specified area. The goal is to model potential paths for exploration based on predefined constraints and parameters.

## Contents

- **Functions**: 
  - `random_walk.R`
  - `Build_magneticDatabase.R`
  - `rotation.R`
  - `addchannel.R`
  - `UpperLeftShift and LowerRightShift.R`
- **Data**: 
  - `supplement1.csv` (Contains well location data)

## Requirements

Make sure to install the following R packages if they are not already installed:

```R
install.packages(c("dplyr", "ggplot2", "SpatialExtremes", "akima", "smoothr"))
