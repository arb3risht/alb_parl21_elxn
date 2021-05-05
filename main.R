# main.R
# The goal of this project is to carry out various analyses on the
# 2021 Albanian Parliamentary Election data.
# CC BY-SA. Arbër Boriçi, 2021. Contact: arberisht@gmail.com. 
# Full license terms at https://creativecommons.org/licenses/by-sa/4.0/.

# Load packages used throughout the project
library(here)       # for file dir
library(tidyverse)  # necessary evil
library(psych)      # data description
library(ggpubr)   # used for density plots in K-S-L tests
library(DescTools) # used for the Lilliefors test
library(benford.analysis) # used for the Benford's Law conformance test
library(Matching) # for ks.boot (discrete-sample K-S test)

# Load library of functions
source(file = here("lib/fun.R"))

# Read source data
source(file = here("source_data.R"))

# Describe the data visually and statistically
source(file = here("data_description.R"))

# Run the Kolmogorov-Smirnov-Lilliefors normality tests
source(file = here("ksl_norm_test.R"))

# Check conformity with Benford's Law for leading and second digits
source(file = here("benford.R"))





