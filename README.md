# SpatialModelBenchmark

Spatial proteomics is a powerful emerging tool to link spatial context with cell biology for research and discovery. However, analysis of spatial proteomics data is complex, requiring comparison of the properties and locations of multiple cell types. Existing workflows are limited in their ability to rapidly and flexibly perform dozens of concurrent statistical analyses of multiple cell types and present the results in an easily understandable manner.

We developed SpatialModelBenchmark a flexible R framework for performing and benchmarking complex statistical analyses of spatial proteomic data. SpatialModelBenchmark encompasses tools for processing, filtering and statistical analysis of data from multiple distinct cell populations in parallel. SpatialModelBenchmark incorporates parametric and non-parametric tests of population means, generalised linear models (GLMs) and generalised linear mixed models (GLMMs) for cell counts and densities from spatial proteomics datasets. Visualisation functions enable rapid assessment of analysis results to identify patterns of cell clustering and cell surface marker expression.

Here, we demonstrate how the SpatialModelBenchmark workflow uncovers important biological features from Opal multiplex-immunohistochemistry (mIHC) and multiplexed ion beam imaging (MIBI) data. SpatialModelBenchmark efficiently extracts immune cell types with greatest differences in frequency between sub-groups of triple negative breast cancer patients and macrophage markers whose density varies according to distance from the tumour core in gastric cancer.
SpatialModelBenchmark can compare performance of multiple hypothesis testing procedures to enable users to select one with robust performance on their data. Our explorations found Negative Binomial had reduced false positive rates over Poisson models, while the Wilcoxon method performed the most consistently across datasets, even when using null data. 

In conclusion, SpatialModelBenchmark introduces a new accessible and fast framework for researchers to gain valuable insights from spatial proteomics, expanding the utility of this important new data modality.


## Features
* **User selected statistical test**: Run and evaluate Wilcox and GLMMs models such as Poisson, Linear Negative Binomial (`nbinom1`), Quadratic Negative Binomial (`nbinom2`), and Tweedie models.
* **sptial proteomics metrics**: Statistical evaluation of any spatial proteomics metrics such as cell density and proximity
* **Random-Effects Customization**: Account for patient and sample levels covariates

## Installation

You can install the development version of `SpatialModelBenchmark` directly from GitHub using the following R commands:

```R
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_github("stephenli-monash/SpatialModelBenchmark")
```

## Example on OPAL MAC data

The package abstracts directory-level benchmarking using the core function `run_spatial_benchmark()`. Use this template to point the engine toward your cell-feature extraction directories:

```R
library(SpatialModelBenchmark)
setwd('/fs04/scratch2/tc48/SPIAT/SpatialModelBenchmark/')
devtools::load_all()
# 1. Define Model Execution Parameters
params <- list(
  patient_col       = "SampleID",          # Patient tracking ID column
  test_column       = "TissueRegion",      # Categorical testing group variable 
  Total_ROI_Area    = "Area.mm2",          # Region of Interest area measurements
  Total_Cell_Number = "Total_Cell_Number", # Absolute total cell counts
  FOV_col           = "FOV_ID",            # Field of view identifier
  colWilcox         = "dens",              # Column targets for Wilcoxon testing
  colGLMM           = "_counts",           # Target suffix pattern for count variables
  GLMM_formula      = "+ (1|patient_id) + offset(log(Area.mm2))" # Mixed effects layout
)

# 2. Configure Your GLMM Family Benchmark Matrix
glmm_models <- list(
  nbinom1 = list(family = "nbinom1", link = "log", formula = params\$GLMM_formula),
  nbinom2 = list(family = "nbinom2", link = "log", formula = params\$GLMM_formula),
  poisson = list(family = "poisson", link = "log", formula = params\$GLMM_formula),
  tweedie = list(family = "tweedie", link = "log", formula = params\$GLMM_formula)
)

# 3. Trigger the Processing Engine Across Feature Directories
run_spatial_benchmark(
  data_dir    = "./path/to/extracted_features/",
  output_dir  = "./path/to/html_reports/",
  params      = params,
  glmm_models = glmm_models,
  n_shuffle   = 100
)
```

## License

This project is licensed under the Apache-2.0 License.
