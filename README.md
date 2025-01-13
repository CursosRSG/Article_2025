# RSG-Brazil Educational Committee Survey Analysis 2023

This R script analyzes and visualizes survey data from the RSG-Brazil Educational Committee Survey conducted in 2023. It generates multiple figures examining participant demographics, academic profiles, and training needs in bioinformatics.

## Prerequisites

The following R packages are required:
- svglite
- readr
- rnaturalearth
- devtools 
- ggplot2
- patchwork
- dplyr
- sf
- ggthemes
- rnaturalearthdata
- cowplot
- stringr
- tidyr
- rnaturalearthhires

## File Structure
. ├── artigoRSG.R # Main analysis script ├── demanda_april.csv # Input survey data └── plots/ # Output directory ├── figure1/ # Geographic distribution plots ├── figure2/ # Gender distribution plots ├── figure3/ # Academic level plots ├── figure4/ # Confidence scale plots ├── figure5/ # Career diversity plots ├── figure6/ # Training demands plots └── figure7/ # Training levels plots


## Functionality

The script performs:
1. Geographic distribution analysis of survey participants
2. Gender distribution analysis across profiles
3. Academic level analysis per profile
4. Self-assessed confidence scoring
5. Career diversity analysis
6. Training demands assessment
7. Training level requirements analysis

Each analysis generates corresponding visualizations saved in multiple formats (PNG, SVG, PDF, TIFF).

## Usage

1. Ensure all prerequisites are installed
2. Place input CSV file in same directory as script
3. Set working directory in script
4. Run script:

```r
source("artigoRSG.R")

Outputs
The script generates 7 main figures:

Figure 1: Geographic distribution
Figure 2: Gender distribution by profile
Figure 3: Academic careers distribution
Figure 4: Confidence scale assessments
Figure 5: Career diversity analysis
Figure 6: Training demands by profile
Figure 7: Required training levels
License
MIT License

Authors
RSG-Brazil Educational Committee ```