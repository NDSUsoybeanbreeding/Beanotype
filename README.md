# Beanotype
Genotype-to-phenotype interpretation tool for soybean molecular marker data.

## Agriplex Trait Marker Genotype to Phenotype Converter

## Overview

**Beanotype** is an R Shiny application designed to convert Agriplex
trait marker genotype calls into breeder-friendly phenotype
interpretations. The app reads genotype data from a user-uploaded Excel
file, matches each marker to an embedded trait marker conversion
template, and returns interpreted allele phenotypes, summaries,
visualizations, and filtered sample lists.

The app supports marker-assisted selection by helping users quickly
identify lines with desired trait marker profiles, including
single-marker phenotypes and multi-trait combinations.

## Features

-   Converts genotype calls into phenotype descriptions
-   Supports both single-base and multi-base Ref/Alt alleles
-   Color-coded genotype classifications (Ref, Alt, Het, Fail)
-   Interactive filtering by trait category, marker, sample, and
    phenotype class
-   Phenotype visualization and sample extraction
-   Multi-trait filtering for simultaneous marker-assisted selection
-   Exportable Excel outputs
-   Upload QC checks for unmatched markers

## Requirements

Required R packages:

``` r
library(shiny)
library(readxl)
library(tidyr)
library(dplyr)
library(DT)
library(writexl)
library(ggplot2)
library(plotly)
```

Install with:

``` r
install.packages(c(
  "shiny",
  "readxl",
  "tidyr",
  "dplyr",
  "DT",
  "writexl",
  "ggplot2",
  "plotly"
))
```

## Required Files

The app folder should contain:

-   `app.R`
-   `ChatGPT_Updated_Trait marker conversion template.xlsx`
-   `README.md`

## Template Structure

  Template Row   Field                  Description
  -------------- ---------------------- -----------------------------------
  Row 1          Marker_Type            Broad marker classification
  Row 2          Agriplex_Trait_Label   Marker label shown to users
  Row 3          Marker_Purpose         Trait description
  Row 4          Category               Trait grouping/filtering category
  Row 5          Marker_Name            Internal marker matching key
  Row 6          Ref_Pheno              Reference allele phenotype
  Row 7          Alt_Pheno              Alternate allele phenotype
  Row 8          Ref_Alt                Ref/Alt allele call

## Usage

1.  Download the trait genotype template
2.  Paste Sample IDs and genotype calls into the template
3.  Upload the completed `.xlsx` file
4.  Filter markers and phenotypes
5.  Explore phenotype summaries
6.  Export selected sample lists

## App Tabs

### Data Table

Displays interpreted genotype-to-phenotype output for all sample-marker
combinations.

### Summary

Displays allele class distributions and summary tables.

### Phenotype

Displays phenotype distributions and allows extraction of sample names
for selected phenotype classes.

### Multi-Trait Filter

Allows users to identify lines meeting multiple phenotype requirements
simultaneously.

## Outputs

Exports include:

-   Full interpreted dataset
-   Phenotype sample lists
-   Multi-trait matching sample lists with filter criteria

## Troubleshooting

### Markers shown as Fail

Check: - Marker names match the template exactly - Ref/Alt calls are
formatted correctly - Multi-base alleles are represented consistently

### Unmatched markers

Uploaded markers do not match row 5 (`Marker_Name`) in the template.

### Deployment issues

Ensure: - Main script is named `app.R` - Template file is present -
Filename matches the code exactly

## Citation

If you use Beanotype in research, please cite the associated publication
when available.

## Author

Carrie Miranda Dottey\
North Dakota State University\
Department of Plant Sciences

## Version

Beanotype v2.1
