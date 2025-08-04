This repository contains three stand‑alone R scripts for working with World Development Indicators (WDI) data. Each script has a specific purpose, inputs, and outputs, as detailed below.

Table of Contents

gdppcPlot.R

extractWDIBeamer.R

extractWDI.R

Notes

Requirements

Usage

1. gdppcPlot.R

PurposeReproduce the regional box plot of 10‑year trend growth in constant‑price GDP per capita, comparable to R17.

Inputs

wbreg.csv: Country–region mapping file. Must include an ISO2 column. Optional columns WBREG and WBINC will be auto‑completed from WDI metadata if missing.

Outputs

gdppcBoxPlot.pdf: Box plot of 10‑year trend β by region, with sample counts per region (R17‑compatible groups).

2. extractWDIBeamer.R

PurposeProduce a Beamer .tex deck with regional time series (median across countries) for selected WDI indicators.

Inputs

wbreg.csv: Same country–region mapping as above (auto‑completed from WDI metadata if needed).

Outputs

pptExample.tex: Beamer source file. Compile with LaTeX/pgfplots to obtain a PDF presentation.

3. extractWDI.R

PurposeGeneric batch extractor driven by an option file.

Inputs

gdpr.opt: Plain text options file with key=value lines. Example options:

begYear=1960

endYear=2014

countries=All

indicators=pop=SP.POP.TOTL,gdppc_kd=NY.GDP.PCAP.KD

output=extract.csv
Comments beginning with # are allowed.

Outputs

extract.csv (or the filename specified by output in gdpr.opt): Long‑form data with columns [iso2c, country, year, value, variable].

Notes

All scripts default to the R17‑comparable period 1960–2014 and constant‑price per‑capita GDP (NY.GDP.PCAP.KD) where applicable.

Region grouping follows R17 conventions:

CHN (China)

HIC (High Income Countries)

MNA (Middle East & North Africa)

EAS (East Asia & Pacific)

ECA (Europe & Central Asia)

LAC (Latin America & Caribbean)

NAC (North America)

SAS (South Asia)

SSA (Sub‑Saharan Africa)

The code is robust to varied region/income column names and will infer them from WDI metadata if absent.

Requirements

R (version ≥ 3.6)

R packages:

WDI for metadata lookup

ggplot2 (for plotting)

dplyr, readr (for data manipulation)

LaTeX distribution with pgfplots (for compiling the Beamer deck)

wbreg.csv in the working directory (or allow scripts to fetch and complete it)
