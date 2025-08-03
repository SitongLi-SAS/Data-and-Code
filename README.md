This canvas contains three stand‑alone R scripts:
#   1) Script 1: gdppcPlot.R
#      Purpose: Reproduce the regional box plot of 10‑year trend growth
#               in constant‑price GDP per capita, comparable to R17.
#      Inputs:
#         - wbreg.csv : Country–region mapping (must include ISO2 column;
#                       WBREG/WBINC are optional; will be auto‑completed
#                       from WDI metadata if missing.)
#      Outputs:
#         - gdppcBoxPlot.pdf : Box plot of 10‑year trend β by region, with
#                              sample counts per region (R17‑compatible groups).
#
#   2) Script 2: extractWDIBeamer.R
#      Purpose: Produce a Beamer .tex deck with regional time series
#               (median across countries) for selected WDI indicators.
#      Inputs:
#         - wbreg.csv : Same as above (auto‑completed if needed).
#      Outputs:
#         - pptExample.tex : Beamer source (compile with LaTeX/pgfplots
#                            to obtain a PDF presentation).
#
#   3) Script 3: extractWDI.R
#      Purpose: Generic batch extractor driven by an option file.
#      Inputs:
#         - gdpr.opt : key=value lines (e.g., begYear=1960, endYear=2014,
#                      countries=All, indicators=pop=SP.POP.TOTL,gdppc_kd=NY.GDP.PCAP.KD,
#                      output=extract.csv). Comments beginning with # are allowed.
#      Outputs:
#         - extract.csv (or the file name given by `output` in gdpr.opt):
#           long‑form data with columns [iso2c, country, year, value, variable].
#
# Notes:
#  - All scripts default to the R17‑comparable period 1960–2014 and
#    constant‑price per‑capita GDP (NY.GDP.PCAP.KD) where applicable.
#  - Region grouping follows R17 conventions: CHN, HIC, MNA, EAS, ECA,
#    LAC, NAC, SAS, SSA (China is singled out; HIC = High income; MNA =
#    Middle East & North Africa). The code is robust to varied region/income
#    column names and will infer them from WDI metadata if absent.
