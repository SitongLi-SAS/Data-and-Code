############################################################
# R17 Reproducibility — Updated Scripts (Canvas Version)
# --------------------------------------------------------
# This canvas contains three stand‑alone R scripts:
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
############################################################


# =============================================
# Script 1: gdppcPlot.R  (R17‑comparable settings)
# Goal:
# 1) Time window 1960–2014
# 2) gap = 10, threshold = 10%
# 3) Use constant‑price GDP per capita: NY.GDP.PCAP.KD
# 4) In each 10‑year window, run log‑linear regression and take slope β
#    (annual percentage) instead of endpoint log‑difference
# 5) Region grouping aligned with R17 (CHN, HIC, MNA, EAS, ECA, LAC, NAC, SAS, SSA)
# 6) Produce a comparable box plot with per‑group sample counts
# =============================================

suppressPackageStartupMessages({
  library(WDI)
  library(countrycode)
  library(dplyr)
  library(purrr)
  library(ggplot2)
  library(stringr)
  library(tidyr)
  library(readr)
})

# ---------------- Configuration ----------------
wDir              <- "D:/Project/cases/R/其他/R17"  # set your working dir
optional_map_file <- "wbreg.csv"                   # optional; see header
outputPlot        <- "gdppcBoxPlot.pdf"

begYear   <- 1960L
endYear   <- 2014L
gap       <- 10L
threshold <- 10.0     # drop windows with |β| > 10%

# If TRUE, ignore wbreg.csv and build a full country mapping from `countrycode`
# (recommended to approach R17’s sample size and to include MNA/HIC properly)
use_full_mapping <- TRUE

# WDI indicator (constant-price GDP per capita)
ind_code <- "NY.GDP.PCAP.KD"

# ------------- Helpers: mapping & grouping -------------
# Map World Bank long region names to short codes used in R17
map_region_long_to_short <- function(x) dplyr::case_when(
  x == "East Asia & Pacific"        ~ "EAS",
  x == "Europe & Central Asia"      ~ "ECA",
  x == "Latin America & Caribbean"  ~ "LAC",
  x == "North America"              ~ "NAC",
  x == "South Asia"                 ~ "SAS",
  x == "Sub-Saharan Africa"         ~ "SSA",
  x == "Middle East & North Africa" ~ "MNA",
  TRUE ~ NA_character_
)

# Build country → (WBREG, WBINC) mapping (robust across countrycode versions)
build_r17_mapping <- function(csv_path = "wbreg.csv", use_full = TRUE) {
  # helper: pick first available column name from codelist
  pick_col <- function(cands) {
    avail <- intersect(cands, names(countrycode::codelist))
    if (length(avail)) avail[[1]] else NA_character_
  }
  # try countrycode safely
  cc_safe <- function(x, origin, destination) {
    if (is.na(destination)) return(rep(NA_character_, length(x)))
    out <- suppressWarnings(try(countrycode(x, origin, destination), silent = TRUE))
    if (inherits(out, "try-error")) rep(NA_character_, length(x)) else out
  }
  
  # --- full mapping (recommended for R17-comparable sample size)
  if (isTRUE(use_full)) {
    # 1) ISO2 universe
    iso2 <- try(unique(na.omit(countrycode::codelist$iso2c)), silent = TRUE)
    if (inherits(iso2, "try-error")) {
      suppressWarnings(utils::data("countrycode_data", package = "countrycode"))
      if (exists("countrycode_data", inherits = FALSE)) {
        iso2 <- unique(na.omit(countrycode_data$iso2c))
      } else {
        wd <- WDI::WDI_data(cache = NULL)$country
        iso2 <- unique(na.omit(wd$iso2c))
      }
    }
    
    # 2) detect region/income columns in your countrycode version
    region_col <- pick_col(c("region", "wb_region", "wbregion"))
    income_col <- pick_col(c("wb_income", "wb.income", "wb_income_cat", "income"))
    
    reg_long <- cc_safe(iso2, "iso2c", region_col)
    inc_long <- cc_safe(iso2, "iso2c", income_col)
    
    map_region <- function(x) dplyr::case_when(
      x == "East Asia & Pacific"        ~ "EAS",
      x == "Europe & Central Asia"      ~ "ECA",
      x == "Latin America & Caribbean"  ~ "LAC",
      x == "North America"              ~ "NAC",
      x == "South Asia"                 ~ "SAS",
      x == "Sub-Saharan Africa"         ~ "SSA",
      x == "Middle East & North Africa" ~ "MNA",
      TRUE ~ NA_character_
    )
    
    out <- tibble::tibble(
      ISO2  = iso2,
      WBREG = map_region(reg_long),
      WBINC = ifelse(grepl("^High income", inc_long, ignore.case = TRUE), "HIC", NA_character_)
    ) %>%
      dplyr::filter(!is.na(WBREG)) %>%           # 丢弃没有区域映射的代码（如 AQ/BV 等）
      dplyr::distinct(ISO2, .keep_all = TRUE)
    
    return(out)
  }
  
  # --- use local wbreg.csv as country set (but still recompute HIC/region)
  stopifnot(file.exists(csv_path))
  df <- read.csv(csv_path, stringsAsFactors = FALSE)
  names(df) <- toupper(names(df))
  stopifnot("ISO2" %in% names(df))
  
  df <- df %>%
    dplyr::transmute(
      ISO2  = toupper(ISO2),
      WBREG = if ("WBREG" %in% names(.)) toupper(WBREG) else NA_character_
    ) %>%
    dplyr::distinct(ISO2, .keep_all = TRUE)
  
  # detect columns for this version
  region_col <- pick_col(c("region", "wb_region", "wbregion"))
  income_col <- pick_col(c("wb_income", "wb.income", "wb_income_cat", "income"))
  
  # fill/standardize WBREG
  reg_long_fb <- cc_safe(df$ISO2, "iso2c", region_col)
  map_region <- function(x) dplyr::case_when(
    x == "East Asia & Pacific"        ~ "EAS",
    x == "Europe & Central Asia"      ~ "ECA",
    x == "Latin America & Caribbean"  ~ "LAC",
    x == "North America"              ~ "NAC",
    x == "South Asia"                 ~ "SAS",
    x == "Sub-Saharan Africa"         ~ "SSA",
    x == "Middle East & North Africa" ~ "MNA",
    TRUE ~ NA_character_
  )
  wbreg_guess <- ifelse(grepl("\\s", df$WBREG %||% ""), map_region(df$WBREG), df$WBREG)
  wbreg_fb    <- map_region(reg_long_fb)
  df$WBREG    <- ifelse(is.na(wbreg_guess) | wbreg_guess == "", wbreg_fb, wbreg_guess)
  
  # recompute HIC
  inc_long <- cc_safe(df$ISO2, "iso2c", income_col)
  df$WBINC <- ifelse(grepl("^High income", inc_long, ignore.case = TRUE), "HIC", NA_character_)
  
  df %>% dplyr::filter(!is.na(WBREG))
}

# Collapse to R17 groups (priority: CHN → HIC → region)
to_r17_groups <- function(map_df) {
  map_df %>%
    mutate(
      WBREG_UP = toupper(WBREG),
      WBINC_UP = toupper(WBINC),
      REG8 = dplyr::case_when(
        ISO2 == "CN"                         ~ "CHN",             # China singled out
        !is.na(WBINC_UP) & WBINC_UP == "HIC" ~ "HIC",             # High income first
        WBREG_UP %in% c("MNA","MEA","MENA")  ~ "MNA",
        WBREG_UP %in% c("EAS","EAP")         ~ "EAS",
        WBREG_UP == "ECA"                    ~ "ECA",
        WBREG_UP == "LAC"                    ~ "LAC",
        WBREG_UP == "NAC"                    ~ "NAC",
        WBREG_UP == "SAS"                    ~ "SAS",
        WBREG_UP %in% c("SSA","SSF")         ~ "SSA",
        TRUE                                 ~ NA_character_
      )
    ) %>%
    filter(!is.na(REG8)) %>%
    select(ISO2, WBREG, WBINC, REG8)
}

# ------------- Helpers: robust WDI retrieval -------------
# Batch pull panel data with chunking and ISO2→ISO3 fallback
pull_wdi_panel_safely <- function(iso2_vec, start, end, chunk_size = 30L, pause = 0.5, retries = 2L) {
  iso2_vec <- unique(na.omit(iso2_vec))
  chunks <- split(iso2_vec, ceiling(seq_along(iso2_vec) / chunk_size))
  out_list <- vector("list", length(chunks))
  
  for (i in seq_along(chunks)) {
    ch <- chunks[[i]]
    attempt <- 0
    res <- NULL
    while (attempt <= retries) {
      attempt <- attempt + 1
      # Try ISO2
      res <- try(WDI(country = ch, indicator = c(val = ind_code),
                     start = start, end = end), silent = TRUE)
      if (!inherits(res, "try-error") && !is.null(res) && nrow(res) > 0) break
      # Fallback ISO3
      ch3 <- countrycode(ch, "iso2c", "iso3c")
      res <- try(WDI(country = ch3, indicator = c(val = ind_code),
                     start = start, end = end), silent = TRUE)
      if (!inherits(res, "try-error") && !is.null(res) && nrow(res) > 0) break
      Sys.sleep(pause)
    }
    if (inherits(res, "try-error") || is.null(res) || nrow(res) == 0) {
      warning(sprintf("WDI chunk %d failed; skipped: %s", i, paste(ch, collapse = ",")))
      next
    }
    out_list[[i]] <- res
    Sys.sleep(pause)
  }
  
  panel <- bind_rows(out_list)
  if (is.null(panel) || nrow(panel) == 0) stop("WDI panel retrieval failed for all chunks.")
  panel %>%
    mutate(
      ISO2 = ifelse(!is.na(iso2c), iso2c, countrycode(country, "country.name", "iso2c")),
      gdppc_kd = val
    ) %>%
    select(ISO2, country, year, gdppc_kd) %>%
    arrange(ISO2, year)
}

# Compute 10-year log-linear trend β for each country
calc_betas_from_panel <- function(panel, gap = 10L, threshold = 10) {
  panel %>%
    group_by(ISO2, country) %>%
    group_modify(~{
      dat <- .x
      if (nrow(dat) < gap + 1) return(tibble(beta = numeric(0)))
      idx <- seq_len(nrow(dat) - gap)
      betas <- vapply(idx, function(i) {
        ywin <- dat$gdppc_kd[i:(i + gap)]
        if (any(is.na(ywin)) || any(ywin <= 0)) return(NA_real_)
        t <- 0:gap
        coef(lm(log(ywin) ~ t))[["t"]] * 100
      }, numeric(1))
      tibble(beta = betas)
    }) %>%
    ungroup() %>%
    filter(!is.na(beta), abs(beta) <= threshold)
}

# ---------------- Main workflow ----------------
setwd(wDir)

# Build mapping and R17 groups
reg_map <- build_r17_mapping(csv_path = optional_map_file, use_full = use_full_mapping)
reg_df  <- to_r17_groups(reg_map)

# Pull WDI panel for the selected countries and compute β
panel_dat <- pull_wdi_panel_safely(reg_df$ISO2, start = begYear, end = endYear)
all_betas <- calc_betas_from_panel(panel_dat, gap = gap, threshold = threshold) %>%
  left_join(reg_df %>% select(ISO2, REG8), by = "ISO2") %>%
  filter(!is.na(REG8))

# Factor order for plotting (R17-compatible)
reg_levels <- c("CHN","HIC","MNA","EAS","ECA","LAC","NAC","SAS","SSA")
all_betas$REG8 <- factor(all_betas$REG8, levels = reg_levels)

# Labels: number of 10-year windows per region
lab_df <- all_betas %>%
  group_by(REG8) %>%
  summarise(n = n(), ymax = suppressWarnings(max(beta, na.rm = TRUE)), .groups = "drop") %>%
  mutate(ypos = ifelse(is.finite(ymax), ymax + 0.4, 0.4),
         label = paste0("n=", n))

# ---------------- Plot ----------------
p <- ggplot(all_betas, aes(x = REG8, y = beta)) +
  geom_boxplot(outlier.shape = 21, width = 0.65) +
  geom_text(data = lab_df, aes(x = REG8, y = ypos, label = label), size = 3.4, na.rm = TRUE) +
  theme_minimal(base_size = 12) +
  labs(
    title    = "Per Capita GDP Growth (Constant-Price, 10-Year Trend β)",
    subtitle = sprintf("%d–%d, gap=%d years; |β| ≤ %s%%", begYear, endYear, gap, threshold),
    x        = "Region (R17-compatible groups)",
    y        = "Annualized growth β (% per year)"
  )
p
ggsave(filename = outputPlot, plot = p, width = 8.5, height = 6.2)
cat("Saved plot to:", file.path(wDir, outputPlot), "\n")
# =============================================
# Script 2: extractWDIBeamer.R  (constant‑price & R17 groups)
# Purpose: Build a Beamer .tex deck with regional time series (yearly median
#          across countries) for selected indicators.
# Requirements: LaTeX with pgfplots to compile the .tex into PDF.
# =============================================

suppressPackageStartupMessages({
  library(WDI)
  library(countrycode)
  library(dplyr)
  library(purrr)
  library(stringr)
})

# ---- Configuration ----
wDir      <- "D:/Project/cases/R/其他/R17"   # Set your working directory
fMap      <- "wbreg.csv"
pptName   <- "pptExample.tex"
pTitle    <- "WDI Indicators Over Time (R17-compatible Regions)"
begYear   <- 1960
endYear   <- 2014
regions   <- c("CHN","HIC","MNA","EAS","ECA","LAC","NAC","SAS","SSA")
indTable  <- list(
  pop        = "SP.POP.TOTL",
  gdppc_kd   = "NY.GDP.PCAP.KD"
)

# ---- Region helpers (duplicate for self‑containment) ----
.region_name_to_code <- c(
  "East Asia & Pacific"        = "EAS",
  "Europe & Central Asia"      = "ECA",
  "Latin America & Caribbean"  = "LAC",
  "North America"              = "NAC",
  "South Asia"                 = "SAS",
  "Sub-Saharan Africa"         = "SSA",
  "Middle East & North Africa" = "MNA"
)

.get_wdi_meta <- function() {
  ctry <- WDI::WDI_data(cache = NULL)$country
  ctry %>% transmute(
    ISO2   = iso2c,
    WBREGN = region,
    WBINCN = income
  )
}

read_and_complete_mapping <- function(csv_path) {
  df <- read.csv(csv_path, stringsAsFactors = FALSE)
  names(df) <- toupper(names(df))
  if (!"ISO2" %in% names(df)) stop("wbreg.csv must contain column ISO2 (2-letter country code)")
  
  need_reg <- !"WBREG" %in% names(df) || all(is.na(df$WBREG))
  need_inc <- !"WBINC" %in% names(df) || all(is.na(df$WBINC))
  
  if (need_reg || need_inc) {
    meta <- .get_wdi_meta()
    df <- df %>% left_join(meta, by = "ISO2")
    
    if (need_reg) {
      guess_reg <- NULL
      for (cand in c("REGION", "WBREGCODE")) {
        if (cand %in% names(df)) { guess_reg <- df[[cand]]; break }
      }
      df$WBREG <- if (!is.null(guess_reg)) {
        toupper(guess_reg)
      } else if (!is.null(df$WBREGN)) {
        unname(.region_name_to_code[df$WBREGN]) %>% as.character()
      } else {
        long <- countrycode(df$ISO2, "iso2c", "region")
        unname(.region_name_to_code[long]) %>% as.character()
      }
    }
    
    if (need_inc) {
      guess_inc <- NULL
      for (cand in c("INCOME", "WBINCCODE")) {
        if (cand %in% names(df)) { guess_inc <- df[[cand]]; break }
      }
      df$WBINC <- if (!is.null(guess_inc)) {
        toupper(guess_inc)
      } else {
        inc_long <- dplyr::coalesce(df$WBINCN, countrycode(df$ISO2, "iso2c", "wb_income"))
        ifelse(str_detect(inc_long %||% "", regex("^High income", ignore.case = TRUE)),
               "HIC", NA_character_)
      }
    }
  }
  df
}

normalize_region <- function(df_completed) {
  # 兜底：没有 WBINC/WBREG 也先建空列
  if (!"WBREG" %in% names(df_completed)) df_completed$WBREG <- NA_character_
  if (!"WBINC" %in% names(df_completed)) df_completed$WBINC <- NA_character_
  
  # 用 countrycode 推断高收入（wb.income），只要是 High income 就标 HIC
  inc_long <- try(countrycode(df_completed$ISO2, "iso2c", "wb.income"), silent = TRUE)
  if (inherits(inc_long, "try-error")) inc_long <- rep(NA_character_, nrow(df_completed))
  df_completed$WBINC <- ifelse(grepl("^High income", inc_long, ignore.case = TRUE), "HIC", df_completed$WBINC)
  
  df_completed %>%
    mutate(
      WBREG_UP = toupper(WBREG),
      WBINC_UP = toupper(WBINC),
      REG8 = dplyr::case_when(
        ISO2 == "CN"                          ~ "CHN",      # 中国单列
        !is.na(WBINC_UP) & WBINC_UP == "HIC"  ~ "HIC",      # 高收入覆盖一切区域（含 US/JP/DE/FR/IT 等）
        WBREG_UP %in% c("MNA","MEA","MENA")   ~ "MNA",
        WBREG_UP %in% c("EAS","EAP")          ~ "EAS",
        WBREG_UP == "ECA"                     ~ "ECA",
        WBREG_UP == "LAC"                     ~ "LAC",
        WBREG_UP == "NAC"                     ~ "NAC",
        WBREG_UP == "SAS"                     ~ "SAS",
        WBREG_UP %in% c("SSA","SSF")          ~ "SSA",
        TRUE                                  ~ NA_character_
      )
    ) %>%
    filter(!is.na(REG8)) %>%
    select(ISO2, WBREG, WBINC, REG8)
}

# ---- Read mapping & build region‑year medians ----
setwd(wDir)
reg_df <- read_and_complete_mapping(fMap) %>% normalize_region() %>% filter(REG8 %in% regions)

pull_indicator <- function(ind_code) {
  WDI(country = reg_df$ISO2, indicator = ind_code, start = begYear, end = endYear)
}
all_wdi <- lapply(indTable, pull_indicator)
names(all_wdi) <- names(indTable)

tidy_bind <- function(df, varname) {
  df %>%
    select(iso2c, country, year, !!sym(indTable[[varname]])) %>%
    rename(value = !!sym(indTable[[varname]])) %>%
    mutate(variable = varname)
}

td <- list_rbind(Map(tidy_bind, all_wdi, names(all_wdi))) %>%
  left_join(reg_df %>% select(ISO2, REG8), by = c("iso2c" = "ISO2")) %>%
  filter(!is.na(REG8))

reg_year_series <- td %>%
  group_by(variable, REG8, year) %>%
  summarise(value = median(value, na.rm = TRUE), .groups = "drop")

# ---- Write Beamer .tex using pgfplots ----
tex <- file(pptName, "w", encoding = "UTF-8")
cat(
  "\\documentclass{beamer}\n",
  "\\usetheme{Madrid}\n",
  "\\usepackage{pgfplots}\n",
  "\\pgfplotsset{compat=1.17}\n",
  "\\title{", pTitle, "}\n",
  "\\author{", author, "}\n",
  "\\institute{", institute, "}\n",
  "\\date{", begYear, "--", endYear, "}\n",
  "\\begin{document}\n",
  "\\frame{\\titlepage}\n",
  file = tex, sep = ""
)

for (var in names(indTable)) {
  cat("\\begin{frame}{", var, "}\n", "\\begin{tikzpicture}\n",
      "\\begin{axis}[xlabel=Year, ylabel=", var, ", legend pos=north west]\n",
      file = tex, sep = "")
  for (reg in regions) {
    sub <- reg_year_series %>% filter(variable == var, REG8 == reg, !is.na(value))
    coords <- paste0("(", sub$year, ",", sprintf("%.6f", sub$value), ")", collapse = " ")
    cat("\\addplot coordinates {", coords, "};\\addlegendentry{", reg, "}\n",
        file = tex, sep = "")
  }
  cat("\\end{axis}\n\\end{tikzpicture}\n\\end{frame}\n", file = tex)
}
cat("\\end{document}\n", file = tex)
close(tex)


# =============================================
# Script 3: extractWDI.R  (generic batch extractor; constant‑price default)
# Purpose: Read parameters from gdpr.opt and export a long‑form CSV for the
#          requested countries/indicators and time span.
# Defaults: If indicators are not provided, use pop=SP.POP.TOTL and
#           gdppc_kd=NY.GDP.PCAP.KD; default years 1960–2014.
# =============================================

suppressPackageStartupMessages({
  library(WDI)
  library(countrycode)
  library(dplyr)
  library(stringr)
})

# ---- Parse option file gdpr.opt ----
opt_file <- "gdpr.opt"   # Path to the options file emitted by GAMS or edited manually
opt_lines <- readLines(opt_file, warn = FALSE)
opt_lines <- opt_lines[nzchar(opt_lines) & !grepl("^\\s*#", opt_lines)]

# Split key=value
keys   <- sub("=.*",     "", opt_lines)
values <- sub("^[^=]*=", "", opt_lines)
opt_list <- setNames(values, keys)

# ---- Parameters with sensible defaults ----
begYear   <- as.integer(opt_list["begYear"]); if (is.na(begYear)) begYear <- 1960L
endYear   <- as.integer(opt_list["endYear"]); if (is.na(endYear)) endYear <- 2014L
countries <- if (!is.na(opt_list["countries"]) && nzchar(opt_list["countries"]) &&
                 toupper(opt_list["countries"]) != "ALL") {
  trimws(unlist(strsplit(opt_list["countries"], ",")))
} else {
  unique(na.omit(countrycode_data$iso2c))
}

if (!is.na(opt_list["indicators"]) && nzchar(opt_list["indicators"])) {
  inds <- trimws(unlist(strsplit(opt_list["indicators"], ",")))
} else {
  inds <- c("pop=SP.POP.TOTL", "gdppc_kd=NY.GDP.PCAP.KD")
}
ind_map <- setNames(
  sapply(inds, function(x) strsplit(x, "=")[[1]][2]),
  sapply(inds, function(x) strsplit(x, "=")[[1]][1])
)

outputCsv <- opt_list["output"]; if (is.na(outputCsv) || !nzchar(outputCsv)) outputCsv <- "extract.csv"

# ---- Retrieve and write ----
data_list <- lapply(names(ind_map), function(var) {
  WDI(
    country   = countries,
    indicator = ind_map[[var]],
    start     = begYear,
    end       = endYear
  ) %>% mutate(variable = var)
})
all_data <- bind_rows(data_list)

write.csv(all_data, file = outputCsv, row.names = FALSE)
cat("Wrote:", outputCsv, " (", nrow(all_data), " rows)\n")
