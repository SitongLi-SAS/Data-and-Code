
lac_iso <- c(
  "ARG","BOL","BRA","CHL","COL","CRI","CUB","DOM","ECU","SLV",
  "GTM","GUY","HTI","HND","JAM","MEX","NIC","PAN","PRY","PER",
  "BLZ","BRB","DMA","GRD","KNA","LCA","VCT","SUR","TTO","URY","VEN"
)
arab_iso <- c(
  "BHR","JOR","KWT","LBN","OMN","QAT","SAU","ARE"
)

lac_data  <- table5_ready %>% filter(iso %in% lac_iso)
arab_data <- table5_ready %>% filter(iso %in% arab_iso)

calc_cor <- function(dat, x, y) {
  tmp <- dat %>% filter(!is.na(.data[[x]]), !is.na(.data[[y]]))
  n   <- nrow(tmp)
  t   <- cor.test(tmp[[x]], tmp[[y]], method="pearson")
  cat(sprintf("%s vs %s: N=%d, p=%.3f\n", x, y, n, t$p.value))
  r   <- round(t$estimate, 2)
  sig <- ifelse(t$p.value<.05, "**", ifelse(t$p.value<.1, "*", ""))
  paste0(r, sig)
}

src_cols   <- c("gdp_wdi2012","gdp_pwt71","gdp_wdi2011","gdp_pwt70")
src_names  <- c("WDI 2012","PWT 7.1","WDI 2011","PWT 7.0")

under5_vals <- sapply(src_cols, function(col) calc_cor(lac_data,  col, "cdr_u5"))
gini_vals   <- sapply(src_cols, function(col) calc_cor(arab_data, col, "gini"))

table5 <- data.frame(
  "Indicator and Sample" = c(
    "Under-5 mortality (LAC, N = 32)",
    "Gini coefficient (Arab, N = 8)"
  ),
  check.names = FALSE
)
table5[ , src_names] <- rbind(under5_vals, gini_vals)

print(table5, row.names = FALSE)


