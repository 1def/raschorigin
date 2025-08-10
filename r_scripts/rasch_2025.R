#!/usr/bin/env Rscript
suppressWarnings(suppressMessages({
  library(ltm)
  library(jsonlite)
}))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  cat(toJSON(list(error="CSV fayl yo'li berilmadi"), auto_unbox=TRUE))
  quit(status=2)
}
csv_path <- args[[1]]
# Read CSV (no header), blanks as NA
x <- tryCatch({
  read.csv(csv_path, header=FALSE, sep=",", na.strings=c("","NA"))
}, error=function(e) e)
if (inherits(x, "error")) {
  cat(toJSON(list(error=paste("CSV o'qishda xato:", x$message)), auto_unbox=TRUE))
  quit(status=2)
}
if (nrow(x) == 0 || ncol(x) == 0) {
  cat(toJSON(list(error="Matritsa bo'sh"), auto_unbox=TRUE))
  quit(status=2)
}
# Ensure integer 0/1/NA
for (j in seq_len(ncol(x))) {
  x[[j]] <- suppressWarnings(as.integer(as.character(x[[j]])))
}
# Fit Rasch model
fit <- tryCatch({
  rasch(as.matrix(x), IRT.param=TRUE)
}, error=function(e) e)
if (inherits(fit, "error")) {
  cat(toJSON(list(error=paste("Model moslashtirishda xato:", fit$message)), auto_unbox=TRUE))
  quit(status=1)
}
# Items
item_coefs <- coef(fit)
diff_col <- NULL
if (is.matrix(item_coefs)) {
  if ("Dffclt" %in% colnames(item_coefs)) diff_col <- "Dffclt"
  if (is.null(diff_col)) diff_col <- colnames(item_coefs)[1]
  items <- lapply(seq_len(nrow(item_coefs)), function(i) list(item_id=paste0("Item", i), difficulty=unname(as.numeric(item_coefs[i, diff_col]))))
} else {
  items <- lapply(seq_along(item_coefs), function(i) list(item_id=names(item_coefs)[i], difficulty=unname(as.numeric(item_coefs[i]))))
}
# Persons via factor.scores
fs <- tryCatch({
  factor.scores(fit, resp.patterns = as.data.frame(x), method = "EAP")
}, error=function(e) e)
persons <- list()
if (!inherits(fs, "error") && !is.null(fs$score.dat)) {
  score_dat <- fs$score.dat
  eap_col <- if ("z1" %in% colnames(score_dat)) "z1" else tail(colnames(score_dat), 1)
  se_col <- if ("se.z1" %in% colnames(score_dat)) "se.z1" else tail(colnames(score_dat), 2)[1]
  patt_cols <- seq_len(ncol(x))
  patt_key <- function(row) paste(row, collapse = "|")
  patt_keys <- apply(score_dat[, patt_cols, drop=FALSE], 1, patt_key)
  eap_vals <- as.numeric(score_dat[[eap_col]])
  se_vals <- suppressWarnings(as.numeric(score_dat[[se_col]]))
  if (length(se_vals) != length(eap_vals) || any(is.na(se_vals))) se_vals <- rep(NA_real_, length(eap_vals))
  map <- setNames(lapply(seq_along(patt_keys), function(i) list(eap=eap_vals[i], se=se_vals[i])), patt_keys)
  persons <- lapply(seq_len(nrow(x)), function(i){
    key <- patt_key(x[i, , drop=TRUE])
    sc <- map[[key]]
    if (is.null(sc)) sc <- list(eap=NA_real_, se=NA_real_)
    list(person_index=i, eap=unname(as.numeric(sc$eap)), se=unname(as.numeric(sc$se)))
  })
}
# Fit stats
fit_stats <- tryCatch({
  list(logLik=as.numeric(logLik(fit)), AIC=as.numeric(AIC(fit)), BIC=as.numeric(BIC(fit)), n_obs=nrow(x), n_items=ncol(x))
}, error=function(e) list(n_obs=nrow(x), n_items=ncol(x)))
result <- list(items=items, persons=persons, fit=fit_stats)
cat(toJSON(result, auto_unbox=TRUE, digits=6, na="null"))
