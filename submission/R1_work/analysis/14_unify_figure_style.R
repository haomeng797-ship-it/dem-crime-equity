# Regenerate all four figures from saved results and confirm the inputs are untouched.
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_dir <- dirname(normalizePath(sub("^--file=", "", script_arg[[1]])))
work_root <- normalizePath(file.path(script_dir, ".."))
figures_dir <- file.path(work_root, "figures")
inputs <- c(list.files(file.path(work_root, "results"), full.names = TRUE),
            list.files(file.path(work_root, "models"), full.names = TRUE))
inputs <- inputs[!grepl("figure2_redesign_check", inputs)]
hashes <- tools::md5sum(inputs)
for (script in c("02_make_prototype_figures.R", "12_redesign_figure2.R",
                 "13_style_figure3.R", "06_posterior_predictive_checks.R")) {
  args <- shQuote(file.path(script_dir, script))
  if (startsWith(script, "06")) args <- c(args, "--style-only")
  status <- system2(file.path(R.home("bin"), "Rscript"), args)
  stopifnot(status == 0)
}
stopifnot(identical(hashes, tools::md5sum(inputs)))
message("Unified figures complete. Saved numerical inputs and model files are unchanged.")
