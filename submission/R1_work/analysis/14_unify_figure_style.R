# Regenerate plot assets from saved results, preserving the previous figure set.
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_dir <- dirname(normalizePath(sub("^--file=", "", script_arg[[1]])))
work_root <- normalizePath(file.path(script_dir, ".."))
figures_dir <- file.path(work_root, "figures")
backup <- file.path(figures_dir, "before_optima_and_interval_check_2026-09-12")
if (!dir.exists(backup)) {
  dir.create(backup)
  assets <- list.files(figures_dir, pattern = "\\.(png|pdf|tif)$", full.names = TRUE)
  stopifnot(all(file.copy(assets, backup)))
}
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
for (pair in list(c("figure_2_publication_redesign", "figure_2_joint_posterior"),
                  c("figure_3_robustness_publication", "figure_3_robustness"))) {
  for (suffix in c(".png", ".pdf", ".tif", "_preview.png")) {
    stopifnot(file.copy(file.path(figures_dir, paste0(pair[1], suffix)),
      file.path(figures_dir, paste0(pair[2], suffix)), overwrite = TRUE))
  }
}
stopifnot(identical(hashes, tools::md5sum(inputs)))
message("Unified figures complete. Saved numerical inputs and model files are unchanged.")
