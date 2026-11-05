# Run from the repository root. Outputs are written only to a temporary project.
repo <- normalizePath('.')
tmp <- tempfile('warmup-test-', tmpdir = '/tmp')
dir.create(tmp)
stopifnot(file.symlink(file.path(repo, 'data'), file.path(tmp, 'data')))
setwd(tmp)
source(file.path(repo, 'scripts/01_run_microclimate_beginner.R'))
stopifnot(all(is.finite(panel_df$temperature)),
          length(unique(round(R_results$RMSE, 8))) > 1)
source(file.path(repo, 'scripts/microclimate_beginner_workflow_SIMPLE.r'))
stopifnot(all(is.finite(karten_df$temperature)),
          length(unique(round(R_test$RMSE, 8))) > 1)
source(file.path(repo, 'scripts/microclimate_warmup_minimal.R'))
stopifnot(file.exists('output/warmup/temperatur_modelle.tif'),
          file.exists('output/warmup/loocv_rmse.csv'))
cat('PASS: three warmup scripts; plot values, height tuning and exports checked\n')
cat('Test outputs:', tmp, '\n')
