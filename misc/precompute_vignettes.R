# 
# This script runs chunks from the vignette files
# and saves results from costly computations in RDS
# files
#
#
source("misc/run_rmd_chunks.R")

#####
#
#  Run forests.Rmd
#
envir_forests <- run_rmd_chunks("vignettes/forests.Rmd")
#
# Store results
#
saveRDS(envir_forests$forest, "vignettes/precomputed/forests_rmd_forest.rds")
saveRDS(envir_forests$vim, "vignettes/precomputed/forests_rmd_vim.rds")
saveRDS(envir_forests$pdp, "vignettes/precomputed/forests_rmd_pdp.rds")

#####
#
#  Run forests.Rmd
#
envir_semforest_focus <- run_rmd_chunks("vignettes/semforest-focus.Rmd")
saveRDS(envir_semforest_focus$forest, "vignettes/precomputed/semforest-focus_rmd_forest.rds")
saveRDS(envir_semforest_focus$vim, "vignettes/precomputed/semforest-focus_rmd_vim.rds")

#####
#
# Run future-parallel-semforest.Rmd
#
envir_future_parallel_semforest <- run_rmd_chunks("vignettes/future-parallel-semforest.Rmd")
saveRDS(envir_future_parallel_semforest$forest, "vignettes/precomputed/future-parallel-semforest_rmd_forest.rds")
saveRDS(envir_future_parallel_semforest$vim, "vignettes/precomputed/future-parallel-semforest_rmd_vim.rds")
