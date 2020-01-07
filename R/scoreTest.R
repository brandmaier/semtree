# Last updated: 11.06.2019


#####################
### Documentation ###
#####################

# fit: a RAM-type OpenMx model
# covariate: covariate used to sort the data. Only a single covariate is
#            implemented.
# level: level of measurement of the covariate. Each level comes with its own
#        methods.
#        - nominal
#        - ordinal
#        - metric
# score_tests: list with different test statistics. The default is
#              list(nominal = 'LM', ordinal = 'DM', metric = 'DM').
#              - DM: double maximum test statistic (ordinal, metric)
#              - CvM: Cramér-von Mises type test statistic (metric)
#              - maxLM: maximum Lagrange multiplier test statistic (ordinal, metric)
#              - LM: Lagrange multiplier (nominal)
# parameter: single or several target parameters. A joint test of all parameters
#            is the default.
# alpha: level of significance. Default is a significance level is alpha = 0.05
#        Currently, only levels of 0.001, 0.005, 0.01, 0.025, 0.05, 0.075, 0.1,
#        0.15, & 0.2 are implemented for CvM and maxLM.
#
#
# Some variable names explained:
# max_obs: Value on the covariate with maximum CSP. This refers most likely to a
#          single individual for metric covariates and to a bin of individuals
#          for nominal and ordinal covariates. The individual(s) with covariate
#          identical to the cut point go(es) to the left side.
# cut_point: Value exported to semtree for splitting the data.
# obs_before_cut: The number of individuals before the cut point. In ohter
#                 words, the number of individuals on the left side.



scoretest <- function(fit, data_sorted, covariate_sorted, level, test,
                      parameter = NULL, alpha, min_bucket, bin_control) {
  
						  stop("Not yet implemented! See our github repository for latest versions.")
}
