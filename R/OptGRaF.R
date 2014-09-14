#'Model module: OptGRaF
#'
#'Model module to fit a (slow) GRaF model, with parameter optimisation.
#'
#'@name OptGRaF


OptGRaF <-
function (df) {
  
  zoon:::GetPackage('GRaF')
  
  if (!all(df$type %in% c('presence', 'absence', 'background'))) {
    stop ('only for presence/absence or presence/background data')
  }
  
  covs <- as.data.frame(df[, 6:ncol(df)])
  names(covs) <- names(df)[6:ncol(df)]
  m <- graf(df$value,
            covs,
            opt.l = TRUE)
  
  return (m)
}
