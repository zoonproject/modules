#'Model module: OptGRaF
#'
#'Model module to fit a (slow) GRaF model, with parameter optimisation.
#'
#'@param df  

#'@name OptGRaF
OptGRaF <-
function (df) {
  
  require ('GRaF')
  
  if (!all(df$type %in% c('presence', 'absence', 'background'))) {
    stop ('only for presence/absence or presence/background data')
  }
  
  covs <- as.data.frame(df[, 5:ncol(df)])
  names(covs) <- names(df)[5:ncol(df)]
  m <- graf(df$value,
            covs,
            opt.l = TRUE)
  
  return (m)
}
