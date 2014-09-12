#'Model module: QuickGRaF
#'
#'Model module to fit a quick GRaF model, without parameter optimisation.
#'
#'@param df  
#'@name QuickGRaF
#'@keywords Model
QuickGRaF <-
function (df) {
  
  require ('GRaF')
  
  if (!all(df$type %in% c('presence', 'absence', 'background'))) {
    stop ('only for presence/absence or presence/background data')
  }
  
  covs <- as.data.frame(df[, 5:ncol(df)])
  names(covs) <- names(df)[5:ncol(df)]
  m <- graf(df$value,
                    covs)
  
  return (m)
}
