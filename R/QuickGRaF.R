#'Model module: QuickGRaF
#'
#'Model module to fit a quick GRaF model, without parameter optimisation.
#'
#'@name QuickGRaF


QuickGRaF <-
function (df) {
  
  zoon:::GetPackage('GRaF')
  
  if (!all(df$type %in% c('presence', 'absence', 'background'))) {
    stop ('only for presence/absence or presence/background data')
  }
  
  covs <- as.data.frame(df[, 6:ncol(df)])
  names(covs) <- names(df)[6:ncol(df)]
  m <- graf(df$value,
                    covs)
  
  return (m)
}
