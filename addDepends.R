# Add dependencies to allow testing on Travis

modules <- list.files('R/', pattern = '.R$', full.names = TRUE)

deps <- NULL

# Find all the packages loaded
for(i in modules){
  
  script <- readLines(i)
  
  linesMatch <- grep(pattern = 'zoon[:]+GetPackage\\(.[^)]+\\)', x = script, value = TRUE)
  
  packages <- lapply(linesMatch, function(x){
      subs <- gregexpr(pattern = 'zoon[:]+GetPackage\\(.[^)]+\\)', text = x)
      str <- substr(x, start = subs[[1]][1], stop = subs[[1]][1] + attr(subs[[1]], which = 'match.length') - 1)
      str <- gsub(')$', '', str)
      str <- gsub('zoon[:]+GetPackage\\(', '', str)
      str <- unlist(strsplit(str, ','))
    })
  
  packages <- gsub("'", '', unlist(packages))
  packages <- gsub('"', '', unlist(packages))
  packages <- trimws(packages)
  
  deps <- c(deps, packages)
  
}

new_deps <- unique(deps)

# add these to the depends of the description file
# if they are not there already

DESCRIPTION <- readLines('DESCRIPTION')

st <- grep('^Depends:', DESCRIPTION) + 1
end <- grep('^Imports:', DESCRIPTION) - 1
current_deps <- unlist(trimws(DESCRIPTION[st:end]))

new_deps <- paste('   ', new_deps[!new_deps %in% current_deps])
deps_add <- c(paste('   ', current_deps), new_deps)
new_DESCRIPTION <- c(DESCRIPTION[1:(st-1)], deps_add, DESCRIPTION[(end+1):length(DESCRIPTION)])

# Write this new file
writeLines(new_DESCRIPTION, con = 'DESCRIPTION', sep = '\n')
