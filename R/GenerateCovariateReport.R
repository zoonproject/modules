#' @name GenerateCovariateReport
#'
#' @title Generate a data profiling report
#'
#' @description This output module generates a report outlining the results of data profiling. This includes data structure, missing values, and correlation testing.
#'
#' @details The results are presented as an html output generated using Rmarkdown that opens in your internet browser
#'
#' @param .model \strong{Internal parameter, do not use in the workflow function}. \code{.model} is list of a data frame (\code{data}) and a model object (\code{model}). \code{.model} is passed automatically in workflow, combining data from the model module(s) and process module(s), to the output module(s) and should not be passed by the user.
#'
#' @param .ras \strong{Internal parameter, do not use in the workflow function}. \code{.ras} is a raster layer, brick or stack object. \code{.ras} is passed automatically in workflow from the covariate module(s) to the output module(s) and should not be passed by the user.
#'
#' @param type The type of data report(s) you wish to generate. This must be one of "D" (data report only), "R" (raster report only), or "DR" (both data and raster reports)
#'
#' @family output
#'
#' @author David Wilkinson, \email{davidpw@@student.unimelb.edu.au}
#'
#' @section Data type: presence-only, presence/absence, abundance, proportion
#'
#' @section Version: 0.1
#'
#' @section Date submitted:  2017-05-15
GenerateCovariateReport <- function(.model, .ras, type = "DR"){
  
  if(type != "D" & type != "R" & type != "DR")                 # stop function if incorrect "type" selection
    stop('type must be one of "D", "R", or "DR"')
  
  #########################
  ### Required Packages ###
  #########################
  
  zoon::GetPackage("DataExplorer")
  zoon::GetPackage("data.table")
  zoon::GetPackage("rmarkdown")
  
  #######################################
  ### Rmarkdown files as text strings ###
  #######################################\
  
  #DataReportText #-------  
  
  DataReportText <- c(
    '---',
    'title: "Data Profiling Report"',
    'output:',
    '  html_document: ',
    '    theme: cerulean',
    '    toc: yes',
    '    toc_depth: 6',
    'params:',
    '  data: data',
    '  fun_options: fun_options',
    '---',
    '',
    '```{r global_options, include = FALSE}',
    'library(rmarkdown)',
    '',
    'data <- params$data',
    'num_discrete <- SplitColType(params$data)$num_discrete',
    'num_continuous <- SplitColType(params$data)$num_continuous',
    'num_all_missing <- SplitColType(params$data)$num_all_missing',
    '',
    'continuous_error <- function(e) {',
    '  if (num_continuous == 0) {',
    '    cat("No continuous features found.")',
    '  } else {',
    '    cat("Continuous features contain too many missing values.")',      # removed \n after values
    '    cat("Try using PlotMissing() to determine which features to drop.")',
    '  }',
    '}',
    '',
    'discrete_error <- function(e) {',
    '  if (num_discrete == 0) {',
    '    cat("No discrete features found.")',
    '  } else {',
    '    cat("Discrete features contain too many categories.")',         # removed \n after categories
    '    cat("Try using CollapseCategory() to reduce categories.")',
    '  }',
    '}',
    '',
    'opts <- params$fun_options',
    'knitr::opts_chunk$set(fig.width = 14, fig.height = 10, echo = FALSE)',
    '```',
    '',
    #    '<script src="d3.min.js"></script>',
    '  ',
    '### Basic Statistics',
    'The data is **`r format(object.size(data), units = "auto")`** in size. There are **`r format(nrow(data), big.mark = ",")`** rows and **`r format(ncol(data), big.mark = ",")`** columns (features). Of all **`r format(ncol(data), big.mark = ",")`** columns, **`r I(num_discrete)`** are discrete, **`r I(num_continuous)`** are continuous, and **`r I(num_all_missing)`** are all missing. There are **`r format(sum(is.na(data)), big.mark = ",")`** missing values out of **`r format(nrow(data)*ncol(data), big.mark = ",")`** data points.',
    '',
    '#### Data Structure',
    '',
    '```{r data_structure}',
    'str(data)',
    '```',
    '',
    '### Missing Values',
    'The following graph shows the distribution of missing values.',
    '```{r plot_missing}',
    'PlotMissing(data)',
    '```',
    '',
    '### Data Distribution',
    '',
    '#### Continuous Features',
    '',
    '###### Histogram',
    '```{r histogram_continuous}',
    'tryCatch(HistogramContinuous(data), error = continuous_error)',
    '```',
    '',
    '#### Discrete Features',
    '###### Bar Charts',
    '```{r bar_discrete}',
    'tryCatch(BarDiscrete(data), error = discrete_error)',
    '```',
    '',
    '### Correlation Analysis',
    '',
    '#### Continuous Features',
    '```{r correlation_continuous}',
    'tryCatch(CorrelationContinuous(data, use = "na.or.complete"), error = continuous_error)',
    '```',
    '',
    '#### Discrete Features',
    '```{r correlation_discrete}',
    'tryCatch(CorrelationDiscrete(data, use = "pairwise.complete.obs"), error = discrete_error)',
    '```'
  )
  
  #RasterReportText #-------
  RasterReportText <- c(
    '---',
    'title: "Raster Profiling Report"',
    'output:',
    '  html_document: ',
    '    theme: cerulean',
    '    toc: yes',
    '    toc_depth: 6',
    'params:',
    '  data: data',
    '  fun_options: fun_options',
    '---',
    '  ',
    '```{r global_options, include = FALSE}',
    'library(rmarkdown)',
    '',
    'data <- params$data',
    'num_discrete <- SplitColType(params$data)$num_discrete',
    'num_continuous <- SplitColType(params$data)$num_continuous',
    'num_all_missing <- SplitColType(params$data)$num_all_missing',
    '',
    'continuous_error <- function(e) {',
    '  if (num_continuous == 0) {',
    '    cat("No continuous features found.")',
    '  } else {',
    '    cat("Continuous features contain too many missing values.")',       # removed /n from after values
    '    cat("Try using PlotMissing() to determine which features to drop.")',
    '  }',
    '}',
    '',
    'discrete_error <- function(e) {',
    '  if (num_discrete == 0) {',
    '    cat("No discrete features found.")',
    '  } else {',
    '    cat("Discrete features contain too many categories.")',             # removed \n after categories
    '    cat("Try using CollapseCategory() to reduce categories.")',
    '  }',
    '}',
    '',
    'opts <- params$fun_options',
    'knitr::opts_chunk$set(fig.width = 14, fig.height = 10, echo = FALSE)',
    '```',
    '',
    #    '<script src="d3.min.js"></script>',
    '  ',
    '### Basic Statistics',
    'The data is **`r format(object.size(data), units = "auto")`** in size. There are **`r format(nrow(data), big.mark = ",")`** rows and **`r format(ncol(data), big.mark = ",")`** columns (features). Of all **`r format(ncol(data), big.mark = ",")`** columns, **`r I(num_discrete)`** are discrete, **`r I(num_continuous)`** are continuous, and **`r I(num_all_missing)`** are all missing. There are **`r format(sum(is.na(data)), big.mark = ",")`** missing values out of **`r format(nrow(data)*ncol(data), big.mark = ",")`** data points.',
    '',
    '#### Data Structure (Text)',
    '',
    '```{r data_structure}',
    'str(data)',
    '```',
    '',
    '### Data Distribution',
    '',
    '#### Continuous Features',
    '',
    '###### Histogram',
    '```{r histogram_continuous}',
    'tryCatch(HistogramContinuous(data), error = continuous_error)',
    '```',
    '',
    '#### Discrete Features',
    '###### Bar Charts',
    '```{r bar_discrete}',
    'tryCatch(BarDiscrete(data), error = discrete_error)',
    '```',
    '',
    '### Correlation Analysis',
    '',
    '#### Continuous Features',
    '```{r correlation_continuous}',
    'tryCatch(CorrelationContinuous(data, use = "na.or.complete"), error = continuous_error)',
    '```',
    '',
    '#### Discrete Features',
    '```{r correlation_discrete}',
    'tryCatch(CorrelationDiscrete(data, use = "pairwise.complete.obs"), error = discrete_error)',
    '```'
  )
  #End hidden code sections #-------
  
  #####################################
  ### Functions to generate reports ###
  #####################################
  
  GenerateDataReport <- function(input_data, output_file = "data_report.html", output_dir = getwd(), ...) {
    ## Get argument list
    args <- as.list(match.call())
    ## Get directory of report markdown template
    data_report_dir <- tempfile(fileext = ".rmd", tmpdir = tempdir())
    cat(DataReportText, file = data_report_dir, fill = 1)
    ## Render report into html
    render(
      input = data_report_dir,
      output_file = output_file,
      output_dir = output_dir,
      intermediates_dir = output_dir,
      params = list(data = input_data, fun_options = list()),
      ...
    )
    ## Open report
    report_path <- file.path(output_dir, output_file)
    browseURL(report_path)
    ## Print report directory
    if (ifelse(is.null(args[["quiet"]]), TRUE, !args[["quiet"]])) message(paste0("\n\nReport is generated at \"", report_path, "\"."))
  }
  
  GenerateRasterReport <- function(input_data, output_file = "raster_report.html", output_dir = getwd(), ...) {
    ## Get argument list
    args <- as.list(match.call())
    ## Get directory of report markdown template
    raster_report_dir <- tempfile(fileext = ".rmd", tmpdir = tempdir())
    cat(RasterReportText, file = raster_report_dir, fill = 1)
    ## Render report into html
    render(
      input = raster_report_dir,
      output_file = output_file,
      output_dir = output_dir,
      intermediates_dir = output_dir,
      params = list(data = input_data, fun_options = list()),
      ...
    )
    ## Open report
    report_path <- file.path(output_dir, output_file)
    browseURL(report_path)
    ## Print report directory
    if (ifelse(is.null(args[["quiet"]]), TRUE, !args[["quiet"]])) message(paste0("\n\nReport is generated at \"", report_path, "\"."))
  }
  
  # if(type != any("D","R","DR")){
  #   stop ('Type must be one of "D", "R", or "DR"')
  # }
  
  ###################################################
  ### Generate reports dependent on selected type ###
  ###################################################
  
  cov_names <- attr(.model$data, 'covCols')   # extract attribute of .model$data saying which colums are covariates
  
  if(type == "D"){   # generate data report only
    print(paste0("Generate data report"))
    GenerateDataReport(data.table(as.data.frame(.model$data[,cov_names, drop = FALSE])))
  } else {
    if(type == "R"){   # generate raster report only
      print(paste0("Generate raster report"))
      GenerateRasterReport(data.table(as.data.frame(.ras)))
    } else {
      if(type == "DR"){   # generate data and raster report
        print(paste0("Generate data and raster report"))
        GenerateDataReport(data.table(as.data.frame(.model$data[,cov_names, drop = FALSE])))
        GenerateRasterReport(data.table(as.data.frame(.ras)))
      }
    }
  }
  unlink(Sys.glob("file*"), force = TRUE, recursive = TRUE)
}
