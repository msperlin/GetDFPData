#' Converts a dataframe from gdfpd_GetDFPData to the wide format
#'
#' @param data.in Data frame with financial information
#' @param data.in.cols Which data to go in rows values ('original' or 'inflation adjusted')
#'
#' @return A dataframe in the wide format
#' @export
#'
#' @examples
#'
#' # get example data from RData file
#' my.f <- system.file('extdata/Example_DFP_Report_Petrobras.RData', package = 'GetDFPData')
#' load(my.f)
#'
#' df.assets <- df.reports$fr.assets[[1]]
#' df.assets.wide <- gdfpd.convert.to.wide(df.assets)
gdfpd.do.cache <- function(my.l, my.id, i.company, cache.folder, type.data) {

  # do cache
  #   check if name wont be a problem
  my.cache.dir.company <- stringr::str_trim(paste0(my.id,'_',
                                                   stringr::str_sub(i.company, 1,8)))
  my.cache.dir.company <- stringr::str_replace_all(my.cache.dir.company, '/', '')

  #   set dirs
  my.cache.dir <- file.path(cache.folder, my.cache.dir.company)

  if (!dir.exists(my.cache.dir)) dir.create(my.cache.dir)

  f.cache <- file.path(my.cache.dir, paste0('GetDFPData_',type.data, '_cache_',
                                            my.id,'_',
                                            stringr::str_sub(i.company, 1,4),'.rds' ) )


  if (file.exists(f.cache)&(do.cache)) {
    cat(paste0('\n\t\tFound cache file ', f.cache) )

    my.l <- readRDS(f.cache)

    return(my.l)
  }

  return(FALSE)

}
