#' Reads a single FCA zip file downloaded from Bovespa
#'
#' @param my.zip.file Full path to zip file
#' @param folder.to.unzip Folder to unzip files, default = tempdir()
#'
#' @return A list with several dataframes containing financial statements
#' @export
#'
#' @examples
#'
#' my.f <- system.file('extdata/FCA_9512_PETR_2015-12-31.zip', package = 'GetDFPData')
#'
#' my.l <- gdfpd.read.fca.zip.file(my.f)
#' print(my.l)
#'
gdfpd.read.fca.zip.file <- function(my.zip.file,
                                    folder.to.unzip = tempdir()) {

  # sanity check
  if (tools::file_ext(my.zip.file) != 'zip') {
    stop(paste('File', my.zip.file, ' is not a zip file.') )
  }

  if (!file.exists(my.zip.file)) {
    stop(paste('File', my.zip.file, ' does not exists.') )
  }

  if (file.size(my.zip.file) == 0){
    stop(paste('File', my.zip.file, ' has size 0!') )
  }

  if (length(my.zip.file) != 1){
    stop('This function only works for a single zip file... check your inputs')
  }

  if (!dir.exists(folder.to.unzip)) {
    cat(paste('Folder', folder.to.unzip, 'does not exist. Creating it.'))
    dir.create(folder.to.unzip)
  }


  my.basename <- tools::file_path_sans_ext(basename(my.zip.file))
  rnd.folder.name <- file.path(folder.to.unzip, paste0('DIR-',my.basename))

  if (!dir.exists(rnd.folder.name)) dir.create(rnd.folder.name)

  utils::unzip(my.zip.file, exdir = rnd.folder.name, junkpaths = TRUE)

  # list files and check it
  my.files <- list.files(rnd.folder.name)

  if (length(my.files) == 0) {
    #browser()

    file.remove(my.zip.file)
    stop(paste0('Zipped file contains 0 files. ',
                'This is likelly a problem with the downloaded file. ',
                'Try running the code again as the corrupted zip file was deleted and will be downloaded again.',
                '\n\nIf the problem persists, my suggestions is to remove the time period with problem.') )
  }


  my.l <- gdfpd.read.zip.file.type.fca(rnd.folder.name, folder.to.unzip)

  return(my.l)
}

#' Reads folder for FCA zip file contents (internal)
#'
#' @inheritParams gdfpd.read.fca.zip.file
#' @param rnd.folder.name Folder where unzipped files are available
#'
#' @return A list with FCA data
#'
#' @examples
#' # no example (this functions is not used directly)
gdfpd.read.zip.file.type.fca <- function(rnd.folder.name, folder.to.unzip = tempdir()) {

  zipped.file <- file.path(rnd.folder.name, list.files(rnd.folder.name, pattern = '*.fca')[1])

  utils::unzip(zipped.file, exdir = rnd.folder.name)

  company.reg.file <- file.path(rnd.folder.name,'ValorMobiliarioMercadoNegociacao.xml')

  if (!file.exists(company.reg.file)) {
    df.governance.listings <- data.frame(listed.segment = NA,
                                         type.market = NA,
                                         name.market = NA)
  } else {
    xml_data <- XML::xmlToList(XML::xmlParse(company.reg.file))

    xml_data$ValorMobiliario$Documento$CompanhiaAberta$DataConstituicaoEmpresa

    xml_data2 <- xml_data$ValorMobiliario$MercadosNegociacao$MercadoNegociacao$Segmento
    listed.segment <- xml_data2$DescricaoOpcaoDominio
    type.market <- xml_data$ValorMobiliario$MercadoNegociacao$DescricaoOpcaoDominio
    name.market <- xml_data$ValorMobiliario$MercadosNegociacao$MercadoNegociacao$EntidadeAdministradora$SiglaOpcaoDominio

    df.governance.listings <- data.frame(listed.segment = fix.fct(listed.segment),
                                         type.market = fix.fct(type.market),
                                         name.market = fix.fct(name.market), stringsAsFactors = FALSE)
  }


  company.reg.file <- file.path(rnd.folder.name,'Documento.xml')
  if (!file.exists(company.reg.file)) {
    df.company.info <- data.frame(cnpj = NA,
                                  date.company.constitution = NA,
                                  date.cvm.registration = NA)
  } else {
    xml_data <- XML::xmlToList(XML::xmlParse(company.reg.file))

    cnpj <- xml_data$CompanhiaAberta$NumeroCnpjCompanhiaAberta
    date.company.constitution <- as.Date(xml_data$CompanhiaAberta$DataConstituicaoEmpresa)
    date.cvm.registration <- as.Date(xml_data$CompanhiaAberta$DataRegistroCvm)

    df.company.info <- data.frame(cnpj = fix.fct(cnpj),
                                  date.company.constitution = fix.fct(date.company.constitution),
                                  date.cvm.registration = fix.fct(date.cvm.registration), stringsAsFactors = FALSE)
  }

  # save output

  my.l <- list(df.governance.listings = df.governance.listings,
               df.company.info = df.company.info)

  return(my.l)
}

