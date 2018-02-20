#' Reads a single FRE zip file downloaded from Bovespa
#'
#' @param my.zip.file Full path to zip file
#' @param folder.to.unzip Folder to unzip files (default = tempdir())
#'
#' @return A list with several dataframes containing financial statements
#' @export
#'
#' @examples
#'
#' my.f <- system.file('extdata/FRE_6629_HERC_2010-12-31.zip', package = 'GetDFPData')
#'
#' my.l <- gdfpd.read.fre.zip.file(my.f)
#' print(my.l)
#'
gdfpd.read.fre.zip.file <- function(my.zip.file,
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


  my.l <- gdfpd.read.zip.file.type.fre(rnd.folder.name, folder.to.unzip)

  return(my.l)
}

#' Reads folder for zip file post 2011 (internal)
#'
#' @inheritParams gdfpd.read.fre.zip.file
#' @param rnd.folder.name Folder where unzipped files are available
#'
#' @return A list with financial statements
#'
#' @examples
#' # no example (this functions not used directly)
gdfpd.read.zip.file.type.fre <- function(rnd.folder.name, folder.to.unzip = tempdir()) {

  zipped.file <- file.path(rnd.folder.name, list.files(rnd.folder.name, pattern = '*.fre')[1])

  utils::unzip(zipped.file, exdir = rnd.folder.name)

  company.reg.file <- file.path(rnd.folder.name,'ControleAcionario.xml')

  xml_data <- XML::xmlToList(XML::xmlParse(company.reg.file, encoding = 'UTF-8'))


  # get stock holders composition

  df.stockholders <- do.call(what = rbind, lapply(xml_data,xml.fct.stockholder))
  rownames(df.stockholders) <- NULL

  # stock composition
  company.reg.file <- file.path(rnd.folder.name,'CapitalSocial.xml')

  xml_data <- XML::xmlToList(XML::xmlParse(company.reg.file, encoding = 'UTF-8'))

  if (length(xml_data) ==0 ) {
    df.capital <- data.frame(stock.type = NA,
                             stock.class = NA,
                             qtd.issued = NA)
  } else {
    # find integralized capital information
    idx <- sapply(xml_data, function(x) x$CodigoTipoCapital) == '3'

    # get data
    if ( !(any(idx)) ) { # fix for non existing integralized stocks (corner case)
      df.capital <- data.frame(stock.type = c('ON', 'PN'),
                               stock.class = c('0', '0'),
                               qtd.issued = c(0, 0), stringsAsFactors = FALSE)
    } else {

      effective.capital <- xml_data[[max(which(idx))]]

      if ( is.null(effective.capital$CapitaisSocialPorClasse)) {

        df.capital <- data.frame(stock.type = c('ON', 'PN'),
                                 stock.class = c('0', '0'),
                                 qtd.issued = c(as.numeric(effective.capital$QuantidadeAcoesOrdinarias),
                                                as.numeric(effective.capital$QuantidadeAcoesPreferenciais)), stringsAsFactors = FALSE )
      } else {

        my.fct <- function(x) {
          my.df <- data.frame(stock.type = 'PN',
                              stock.class = x$CodigoClasseAprf,
                              qtd.issued = as.numeric(x$QuantidadeAcoes))
          return(my.df)
        }

        temp.df <- do.call(what = rbind, lapply(effective.capital$CapitaisSocialPorClasse,FUN = my.fct))
        rownames(temp.df) <- NULL

        df.capital <- rbind(data.frame(stock.type = c('ON'),
                                       stock.class = c('0'),
                                       qtd.issued = as.numeric(effective.capital$QuantidadeAcoesOrdinarias),
                                       stringsAsFactors = FALSE ),
                            temp.df)

      }

    }

  }


  # market value of company
  company.reg.file <- file.path(rnd.folder.name,'CotacaoValoresMobiliarios.xml')
  xml_data <- XML::xmlToList(XML::xmlParse(company.reg.file, encoding = 'UTF-8'))

  if (is.null(xml_data)) {

    df.stock.values <- data.frame(stock.type = NA,
                                  stock.class = NA,
                                  max.price = NA,
                                  min.price = NA,
                                  avg.price = NA,
                                  qtd.issued = NA,
                                  flag.missing.avg.price = NA)

    df.mkt.value <- data.frame(mkt.avg.value = NA,
                               mkt.min.value = NA,
                               mkt.max.value = NA)

  } else {

    #   find data for current ref.date
    temp.dates <- as.Date(sapply(xml_data, function(x) stringr::str_sub(x$DataFimTrimestre,1,10) ))
    ref.date <- max(temp.dates)

    xml_data <- xml_data[ref.date == temp.dates]

    df.stock.values <- do.call(what = rbind, lapply(xml_data,FUN = xml.fct.stock.values))
    rownames(df.stock.values) <- NULL

    df.stock.values <- merge(df.stock.values, df.capital, by = c('stock.class', 'stock.type'), all = TRUE )

    # fix for 0 qtd.issued
    idx <- df.stock.values$qtd.issued ==0
    df.stock.values$avg.price[idx] <- 0
    df.stock.values$max.price[idx] <- 0
    df.stock.values$min.price[idx] <- 0

  }

  # company value
  df.mkt.value <- data.frame(mkt.avg.value = sum(df.stock.values$qtd.issued*df.stock.values$avg.price),
                             mkt.min.value = sum(df.stock.values$qtd.issued*df.stock.values$min.price),
                             mkt.max.value = sum(df.stock.values$qtd.issued*df.stock.values$max.price) )

  # get: increases of capital
  company.reg.file <- file.path(rnd.folder.name,'AumentoCapitalEmissor.xml')
  xml_data <- XML::xmlToList(XML::xmlParse(company.reg.file, encoding = 'UTF-8'))

  df.increase.capital <- do.call(what = rbind, lapply(xml_data, xml.fct.capital))
  rownames(df.increase.capital) <- NULL

  # get: capital reduction
  company.reg.file <- file.path(rnd.folder.name,'ReducaoCapitalEmissor.xml')
  xml_data <- XML::xmlToList(XML::xmlParse(company.reg.file))

  df.capital.reduction <- do.call(what = rbind,
                                  lapply(xml_data,
                                         xml.fct.capital.reduction))
  rownames(df.capital.reduction) <- NULL

  # get: compensation details
  company.reg.file <- file.path(rnd.folder.name,'RemuneracaoReconhecidaAdministradores.xml')
  xml_data <- XML::xmlToList(XML::xmlParse(company.reg.file, encoding = 'UTF-8'))

  df.compensation <- do.call(what = rbind,
                             lapply(xml_data[[1]]$RemuneracaoReconhecidaOrgao,
                                    xml.fct.compensation))
  rownames(df.compensation) <- NULL

  # get compensation summary
  company.reg.file <- file.path(rnd.folder.name,'RemuneracaoOrgaos.xml')
  xml_data <- XML::xmlToList(XML::xmlParse(company.reg.file, encoding = 'UTF-8'))

  df.compensation.summary <- do.call(what = rbind, lapply(xml_data[1:3], xml.fct.compensation.summary))
  rownames(df.compensation.summary) <- NULL

  # get: transactions related parts

  company.reg.file <- file.path(rnd.folder.name,'TransacaoComParteRelacionada.xml')
  xml_data <- XML::xmlToList(XML::xmlParse(company.reg.file, encoding = 'UTF-8'))

  df.transactions.related <- do.call(what = rbind, lapply(xml_data, xml.fct.transactions.related))
  rownames(df.transactions.related) <- NULL

  # get: splits, inplits and other events

  company.reg.file <- file.path(rnd.folder.name,'DesdobramentoGrupamentoBonificacao.xml')
  xml_data <- XML::xmlToList(XML::xmlParse(company.reg.file, encoding = 'UTF-8'))

  df.other.events <- do.call(what = rbind, lapply(xml_data, xml.fct.splits.inplits))
  rownames(df.other.events) <- NULL

  # get: repurchases
  company.reg.file <- file.path(rnd.folder.name,'PlanoRecompraAcoes.xml')
  xml_data <- XML::xmlToList(XML::xmlParse(company.reg.file, encoding = 'UTF-8'))

  df.stock.repurchases <- do.call(what = rbind, lapply(xml_data, xml.fct.repurchases))
  rownames(df.stock.repurchases) <- NULL

  # get: debt
  company.reg.file <- file.path(rnd.folder.name,'Dividas.xml')
  xml_data <- XML::xmlToList(XML::xmlParse(company.reg.file, encoding = 'UTF-8'))

  number.time.period <- as.numeric(sapply(xml_data, function(y) return(y$ExercicioSocial$NumeroExercicioSocial)))
  max.exercicio <- max(number.time.period)

  idx <- number.time.period == max.exercicio

  xml_data <- xml_data[idx]

  df.debt.composition <- do.call(what = rbind, lapply(xml_data, xml.fct.debt))
  rownames(df.debt.composition) <- NULL

  # get: composition management and fiscal council
  company.reg.file <- file.path(rnd.folder.name,'AdministradorMembroConselhoFiscalNegocios.xml')
  xml_data <- XML::xmlToList(XML::xmlParse(company.reg.file, encoding = 'UTF-8'))

  df.board.composition <- do.call(what = rbind, lapply(xml_data, xml.fct.board.composition))
  rownames(df.board.composition) <- NULL

  # get: composition commitees
  company.reg.file <- file.path(rnd.folder.name,'MembroComiteNegocios.xml')
  xml_data <- XML::xmlToList(XML::xmlParse(company.reg.file, encoding = 'UTF-8'))

  df.committee.composition <- do.call(what = rbind, lapply(xml_data, xml.fct.committee.composition))
  rownames(df.committee.composition) <- NULL

  # get: family relations

  company.reg.file <- file.path(rnd.folder.name,'RelacaoConjugalNegocios.xml')
  xml_data <- XML::xmlToList(XML::xmlParse(company.reg.file, encoding = 'UTF-8'))

  df.family.relations <- do.call(what = rbind, lapply(xml_data, xml.fct.family.relations))
  rownames(df.family.relations) <- NULL

  # get: family relations in related companies

  company.reg.file <- file.path(rnd.folder.name,'HistoricoRelacaoSubordinacaoAdministradorEmissor.xml')
  xml_data <- XML::xmlToList(XML::xmlParse(company.reg.file, encoding = 'UTF-8'))

  df.family.related.companies <- do.call(what = rbind, lapply(xml_data, xml.fct.family.related.parts))
  rownames(df.family.related.companies) <- NULL


  # get: auditing information

  company.reg.file.1 <- file.path(rnd.folder.name, 'AuditorFormularioReferencia_v2.xml')
  company.reg.file.2 <- file.path(rnd.folder.name, 'AuditorFormularioReferencia.xml')
  my.files <-  c(company.reg.file.1, company.reg.file.2)
  company.reg.file <-my.files[file.exists(my.files)]

  xml_data <- XML::xmlToList(XML::xmlParse(company.reg.file, encoding = 'UTF-8'))

  df.auditing <- do.call(what = dplyr::bind_rows, lapply(xml_data, xml.fct.auditing))
  rownames(df.auditing) <- NULL

  # save output

  my.l <- list(df.stockholders = df.stockholders,
               df.capital = df.capital,
               df.stock.values = df.stock.values,
               df.mkt.value = df.mkt.value,
               df.increase.capital = df.increase.capital,
               df.capital.reduction = df.capital.reduction,
               df.compensation = df.compensation,
               df.compensation.summary = df.compensation.summary,
               df.transactions.related = df.transactions.related,
               df.other.events = df.other.events,
               df.stock.repurchases = df.stock.repurchases,
               df.debt.composition = df.debt.composition,
               df.board.composition = df.board.composition,
               df.committee.composition = df.committee.composition,
               df.family.relations = df.family.relations,
               df.family.related.companies = df.family.related.companies,
               df.auditing = df.auditing )

  return(my.l)
}

