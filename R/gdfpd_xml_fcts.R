#' Fix NULL values in dataframe
#'
#' @param x Am object, possibly NULL
#' @param type.info Type of object
#' @param format.date Format of data, as string
#'
#' @return A single object
#' @export
#'
#' @examples
#'
#' x <- NULL
#' x2 <- fix.fct(x)
fix.fct <- function(x, type.info = 'character', format.date = '%Y-%m-%d') {
  if (is.null(x)) return(NA)

  if (!(type.info %in% c('character', 'date', 'numeric'))) {
    stop('type.info should be ', paste0(c('character', 'date', 'numeric'), collapse = ' or '))
  }

  if (type.info == 'date') {
    if (nchar(x) > 10) return(as.Date(NA))

    x <- as.Date(x, format.date)

    if (is.na(x)) return(x)

    if ( x == '1-01-01') {
      x <- as.Date(NA)
    }
  }

  if (type.info == 'numeric') {

    x <- as.numeric(x)
  }

  return(x)
}

#' Reads XML data for compensation
#'
#' @param x A list with compensation data
#'
#' @return A dataframe
#' @export
#'
#' @examples
#'
#' # No example (INTERNAL)
xml.fct.compensation <- function(x) {

  x <- x$RemuneracaoReconhecidaOrgao$RemuneracaoReconhecidaOrgao

  if (is.null(x)) {
    return(data.frame(level.remuneration = NA,
                      qtd.members = NA,
                      qtd.remunerated.members = NA,
                      total.value.remuneration = NA,
                      fixed.salary = NA,
                      fixed.benefits = NA,
                      fixed.participations = NA,
                      fixed.others = NA,
                      variable.bonus = NA,
                      variable.results.participation = NA,
                      variable.meetings.participation = NA,
                      variable.commissions.participation = NA,
                      variable.others = NA,
                      post.job.compensation = NA,
                      ceasing.job.compensation = NA,
                      stocks.options.benefits = NA,
                      stringsAsFactors = FALSE))
  }

  df.out <- data.frame(level.remuneration = switch(fix.fct(x$CodigoOrgaoAdministrador),
                                                   '0' = NA,
                                                   '1' = 'Management Council',
                                                   '2' = 'Statutory Directors',
                                                   '3' = 'Fiscal Council'),
                       qtd.members = as.numeric(fix.fct(x$QuantidadeMembros)),
                       qtd.remunerated.members = switch(as.character(is.null(x$QtdMembrosRemunerados)),
                                                        'TRUE' = NA,
                                                        'FALSE' = as.numeric(x$QtdMembrosRemunerados)),
                       total.value.remuneration = as.numeric(fix.fct(x$ValorTotalRemuneracao)),
                       fixed.salary = as.numeric(fix.fct(x$ValorFixoSalario)),
                       fixed.benefits = as.numeric(fix.fct(x$ValorFixoBeneficios)),
                       fixed.participations = as.numeric(fix.fct(x$ValorFixoParticipacoesComites)),
                       fixed.others = as.numeric(fix.fct(x$ValorFixoOutros)),
                       variable.bonus = as.numeric(fix.fct(x$ValorVariavelBonus)),
                       variable.results.participation = as.numeric(fix.fct(x$ValorVariavelParticipacaoResultados)),
                       variable.meetings.participation = as.numeric(fix.fct(x$ValorVariavelParticipacoesReunioes)),
                       variable.commissions.participation = as.numeric(fix.fct(x$ValorVariavelComissoes)),
                       variable.others = as.numeric(fix.fct(x$ValorVariavelOutros)),
                       post.job.compensation = as.numeric(fix.fct(x$ValorBeneficiosPosEmprego)),
                       ceasing.job.compensation = as.numeric(fix.fct(x$ValorBeneficiosCessacaoCargo)),
                       stocks.options.benefits = as.numeric(fix.fct(x$ValorBeneficiosBaseadaAcoes)),
                       stringsAsFactors = FALSE)



  return(df.out)
}

#' Reads XML data for compensation summary data
#'
#' @param x A list with compensation summary data
#'
#' @return A dataframe
#' @export
#'
#' @examples
#'
#' # No example (INTERNAL)
xml.fct.compensation.summary <- function(x) {

  if (is.null(x)) return(data.frame())

  df.out <- data.frame(level.remuneration = switch(x$CodigoOrgaoAdministrador,
                                                   '1' = 'Management Council',
                                                   '2' = 'Statutory Directors',
                                                   '3' = 'Fiscal Council'),
                       qtd.members = as.numeric(x$QuantidadeMembros),
                       qtd.remunerated.members = switch(as.character(is.null(x$QtdMembrosRemunerados)),
                                                        'TRUE' = NA,
                                                        'FALSE' = as.numeric(x$QtdMembrosRemunerados)),
                       max.remuneration = as.numeric(x$ValorMaiorRemuneracao),
                       mean.remuneration = as.numeric(x$ValorMedioRemuneracao),
                       min.remuneration = as.numeric(x$ValorMenorRemuneracao),
                       observations = fix.fct(x$Observacao),
                       stringsAsFactors = FALSE)



  return(df.out)
}


#' Reads XML data for capita
#'
#' @param x A list with capital summary data
#'
#' @return A dataframe
#' @export
#'
#' @examples
#'
#' # No example (INTERNAL)
xml.fct.capital <- function(x) {
  df.out <- data.frame(date.increase.capital = as.Date(fix.fct(x$DataDeliberacao)),
                       name.authorizing.department = fix.fct(x$NomeOrgaoDeliberacaoAcrescimo),
                       value.increase.capital = as.numeric(fix.fct(x$ValorTotalEmissao)),
                       type.increase.capital = as.numeric(fix.fct(x$CodigoTipoSubscricao)),
                       qtd.new.ordinary.shares = as.numeric(fix.fct(x$QuantidadeAcaoOrdinaria)),
                       qtd.new.preferred.shares = as.numeric(fix.fct(x$QuantidadeAcaoPreferencial)),
                       stringsAsFactors = FALSE)

  return(df.out)
}

#' Reads XML data for stock value
#'
#' @param x A list with stock value data
#'
#' @return A dataframe
#' @export
#'
#' @examples
#'
#' # No example (INTERNAL)
xml.fct.stock.values <- function(x) {

  if (is.null(x$CotacaoMedia)) {

    my.avg.price <- mean(c(as.numeric(x$MaiorCotacao),
                           as.numeric(x$MenorCotacao)))
    flag.missing.avg.price <- TRUE

  } else {
    if (as.numeric(x$CotacaoMedia) == 0) {
      my.avg.price <- mean(c(as.numeric(x$MaiorCotacao),
                             as.numeric(x$MenorCotacao)))
      flag.missing.avg.price <- TRUE
    } else {
      my.avg.price <- as.numeric(x$CotacaoMedia)
      flag.missing.avg.price <- FALSE
    }
  }


  my.df <- data.frame(stock.type = switch(x$EspecieAcao,
                                          '0' = 'ON',
                                          '1' = 'ON',
                                          '2' = 'PN'),
                      stock.class = x$ClasseAcaoPN,
                      max.price = as.numeric(x$MaiorCotacao),
                      min.price = as.numeric(x$MenorCotacao),
                      avg.price = my.avg.price,
                      flag.missing.avg.price = flag.missing.avg.price,
                      stringsAsFactors = F )

  return(my.df)
}




#' Reads XML data for stockholder data
#'
#' @param x A list with stockholder data
#'
#' @return A dataframe
#' @export
#'
#' @examples
#'
#' # No example (INTERNAL)
xml.fct.stockholder <- function(x) {

  df.out <- data.frame(type.register = x$TipoRegistro,
                       id.person = fix.fct(x$Pessoa$IdentificacaoPessoa),
                       id.nationality = fix.fct(x$Nacionalidade),
                       id.state = fix.fct(x$Estado$NomeEstado),
                       id.country = fix.fct(x$Estado$Pais$NomePais),
                       name.stockholder = fix.fct(x$Pessoa$NomePessoa),
                       type.stockholder = fix.fct(x$Pessoa$TipoPessoa),
                       qtd.ord.shares = x$QuantidadeAcoesOrdinarias,
                       perc.ord.shares = x$PercentualAcoesOrdinarias,
                       qtd.pref.shares = x$QuantidadeAcoesPreferenciais,
                       perc.pref.shares = x$PercentualAcoesPreferenciais,
                       controlling.stockholder = switch(x$AcionistaControlador,
                                                        '1' = TRUE,
                                                        '2' = FALSE,
                                                        '0' = FALSE),
                       stringsAsFactors = FALSE )


  return(df.out)
}


#' Reads XML data for transaction data
#'
#' @param x A list with transaction data
#'
#' @return A dataframe
#' @export
#'
#' @examples
#'
#' # No example (INTERNAL)
xml.fct.transactions.related <- function(x) {

  df.out <- data.frame(id.transaction = fix.fct(x$NumIdtTnsaPateRelc),
                       name.related.part = fix.fct(x$NomePateRelc),
                       date.transaction = as.Date(x$DataTnsa),
                       description.related.part = fix.fct(x$DescRelcPateRelcEmss),
                       description.transaction = fix.fct(x$DescObjtCotr),
                       value.transaction = fix.fct(x$ValMontEnvldoNeg),
                       description.guarantees = fix.fct(x$DescGarnSeguRelc),
                       description.transaction.period = fix.fct(x$DescDuraTnsa),
                       description.rescision = fix.fct(x$DescCondResc),
                       interest.rate = as.numeric(fix.fct(x$FatTaxaJuro)),
                       value.balance = fix.fct(x$ValSaldExis),
                       stringsAsFactors = FALSE)

  return(df.out)
}

#' Reads XML data for splits/inplits data
#'
#' @param x A list with data
#'
#' @return A dataframe
#' @export
#'
#' @examples
#'
#' # No example (INTERNAL)
xml.fct.splits.inplits <- function(x) {

  df.out <- data.frame(approval.date = as.Date(x$DataAprovacao),
                       type.event = x$DetalheDominio$DescricaoOpcaoDominio,
                       qtd.ord.shares.before = as.numeric(x$QuantidadeAcaoOrdinariaAntesAprovacao),
                       qtd.ord.shares.after = as.numeric(x$QuantidadeAcaoOrdinariaDepoisAprovacao),
                       qtd.pref.shares.before = as.numeric(x$QuantidadeAcaoPreferencialAntesAprovacao),
                       qtd.pref.shares.after = as.numeric(x$QuantidadeAcaoPreferencialDepoisAprovacao),
                       stringsAsFactors = FALSE)


  return(df.out)

}

#' Reads XML data for repurchases
#'
#' @param x A list with data
#'
#' @return A dataframe
#' @export
#'
#' @examples
#'
#' # No example (INTERNAL)
xml.fct.repurchases <- function(x) {

  info.stock = x$HistoricosPlanosRecompraClasseEspecieAcao$HistoricoPlanoRecompraClasseEspecieAcao

  df.out <- data.frame(date.decision = as.Date(x$DataDeliberacao),
                       date.start.repurchase = as.Date(x$DataInicialPeriodoRecompraAprovado),
                       date.end.repurchase = as.Date(x$DataFinalPeriodoRecompraAprovado),
                       available.capital.repurchase = as.numeric(x$ReservaLucrosParaRecompraAprovado),
                       type.stock = info.stock$DescricaoEspecieAcaoAdquirida,
                       qtd.stocks.repurchased = as.numeric(info.stock$QuantidadeAcoesAdquirida),
                       qtd.stocks.predicted = as.numeric(info.stock$QuantidadeAcoesPrevista),
                       average.price = as.numeric(info.stock$PrecoMedioPonderadoAdquirida),
                       percent.stock.float.purchased = as.numeric(info.stock$PercentualAcoesCirculacaoAdquirida),
                       percent.stock.float.predicted = info.stock$PercentualAcoesCirculacaoPrevista,
                       stringsAsFactors = FALSE)

  return(df.out)

}

#' Reads XML data for debt
#'
#' @param x A list with data
#'
#' @return A dataframe
#' @export
#'
#' @examples
#'
#' # No example (INTERNAL)
xml.fct.debt <- function(x) {

  df.out <- data.frame(type.debt = fix.fct(x$CodigoTipoObrigacao$DescricaoOpcaoDominio),
                       type.debt.guarantee = fix.fct(x$CodigoTipoGarantia$DescricaoOpcaoDominio),
                       debt.value.under.1.year = fix.fct(as.numeric(x$ValorDividaInferiorAUmAno)),
                       debt.value.1.to.3.years = as.numeric(x$ValorDividaUmATresAnos),
                       debt.value.3.to.5.years = as.numeric(x$ValorDividaTresACincoAnos),
                       debt.value.more.5.years = as.numeric(x$ValorDividaSuperiorACincoAnos),
                       stringsAsFactors = FALSE)

  df.out$debt.total <- with(df.out, debt.value.under.1.year +
                              debt.value.1.to.3.years +
                              debt.value.3.to.5.years +
                              debt.value.more.5.years)

  return(df.out)

}

#' Reads XML data for capital reduction data
#'
#' @param x A list with data
#'
#' @return A dataframe
#' @export
#'
#' @examples
#'
#' # No example (INTERNAL)
xml.fct.capital.reduction <- function(x) {

  if (is.null(x)) return(data.frame())

  df.out <- data.frame(date.deliberation = as.Date(fix.fct(x$DataDeliberacao)),
                       date.capital.reduction = as.Date(fix.fct(x$DataReducaoCapital)),
                       total.value.reduction = as.numeric(fix.fct(x$ValorTotalReducaoCapital)),
                       qtd.ordinary.shares = as.numeric(fix.fct(x$QuantidadeAcaoOrdinaria)),
                       qtd.preferred.shares = as.numeric(fix.fct(x$QuantidadeAcaoPreferencial)),
                       qtd.shares = as.numeric(fix.fct(x$QuantidadeTotalAcao)),
                       value.per.stock = as.numeric(fix.fct(x$ValorRestituidoPorAcao)),
                       description.restitution = fix.fct(x$DescricaoFormaRestituicao),
                       reason.restitution = fix.fct(x$RazaoParaReducao),
                       type.action = fix.fct(x$TipoAcaoRealizada),
                       stringsAsFactors = FALSE)



  return(df.out)
}

#' Reads XML data for commitee composition
#'
#' @param x A list with data
#'
#' @return A dataframe
#' @export
#'
#' @examples
#'
#' # No example (INTERNAL)
xml.fct.committee.composition <- function(x) {

  df.out <- data.frame(person.name = fix.fct(x$PessoaMembro$NomePessoa),
                       person.cpf = fix.fct(x$PessoaMembro$IdentificacaoPessoa, 'numeric'),
                       person.profession = fix.fct(x$DescricaoProfissao),
                       person.cv = fix.fct(x$ExperienciaProfissional),
                       person.dob = fix.fct(x$DataNascimento, 'date'),
                       code.type.committee = fix.fct(x$CodTipoComite),
                       desc.type.committee = switch(fix.fct(x$CodTipoComite),
                                                '1' = 'Auditing Committee',
                                                '2' = 'Risk Committee',
                                                '3' = 'Financial Committee',
                                                '4' = 'Remuneration Committee',
                                                '9' = 'Other Committee',
                                                NA),
                       code.type.job = fix.fct(x$CodTipoCargo),
                       desc.committee = fix.fct(x$DescricaoOutroComite),
                       desc.job = fix.fct(x$DescricaoOutroCargo),
                       date.election = fix.fct(as.Date(x$DataEleicao)),
                       date.effective = fix.fct(as.Date(x$DataPosse)),
                       mandate.duration = fix.fct(x$PrazoMandato),
                       qtd.consecutive.mandates = as.numeric(fix.fct(x$QteMandatosConsecutivos)),
                       percentage.participation = as.numeric(fix.fct(x$PercParticipacaoReunioes)),
                       other.committes = fix.fct(x$DescricaoOutroComite),
                       stringsAsFactors = FALSE)

  return(df.out)

}

#' Reads XML data for board composition
#'
#' @param x A list with data
#'
#' @return A dataframe
#' @export
#'
#' @examples
#'
#' # No example (INTERNAL)
xml.fct.board.composition <- function(x) {

  df.out <- data.frame(person.name = fix.fct(x$PessoaMembro$NomePessoa),
                       person.cpf = fix.fct(x$PessoaMembro$IdentificacaoPessoa, 'numeric'),
                       person.profession = fix.fct(x$DescricaoProfissao),
                       person.cv = fix.fct(x$DescricaoCv),
                       person.dob = fix.fct(x$DataNascimento, 'date'),
                       code.type.board = fix.fct(x$CodTipoOrgaoAdministracao),
                       desc.type.board = switch(fix.fct(x$CodTipoOrgaoAdministracao),
                                                '1' = 'Director',
                                                '2' = 'Management Council',
                                                '3' = 'Counselor',
                                                '4' = 'Fiscal Council',
                                                NA),
                       desc.type.board2 = fix.fct(x$DescricaoCargoFuncaoExercida),
                       code.type.job = fix.fct(x$CodTipoOrgaoFuncaoExercida),
                       desc.job = fix.fct(x$DescricaoOutroCargoFuncaoExercida),
                       date.election = fix.fct(as.Date(x$DataEleicao)),
                       date.effective = fix.fct(as.Date(x$DataPosse)),
                       mandate.duration = fix.fct(x$PrazoMandato),
                       ellected.by.controller = switch(fix.fct(x$CodEleitoPeloControlador),
                                                       '0' = NA,
                                                       '1' = TRUE,
                                                       '2' = FALSE),
                       qtd.consecutive.mandates = as.numeric(fix.fct(x$QteMandatosConsecutivos)),
                       percentage.participation = as.numeric(fix.fct(x$PercParticipacaoReunioes)),
                       stringsAsFactors = FALSE)

  return(df.out)

}

#' Reads XML data for family relations
#'
#' @param x A list with data
#'
#' @return A dataframe
#' @export
#'
#' @examples
#'
#' # No example (INTERNAL)
xml.fct.family.relations <- function(x) {

  df.out <- data.frame(person.name = fix.fct(x$PessoaAdministrador$NomePessoa),
                       person.cpf = as.numeric(fix.fct(x$PessoaAdministrador$IdentificacaoPessoa)),
                       person.job = fix.fct(x$FuncaoAdministrador),
                       related.person.name = fix.fct(x$PessoaRelacaoConjugal$NomePessoa),
                       related.person.cpf = as.numeric(fix.fct(x$PessoaRelacaoConjugal$IdentificacaoPessoa)),
                       related.person.job = fix.fct(x$FuncaoRelacaoConjugal),
                       code.relationship = fix.fct(x$RelacaoParentesco),
                       desc.relationship = fix.fct(x$DescRelacaoParentesco),
                       stringsAsFactors = FALSE)

  return(df.out)

}

#' Reads XML data for family related parts
#'
#' @param x A list with data
#'
#' @return A dataframe
#' @export
#'
#' @examples
#'
#' # No example (INTERNAL)
xml.fct.family.related.parts <- function(x) {

  df.out <- data.frame(person.name = fix.fct(x$AdministradorCadastroPessoa$NomePessoa),
                       person.cpf = fix.fct(x$AdministradorCadastroPessoa$IdentificacaoPessoa, 'numeric'),
                       person.job = fix.fct(x$DescricaoCargoFuncaoAdministrador),
                       type.related.person = fix.fct(x$DescricaoSubordinacao),
                       type.relationship = fix.fct(x$DescricaoTipoRelacao),
                       observations = fix.fct(x$DescricaoObservacao),
                       related.company.name = fix.fct(x$PessoaSubordinadaAdministradorCadastroPessoa$NomePessoa),
                       related.company.cnpj = fix.fct(x$PessoaSubordinadaAdministradorCadastroPessoa$IdentificacaoPessoa, 'numeric'),
                       related.company.job = fix.fct(x$DescricaoCargoFuncaoPessoaSubordinada),
                       stringsAsFactors = FALSE)

  return(df.out)

}

#' Reads XML data for auditing
#'
#' @param x A list with data
#'
#' @return A dataframe
#' @export
#'
#' @examples
#'
#' # No example (INTERNAL)
xml.fct.auditing <- function(x) {

  df.out <- data.frame(auditor.name = fix.fct(x$PessoaAuditor$NomePessoa),
                       auditor.cnpj = fix.fct(x$PessoaAuditor$IdentificacaoPessoa),
                       contract.first.date = fix.fct(x$DataInicioContratacaoAuditorServico, 'date'),
                       contract.last.date = fix.fct(x$DataFimContratacaoAuditorServico, 'date'),
                       description.contract = fix.fct(x$DescricaoServicoContratado),
                       compensation = fix.fct(x$HonorariosServicosPresta),
                       justification.substitution = fix.fct(x$JustificativaSubstituicao),
                       reason.discordance = fix.fct(x$RazaoApresentadaAuditorDiscordancia),
                       stringsAsFactors = FALSE)

  return(df.out)

}

#' Reads XML data for responsibles documents
#'
#' @param x A list with data
#'
#' @return A dataframe
#' @export
#'
#' @examples
#'
#' # No example (INTERNAL)
xml.fct.responsible <- function(x) {


  df.out <- data.frame(person.cod = fix.fct(x$CodResponsavel),
                       person.name = fix.fct(x$NomeResponsavel),
                       person.job = fix.fct(x$DescricaoCargo),
                       stringsAsFactors = FALSE )

  return(df.out)

}

#' Reads XML data for stock details
#'
#' @param x A list with data
#'
#' @return A dataframe
#' @export
#'
#' @examples
#'
#' # No example (INTERNAL)
xml.fct.stocks.details <- function(x) {

  df.out <- data.frame(type.stock.id = fix.fct(x$CodigoEspecieAcao),
                       type.stock.text = fix.fct(x$DescricaoEspecieAcao),
                       tag.along = fix.fct(x$PercentualTagAlong, type.info = 'numeric'),
                       preferential.code = fix.fct(x$CodigoClasseAcaoPreferencial),
                       preferential.text = fix.fct(x$DescricaoClasseAcaoPreferencial),
                       dividend.text = fix.fct(x$TextoDireitoDividendo),
                       flag.voting.rights = fix.fct(x$IndicadorDireitoVoto),
                       flag.voting.text = fix.fct(x$DescricaoIndicadorDireitoVoto),
                       flag.conversibility = fix.fct(x$DescricaoIndicadorConversibilidade),
                       other.info.text = fix.fct(x$DescricaoOutraCaracteristicaRelevante),
                       stringsAsFactors = FALSE )

  return(df.out)

}

#' Reads XML data for div details
#'
#' @param x A list with data
#'
#' @return A dataframe
#' @export
#'
#' @examples
#'
#' # No example (INTERNAL)
xml.fct.div.details <- function(x) {

  df.out <- data.frame(net.profit = fix.fct(x$LucroLiquido, type.info = 'numeric'),
                       distributed.dividend = fix.fct(x$DividendoDistribuido, type.info = 'numeric'),
                       retained.profit = fix.fct(x$LucroLiquidoRetido, type.info = 'numeric'),
                       payout = fix.fct(x$PercentualDividendoDistribuido, type.info = 'numeric'),
                       div.yeild.on.equity = fix.fct(x$TaxaRetorno, type.info = 'numeric'),
                       stringsAsFactors = FALSE )

  return(df.out)

}

#' Reads XML data for patents details
#'
#' @param x A list with data
#'
#' @return A dataframe
#' @export
#'
#' @examples
#'
#' # No example (INTERNAL)
xml.fct.intangible.details <- function(x) {

  df.out <- data.frame(id = fix.fct(x$Id, type.info = 'numeric'),
                       id.type = fix.fct(x$CodigoTipo, 'numeric'),
                       patent.desc = fix.fct(x$Ativo, 'character'),
                       duration = fix.fct(x$Duracao, 'date', '%d/%m/%Y'),
                       stringsAsFactors = FALSE )

  return(df.out)

}
