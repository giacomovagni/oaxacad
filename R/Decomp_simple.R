
#' Generate Simulated Data for Oaxaca Decomposition
#' @name Decomp_simple
#' @param results a oaxaca object
#' @returns A list
#' @examples
#' dff = oaxaca_data()
#' output = oaxaca(Y ~ X | group, data = dff$dataframe)
#' Decomp_simple(output)
#' @export

#
Decomp_simple = function(results){

  # Oaxaca # Omega = 0 #
  rb = results$twofold$overall[1, ]
  #

  #
  oaxaca = data.frame(explained = rb[2], se_ex = rb[3], unexplained = rb[4], se_unex = rb[5])
  #

  #
  tex = oaxaca$explained / oaxaca$se_ex
  tunex =  oaxaca$unexplained / oaxaca$se_unex

  pvalue1 = pnorm( abs(tex), lower.tail = F)
  pvalue2 = pnorm( abs(tunex), lower.tail = F)
  #

  #
  oaxaca$pvalue_ex = stars.pval(pvalue1)
  oaxaca$pvalue_unex = stars.pval(pvalue2)
  #

  oaxaca$gap = oaxaca$explained + oaxaca$unexplained
  oaxaca$perc_explained = (oaxaca$explained / oaxaca$gap)*100

  # rearrange #
  oxx = oaxaca[c(7,1,5,3,6,8)]
  #

  oxx$gap = round(oxx$gap, 4)
  oxx$explained = round(oxx$explained, 4)
  oxx$unexplained = round(oxx$unexplained, 4)

  #
  colnames(oxx) = c('gap', 'Explained', 'pvale', 'Unexplained', 'pvalu', '% explained')

  oxx$Explained = paste(oxx$Explained, oxx$pvale)
  oxx$Unexplained = paste(oxx$Unexplained, oxx$pvalu)

  oxx = oxx[, c(1,2,4,6)]

  return(oxx)
}

