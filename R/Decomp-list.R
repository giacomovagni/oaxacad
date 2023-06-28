
#' Generate Simulated Data for Oaxaca Decomposition
#' @name Decomp
#' @param results a oaxaca object
#' @param decomp either 'threefold' or 'twofold'
#' @param name a customisable output name
#' @returns A list
#' @examples
#' dff = oaxaca_data()
#' output = oaxaca(Y ~ X | group, data = dff$dataframe)
#' Decomp(output)
#' Decomp(output, decomp = 'twofold')
#' @export


Decomp = function(results, name = 'Decomposition', decomp = 'threefold'){

  message('Omega = 1')
  message('Group 2 is reference')

  if(decomp == 'twofold'){

    x = results$twofold$overall; x = as.data.frame(x); x$var = row.names(x)

    Compositional = x[1, c('coef(explained)')] %>% round(3)
    Unexplained = x[1, c( 'coef(unexplained)')] %>% round(3)

    #
    GAP = results$y$y.diff %>% round(3)
    Group1 = results$y$y.A %>% round(3)
    Group2 = results$y$y.B %>% round(3)

    . = ''
    Periods = rbind(Group2, Group1)

    `% explained` = round(Compositional / GAP, 3) * 100
    `% unexplained` = round(Unexplained / GAP, 3) * 100

    Decomp = rbind(Group1, Group2, GAP, ., Compositional, Unexplained, `% explained`, `% unexplained`)
    Decomp

    colnames(Decomp) = 'Decomposition'
    OaxDec = as.data.frame(Decomp)

    return(OaxDec)
  }

  if(decomp == 'threefold'){
    x = results$threefold$overall; x = as.data.frame(x); x$var = row.names(x)
    Compositional = x$x[x$var == 'coef(endowments)'] %>% round(4)
    Unexplained = x$x[x$var == 'coef(coefficients)']  %>% round(4)
    Interaction = x$x[x$var == 'coef(interaction)']   %>% round(4)
    GAP = results$y$y.diff %>% round(4)
    Group1 = results$y$y.A %>% round(4)
    Group2 = results$y$y.B %>% round(4)

    . = ''
    Periods = rbind(Group1, Group2)

    Decomp = rbind(GAP, ., Compositional, Unexplained, Interaction);

    OaxDec = rbind(Periods, Decomp)
    colnames(OaxDec) = 'Decomposition'

    rownames(OaxDec) = c("Group 1 Y", "Group 2 Y", "GAP = Y1-Y2", "", "Compositional", 'Coefficient', "Interaction")
    OaxDec = as.data.frame(OaxDec)

    # interpretation
    #message('If people in Group 1, looked liked people in Group 0 regarding measured characteristics (compositional), they WOULD have spend ', Unexplained, ' more in total,
    #        yet the change in characteristics decreases by ', CompositionalChange, ' yielding an increase in total time of ', TotalChange)

    message("If people in Group 2 had the same measured characteristics as those in Group 1, we WOULD have observed ", Compositional,
        if(Compositional) sign = " less " else sign = " more ", "of Y. \n The explained part of the gap is ", round(Compositional/GAP,2), "%")

    #
    CI_composition = as.data.frame( cbind(results$threefold$variables[,1] - results$threefold$variables[,2]*1.96, results$threefold$variables[,1] + results$threefold$variables[,2]*1.96) )
    CI_unexplained = as.data.frame( cbind(results$threefold$variables[,3] - results$threefold$variables[,4]*1.96, results$threefold$variables[,3] + results$threefold$variables[,4]*1.96) )

    tvalue_composition = results$threefold$variables[,1] / results$threefold$variables[,2]
    pvalues_composition = 2 * pnorm( abs(tvalue_composition), lower.tail = F)

    tvalue_unexplained = results$threefold$variables[,3] / results$threefold$variables[,4]
    pvalues_unexplained = 2 * pnorm( abs(tvalue_unexplained), lower.tail = F)

    #
    names(CI_composition) = c('conf.low', 'conf.high')
    names(CI_unexplained) = c('conf.low', 'conf.high')

    # Compositional #
    brows = bind_rows(results$x) %>% as.data.frame() %>% t
    rownames(brows) = names(results$x$x.mean.A)

    brows = cbind(brows, CI_composition)
    brows = apply(brows, 2, function(x) round(x, 3))
    brows = as.data.frame(brows)
    # p-value #
    brows$pvalue = round(pvalues_composition, 5)
    # colnames #
    colnames(brows) = c('Composition_Group1', 'Composition_Group2', 'diff', 'conf.low', 'conf.high', 'pvalue')
    brows = brows[, c('Composition_Group1', 'Composition_Group2', 'diff', 'pvalue')]

    # Coefficients #
    betas = cbind( round(results$beta[[1]], 3), round(results$beta[[2]], 3), round(results$beta[[3]],3) ) %>% as.data.frame()
    #
    betas = cbind(betas, CI_unexplained)
    betas = apply(betas, 2, function(x) round(x, 3))
    betas = as.data.frame(betas)
    # p-value #
    betas$pvalue = round(pvalues_unexplained, 5)
    # colnames #
    colnames(betas) = c('Coeff_Group1', 'Coeff_Group2', 'diff', 'conf.low', 'conf.high', 'pvalue')
    #

    betas = betas[, c('Coeff_Group1', 'Coeff_Group2', 'diff', 'pvalue')]

    # Raw differential (R)
    R = GAP

    # Explained #
    E = Compositional # explained
    D = 0 # group
    CE = Interaction # interaction

    # Explained
    V = E+D*CE

    # Unexplained (U){C+(1-D)CE}:
    C = Unexplained # explained
    U = C + (1-D) * CE

    # perc_explained = abs(V) / R
    (V) / R
    #
    perc_explained = (V / R) * 100
    #
    perc_explained = data.frame(perc_explained = round(perc_explained, 3))

    # three fold coef #
    threefold_coef = round(results$threefold$variables, 2)

    # var #
    threefold_var = results$threefold$overall
    #

    p1 = stars.pval( pnorm(abs(threefold_var[1] / threefold_var[2]), lower.tail = F) )
    p2 = stars.pval( pnorm(abs(threefold_var[3] / threefold_var[4]), lower.tail = F) )
    p3 = stars.pval( pnorm(abs(threefold_var[5] / threefold_var[6]), lower.tail = F) )

    dff = data.frame(endowment = round(threefold_var[1], 3) , pval_endow = p1, coefficients = round(threefold_var[3], 3), pval_coef = p2, interaction = round(threefold_var[5], 3), pval_int = p3)
    dff

    # rename #
    names(OaxDec) = name

    # list
    OaxDecList = list(threefold_coef, dff, betas, brows, OaxDec, perc_explained)
  }

  return(OaxDecList)
}
#
