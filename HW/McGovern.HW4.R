simuLinQuadDat <- function(inpNobs=100, inpNlinVars=5, inpYidx=1:2, inpSDerr=0.5) {
  # Nobs x Nvars matrix of linear terms:
  xTmp <- matrix(rnorm(inpNobs*inpNlinVars),ncol=inpNlinVars)
  # Make all pairwise products of linear terms,
  # X1*X1, X1*X2, X1*X3, ..., Xn*Xn:
  x2Tmp <- NULL
  tmpCnms <- NULL
  # for each linear term:
  for ( iTmp in 1:dim(xTmp)[2] ) {
    # multiply it by itself and all other terms,
    # excluding already generated pairwise combinations: 
    for ( jTmp in iTmp:dim(xTmp)[2] ) {
      x2Tmp <- cbind(x2Tmp,xTmp[,iTmp]*xTmp[,jTmp])
      # maintain vector of column names for quadratic
      # terms along the way:
      tmpCnms <- c(tmpCnms,paste0("X",iTmp,"X",jTmp))
    }
  }
  # name attributes in the matrix of quadratic terms:
  colnames(x2Tmp) <- tmpCnms
  # create outcome as a sum of an unweighted average of 
  # specified columns and controlled amount 
  # of gaussian noise:
  yTmp <- rowMeans(cbind(xTmp,x2Tmp)[,inpYidx])+rnorm(inpNobs,sd=inpSDerr)
  # return data.frame with outcome as a first column,
  # followed by linear, then by quadratic terms:
  data.frame(Y=yTmp,xTmp,x2Tmp)
}
