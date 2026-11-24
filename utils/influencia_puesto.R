# Diagnostico WLS con efectos fijos absorbidos. h=1 no permite borrado de rango fijo.
influencia_puesto <- function(datos, formula) {
  mf <- model.frame(formula, datos, na.action=na.fail)
  y <- model.response(mf)
  x <- model.matrix(formula,mf)[,-1,drop=FALSE]
  w <- datos$peso
  id <- as.integer(factor(datos$puesto))
  sw <- as.numeric(rowsum(w,id,reorder=TRUE))
  yd <- y-(as.numeric(rowsum(y*w,id,reorder=TRUE))/sw)[id]
  xd <- x-(rowsum(x*w,id,reorder=TRUE)/sw)[id,,drop=FALSE]
  fit <- lm.wfit(xd,yd,w)
  stopifnot(fit$rank==ncol(xd))
  inv <- solve(crossprod(xd,xd*w))
  n <- nrow(xd);k <- length(sw)+fit$rank
  h <- w/sw[id]+w*rowSums((xd%*%inv)*xd)
  sigma2 <- sum(w*fit$residuals^2)/(n-k)
  ok <- h < 1-1e-9
  estandar <- cook <- delta_titulo <- rep(NA_real_,n)
  estandar[ok] <- sqrt(w[ok])*fit$residuals[ok]/sqrt(sigma2*(1-h[ok]))
  cook[ok] <- w[ok]*fit$residuals[ok]^2*h[ok]/(k*sigma2*(1-h[ok])^2)
  delta_titulo[ok] <- (xd[ok,,drop=FALSE]%*%inv[,"titulo"])*w[ok]*fit$residuals[ok]/(1-h[ok])
  data.frame(ajustado=y-fit$residuals,residuo=fit$residuals,
    residuo_estandar=estandar,palanca=h,cook=cook,delta_titulo=delta_titulo,
    borrado_identificable=ok)
}
