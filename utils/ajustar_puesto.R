ajustar_puesto <- function(datos_fe, formula_fe) {
  mf <- model.frame(formula_fe, datos_fe, na.action = na.fail)
  y <- model.response(mf)
  matriz_x <- model.matrix(formula_fe, mf)[, -1, drop = FALSE]
  w <- datos_fe$peso
  id <- as.integer(factor(datos_fe$puesto))
  pesos_puesto <- as.numeric(rowsum(w, id, reorder = TRUE))
  media_y <- as.numeric(rowsum(y*w, id, reorder = TRUE))/pesos_puesto
  media_x <- rowsum(matriz_x*w, id, reorder = TRUE)/pesos_puesto
  y_dentro <- y-media_y[id]
  x_dentro <- matriz_x-media_x[id, , drop = FALSE]
  fit <- lm.wfit(x_dentro, y_dentro, w)
  stopifnot(fit$rank == ncol(x_dentro))
  n <- nrow(x_dentro)
  k <- length(pesos_puesto)+fit$rank
  aportes <- rowsum(x_dentro*(w*fit$residuals), datos_fe$hogar)
  g <- nrow(aportes)
  inv <- solve(crossprod(x_dentro, x_dentro*w))
  v <- (g/(g-1))*((n-1)/(n-k))*inv%*%crossprod(aportes)%*%inv
  b <- unname(fit$coefficients["titulo"])
  se <- sqrt(v["titulo","titulo"])
  critico <- qt(.975,g-1)
  list(b=b, se=se, t=b/se, gl=g-1, residuos=fit$residuals, ajustados=y-fit$residuals,
       inf=b-critico*se, sup=b+critico*se,
       p=2*pt(-abs(b/se),g-1), n=n, hogares=g,
       r2=1-sum(w*fit$residuals^2)/sum(w*(y-weighted.mean(y,w))^2))
}
