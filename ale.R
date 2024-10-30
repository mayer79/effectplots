diff_ale <- function(
    object,
    v,
    X,
    mid,
    width,
    g,
    ale_n_bin = 50L,
    pred_fun = stats::predict,
    w = NULL,
    ...
) {
  J <- lapply(
    collapse::gsplit(1:length(g), g, use.g.names = TRUE),
    function(z) if (length(z) <= ale_n_bin) z else sample(z, size = ale_n_bin)
  )
  J <- J[!is.na(names(J))]

  out <- numeric(length(J))
  for (j in seq_along(out)) {
    pdj <- partial_dep(
      object,
      v = v,
      X = ss(X, J[[j]]),
      grid = mid[j] + c(-1, 1) * width[j] / 2,
      pred_fun = pred_fun,
      w = if (!is.null(w)) w[J[[j]]],
      ...
    )
    out[j] <- diff(pdj)
  }
  out
}
