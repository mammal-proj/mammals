get_params <- function(x, env) {
  .call <- quote(substitute(x))
  .name <- eval(.call)

  .envs <- rev(x = sys.frames())
  if (!missing(env) && is.environment(env)) {
    to_enclos <- vapply(.envs, identical, env, FUN.VALUE = NA)
    if (length(to_enclos) > 1) {
    to_enclos <- seq_len(length.out = which(to_enclos))
    .envs <- .envs[to_enclos]
    }
  }

  for (i in .envs) {
    .call[[2]] <- .name
    .name <- eval(.call, i)
  }

  if (is.null(.name)) {
    return(NULL)
  }
  .name <- as.list(.name)
  if (length(.name) > 1) {
    .name <- .name[-1L]
  }

  .name <- lapply(.name, as.character)
  .name <- as.character(.name)

  return(.name)
}
