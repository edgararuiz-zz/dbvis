#' @export
apply_props.tbl_dbi <- function(data, props) {
  cols <- lapply(props, prop_value, data = data)
  colnames(cols) <- vapply(props, prop_label, character(1))
  quickdf(cols)
}

#' @export
prop_type.tbl_dbi <- function(data, prop) {

  if("tbl_dbi" %in% class(data))
  {
    suppressMessages({
      top_rows <- dplyr::mutate_(data, "new_field" = prop$value)
      top_rows <- dplyr::select(top_rows, new_field)
      top_rows <- dbplyr:::head.tbl_lazy(top_rows, 10)
      top_rows <- dplyr::collect(top_rows, 10)
    })
    s <- ggvis::vector_type(top_rows[[1]])
    return(s)

  }else
  {ggvis::vector_type(ggvis:::prop_value(prop, data))}
}


#' @export
eval_vector.tbl_dbi <- function(x, f) {
  suppressMessages({
    top_rows <- dplyr::mutate_(x, "new_field" = f)
    top_rows <- dplyr::select(top_rows,new_field)
    top_rows <- dbplyr:::head.tbl_lazy(top_rows, 10)
    top_rows <- dplyr::collect(top_rows)
  })
  return(top_rows[[1]])
}


#' @export
preserve_constants.tbl_dbi  <- function(input, output) {
  if("tbl_dbi" %in% class(input))
  {output}else
  {ggvis:::preserve_constants.data.frame(input, output)}
}

#' @export
compute_count.tbl_dbi <- function(x, x_var, w_var = NULL) {

  #x_field <- as.character(x_var)[2]
  data_prep <- dplyr::mutate_(x, "x" = x_var)
  data_prep <- dplyr::filter(data_prep, !is.na(x))

  s <- dplyr::group_by(data_prep, x)

  if(is.null(w_var)){
    s <- dplyr::tally(s)
  }else{
    w <- as.character(w_var)[2]
    s <- dplyr::mutate_(s, weight = w)
    s <- dplyr::summarise(s, n = sum(weight))}



  s <- dplyr::mutate(s, count_ = n, x_ = x)
  s <- dplyr::select(s, count_, x_)
  s <- dplyr::collect(s)

  s <- as.data.frame(s,  stringsAsFactors = FALSE)


  return(s)

}
