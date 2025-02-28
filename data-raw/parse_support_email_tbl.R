normalize_str <- function(.str){
  .str |> 
    stringi::stri_replace_all_charclass("[[:cntrl:]]", " ") |> 
    stringi::stri_replace_all_regex("((?<=^) +)|( +(?=$))", "") |> 
    stringi::stri_replace_all_regex(" +", " ")
}

support_tbl_raw_dir <- here::here("data-raw", "input", "omq_public_email_data")

cat_tbl <- 
  xml2::read_xml(
    fs::path(support_tbl_raw_dir, "omq_public_categories.xml")
  ) |> 
  xml2::xml_find_all("//categories/categoryGroup/category") |> 
  xml2::as_list() |> 
  purrr::map_dfr(\(.node){
    tibble::tibble(
      cat_id = attr(.node, "id", exact=TRUE),
      cat_label = purrr::list_c(.node, ptype=character())
    )
  }) |> 
  dplyr::mutate(
    cat_id = as.integer(cat_id),
    cat_label = forcats::fct_reorder(factor(normalize_str(cat_label)), cat_id)
  )

support_mail_tbl <- 
  xml2::read_xml(
    fs::path(support_tbl_raw_dir, "omq_public_interactions.xml")
  ) |>
  xml2::xml_find_all("//interactions/interaction") |>
  xml2::as_list() |>
  purrr::map_dfr(\(.node){
    .meta_tbl <-
      .node |>
      purrr::pluck("metadata") |>
      (\(..x){..x[lengths(..x) > 0]})() |>
      purrr::map(unlist) |>
      tibble::as_tibble_row()
    .chunk_tbl <-
      .node |>
      purrr::pluck("text") |>
      purrr::map_dfr(\(..x){
        tibble::tibble(
          sen_str=as.character(..x[[1]]), 
          sen_cat_id=attr(..x, "goldCategory")
        )
      }) |>
      dplyr::filter(
        stringi::stri_detect_charclass(sen_str, "[^[:cntrl:]]")
      ) |>
      tibble::rowid_to_column("doc_sen_id")
    dplyr::bind_cols(.meta_tbl, .chunk_tbl)
  }) |> 
  dplyr::mutate(
    doc_id = as.integer(id),
    doc_date = lubridate::ymd(date),
    doc_sen_id = doc_sen_id,
    sen_str = normalize_str(sen_str),
    sen_cat_tbl = purrr::map(
      .x=stringi::stri_split_fixed(sen_cat_id, ","),
      .f=function(.x){
        dplyr::left_join(
          tibble::tibble(sen_cat_id = as.integer(.x)),
          dplyr::rename_with(cat_tbl, \(.n){stringi::stri_c("sen_", .n)}),
          by=dplyr::join_by(sen_cat_id)
        )
      }
    ),
    .keep="none"
  ) |> 
  dplyr::relocate(doc_id, doc_date, doc_sen_id, sen_str, sen_cat_tbl)

usethis::use_data(support_mail_tbl, overwrite=TRUE)
