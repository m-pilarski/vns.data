amazon_review_tbl <- 
  fs::dir_ls(
    path=here::here("data-raw/input/mteb-amazon_reviews_multi/de/"),
    glob="*/*.jsonl"
  ) |> 
  purrr::map_dfr(function(.path){
    .path |> 
      file() |> 
      jsonlite::stream_in() |> 
      dplyr::mutate(
        doc_id = id, doc_title_text = text, doc_label_num = as.integer(label),
        .keep="none"
      ) |> 
      tidyr::separate_wider_delim(
        doc_title_text, names=c("doc_title", "doc_text"), delim="\n\n", 
        too_many="merge"
      ) |> 
      tibble::as_tibble()
  })
  
usethis::use_data(amazon_review_tbl, overwrite=TRUE, compress="gzip")
