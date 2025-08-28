sword_vec <- quanteda::stopwords(language = "de", source = "nltk")

usethis::use_data(sword_vec, overwrite = TRUE)