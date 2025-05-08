library(dplyr)

if (!file.exists("data-raw/bog.txt")) {
  pige_først_p <- .5
  drenge <- c("Bo", "Ib", "Kim", "Ole")
  drenge_p <- c(.25, .25, .25, .25)
  piger <- c("Anne", "Eva", "Ida", "Mia")
  piger_p <- c(.25, .25, .25, .25)
  og_p <- .5
  en_dyr <- c("ko", "hest")
  en_dyr_p <- c(.5, .5)
  et_dyr <- c("marsvin", "æsel")
  et_dyr_p <- c(.5, .5)
  en_ting <- c("bil", "cykel")
  en_ting_p <- c(.5, .5)
  et_ting <- c("hus", "skur")
  et_ting_p <- c(.5, .5)
  en_farve <- c("blå", "grøn")
  en_farve_p <- c(.5, .5)
  et_farve <- c("blåt", "grønt")
  et_farve_p <- c(.5, .5)
  verber <- c("ser", "får", "har")
  verber_p <- c(1/3, 1/3, 1/3)

  generator <- function(){
    # Sample dreng og pige
    person <- c(sample(drenge, 1, prob = drenge_p), "og", sample(piger, 1, prob = piger_p))
    # Pige først?
    if(runif(1)<pige_først_p){
      person <- rev(person)
    }
    # Kun en person?
    if(runif(1)>og_p){
      person <- person[1]
    }
    # Person(er) som enkelt tekststreng
    person <- paste(person, collapse = " ")
    # Sample verbum
    output <- paste(person, sample(verber, 1, prob = verber_p))
    # En eller et?
    en_et <- ifelse(runif(1)>.5, "en", "et")
    dyr_ting <- ifelse(runif(1)>.5, "dyr", "ting")
    output <- paste(output, en_et)
    # Sample resten
    if(en_et=="en"){
      if(p_adj <- runif(1)>.5){
        output <- paste(output, sample(en_farve, 1, prob = en_farve_p))
      }
      if(dyr_ting=="dyr"){
        output <- paste(output, sample(en_dyr, 1, prob = en_dyr_p))
      } else {
        output <- paste(output, sample(en_ting, 1, prob = en_ting_p))
      }
    } else {
      if(p_adj <- runif(1)>.5){
        output <- paste(output, sample(et_farve, 1, prob = et_farve_p))
      }
      if(dyr_ting=="dyr"){
        output <- paste(output, sample(et_dyr, 1, prob = et_dyr_p))
      } else {
        output <- paste(output, sample(et_ting, 1, prob = et_ting_p))
      }
    }
    return(output)
  }

  set.seed(42)
  bog <- unique(replicate(2000, generator()))

  writeLines(bog, file.path("data-raw", "bog.txt"), sep = ".\n")
}

bog <- readLines(file.path("data-raw", "bog.txt"))
bog <- paste(bog, collapse = " ")
bog <- gsub(".", " .", bog, fixed = TRUE)
bog <- strsplit(bog, " ")[[1]]

usethis::use_data(bog, compress = "xz", overwrite = T)
