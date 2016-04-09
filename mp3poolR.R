## Brian Espinoza
## briane@uci.edu

mp3poolR <- function(username, pw, path = "", download = TRUE, ask = TRUE, QuickHitter = FALSE){

  if(path == "~/Desktop/"){
    stop("choose another folder")
  }

  library(httr)
  library(rvest)
  library(dplyr)
  library(stringr)
  library(curl)

  link <- "http://mp3poolonline.com/user/login"
  music_session <- html_session(link)

  login_form <- link %>%
    read_html() %>%
    html_form()
  login_form <- login_form[[2]]

  login <- login_form %>%
    set_values(name = username, pass = pw)

  auth <- submit_form(music_session, form = login) #login submit
  musicfilter_form <- read_html(auth) %>% html_form()
  musicfilter_form <- musicfilter_form[[2]] %>% set_values(ratingvalue = "3+")

  music_session <- submit_form(auth,
                                form = musicfilter_form,
                                submit = "search")
  if (path == ""){
    download_folder <- paste0("~/Desktop/mp3pool_downloads/", Sys.Date(),"/")
    log_file <- "~/Desktop/mp3pool_downloads/download_log.txt"
  }else {
    path <-
      paste0(
        str_c(
          str_split(
            str_trim(
              str_replace_all(path, pattern = "/", replacement = " ")),
            pattern = " ")[[1]],
          collapse = "/"),"/")

    download_folder <- paste0(path,Sys.Date(),"/")
    log_file <- paste0(path,"download_log.txt")
  }
  if (!dir.exists(download_folder)){
    dir.create(path = download_folder)
  }

  download_list <- tbl_df(data.frame(music_links = NULL, titles = NULL, bpm = NULL))

  make_links_and_titles <- function(session, df){

    message("Getting Links...")
    ml <- read_html(music_session) %>% html_nodes("div .innerPlayer1") %>%
      html_nodes(".innerPlayList1") %>% html_nodes(".play_listing") %>% html_nodes("li") %>%
      html_nodes("audio") %>% html_attr("src")

    message("Getting BPM...")
    bpm2 <- read_html(music_session) %>% html_nodes("div .innerPlayer1") %>%
      html_nodes(".innerPlayList1") %>% html_nodes(".play_listing") %>% html_nodes("li") %>%
      html_nodes("div .bpm") %>% html_text()

    music_links <- c(df$music_links, (ml))
    bpm <- c(df$bpm, bpm2)


    titles <- c(df$titles, (ml %>% str_split("/") %>% sapply(`[`,7) %>%
      str_split("%20") %>% sapply(paste, collapse = " ")))

    download_list <- tbl_df(data_frame(music_links, titles, bpm))


  }

  download_list <- make_links_and_titles(music_session, download_list)
  # go to the next page
  message("Scraping Page 2...")
  music_session <- read_html(music_session) %>% html_nodes(".item-2 .active") %>%
    html_attr("href") %>% paste0("http://mp3poolonline.com",.) %>%
    jump_to(music_session,.)
  download_list <- make_links_and_titles(music_session, download_list)

  ## Prepares the download log
  if (!file.exists(log_file)){
    write(x = NULL, file = log_file, append = FALSE)
  }
  logged_tracks <- readLines(log_file)


  download_list <- download_list[(as.integer(which(sapply(X = download_list$titles, FUN = '%in%', logged_tracks) == FALSE))),]

  if (QuickHitter == FALSE){ ## If false, don't download songs with QuickHitter in the title
    download_list <- download_list[!str_detect(download_list$titles, pattern = "QuickHitter")]
  }

  if (length(download_list$titles) == 0){
    return(message("no new songs to download"))
  }

  readkey <- function()
    {
    line <- readline("Download? (y/n) \n")
    decision <- tolower(line[1])
  }
  ## Should the program download?
  if (download == TRUE){
    download_list[,2]
    for (i in 1:length(download_list$titles)){ # download songs and write to designated file
      cat(download_list$titles[i], paste0("[", i,"/", length(download_list$titles),"]"), sep = "\n")
      cat(download_list$bpm[i], "\n")
      if (ask == TRUE){
        decision <- readkey()
        if (decision == "y"){
          httr::GET(url = download_list$music_links[i], write_disk(paste0(download_folder, download_list$titles[i])), progress())
          write(download_list$titles[i], file = log_file, append = TRUE)
        } else{
          write(download_list$titles[i], file = log_file, append = TRUE)
        }
      } else{
        httr::GET(url = download_list$music_links[i], write_disk(paste0(download_folder, download_list$titles[i])), progress())
        write(download_list$titles[i], file = log_file, append = TRUE)
      }
      cat("", "", sep = "\n") # space post-user input
    }
  }
}
