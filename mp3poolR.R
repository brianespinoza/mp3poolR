## Brian Espinoza
## briane@uci.edu

mp3poolR <- function(username, pw, start = 0, end = 1, path = "", ask = TRUE, QuickHitter = FALSE){

  ### username    = String. Username
  ### pw          = String. Password
  ### start       = Integer. Starting page to scrape
  ### end         = Integer. Last page to scrape
  ### path        = String. Path to download folder. Default = "~/Desktop/mp3pool_downloads/"
  ### ask         = Boolean. If TRUE, will be asked if you want to download the song in queue
  ### QuickHitter = Boolean. If TRUE, will remove QuickHitter tracks from queue.

  if(path == "~/Desktop/"){
    stop("choose another folder")
  }

  # load libraries, suppress loading messages
  library(httr, quietly = TRUE)
  library(rvest, quietly = TRUE)
  library(dplyr, quietly = TRUE)
  library(stringr, quietly = TRUE)
  library(curl, quietly = TRUE)


  # initiate session and log in
  link <- "http://mp3poolonline.com/user/login"
  music_session <- html_session(link)

  # function to find login form and submit credentials
  login <- function(){
    login_form <- link %>%
      read_html() %>%
      html_form()
    login_form <- login_form[[2]]

    login <- login_form %>% set_values(name = username, pass = pw)
    auth <- submit_form(music_session, form = login)
  }

  # function to be called if credentials are incorrect
  wrong_login <- function(){
    message("Incorrect username/password")
    username <<- readline("username: ")
    pw <<- readline("password: ")
    login()
  }


  auth <- login() # first login
  cookie.value <- auth$response$cookies$value
  while (auth$url != "http://mp3poolonline.com/viewnewreleases") auth <- wrong_login()


  # only search for 4 & 5 star songs
  musicfilter_form <- read_html(auth) %>% html_form()
  musicfilter_form <- musicfilter_form[[2]] %>% set_values(ratingvalue = "3+")

  music_session <- submit_form(auth,
                                form = musicfilter_form,
                                submit = "search")

  # create download folder unless already made
  if (path == ""){
    download_folder <- paste0("~/Desktop/mp3pool_downloads/", Sys.Date(),"/")
    log_file <- "~/Desktop/mp3pool_downloads/download_log.txt"
    trash_file <- "~/Desktop/mp3pool_downloads/trash_log.txt"
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
    trash_file <- paste0(path, "download_log.txt")
  }
  if (!dir.exists(download_folder)){
    dir.create(path = download_folder)
  }

  # this is where download links, titles, and bpm info will go
  download_list <- tbl_df(data.frame(music_links = NULL, titles = NULL, bpm = NULL))

  # scrape links, bpm, and generate titles from links
  make_links_and_titles <- function(session, df){

    li <- read_html(music_session) %>% html_nodes("div .innerPlayer1") %>%
      html_nodes(".innerPlayList1") %>% html_nodes(".play_listing") %>% html_nodes("li")

    message("Getting Links...")
    ml <- li %>% html_nodes("audio") %>% html_attr("id") %>% str_split_fixed(pattern = "_", 2) %>%
      .[,2] %>% str_c("http://mp3poolonline.com/music/download/", .)

    ml2.titles <- li %>% html_nodes("audio") %>% html_attr("src")

    message("Getting BPM...")
    bpm2 <- li %>% html_nodes("div .bpm") %>% html_text()

    music_links <- c(df$music_links, ml)
    bpm <- c(df$bpm, bpm2)


    titles <- c(df$titles, (ml2.titles %>% str_split("/") %>% sapply(`[`,7) %>%
      str_split("%20") %>% sapply(paste, collapse = " ")))

    download_list <- tbl_df(data_frame(music_links, titles, bpm))


  }
  # scraping and pagination
  if (round(start, 0) == 0){
    start <- round(start, 0) + 1
    download_list <- make_links_and_titles(music_session, download_list)
  }

  end <- round(end, 0)
  if (end > 0){
    for (j in start:end){
      message(str_c("Scraping Page ", ifelse(start == 0, j+1, j), "..."))
      music_session <- music_session$url %>% str_replace("viewnewrelease",
                                        str_c("viewnewrelease?page=", j)) %>%
        jump_to(music_session, .)
      download_list <- make_links_and_titles(music_session, download_list)
    }
  }

  ## Prepares the download log
  if (!file.exists(log_file)){
    write(x = NULL, file = log_file, append = FALSE)
  }
  if (!file.exists(trash_file)){
    write(x = NULL, file = trash_file, append = FALSE)
  }
  logged_tracks <- readLines(log_file)
  trashed_tracks <- readLines(trash_file)


  download_list <- download_list[(as.integer(which(sapply(X = download_list$titles, FUN = '%in%', logged_tracks) == FALSE))),]
  download_list <- download_list[(as.integer(which(sapply(X = download_list$titles, FUN = '%in%', trashed_tracks) == FALSE))),]

  if (QuickHitter == FALSE){ ## If false, don't download songs with QuickHitter in the title
    download_list <- download_list[!str_detect(download_list$titles, pattern = "QuickHitter"), ]
    download_list <- download_list[!str_detect(download_list$titles, pattern = "Quickhitter"), ]
  }

  if (length(download_list$titles) == 0){
    return(message("no new songs to download"))
  }

  # takes simple user input
  readkey <- function(){
    line <- readline("Download? (yes/no) ... quit, skip \n")
    decision <- tolower(line[1])
    if (decision == "y" | decision == "n" | decision == "q" | decision == "s"){
      return(decision)
    } else readkey()
  }

  # download_list[,2]
  for (i in 1:length(download_list$titles)){ # download songs and write to designated file
    artist <- download_list$titles[i] %>% str_split_fixed(pattern = " - ", 2)
    song <- artist[,2]
    artist <- artist[,1]
    cat(paste0("[", i,"/", length(download_list$titles),"]"),
        paste0("Artist: ", artist),
        paste0("Title: ", song),
        paste0("BPM: ", download_list$bpm[i], "\n"), sep = "\n")
    # cat(logged_tracks[str_detect(logged_tracks, pattern = artist)], "\n")
    if (ask == TRUE){
      decision <- readkey()
      if (decision == "y"){
        GET(url = download_list$music_links[i],
            write_disk(paste0(download_folder,
                              download_list$titles[i]), overwrite = T),
            progress(),
            user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_4) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/50.0.2661.102 Safari/537.36"),
            authenticate(user = username, password = pw),
            set_cookies(.cookies = c(SESS708898d206bb1ac5c9ea06d86d34bd8c = cookie.value)))
        write(download_list$titles[i], file = log_file, append = TRUE)
      } else if (decision == "n"){
        write(download_list$titles[i], file = trash_file, append = TRUE)
      }else if (decision == "s"){
        next
      }else if (decision == "q"){
        cat("goodbye")
        break
      }
    } else{ # ask == FALSE
      GET(url = download_list$music_links[i],
          write_disk(paste0(download_folder,
                            download_list$titles[i]), overwrite = T),
          progress(),
          user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_4) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/50.0.2661.102 Safari/537.36"),
          authenticate(user = username, password = pw),
          set_cookies(.cookies = c(SESS708898d206bb1ac5c9ea06d86d34bd8c = cookie.value)))
      write(download_list$titles[i], file = log_file, append = TRUE)
    }
    cat("", "", sep = "\n") # space post-user input
  }
}
