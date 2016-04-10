# mp3poolR
Webscraper allowing you to downloaded top rated songs (4-5 stars) from mp3poolonline.com. User must already have an account. (for now)

- Scrapes mp3poolonline.com for download links then parses those links for file names.
- Updates a log to keep track of previously downloaded songs in order to avoid duplicates.
- Organizes downloads according to date.

### Arguments:
- username = string containing your account username

- pw = string containing your account password

- download = boolean, should the script download the songs? *used for bug testing*

- path = character string with a path to your download folder. default is "~/Desktop/mp3pool_downloads"

- ask = boolean, should the program ask before downloading each song?

- QuickHitter = bolean, should the program download QuickHitter versions of songs from the download queue?
  - FALSE = does not download QuickHitter versions
  - TRUE = downloads all versions

 
#### mp3poolR(username, pw, download = TRUE, path = "", ask = TRUE, QuickHitter = FALSE)
