# mp3poolR
mp3poolR allows you to download top rated songs (4-5 stars) from mp3poolonline.com. However, you must already have an account. (for now)

### Uses
- Scrapes mp3poolonline.com for download links then parses those links for file names.
- Updates a log to keep track of previously downloaded songs in order to avoid duplicates.
- Organizes downloads according to date.

### Notes
mp3poolR..
- only works in interactive mode
- creates a download log and a trash log, this keeps track of downloaded songs and songs the user has declined to download. Deleting these files or songs within the files will cause the program to re-download old songs.

### Arguments:
- username = string containing your account username

- pw = string containing your account password

- start = integer of the page number to start scraping. 1 refers to the first page.

- end = integer of the page number to stop scraping. 1 refers to the first page.

- path = character string with a path to your download folder. default is "~/Desktop/mp3pool_downloads"

- ask = boolean, should the program ask before downloading each song?

- QuickHitter = bolean, should the program download QuickHitter versions of songs from the download queue?
  - FALSE = does not download QuickHitter versions
  - TRUE = downloads all versions

 
#### mp3poolR(username, pw, start = 1, end = 1, path = "", ask = TRUE, QuickHitter = FALSE)
