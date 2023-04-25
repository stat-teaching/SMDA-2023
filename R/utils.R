download_link <- function(x, url_name = NULL, print_url = FALSE){
    xs <- gsub("https://github.com/", "", x)
    xsl <- unlist(strsplit(xs, "/"))
    file <- xsl[(which(xsl %in% c("master", "main")) + 1):length(xsl)]
    file <- paste0(file, collapse = "/")
    url <- sprintf("https://%s.github.io/%s/%s", xsl[1], xsl[2], file)
    if(print_url){
        if(!is.null(url_name)){
            url <- sprintf("[%s](%s)", url_name, url)
        }else{
            url <- sprintf("[%s](%s)", url, url)
        }
    }
    return(url)
}
