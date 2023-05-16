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

extract_slides <- function(file, slides, out = NULL){
    basefile <- tools::file_path_sans_ext(basename(file))
    file_sans_ext <- tools::file_path_sans_ext(file)
    if(is.null(out)){
        out <- sprintf("%s_updating.pdf", file_sans_ext)
    }
    msg <- sprintf("Extracted from %s, from slide %s to slide %s!",
                   cli::col_blue(basefile), 
                   cli::col_green(slides[1]), 
                   cli::col_green(slides[length(slides)]))
    cli::cli_alert_success(msg)
    pdf <- qpdf::pdf_subset(file, pages = slides, output = out)
}

update_script_date <- function(file){
    script <- readLines(file)
    where_date <- grepl("%% DATE %%", script)
    script[where_date] <- gsub("%% DATE %%", Sys.Date(), date[where_date])
    writeLines(script, file)
}

purl_here <- function(file, output = NULL){
    if(is.null(output)){
        output <- gsub("Rmd", "R", file)
        knitr::purl(file, output = output, documentation = 2) 
    }
}

compile_and_purl <- function(file){
    rmarkdown::render(file, quiet = TRUE)
    purl_here(file)
}

rmat <- function(r, J){
    r + diag(1 - r, nrow = J)
}

round_cut <- function(x, min, max){
    x <- round(x)
    x <- ifelse(x < min, min, x)
    x <- ifelse(x > max, max, x)
    x
}

center <- function(data, append = TRUE){
    nums <- sapply(data, is.numeric)
    centered <- lapply(data[, nums], function(x) x - mean(x))
    names(centered) <- paste0(names(data)[nums], "0")
    cbind(data, centered)
}

print_beta <- function(fit, b, onlyb = FALSE){
    ss <- data.frame(summary(fit)$coefficients)
    ss$ps <- ifelse(ss$Pr...z.. < 0.001, "< 0.001", round(ss$Pr...z.., 3))
    id <- which(rownames(ss) == b)
    if(!onlyb){
        bs <- sprintf("$\\beta_{%s} = %.3f$, $SE = %.3f$, $z = %.3f$, $p = %s$",
                      rownames(ss), ss$Estimate, ss$Std..Error, ss$Std..Error, ss$ps)   
    }else{
        bs <- sprintf("$\\beta_{%s} = %.3f$", rownames(ss), ss$Estimate)
    }
    bs[id]
}
