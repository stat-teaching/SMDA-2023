success <- function(msg){
    cli::cli_alert_success(msg)
}

warn <- function(msg){
    cli::cli_alert_danger(msg)
}

new_material <- function(what, name){
    if(!fs::dir_exists(here::here(what, name))){
        
        if(fs::is_dir(here::here("template", what, "skeleton"))){
            fs::dir_copy(here::here("template", what, "skeleton"),
                         here::here(what, name))
            file.rename(file.path(here::here(what, name), "skeleton.Rmd"),
                        file.path(here::here(what, name), paste0(name, ".Rmd")))
        }else{
            dir <- fs::dir_create(here::here(what, name))
            fs::file_copy(here::here("template", what, "skeleton.Rmd"),
                          here::here(dir, paste0(name, ".Rmd")))
        }
        success(paste("material", cli::col_blue(what, "/", name), "created! :)"))
    }else{
        warn(paste("material", cli::col_blue(what, "/", name), "already exist!"))
    }
}

create_index <- function(){
    slides <- list.files("slides/", 
                         pattern = ".Rmd|.pdf",
                         recursive = TRUE, 
                         full.names = TRUE)
    slides <- slides[!grepl("_files", slides)]
    
    name <- unique(tools::file_path_sans_ext(basename(slides)))
    rmd <- slides[grepl(".Rmd", slides)]
    pdf <- slides[grepl(".pdf", slides)]
    sprintf("- **%s**: [Rmd](%s), [PDF](%s)", name, rmd, pdf)
}

get_funs <- function(file){
    file <- suppressWarnings(readLines(file))
    cutpoints <- grep("<- function", file)
    cutpoints[length(cutpoints) + 1] <- length(file)
    
    out <- vector(mode = "list", length = length(cutpoints)-1)
    fun_names <- vector(mode = "character", length = length(cutpoints)-1)
    
    for(i in 1:(length(cutpoints) - 1)){
        if(i == length(cutpoints) - 1){
            out[[i]] <- file[cutpoints[i]:(cutpoints[i + 1])]
        }else{
            out[[i]] <- file[cutpoints[i]:(cutpoints[i + 1] - 1)]
        }
        fun_names[i] <- stringr::str_extract(out[[i]][1], ".+?(?=<-)")
    }
    fun_names <- gsub(" ", "", fun_names)
    names(out) <- fun_names
    out <- lapply(out, function(x) x[!grepl("#'", x)]) # remove roxygen
    return(out)
}

print_fun <- function(fun){
    cat("```r\n", fun, sep = "\n", "```\n")
}

trim_df <- function(data, prows = 4){
    data <- dplyr::mutate(data, across(where(is.factor), as.character))
    data <- data.frame(data)
    dots <- data[1, ]
    dots[1, ] <- "..."
    nrows <- nrow(data)
    trimmed <- rbind(
        data[1:prows,],
        dots,
        data[(nrows-(prows - 1)):nrows, ]
    )
    rownames(trimmed) <- NULL
    return(trimmed)
}

mytab <- function(data, fontsize = 7, colnames = NA){
    data |> 
        kable(booktabs = TRUE, 
              digits = 3,
              col.names = colnames, 
              format = "latex",
              escape = FALSE) |> 
        kable_styling(position = "center", 
                      font_size = fontsize) |> 
        row_spec(0, bold = TRUE)
}

mytheme <- function(size = 15){
    ggthemes::theme_par(base_size = size) +
        theme(axis.text = element_text(size = size),
              axis.title = element_text(size = size * 1.2),
              title = element_text(size = size * 1.3))
}

ref <- function(book, chapter, page){
    sprintf("\\addnote{%s, Ch. %s, pp. %s}",
            book, chapter, page)
}
