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

create_slide_index <- function(){
    slides <- list.files("slides/", 
                         pattern = ".Rmd|.pdf",
                         recursive = TRUE, 
                         full.names = TRUE)
    slides <- slides[!grepl("_files|deprecated|updating|img", slides)]
    name <- unique(tools::file_path_sans_ext(basename(slides)))
    rmd <- slides[grepl(".Rmd", slides)]
    pdf <- slides[grepl(".pdf", slides)]
    name <- gsub("_", " ", name)
    sprintf("- **%s**: [Rmd](%s), [PDF](%s)", name, rmd, pdf)
}

create_lab_index <- function(){
    labs <- list.files("labs/", 
                       pattern = ".R|.html",
                       recursive = TRUE, 
                       full.names = TRUE)
    name <- unique(tools::file_path_sans_ext(basename(labs)))
    rmd <- labs[grepl(".Rmd", labs)]
    r <- labs[grepl(".R$", labs)]
    html <- labs[grepl(".html", labs)]
    name <- gsub("_", " ", name)
    sprintf("- **%s**: [Rmd](%s), [html](%s), [R code](%s)", name, rmd, html, r)
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
    out <- lapply(out, function(x) x[!grepl("#", x)]) # remove roxygen
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
    if(nrows <= 5){
        trimmed <- data
    } else{
        if(nrows <= prows*2){
            prows <- floor(prows/2)
        }
        trimmed <- rbind(
            data[1:prows,],
            dots,
            data[(nrows-(prows - 1)):nrows, ]
        )
    }
    rownames(trimmed) <- NULL
    return(trimmed)
}

fixlatex <- function(x){
    stringr::str_replace_all(x, "_", ".")
}

mytab <- function(data, fontsize = 7, colnames = NA, ...){
    if(is.na(colnames)){
        colnames(data) <- stringr::str_replace_all(names(data), "_", ".")
    }
    
    # data <- dplyr::mutate(data, 
    #                       dplyr::across(where(is.character)|where(is.factor),fixlatex))
    data |> 
        kableExtra::kable(booktabs = TRUE, 
              digits = 3,
              col.names = colnames, 
              format = "latex",
              escape = FALSE,
              align = "c") |> 
        kableExtra::kable_styling(position = "center", 
                                  font_size = fontsize) |> 
        kableExtra::row_spec(0, bold = TRUE)
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

note <- function(x){
    sprintf("\\addnote{%s}", x)
}

color <- function(x, color){
    sprintf("\\textcolor{%s}{%s}", color, x)
}

format <- function(x, what){
    switch(what,
           "code" = code(x)
    )
}

code <- function(x){
    sprintf("\\texttt{%s}", x)
}

question <- function(txt){
    p <- "\\begin{center} \\huge \\textcolor{myblue}{\\textbf{%s}} \\end{center}"
    sprintf(p, txt)
}

center <- function(x){
    sprintf("\\begin{center} %s \\end{center}", x)
}

bold <- function(x){
    sprintf("\\textbf{%s}", x)
}

quote <- function(quote, name){
    sprintf("\\epigraph{\\centering %s} {— %s}", quote, name)
}

# thanks to https://community.rstudio.com/t/kableextra-collapse-rows-not-working-reason-unknown/134721/2
collapse_rows_df <- function(df, variable){
    group_var <- enquo(variable)
    df %>%
        group_by(!! group_var) %>%
        mutate(groupRow = 1:n()) %>%
        ungroup() %>%
        mutate(!!quo_name(group_var) := ifelse(groupRow == 1, as.character(!! group_var), "")) %>%
        select(-c(groupRow))
}

nprint <- function(..., digits = 3){
    dots <- list(...)
    dots <- lapply(dots, round, digits)
    print(unlist(dots))
}

data_struct <- function(data){
    list(n = nrow(data),
         ncat = sum(sapply(data, function(x) sum(is.factor(x) | is.character(x)))),
         nnum = sum(sapply(data, function(x) sum(is.numeric(x)))))
}

to0 <- function(x){
    ifelse(x < 0, 0, x)
}

solution <- function(x = NULL, eval = TRUE){
    if(eval){
        if(!is.null(x)){
            sol <- sprintf("\\textbf{\\textcolor{red}{%s}}", x)
            cat("**Solution**: ", sol)
        }else{
            cat("**Solution**")
        }
    }
}
