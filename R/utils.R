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

add_predict <- function(data, fit, ...){
    pr <- predict(fit, newdata = data, ...)
    cbind(data, data.frame(pr))
}

# thanks to https://stats.stackexchange.com/a/433060
model_equation <- function(model, values = NULL, ..., only_print = FALSE) {
    format_args <- list(...)
    model_coeff <- model$coefficients
    model_coeff <- round(model_coeff, 3)
    format_args$x <- abs(model_coeff)
    model_coeff_sign <- sign(model_coeff)
    model_coeff_prefix <- case_when(model_coeff_sign == -1 ~ " - ",
                                    model_coeff_sign == 1 ~ " + ",
                                    model_coeff_sign == 0 ~ " + ")
    nms <- names(model_coeff[-1])
    # check if there are values to print
    if(!is.null(values)){
        # check if there are too much values to print
        if(length(values[[1]]) > 3){
            # formatting
            values <- lapply(values, function(x) sprintf("[%s ... %s]", x[1], x[length(x)]))
        }
        
        nms <- purrr::reduce2(names(values), 
                              as.character(values), 
                              .init = nms, 
                              stringr::str_replace)
    }
    
    
    y <- strsplit(as.character(model$call$formula), "~")[[2]]
    b0 <- paste0(ifelse(model_coeff[1] < 0, "-", ""), do.call(format, format_args)[1])
    bs <- paste0(model_coeff_prefix[-1],
                 do.call(format, format_args)[-1],
                 "*",
                 cli::col_blue(nms),
                 sep = "", collapse = "")
    
    model_eqn <- sprintf("%s ~ %s%s", cli::col_green(y), b0, bs)
    cat(model_eqn)
    if(!only_print){
        invisible(model_eqn)
    }
}

epredict <- function(fit, values, ...){
    message(model_equation(fit, values, only_print = TRUE))
    pr <- predict(fit, values, ...)
    lapply(pr, unname)
}

lgt <- function(letter){
    sprintf("$log(\\frac{%s}{1 - %s})$", letter, letter)
}

invlgt <- function(letter){
    sprintf("$\\frac{e^{%s}}{1 + e^{%s}}$", letter, letter)
}

odds <- function(p){
    p / (1 - p)
}

odds_ratio <- function(pn, pd){
    odds(pn) / odds(pd)
}


plot_param <- function(fit, b){
    fits <- broom::tidy(fit, conf.int = TRUE)
    fits <- fits[fits$term == b, ]
    ci <- c(fits$estimate - fits$std.error * abs(qnorm(0.025)),
            fits$estimate + fits$std.error * abs(qnorm(0.025)))
    m <- fits$estimate[1]
    se <- fits$std.error[1]
    out <- ggnorm(m, se) +
        stat_function(fun = dnorm,
                      geom = "area",
                      args = list(mean = m, sd = se),
                      fill = "lightblue",
                      alpha = 0.5,
                      xlim = ci) +
        ggtitle(latex2exp::TeX("Sampling distribution of $\\beta$")) +
        geom_segment(x = ci, y = c(0, 0),
                     xend = ci, yend = dnorm(ci, m, se)) +
        xlab("x")
    if(!(0 < (m - se*5) | 0 > (m + se*5))){
        out +
            geom_vline(xintercept = 0, color = "firebrick3", linewidth = 1)
    }else{
        out
    }
}


ggnorm <- function(mean, sd){
    range <- c(mean - sd*5, mean + sd*5)
    ggplot(data = data.frame(x = range)) +
        stat_function(fun = dnorm, args = list(mean = mean, sd = sd)) +
        ylab("Density") +
        xlim(range)
}

