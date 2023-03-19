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
