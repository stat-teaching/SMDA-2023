init_project <- function(){
    
    utils_glm <- "https://raw.githubusercontent.com/stat-teaching/SMDA-2023/master/R/utils-glm.R"
    
    mkdir_if("data")
    mkdir_if("exercizes")
    mkdir_if("R")
    
    if(!file.exists("R/utils.glm.R")){
        download.file(utils_glm, "R/utils.glm.R", quiet = TRUE)
    }
    
}

mkdir_if <- function(dir){
    if(!dir.exists(dir)){
        dir.create(dir)
    }
}

