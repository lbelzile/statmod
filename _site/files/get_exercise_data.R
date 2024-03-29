exdir <- "../files/exercises"
linkstring <- "https://nbviewer.jupyter.org/github/lbelzile/statmod/blob/main/files/exercises/"
linkgithub <- "https://raw.githubusercontent.com/lbelzile/statmod/main/"
## exercise and solution files
fn <- FALSE
ex <- list.files(path = exdir, pattern = "MATH60604A-Exercise[[:digit:]].pdf", 
    full.names = fn)
codesas <- list.files(path = exdir, pattern = "MATH60604A-Exercise[[:digit:]].sas", 
                 full.names = fn)

sas <- rep("", 7)
sas[as.integer(substr(codesas, start = 20, stop = 20))] <- 
  paste0("[<span style='color: #4b5357;'><i class='fas fa-file-code fa-lg'></i></span>](", linkgithub, "exercises/",codesas, ")")


coder <- list.files(path = exdir, pattern = "MATH60604A-Exercise[[:digit:]].R", 
                      full.names = fn)
rc <- rep("",7)
rc[as.integer(substr(coder, start = 20, stop = 20))] <- 
  paste0("[<span style='color: #276dc2;'><i class='fab fa-r-project fa-lg'></i></span>](", linkgithub, "exercises/", coder, ")")


so <- list.files(path = exdir, pattern = "MATH60604A-Exercise[[:digit:]]-sol.pdf", 
    full.names = fn)



## Numbers + Topics
# exid <- as.numeric(gsub("[^0-9.-]+", "", ex))
topics <- 
  c("Basics of statistical inference", 
    "Linear regression",
    "Likelihood methods",
    "Generalized linear models",
    "Correlated and longitudinal data",
    "Linear mixed models",
    "Survival analysis")
exdat <- data.frame(Chapter = topics)


## Links

exdat$Exercise <- c(paste0("[<span style='color: #4b5357;'><i class='fas fa-file-pdf fa-lg'></i></span>](", linkstring, ex, ")"),rep("", length.out = 7-length(ex)))
exdat$Solution <- c(paste0("[<span style='color: #bfc2c5;'><i class='far fa-file-pdf fa-lg'></i></span>](", linkstring, so, ")"),rep("", length.out = 7-length(so)))
exdat$`SAS` <- sas
exdat$`R` <- rc

