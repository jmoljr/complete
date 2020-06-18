# complete
complete <- function(directory, id = 1:332){
  #colocar directorio de trabajo
  spec <- grepl("specdata", getwd())
  if (spec == F) {
    setwd(paste(getwd(), directory, sep = "/"))
  } 
  #funcion para armar nombre de archivos a leer 
  armarnombre <- function(id){
    lista1 <- list()
    for (n in id) {
      if(n < 10){
        nombre <- paste("00", n, ".csv", sep = "")
      }
      if((n >= 10) && (n < 100)){
        nombre <- paste("0", n, ".csv", sep = "")
      }
      if((n >= 100) && (n <= length(id))) { 
        nombre <- paste(n, ".csv", sep = "")
      }
      lista1 <- c(lista1, nombre)
    }
    lista1
  }
  lista1 <- armarnombre(id)
  
  #variables used to crate lists
  ids <- list()
  nobs <- list()
  #funcion para armar subset
  #mycomplete <- function(id, lista1){
  for (id in lista1) {
    f <- read.csv(id)
    p <- subset(f, select = ID)
    ids <- c(ids, p[1,1])
    tcomp <- sum(complete.cases(f))
    nobs <- c(nobs, tcomp)
      
    #imp <- data.frame(list(id=pl, nobs= cl))
    #print(imp)
  }
  imp <- matrix(c(ids, nobs), ncol = 2)
  impd <- data.frame(imp)
  colnames(impd)[1] <- "ID"
  colnames(impd)[2] <- "nobs"
  print(impd)
}
wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
#Llamar complete.r con source
source("C:/Users/jmol-/Documents/Proyectos R/complete.R")
com <- complete("specdata")

corr <- function(directory, threshold = 0){
  #colocar directorio de trabajo
  id <- 1:332
  armarnombre <- function(id){
    lista1 <- list()
    for (n in id) {
      if(n < 10){
        nombre <- paste("00", n, ".csv", sep = "")
      }
      if((n >= 10) && (n < 100)){
        nombre <- paste("0", n, ".csv", sep = "")
      }
      if((n >= 100) && (n <= length(id))) { 
        nombre <- paste(n, ".csv", sep = "")
      }
      lista1 <- c(lista1, nombre)
    }
    lista1
  }
  lista1 <- armarnombre(id)
  
  #variables used to crate lists
  #sulfatos <- list()
  #nitratos <- list()
  
  rescor <- list()
  
  #funcion para armar subset
  #mycomplete <- function(id, lista1){
  for (i in lista1) {
    f <- read.csv(i)
    p <- subset(f, select = c(sulfate, nitrate))
    ccs <- sum(complete.cases(p))
    
    if(ccs > threshold){
      pomit <- na.omit(p)
      c1 <- cor(pomit$sulfate, pomit$nitrate)
      rescor <- c(rescor, c1)
      
    }
  }
  as.list.data.frame(rescor)
}
