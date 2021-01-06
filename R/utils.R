# recode missing values
rec_missing<-function(x,missings=c('N/A','n/a',999,888,' ','(vide)','d/m','','NA','na')) {
  x[x %in% missings] <- NA
  return(x)
}

r3c<-function(vec,name,label){
  name <-  name %>% ch
  vec <-  vec %>% ch
  label <-  label %>% ch

  if(length(name)==length(label)){
    for (i in 1:length(name)){
      cond<-which(vec%in%name[i])
      if(length(cond)>0){
        vec[cond]<-label[i]
      }
      if(length(grep(paste0(name[i], "\\."), vec)) > 0){
        vec[grep(paste0(name[i], "\\."), vec)] <- gsub(paste0(name[i], "\\."), paste0(label[i],"\\."), vec[grep(paste0(name[i], "\\."), vec)])
      }
    }
    return(vec)
  } else {
    print("y and z must have the length")
  }
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

############################################################################################
############################################################################################

# character operation
ch<-as.character
chr<-as.character

coerc<-function(x){as.numeric(chr(x))}

