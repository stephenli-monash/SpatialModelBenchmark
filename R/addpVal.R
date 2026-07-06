#' @keywords internal 
#' NULL 
addpVal<-function(data,type,false.sig,set_col="set",ind_col="ind"){
  
  # data = dt[[1]]
  # type = cols2[1]
  # false.sig = readRDS("false.sig/false.sig.nb-weighted-shuffle.1.25.rds")
  # set_col="set"
  # ind_col="ind"
  
  term <- unique(false.sig[["term"]])
  comparison <- unique(false.sig[["comparison"]])
  
  pval_col <- paste0("pval05.", term)
  for(col in pval_col){
    for(comp in comparison){
      data[[col]] <- "."
      ind2 <- which(data[["comparison"]] == comp)
      ind <- false.sig[["type"]] == type & false.sig[["term"]] == term & false.sig[["comparison"]] == comp
      data2 <- false.sig[ind,]
      data[[col]] <- ifelse(data[[set_col]][ind2] %in% unlist(data2[[ind_col]]),1,0)
    }
  }
  return(data)
}

# add.pval<-function(data,type,type,method){
#   tmp<-data
#   term<-c("fconditioncIM","fconditionimIM","fconditionDYS")
#   
#   if (method=="shuffled") {
#     term <- gsub("fcondition","shuffled.fcond",term)
#   } else if (method=="weighted") {
#     term<-term
#   }
#   
#   if (type=="wilcox"){
#     
#     tmp$pval05.less <- "."
#     tmp$pval05.greater <- "."
#     tmp$pval05.two.sided <- "."
#     
#     for (i in 1:12){
#       ind2<-which(tmp$comparison==i)
#       
#       ind<-which(false.sig$type==type & false.sig$alternative=="less" & false.sig$comparison==i)
#       tmp2<-false.sig[ind,]
#       tmp$pval05.less[ind2] <- ifelse(tmp$set[ind2] %in% unlist(tmp2$ind),1,0)
#       
#       ind<-which(false.sig$type==type & false.sig$alternative=="greater" & false.sig$comparison==i)
#       tmp2<-false.sig[ind,]
#       tmp$pval05.greater[ind2] <- ifelse(tmp$set[ind2] %in% unlist(tmp2$ind),1,0)
#       
#       ind<-which(false.sig$type==type & false.sig$alternative=="two.sided" & false.sig$comparison==i)
#       tmp2<-false.sig[ind,]
#       tmp$pval05.two.sided[ind2] <- ifelse(tmp$set[ind2] %in% unlist(tmp2$ind),1,0)
#     }  
#     
#   } else if (type=="glmm.nbinom") {
#     
#     tmp$pval05.cIM <- "."
#     tmp$pval05.imIM <- "."
#     tmp$pval05.DYS <- "."
#     
#     for (i in 1:12) {
#       ind2<-which(tmp$comparison==i)
#       
#       ind<-which(false.sig.nb$type==type & false.sig.nb$term==term[1] & false.sig.nb$comparison==i)
#       tmp2<-false.sig.nb[ind,]
#       tmp$pval05.cIM[ind2] <- ifelse(tmp$set[ind2] %in% unlist(tmp2$ind),1,0)
#       
#       ind<-which(false.sig.nb$type==type & false.sig.nb$term==term[2] & false.sig.nb$comparison==i)
#       tmp2<-false.sig.nb[ind,]
#       tmp$pval05.imIM[ind2] <- ifelse(tmp$set[ind2] %in% unlist(tmp2$ind),1,0)
#       
#       ind<-which(false.sig.nb$type==type & false.sig.nb$term==term[3] & false.sig.nb$comparison==i)
#       tmp2<-false.sig.nb[ind,]
#       tmp$pval05.DYS[ind2] <- ifelse(tmp$set[ind2] %in% unlist(tmp2$ind),1,0)
#     }
#     
#   } else if (type=="glmm.pois") {
#     
#     tmp$pval05.cIM <- "."
#     tmp$pval05.imIM <- "."
#     tmp$pval05.DYS <- "."
#     
#     for (i in 1:12){
#       ind2<-which(tmp$comparison==i)
#       
#       ind<-which(false.sig.p$type==type & false.sig.p$term==term[1] & false.sig.p$comparison==i)
#       tmp2<-false.sig.p[ind,]
#       tmp$pval05.cIM[ind2] <- ifelse(tmp$set[ind2] %in% unlist(tmp2$ind),1,0)
#       
#       ind<-which(false.sig.p$type==type & false.sig.p$term==term[2] & false.sig.p$comparison==i)
#       tmp2<-false.sig.p[ind,]
#       tmp$pval05.imIM[ind2] <- ifelse(tmp$set[ind2] %in% unlist(tmp2$ind),1,0)
#       
#       ind<-which(false.sig.p$type==type & false.sig.p$term==term[3] & false.sig.p$comparison==i)
#       tmp2<-false.sig.p[ind,]
#       tmp$pval05.DYS[ind2] <- ifelse(tmp$set[ind2] %in% unlist(tmp2$ind),1,0)
#     }
#     
#   }
#   return(tmp)
# }