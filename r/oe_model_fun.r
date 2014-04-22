'%nin%' <- function(x,y)!(x %in% y)

rcomm <- function(x, num){
  taxa <- colnames(x)
  x <- as.data.frame(t(x))
  t(mapply(function(id, s){
    if(all(id == 0))return(id)
    set.seed(12345)
    res <- table(sample(rep(taxa, id), s))
    ret <- rep(0, length(id))
    names(ret) <- taxa
    ret[names(res)] <- res
    ret
  }, x, num))
}


communityMatrix <- function(bugs, OTU, x=200){ # x = value to subsample to
  bugs.sub <- bugs[bugs[, OTU] %nin% c("Exclude", "Ambiguous", "NA"), ]
  bugall<- acast(bugs.sub, SampleID ~ Family_OTU, value.var="BAResult", fun.aggregate=sum)
  bugall <- bugall[, colnames(bugall) != "NA"]
  samp <- rep(x, times=nrow(bugall))
  samp[rowSums(bugall)< x] <- rowSums(bugall)[rowSums(bugall)< x]  
  rcomm(bugall, samp)
}

OEModelPredict <- function(bugs, predictors, rfmod = rfmod, calibration = bugs_pa_cal, 
                           calibration_preds = predictors_cal, cutoff = 0.5,
                           subsamp = 200){
  if(!all(names(rfmod$forest$ncat) %in% names(predictors)))stop("Missing predictor variables")
  
  bugs_pa <- communityMatrix(bugs, "Family_OTU", x=subsamp)
  bugs_pa[bugs_pa > 0] <- 1
  predictors <- merge(
    unique(bugs[, c("StationCode", "SampleID")]), predictors, all.x=TRUE
  )
  
  bugs_pa <- bugs_pa[as.character(predictors$SampleID),]
  
  
  group_probs <- predict(rfmod, newdata = predictors, type = 'prob')
  group_occurence <- apply(calibration, 2, function(x)tapply(x, calibration_preds$GroupID, mean))
  capture_probs <- group_probs %*% group_occurence
  row.names(capture_probs) <- row.names(bugs_pa)
  
  result <- Reduce(rbind, lapply(row.names(capture_probs), function(sample){
    in_group <- colnames(capture_probs)[which(capture_probs[sample, ] >= cutoff)]
    data.frame(E = sum(capture_probs[sample, in_group]),
               O = sum(bugs_pa[sample, in_group])
    )
  }))
  row.names(result) <- row.names(capture_probs)
  
  
  null_model <- apply(calibration, 2, mean)
  null_taxa <- names(null_model)[null_model >= cutoff]
  result$E_Null <- sum(null_model[null_taxa])
  result$O_Null <- apply(bugs_pa[, null_taxa], 1, sum)
  
  result <- within(result, {
    OoverE <- O/E
    OoverE_Null <- O_Null/E_Null
  })
  list(OoverE = result[, c("E", "O", "OoverE", "E_Null", "O_Null", "OoverE_Null")],
       Capture_Probs = capture_probs,
       Group_Probs = group_probs)
}