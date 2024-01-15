library(GMPR)


transform_and_filter_taxa=function(x, samples_row=T, method="clr", missing_filter=0){
  x[x=="NA"]=0
  x[x=="NaN"]=0
  #if samples are in columns transpose
  if (!samples_row){
    
    x=as.data.frame(t(x))
    
  } 
  #Exclude/keep columns that pass the missigness threshold
  if (missing_filter>100){
    
    stop("\n Hey! \n Values should be a proportion of missing values allowed per column: a value from 0 to 100") 
    
  }
  
  x_filt=x[,((colSums(x !=0) / nrow(x)) *100 )>missing_filter]
  #x_filt=x[,((colSums(is.na(x)) / nrow(x)) *100 )>missing_filter]
  my_num_removed=ncol(x)-ncol(x_filt)
  print (paste(my_num_removed, "species removed due to many missing values"))
  
  if (method=="asin"){
    print ("ASIN - make sure that your data are relative abundances 0 to 100%")
    x_filt=x_filt/100
    x_filt=asin(sqrt(x_filt))
    
  } else if (method=="log"){
    print ("LOG10")
    #replace 0 by the half of the smallest value observed
    my_min=min(x_filt[x_filt>0])/2
    x_filt=x_filt+my_min
    x_filt=log10(x_filt)
    
  }else if (method=="clr"){
    print ("CLR")
    my_min=min(x_filt[x_filt>0], na.rm=T)*0.66
    x_filt=x_filt+my_min
    #Calculate geometric mean
    gm_mean = function(a, na.rm=TRUE){
      exp(mean(log(a)))
    }
    Gmean_core = apply(x_filt, 1, gm_mean)
    x_filt = cbind(Gmean_core,x_filt)
    d <- t(apply(x_filt, 1, function(b) {
      log(b/b[1])[-1]
    }))
    x_filt=d
  }else if (method=="rclr"){
    print ("robust-CLR")
   
    #Calculate geometric mean
    gm_mean = function(a, na.rm=TRUE){
      exp(mean(log(a[a>0])))
    }
    Gmean_core = apply(x_filt, 1, gm_mean)
    x_filt = cbind(Gmean_core,x_filt)
    d <- t(apply(x_filt, 1, function(b) {
      log(b/b[1])[-1]
    }))
    x_filt=d
  } else if (method =="GMPR"){
    print ("geometric mean of pairwise ratios")
    tx=as.data.frame(t(x_filt))
    gmpr.size.factor <- GMPR(tx)
    x_filt <- t(tx) / gmpr.size.factor
  }
  return(as.data.frame(x_filt))
}

