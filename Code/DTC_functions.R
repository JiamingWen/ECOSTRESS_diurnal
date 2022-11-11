######################################################################
#This function is to evaluate LST at time t (in hours since t_sr), 
#using a DTC model with given parameters T_sr, T_max, t_max, t_ss

#input
# T_sr is the vector of length D+1 containing the temperatures at sunrise for the D days of interest, as well as the temperature at time of sunrise for day D+1
# T_max is the vector of maximum temperatures for the D days
# t_max is the numeric vector of length 1, containing the time of maximum temperature (assumed to be the same across all D days), measured in hours after sunrise
# t_ss is the numeric vector of length 1, containing the time of sunset (assumed to be the same across all D days), measured in hours after sunrise
# t is the vector of times at which evaluation of the function is desired, measured in hours after sunrise on day 1.

#output
# LST at time t

diurn <- function(T_sr, T_max, t_max, t_ss, t){
  D <- length(T_max)
  if(length(T_sr) != length(T_max) + 1){
    warning('Should have length(T_sr) == (length(T_max) + 1)')
  }else{
    if(max(t/24) > D | min(t/24) < 0){
      warning('Should have 0 < t < D')
    }else{
      T_sr.0 <- T_sr[-(D+1)]
      
      if (any(T_sr.0 >= T_max)){
        which.wrong <- which(T_sr.0 >= T_max)
        for(w in 1:length(which.wrong)){
          warning(paste('The parameters do not conform: T_sr >= T_max for day', which.wrong[w]))
        }
      }else{
        T_sr.24 <- T_sr[-1]
        
        t_s <- t_ss - 1 # Set "time of free attenuation" to 1 hour before sunset
        T_a <- (T_max - T_sr.0) / (cos(pi/4)+1)
        T_0 <- T_sr.0 + T_a * cos(pi/4)
        w <- 4/3 * t_max
        u <- pi/w * (t_s - t_max)
        T_knot <- T_0 + T_a * cos(u)
        
        
        if(any(T_knot >= T_max)){
          which.wrong <- which(T_knot >= T_max)
          for(w in 1:length(which.wrong)){
            warning(paste('The parameters do not conform: temp at t_s >= T_max for day', which.wrong[w]))
          }
        }
        if(any(T_knot < T_sr.24)){
          which.wrong <- which(T_knot < T_sr.24)
          for(w in 1:length(which.wrong)){
            warning(paste('The parameters do not conform: temp at t_s on day', which.wrong[w], '< T_sr on day', which.wrong[w]+1))
          }
        }
        
        
        dT <- T_a * (cos(u) * (T_a * cos(u) + T_0 - T_sr.24) + pi/w * sin(u) * (24 - t_s) * (T_0 - T_sr.24)) / 
          (T_0 - T_sr.24 - T_a * (pi/w * sin(u) * (24 - t_s) - cos(u)))
        k <- (T_a*cos(u)-dT) / (T_a*pi/w*sin(u))    
        
        temps <- numeric(length(t))
        for(d in 1:D){
          which.d <- which(t < 24*d  &  t >= 24*(d-1))
          all.t <- t[which.d] - 24*(d-1)
          which.daytime <- which(all.t <= t_s)
          which.nighttime <- which(all.t > t_s)
          temps[which.d][which.daytime] <- T_0[d] + T_a[d] * cos(pi/w * (all.t[which.daytime] - t_max))
          temps[which.d][which.nighttime] <- T_0[d] + dT[d] + (T_a[d] * cos(pi/w * (t_s - t_max)) - dT[d]) * k[d]/(k[d] + all.t[which.nighttime] - t_s)
        }
        temps
      }
    }
  }
}

######################################################################
#fit the parameters of a regular DTC model, with LST measurements temps acquired at time t (in hours since t_sr)

#input
# temps is the vector of temperature readings for which we'd like to estimate certain parameters
# t is the time at which the data (temps) were recorded, measured in hours since the time of sunrise on the first day.
# T_sr, T_max, t_max, and t_ss are each either NULL (if an estimate of the parameter is desired) or are specified as fixed, known values.
# T_sr.guess, T_max.guess, t_max.guess, and t_ss.guess may be specified to help convergence of the estimate. If unaltered, the starting place is set as the default argument.

#output
# estimate of DTC parameters T_sr, T_max, t_max, t_ss

diurn.fit <- function(temps, t, T_sr=NULL, T_max=NULL, t_max=NULL, t_ss=NULL,
                      T_sr.guess=20, T_max.guess=30, t_max.guess=8, t_ss.guess=15,
                      T_sr.all.equal=FALSE){
  D <- ceiling(max(t)/24)
  
  is.null.T_sr <- is.null(T_sr)
  is.null.T_max <- is.null(T_max)
  is.null.t_max <- is.null(t_max)
  is.null.t_ss <- is.null(t_ss)
  
  length.theta <- is.null.T_sr*(D*(!T_sr.all.equal)+1) + is.null.T_max*D + is.null.t_max + is.null.t_ss
  
  diurn.ss <- function(theta){
    k <- 1
    if(is.null.T_sr){
      if(T_sr.all.equal){
        T_sr <- rep(theta[1], D+1)
        k <- k + 1
      }else{
        T_sr <- theta[1:(D+1)]
        k <- k + D + 1
      }
    }
    if(is.null.T_max){
      T_max <- theta[k:(k+D-1)]
      k <- k + D
    }
    if(is.null.t_max){
      t_max <- theta[k]
      k <- k + 1
    }
    if(is.null.t_ss){
      t_ss <- theta[k]
    }
    temp.est <- diurn(T_sr, T_max, t_max, t_ss, t)
    if(is.character(temp.est)){
      Inf
    }else{
      sum((temps - temp.est)^2)
    }
  }
  
  if(length.theta > 0){
    theta.start <- c(rep(T_sr.guess, is.null.T_sr*(D*(!T_sr.all.equal)+1)),
                     rep(T_max.guess, D*(is.null.T_max)),
                     rep(t_max.guess, is.null.t_max),
                     rep(t_ss.guess, is.null.t_ss))
  }else{
    list(T_sr.est=T_sr, T_max.est=T_max, t_max.est=t_max, t_ss.est=t_ss)
  }
  
  theta.est <- suppressWarnings(optim(par=theta.start, diurn.ss)$par)
  k <- 1
  if(is.null.T_sr){
    if(T_sr.all.equal){
      T_sr <- rep(theta.est[1], D+1)
      k <- k + 1
    }else{
      T_sr <- theta.est[1:(D+1)]
      k <- k + D + 1
    }
  }
  if(is.null.T_max){
    T_max <- theta.est[k:(k+D-1)]
    k <- k + D
  }
  if(is.null.t_max){
    t_max <- theta.est[k]
    k <- k + 1
  }
  if(is.null.t_ss){
    t_ss <- theta.est[k]
  }
  list(T_sr.est=T_sr, T_max.est=T_max, t_max.est=t_max, t_ss.est=t_ss)
}

####################################################################
#this is a modified DTC fitting function, which enables two reference sources (e.g., ECOSTRESS and GOES LST)
#temps, t - LST and time for ECOSTRESS measurements
#temps_ref, t_ref - LST and time for GOES measurements
#T_sr_diff - T_sr of all days to T_sr at day1 (length = D+1)
#T_max_diff - T_max of all days to T_max at day1 (length = D)

diurn.fit_Eco <- function(temps, t,temps_ref,t_ref,alpha=1, T_sr=NULL, T_max=NULL, t_max=NULL, t_ss=NULL, T_sr_diff, T_max_diff,
                          T_sr.guess=20, T_max.guess=30, t_max.guess=8, t_ss.guess=15){
  
  tmp=na.omit(data.frame(temps_ref,t_ref))
  temps_ref=tmp$temps_ref
  t_ref=tmp$t_ref
  
  D <- ceiling(max(t)/24)
  
  is.null.T_sr <- is.null(T_sr)
  is.null.T_max <- is.null(T_max)
  is.null.t_max <- is.null(t_max)
  is.null.t_ss <- is.null(t_ss)
  
  length.theta <- is.null.T_sr + is.null.T_max + is.null.t_max + is.null.t_ss
  
  diurn.ss <- function(theta){
    k <- 1
    if(is.null.T_sr){
      T_sr <- theta[k]+T_sr_diff
      k <- k + 1
    }
    if(is.null.T_max){
      T_max <- theta[k]+T_max_diff
      k <- k + 1
    }
    if(is.null.t_max){
      t_max <- theta[k]
      k <- k + 1
    }
    if(is.null.t_ss){
      t_ss <- theta[k]
    }
    temp.est <- diurn(T_sr, T_max, t_max, t_ss, t)
    temp_ref.est <- diurn(T_sr, T_max, t_max, t_ss, t_ref)
    if((!is.numeric(temp.est)) |(!is.numeric(temp_ref.est))){
      1e20
    }else{
      nRMSE1 <- sqrt(sum(((temps - temp.est)/temps)^2)/length(temps))
      nRMSE2 <- sqrt(sum(((temps_ref - temp_ref.est)/temps_ref)^2)/length(temps_ref))
      nRMSE1 + alpha * nRMSE2
    }
  }
  
  if(length.theta > 0){
    theta.start <- c(rep(T_sr.guess,is.null.T_sr),
                     rep(T_max.guess,is.null.T_max),
                     rep(t_max.guess,is.null.t_max),
                     rep(t_ss.guess,is.null.t_ss))
  }else{
    list(T_sr.est=T_sr, T_max.est=T_max, t_max.est=t_max, t_ss.est=t_ss)
  }
  
  theta.est <- suppressWarnings(optim(par=theta.start, diurn.ss)$par)
  k <- 1
  if(is.null.T_sr){
    T_sr <- theta.est[k]+T_sr_diff
    k <- k + 1
  }
  if(is.null.T_max){
    T_max <- theta.est[k]+T_max_diff
    k <- k + 1
  }
  if(is.null.t_max){
    t_max <- theta.est[k]
    k <- k + 1
  }
  if(is.null.t_ss){
    t_ss <- theta.est[k]
  }
  list(T_sr.est=T_sr, T_max.est=T_max, t_max.est=t_max, t_ss.est=t_ss)
}

###################################################################################
#this is a modified DTC model, specifically for constructing the diurnal cycle of Ecostress LST using ECOSTRESS and GOES LST. It contains two steps 
#first, it constructs diurnal cycle of GOES LST, gets estimation of t_max and t_ss, and day-to-day variation of T_max and T_sr, which are set as known in the second step
#second, it constructs diurnal cycle of ECOSTRESS LST using observations from both ECOSTRESS and GOES
#the loss function is set as nRMSE(y_Ecostress, y_Ecostress_pred) + alpha * nRMSE(y_GOES, y_GOES_pred), alpha can be calibrated

################################################
#fit a DTC model with GOES LST
#input
#t0_GOES - GOES acquisition time, in hours since the sunrise of startdate
#temps0_GOES - corresponding GOES LST (in degC)
#t0_output - the timestamps to estimate LST as output, in hours since the sunrise of startdate
#output
#t_max - calibrated t_max (time when LST reaches daily maximum, in hours since sunrise, assumed constant during the time window)
#t_ss - calibrated t_ss (sunset time, in hours since sunrise, assumed constant during the time window)
#T_sr_all - fitted sunrise temperature for each day
#T_max_all - fitted maximum temperature for each day
#temps.fit_GOES - fitted temperature for t0_GOES
#temps.est_GOES - fitted temperature for t0_output (if t0_output is not NULL)

DTC_GOES=function(t0_GOES,temps0_GOES,t0_output=NULL){
  #remove NA values in GOES
  tmp=na.omit(data.frame(t0_GOES,temps0_GOES))
  t0_GOES2=tmp$t0_GOES
  temps0_GOES2=tmp$temps0_GOES
  #fit for GOES
  params.est_GOES <- diurn.fit(temps0_GOES2, t0_GOES2,T_sr.guess=min(temps0_GOES2), T_max.guess=max(temps0_GOES2))
  temps.fit_GOES = diurn(T_sr=params.est_GOES$T_sr.est, T_max=params.est_GOES$T_max.est,
                         t_max=params.est_GOES$t_max.est, t_ss=params.est_GOES$t_ss.est,
                         t0_GOES)
  
  #prediction for t0_output
  if(!is.null(t0_output)){
    pred_tmp = diurn(T_sr=params.est_GOES$T_sr.est, T_max=params.est_GOES$T_max.est,
                     t_max=params.est_GOES$t_max.est, t_ss=params.est_GOES$t_ss.est,
                     t0_output)
    if (is.numeric(pred_tmp)){
      temps.est_GOES=pred_tmp
    }else{
      temps.est_GOES=rep(NA,length(t0_output))
    }
  }else{
    temps.est_GOES = NULL
  }
  return(list(t_max=params.est_GOES$t_max.est,t_ss=params.est_GOES$t_ss.est,
              T_sr_all=params.est_GOES$T_sr.est,T_max_all=params.est_GOES$T_max.est,
              temps.fit_GOES=temps.fit_GOES,temps.est_GOES=temps.est_GOES))
  
}

#input

#t0_Eco - Ecostress acquisition time, in hours since the sunrise of startdate
#temps0_Eco - corresponding Ecostress LST (in degC)
#t0_GOES - GOES acquisition time, in hours since the sunrise of startdate
#temps0_GOES - corresponding GOES LST (in degC)
#t_max_GOES - t_max fitted from DTC_GOES (assumed constant during the time window)
#t_ss_GOES - t_ss fitted from DTC_GOES (assumed constant during the time window)
#T_sr_GOES - T_sr fitted from DTC_GOES for each day
#T_max_GOES - T_max fitted from DTC_GOES for each day
#t0_output - the timestamps to estimate LST as output, in hours since the sunrise of startdate
#alpha - weight between nRMSE of the fitted curve for ECOSTRESS and that for GOES in the loss function
#alpha_cali - whether to calibrate alpha, using leave-one-out approach
#leave_one_out - whether to report leave-one-out prediction

#output:
#alpha - calibrated alpha (1 if alpha_cali=F)
#t_max - t_max from input (time when LST reaches daily maximum, in hours since sunrise)
#t_ss - t_ss from input (sunset time, in hours since sunrise)
#T_sr.est - fitted sunrise temperature for each day
#T_max.est - fitted maximum temperature for each day
#leave_one_out_pred - leave-one-out prediction for each ECOSTRESS LST observation (corresponding to temps0_Eco)
#temps.est_Eco - estimated ECOSTRESS LST at time t0_output (if t0_output is not NULL)

DTC_Ecostress=function(t0_Eco,temps0_Eco,t0_GOES,temps0_GOES,t_max_GOES,t_ss_GOES,T_sr_GOES, T_max_GOES,t0_output=NULL,alpha=1,alpha_cali=T,leave_one_out=T){
  #calibrate for alpha
  if (alpha_cali){
    alpha_set=seq(0,1,0.05)
    rmse=rep(NA,length(alpha_set))
    
    for (j in 1:length(alpha_set)){
      alpha0=alpha_set[j]
      LOO_pred=rep(NA,length(t0_Eco))
      
      for (i in 1:length(t0_Eco)){
        #remove NA values in Ecostress
        tmp=na.omit(data.frame(t0_Eco[-i],temps0_Eco[-i]))
        t0_Eco2=tmp$t0_Eco
        temps0_Eco2=tmp$temps0_Eco
        
        params.est_Eco <- diurn.fit_Eco(temps0_Eco2, t0_Eco2,temps0_GOES,t0_GOES,alpha0,t_max=t_max_GOES, t_ss=t_ss_GOES,
                                     T_sr.guess=T_sr_GOES[1], T_max.guess=T_max_GOES[1],
                                     T_sr_diff =T_sr_GOES-T_sr_GOES[1],
                                     T_max_diff =T_max_GOES-T_max_GOES[1])
        
        result=diurn(T_sr=params.est_Eco$T_sr.est, T_max=params.est_Eco$T_max.est,
                     t_max=params.est_Eco$t_max.est, t_ss=params.est_Eco$t_ss.est,
                     t0_Eco[i])
        if(!is.null(result)){
          LOO_pred[i]=result
        }
      }
      rmse[j]=calRMSE(temps0_Eco,LOO_pred)
    }
    # plot(alpha_set,rmse,type="l",xlab="alpha",ylab="RMSE")
    if (sum(!is.na(rmse))>0){
      alpha=alpha_set[rmse==min(rmse)][1]
    }else{
      alpha=NA
    }
  }
  
  #leave-one-out for Eco overpasses
  leave_one_out_pred=rep(NA,length(t0_Eco))
  if (leave_one_out){
    for (i in 1:length(t0_Eco)){
      #remove NA values in Ecostress
      tmp=na.omit(data.frame(t0_Eco[-i],temps0_Eco[-i]))
      t0_Eco2=tmp$t0_Eco
      temps0_Eco2=tmp$temps0_Eco
      
      params.est_Eco <- diurn.fit_Eco(temps0_Eco2, t0_Eco2,temps0_GOES,t0_GOES,alpha,t_max=t_max_GOES, t_ss=t_ss_GOES,
                                   T_sr.guess=T_sr_GOES[1], T_max.guess=T_max_GOES[1],
                                   T_sr_diff =T_sr_GOES-T_sr_GOES[1],
                                   T_max_diff =T_max_GOES-T_max_GOES[1])
      
      
      pred_tmp=diurn(T_sr=params.est_Eco$T_sr.est, T_max=params.est_Eco$T_max.est,
                     t_max=params.est_Eco$t_max.est, t_ss=params.est_Eco$t_ss.est,
                     t0_Eco[i])
      if (is.numeric(pred_tmp)){
        leave_one_out_pred[i]=pred_tmp
      }
    }
  }
  
  #prediction at given time, calibrated with all data
  if (!is.null(t0_output)){
    #remove NA values in Ecostress
    tmp=na.omit(data.frame(t0_Eco,temps0_Eco))
    t0_Eco2=tmp$t0_Eco
    temps0_Eco2=tmp$temps0_Eco
    
    params.est_Eco <- diurn.fit_Eco(temps0_Eco2, t0_Eco2,temps0_GOES,t0_GOES,alpha,t_max=t_max_GOES, t_ss=t_ss_GOES,
                                 T_sr.guess=T_sr_GOES[1], T_max.guess=T_max_GOES[1],
                                 T_sr_diff =T_sr_GOES-T_sr_GOES[1],
                                 T_max_diff =T_max_GOES-T_max_GOES[1])
    
    pred_tmp=diurn(T_sr=params.est_Eco$T_sr.est, T_max=params.est_Eco$T_max.est,
                   t_max=params.est_Eco$t_max.est, t_ss=params.est_Eco$t_ss.est,
                   t0_output)
    if (is.numeric(pred_tmp)){
      temps.est_Eco=pred_tmp
    }else{
      temps.est_Eco=rep(NA,length(t0_output))
    }
  }else{
    temps.est_Eco=NULL
  }
  
  return(list(alpha=alpha,t_max=params.est_Eco$t_max.est,t_ss=params.est_Eco$t_ss.est,
              T_sr.est=params.est_Eco$T_sr.est,T_max.est=params.est_Eco$T_max.est,
              leave_one_out_pred=leave_one_out_pred, temps.est_Eco=temps.est_Eco))
}


#predict ECOSTRESS LST at given time, using fitted parameters
DTC_predict=function(T_sr,T_max,t_max,t_ss,t0_output){
  pred_tmp=diurn(T_sr=params.est_Eco$T_sr.est, T_max=params.est_Eco$T_max.est,
                 t_max=params.est_Eco$t_max.est, t_ss=params.est_Eco$t_ss.est,
                 t0_output)
  if (is.numeric(pred_tmp)){
    temps.est_Eco=pred_tmp
  }else{
    temps.est_Eco=rep(NA,length(t0_output))
  }
  return(temps.est_Eco)
}

######################################################################
#calculate RMSE
calRMSE=function(x,y){
  tmp=na.omit(data.frame(x,y))
  x=tmp$x
  y=tmp$y
  error=x-y
  return(sqrt(sum(error^2)/length(error)))
}


