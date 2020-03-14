# --- library of packages to load
library(data.table)
library(gdata)
library(janitor)
library(dplyr)
library(stringr)
library(sqldf)
library(gsubfn)
library(forecast)
library(psych)

# --- directories
input_loc= file.path("~/Desktop/Task/Task 2 - TS Engines/1. Inputs/") 
output_loc = file.path("~/Desktop/Task/Task 2 - TS Engines/3. Outputs/") 
training_name="train.csv"
test_x_name="test_x.csv"
test_y_name="test_y.csv"
training_loc=paste(input_loc,training_name,sep="")
test_x_loc=paste(input_loc,test_x_name,sep="")
test_y_loc=paste(input_loc,test_y_name,sep="")

# --- import data, create headers
training=data.frame(read.csv(training_loc, header=FALSE))
test_x=data.frame(read.csv(test_x_loc, header=FALSE))
test_y= data.frame(read.csv(test_y_loc, header=FALSE))
ts_headers=c("Unit","Time","Setting_1","Setting_2","Setting_3")
for (i in 1:21){
  sensor_name = paste("Sensor_",i,sep="")
  ts_headers= append(ts_headers,sensor_name)
}
colnames(training)=ts_headers
colnames(test_x)=ts_headers
#---log transform all sensor data to remove 0's & negatives
for (i in 1:21){
  new_name = paste("ln_Sensor_",i,sep="")
  training[,new_name]=log((training[,i+5])+1)
  test_x[,new_name]=log((test_x[,i+5])+1)
}

# --- pick unit with max time values, to match test_y
unit_most_time=training[training$Time==max(training$Time),]$Unit # unit 69

# --- convert to time series
training_ts=ts(training[training$Unit==unit_most_time,])
test_x_ts=ts(test_x[test_x$Unit==unit_most_time,])
test_y_ts=ts(test_y)

# --- summary stats of data
summary_training=describe(training_ts)
saveRDS(summary_training,paste(output_loc,"Summary_training.RDS",sep=""))
write.csv(summary_training,paste(output_loc,"Summary_training.csv",sep=""))
summary_test_x=describe(test_x_ts)
saveRDS(summary_test_x,paste(output_loc,"Summary_training.RDS",sep=""))
write.csv(summary_test_x,paste(output_loc,"Summary_training.csv",sep=""))

# --- Stationary Testing 
# PP test - H0: unit root, H1: stationary
sensor_list=c(colnames(training_ts)[(6:26)])
sensor_list_ln=c(colnames(training_ts)[-(1:26)])
pval=c()
pval_ln=c()
stationary=c()
stationary_ln=c()
no_var=c()
no_var_ln=c()
print("Conducting PP-test for stationarity")
for (i in 1:length(sensor_list)){
  if (var(training_ts[,sensor_list[i]])==0){
    pval[i]=1
    no_var[i]=1
    stationary[i]=0
  } else {
    pval[i]=PP.test(training_ts[,sensor_list[i]])$p.value
    no_var[i]=0
    if(pval[i]<=0.05){
      stationary[i]=1 # reject null = stationary 
    } else {
      stationary[i]=0 # accept null = not stationary 
    }
  }
}
for (i in 1:length(sensor_list_ln)){
  if (var(training_ts[,sensor_list_ln[i]])==0){
    pval_ln[i]=1
    no_var_ln[i]=1
    stationary_ln[i]=0
  } else {
    pval_ln[i]=PP.test(training_ts[,sensor_list_ln[i]])$p.value
    no_var_ln[i]=0
    if(pval_ln[i]<=0.05){
      stationary_ln[i]=1
    } else {
      stationary_ln[i]=0
    }
  }
}
pptest_sens=cbind(sensor_list,pval,no_var,stationary)
pptest_ln_sens=cbind(sensor_list_ln,pval_ln,no_var_ln,stationary_ln)
pptest_training=cbind(pptest_sens,pptest_ln_sens)
saveRDS(pptest_training,paste(output_loc,"PP_test_training.RDS",sep=""))
write.csv(pptest_training,paste(output_loc,"PP_test_training.csv",sep=""))

#Produce jpeg seasonality & trend plots
#training data 
for (i in 27:47){
  jpeg(paste(output_loc,"stationary training ln/",colnames(training_ts)[i],"_train.jpeg",sep=""),
       width = 1800, height = 1000, units = "px")
  par(mfrow=c(2,3))
  sensor_test_log=training_ts[,i]
  sensor_test_log_d1=diff(sensor_test_log, lag=1, difference=1)
  fit=lm(sensor_test_log~training_ts[,2])
  yt<-fit$fitted.values
  zt<-fit$residuals
  ts.plot(sensor_test_log,main=paste(colnames(training_ts)[i], "_train",sep=""), ylab="Value")
  lines(yt, col="red", type="l")
  ts.plot(sensor_test_log_d1,main="Differenced",ylab="Change in value") 
  plot(zt, col="blue",type="l",xlab="Time",ylab="Residuals", main="Detrended")
  if (var(sensor_test_log)!=0) {
    acf(sensor_test_log, main="",ylab= "Sample ACF") 
    acf(sensor_test_log_d1,main="",ylab= "Sample ACF")
  } else { # some sensors were all fixed values, so it is irrelivent for study
    plot(var(sensor_test_log), main = "this sensor no variance", ylab="Varience")
    #plot(var(sensor_test_log_d1), main = "this sensor no variance", ylab="Varience")
  }
  dev.off()
}
for (i in 6:26){
  jpeg(paste(output_loc,"stationary training/",colnames(training_ts)[i],"_train.jpeg",sep=""),
       width = 1800, height = 1000, units = "px")
  par(mfrow=c(2,3))
  sensor_test_log=training_ts[,i]
  sensor_test_log_d1=diff(sensor_test_log, lag=1, difference=1)
  fit=lm(sensor_test_log~training_ts[,2])
  yt<-fit$fitted.values
  zt<-fit$residuals
  ts.plot(sensor_test_log,main=paste(colnames(training_ts)[i], "_train",sep=""), ylab="Value")
  lines(yt, col="red", type="l")
  ts.plot(sensor_test_log_d1,main="Differenced",ylab="Change in value") 
  plot(zt, col="blue",type="l",xlab="Time",ylab="Residuals", main="Detrended")
  if (var(sensor_test_log)!=0) {
    acf(sensor_test_log, main="",ylab= "Sample ACF") 
    acf(sensor_test_log_d1,main="",ylab= "Sample ACF")
  } else {
    plot(var(sensor_test_log), main = "this sensor no variance", ylab="Varience")
  }
  dev.off()
}
# test data
for (i in 27:47){
  jpeg(paste(output_loc,"stationary test x ln/",colnames(training_ts)[i],"_test.jpeg",sep=""),
       width = 1800, height = 1000, units = "px")
  par(mfrow=c(2,3))
  sensor_test_log=test_x_ts[,i]
  sensor_test_log_d1=diff(sensor_test_log, lag=1, difference=1)
  fit=lm(sensor_test_log~test_x_ts[,2])
  yt<-fit$fitted.values
  zt<-fit$residuals
  ts.plot(sensor_test_log,main=paste(colnames(test_x_ts)[i],"_test",sep=""), ylab="Value")
  lines(yt, col="red", type="l")
  ts.plot(sensor_test_log_d1,main="Differenced",ylab="Change in value") 
  plot(zt, col="blue",type="l",xlab="Time",ylab="Residuals", main="Detrended")
  if (var(sensor_test_log)!=0) {
    acf(sensor_test_log, main="",ylab= "Sample ACF") 
    acf(sensor_test_log_d1,main="",ylab= "Sample ACF")
  } else {
    plot(var(sensor_test_log), main = "this sensor no variance", ylab="Varience")
  }
  dev.off()
}
for (i in 6:26){
  jpeg(paste(output_loc,"stationary test x/",colnames(training_ts)[i],"_test.jpeg",sep=""),
       width = 1800, height = 1000, units = "px")
  par(mfrow=c(2,3))
  sensor_test_log=test_x_ts[,i]
  sensor_test_log_d1=diff(sensor_test_log, lag=1, difference=1)
  fit=lm(sensor_test_log~test_x_ts[,2])
  yt<-fit$fitted.values
  zt<-fit$residuals
  ts.plot(sensor_test_log,main=paste(colnames(test_x_ts)[i],"_test",sep=""), ylab="Value")
  lines(yt, col="red", type="l")
  ts.plot(sensor_test_log_d1,main="Differenced",ylab="Change in value") 
  plot(zt, col="blue",type="l",xlab="Time",ylab="Residuals", main="Detrended")
  if (var(sensor_test_log)!=0) {
    acf(sensor_test_log, main="",ylab= "Sample ACF") 
    acf(sensor_test_log_d1,main="",ylab= "Sample ACF")
  } else {
    plot(var(sensor_test_log), main = "this sensor no variance", ylab="Varience")
  }
  dev.off()
}
par(mfrow=c(1,1)) 

# is d needed? variance table 
sensor_list=c(colnames(training_ts)[(6:26)]); sensor_list_ln=c(colnames(training_ts)[-(1:26)])
variances=c(); variances_ln=c()
variances_diff=c(); variances_diff_ln=c()
variances_diff2=c(); variances_diff2_ln=c()
d_values=c(); d_values_ln=c()
for (i in 1:length(sensor_list)){
    sensor=training_ts[,sensor_list[i]]
    variances[i]=var(sensor)
    variances_diff[i]=var(diff(sensor, lag=1, difference=1))
    variances_diff2[i]=var(diff(sensor, lag=1, difference=2))
    min_var=min(variances[i],variances_diff[i],variances_diff2[i])
    if(min_var==0) {d_values[i]="N/A"
    } else if(min_var==variances[i]) {d_values[i]=0
    } else if(min_var==variances_diff[i]) {d_values[i]=1 
    } else d_values[i]=2
}
for (i in 1:length(sensor_list_ln)){
  sensor=training_ts[,sensor_list_ln[i]]
  variances_ln[i]=var(sensor)
  variances_diff_ln[i]=var(diff(sensor, lag=1, difference=1))
  variances_diff2_ln[i]=var(diff(sensor, lag=1, difference=2))
  min_var=min(variances_ln[i],variances_diff_ln[i],variances_diff2_ln[i])
  if(min_var==0) {d_values_ln[i]="N/A"
  } else if(min_var==variances_ln[i]) {d_values_ln[i]=0
  } else if(min_var==variances_diff_ln[i]) {d_values_ln[i]=1 
  } else d_values_ln[i]=2
}
variance_d=cbind(sensor_list,variances,variances_diff,variances_diff2,d_values)
variance_d_ln=cbind(sensor_list_ln,variances_ln,variances_diff_ln,variances_diff2_ln,d_values_ln)
variance_d_training=cbind(variance_d,variance_d_ln)
saveRDS(variance_d_training,paste(output_loc,"Variance_d_training.RDS",sep=""))
write.csv(variance_d_training,paste(output_loc,"Variance_d_training.csv",sep=""))

# drop variables with 0 variance (... or N/A d's or no SACF plots)
# use only log transformed data
training_ts_ln=data.frame(training_ts[,-c(1:26)])
test_x_ts_ln=data.frame(test_x_ts[,-c(1:26)])
new_sensor_list_ln=colnames(training_ts_ln)
dropped_sensors=c()
dropped_sensors_test=c()
for (i in 1:length(new_sensor_list_ln)){
  var_sensor=var(training_ts_ln[,i])
  var_sensor_test=var(test_x_ts_ln[,i])
 if (var_sensor==0){
    dropped_sensors[i]=new_sensor_list_ln[i]
  } else {
    dropped_sensors[i]=""
  }
  if (var_sensor_test==0){
    dropped_sensors_test[i]=new_sensor_list_ln[i]
  } else {
    dropped_sensors_test[i]=""
  }
}
dropped_sensors=dropped_sensors[dropped_sensors!=""]
dropped_sensors_test=dropped_sensors_test[dropped_sensors_test!=""]
training_ts_ln_clean=training_ts_ln[,-which(names(training_ts_ln) %in% dropped_sensors)]
test_x_ts_ln_clean=test_x_ts_ln[,-which(names(test_x_ts_ln) %in% dropped_sensors)] #sensor 6 cannot be used


# --- Model selection
sensor_selected=training_ts_ln[,9] #least trend & already stationary
d=0 # from table 
#guess = AR(1) due to SACF plot drop off point 
model_1=auto.arima(sensor_selected) #arima(1,1,2)
#BIC=-4484.76, AIC= - 4500.32
model_2=arima(sensor_selected,c(1,0,0))
# AIC= -4513.31
# therefore AR(1) is the better fit
npar=length(model_2$coef)+1
nstar=length(model_2$residuals)-model_2$arma[6]-model_2$arma[7]*-model_2$arma[5]
bic_model_2=model_2$aic+npar*(log(nstar)-2)
#-4501.633
# therefore AR(1) is the better fit


