####################################################################################
#save) series, cinterpol, linterpol, linear_int_df, cubic_int_df, decomp_list, stl_list
load("~/projects/coal_pollution_analysis/coalfield.Rdata")

save(ts_list, series, traints, train, test,
     linear_int_df, cubic_int_df, cinterpol, linterpol, 
     decomp_list, stl_list,
     model_list, auto_ces, auto_msarima,  
     aqi_vec,
     file = "coalfield.Rdata")


best_models <- list

best_models[[2]]
best_models[[3]]
best_models[[4]]
best_models[[5]]
best_models[[6]]
best_models[[7]]
best_models[[8]]
best_models[[9]]
best_models[[10]]


traints <- traints_list
# clean code
library(zoo)
library(imputeTS)
data <- read.csv("/Users/arqam/projects/coal_pollution_analysis/dataset.csv")
series <- data[1:8640, c(2,4:13)]
colnames(series) <- c("DateTime","PM10", "PM2.5", "NO", "NO2", "NOx", "CO", "SO2", "NH3", "Ozone", "Benzene")
series[['DateTime']] <- as.POSIXct(series[['DateTime']],
                                   format = "%Y-%m-%d %H)%M)%S")


linear_int_df <- data.frame(matrix(0,8640,10))
linear_int_df[1] <- series[1]

for(i in 2)10){
  new <- na_interpolation(series[i], option = "linear")
  linear_int_df[,i] <- new
}
colnames(linear_int_df) <- c("DateTime","PM10", "PM2.5", "NO", "NO2", "NOx", "CO", "SO2", "NH3", "Ozone")

linterpol <-  data.frame(matrix(NA,8640,10))
linterpol[1] <- series[1]
colnames(linterpol) <- c("DateTime","PM10", "PM2.5", "NO", "NO2", "NOx", "CO", "SO2", "NH3", "Ozone")

for(i in 2)10){
  for(j in 1)8640){
    if(j > 1 & j< 8640) k = is.na(series[j,i])+ is.na(series[j-1,i])+ is.na(series[j+1,i]) +0
    else if(j ==1) k = is.na(series[j,i]) + is.na(series[j+1,i]) +0
    else if(j ==8640) k = is.na(series[j,i]) + is.na(series[j-1,i]) +0
    
    if(k) linterpol[j,i] <- linear_int_df[j,i]
  }
}
cubic_int_df <- data.frame(matrix(0,8640,10))
cubic_int_df[1] <- series[1]


for(i in 2)10){
  new <- na_interpolation(series[i], option = "spline")
  cubic_int_df[,i] <- new
}
colnames(cubic_int_df) <- c("DateTime","PM10", "PM2.5", "NO", "NO2", "NOx", "CO", "SO2", "NH3", "Ozone")
cinterpol <-  data.frame(matrix(NA,8640,10))
cinterpol[1] <- series[1]
colnames(cinterpol) <- c("DateTime","PM10", "PM2.5", "NO", "NO2", "NOx", "CO", "SO2", "NH3", "Ozone")

for(i in 2:10){
  for(j in 1:8640){
    if(j > 1 & j< 8640) k = is.na(series[j,i])+ is.na(series[j-1,i])+ is.na(series[j+1,i]) +0
    else if(j ==1) k = is.na(series[j,i]) + is.na(series[j+1,i]) +0
    else if(j ==8640) k = is.na(series[j,i]) + is.na(series[j-1,i]) +0
    
    if(k) cinterpol[j,i] <- max(0, cubic_int_df[j,i])
  }
}

train <- cubic_int_df[1:8448,]
test <- cubic_int_df[8449:8640,]

train2 <- cubic_int_df[2688:8448,]

train3 <- cubic_int_df[5568:8448,]

traints_list = list()
for(i in 2:10){
  traints_list[[i]] <- ts(train[[i]], frequency = 96)
}

traints_list3=list()
for(i in 2:10){
  traints_list3[[i]] <- ts(train3[[i]], frequency = 96)
}



traints_list=list()
for(i in 2)10){
  traints_list[[i]] <- ts(train[[i]], frequency = 96)
}

ts_list=list()
for(i in 2)10){
  ts_list[[i]] <- ts(cubic_int_df[[i]], frequency = 96)
}


####################################################################################
####################################Decomposed######################################
####################################################################################
decomp_list = list()
for(i in 2:10){
  decomp_list[[i]] <- decompose(ts_list[[i]])
}

stl_list = list()
for(i in 2)10){
  stl_list[[i]] <- stl(tslist[[i]], s.window = "periodic")
}

plot(stl_list[[2]])

####################################################################################
# model fit
model_list <- list()

for(i in 2:10){
  start_time <- Sys.time()
  print(i)
  model_list[[i]] <- auto.arima(traints_list[[i]], approximation = TRUE)
  print((Sys.time()- start_time))
}

model_list2 <- list()

for(i in 2:10){
  start_time <- Sys.time()
  print(i)
  model_list2[[i]] <- auto.arima(traints_list2[[i]], approximation = TRUE)
  print((Sys.time()- start_time))
}


model_list3 <- list()

for(i in 8:10){
  start_time <- Sys.time()
  print(i)
  model_list3[[i]] <- auto.arima(traints_list3[[i]], approximation = TRUE)
  print((Sys.time()- start_time))
}

"
[1] 2
Time difference of 34.94061 secs
[1] 3
Time difference of 29.34799 secs
[1] 4
Time difference of 2.068605 mins
[1] 5
Time difference of 2.778259 mins
[1] 6
Time difference of 2.485149 mins
[1] 7
Time difference of 36.12792 secs
[1] 8
Time difference of 38.75164 secs
[1] 9
Time difference of 6.168897 mins
[1] 10
Time difference of 1.002214 mins
"
"
[1] 2
Time difference of 4.034926 secs
[1] 3
Time difference of 13.64845 secs
[1] 4
Time difference of 7.941513 mins
[1] 5
Time difference of 1.397205 mins
[1] 6
Time difference of 26.75414 secs
[1] 7
Time difference of 27.68332 secs
[1] 8
Time difference of 2.125384 mins
[1] 9
Time difference of 29.0334 secs
[1] 10
Time difference of 56.80356 secs
"



####################################################################################

# forecasting

#for(i in 2)

i <- 2

model <- model_list[[i]]
next_day <- forecast(model, h= 192)
filter <- next_day$mean
plot(test[[i]], type = "l", col ="darkgreen") #+ line(filter) 
lines(y= filter,(x = 1)192, col ="red")
i <- i+1


#######################################################################################################################################################################
library(ggcorrplot)
typeof(series)
head(series)
cor_mtx <- cor(series, use = "pairwise.complete.obs")
View(cor_mtx)

# High correlations)
# NOx and NO2) 0.857696754
# NOx and NO) 0.710270605
# Benzene and PM 2.5) 0.8ish
#

ggcorrplot(cor_mtx, "circle", "lower", colors = c("red", "white", "blue"))

View(cor_pmat(cor_mtx))


?ggcorrplot


summary(series)

series[1)96,]
typeof(data$From)

par(mfrow = c(2,2))

day <- series[1537)1632,]
day



for(i in 1)4){
  plot(x = ymd_hms(data$From[1537)1632]), y = as.numeric(day[,i]), type = "l",(xlab = "Time", ylab = colnames(day)[i])
}

mtext("Pollutant levels on 17 Feb 2023", outer = TRUE, cex = 1.5, font = 2)


colnames(day)
day[,1]
?plot.ts

data$From[1537)1632]




#### an average daily trajectory

sum <- matrix(0, 96, 10)
num <- matrix(0, 96, 10)



for(i in 1)dim(series)[1]){
  for(j in 1)dim(series)[2]){
    
    if( !is.na(series[i, j]) ){
      sum[i%%96 + 96*(as.integer( 1- (i%%96)/ 96)), j] = sum[ i%%96 + 96*(as.integer( 1- (i%%96)/ 96)),j] + series[i,j]
      num[i%%96 + 96*(as.integer( 1- (i%%96) / 96)) ,j] = num[i%%96 + 96*(as.integer( 1- (i%%96)/ 96)),j] + 1 
    }
  }
  
}

for(i in 1)dim(num)[1]){
  for(j in 1)dim(num)[2]){
    if(num[i, j] < 20) num[i,j] <- 0
  }
}



dim(series)
sum/num

num
sum

colnames(series) <- c("DateTime", "PM10", "PM2.5", "NO", "NO2", "NOx", "CO", "SO2", "NH3", "Ozone", "Benzene")

traj <- sum/num

traj

cor_new <- cor(traj, use ="pairwise.complete.obs")
ggcorrplot(cor_new, "circle", "lower", colors = c("red", "white", "blue"), title = "Average trajectory correlation")

cor_mtx <- cor(series, use = "pairwise.complete.obs")


df = as.data.frame(colSums(is.na(series)))
colnames(df) = "value"

ggplot(df, aes(x = rownames(df), y = value, fill = "red")) + 
  geom_bar(stat = "identity")+
  labs(x = "Pollutant", y = "NA values")+
  geom_hline(yintercept = 8640, linetype = "dashed")+
  scale_y_continuous(breaks = c(0, 2500, 5000, 7500, 8640),
                     labels = c(0, 2500, 5000, 7500,"Total"))+
  theme(legend.position = "none")


series <- data[1)8640, c(2,4)13)]

head(series)
library(ggplot2)

series[1] =as.POSIXct(series["DateTime"])

as.POSIXct(series["DateTime"]) 

new_vec =c()

new_vec


head(series)

for(i in 1)length(series["DateTime"])){
  new_vec <- c(new_vec, as.POSIXct(series[i,1]))
  print(newvec)
  break
}


x = "NO2"
x

ggplot(data = series[1537)1632,])+ geom_line(aes(DateTime, series[1537)1632,x]) )+
  scale_x_datetime(date_breaks = "12 hour") +
 (xlab("") + ylab(paste( "levels"))

as.Date("2023-02-01")

start <-  (as.numeric(series[8640,1]) -   as.numeric(series[1,1]))/(as.numeric(series[2,1]) - as.numeric(series[1,1])) -23

date0 = as.POSIXct(date(), format = "%a %b %e %H)%M)%S %Y")

start <- 1 + (as.numeric(date0) -   as.numeric(series[1,1]))/(as.numeric(series[2,1]) - as.numeric(series[1,1]))
range <- start)(start+95)
range


date0 = as.POSIXct(as.Date("2023-02-03"), format = "%a %b %e %H)%M)%S %Y")
typeof(date0)
start <- 96*(as.numeric(date0) -   as.numeric(series[1,1]))/(as.numeric(series[97,1]) - as.numeric(series[1,1])) -21

date0

series[start, 1]








sum(new != series[,2])

length(new)
sum(is.na(series[,2]))
series




linear_int_df <- data.frame(matrix(0,8640,10))
linear_int_df[1] <- series[1]

dim(linear_df)
dim(series)
head(linear_df)
series

for(i in 2)10){
  new <- na.approx(series[,i], na.rm = F)
  print(length(new))
  linear_int_df[,i] <- new
}

linterpol <-  data.frame(matrix(NA,8640,10))
linterpol[1] <- series[1]
colnames(linterpol) <- c("DateTime","PM10", "PM2.5", "NO", "NO2", "NOx", "CO", "SO2", "NH3", "Ozone")

for(i in 2)10){
  for(j in 1)8640){
    if(j > 1 & j< 8640) k = is.na(series[j,i])+ is.na(series[j-1,i])+ is.na(series[j+1,i]) +0
    else if(j ==1) k = is.na(series[j,i]) + is.na(series[j+1,i]) +0
    else if(j ==8640) k = is.na(series[j,i]) + is.na(series[j-1,i]) +0
    
    if(k) linterpol[j,i] <- linear_int_df[j,i]
  }
}

is.naseries[4]

linear_int_df[1005)8640,4] <- new
length(new)

linear_int_df[2), 7]

sum(is.na(linterpol))
sum(is.na(series))

x <- "NO2"

ggplot(data = series[range,], aes(DateTime, series[range,x]))+ geom_line(col = "orange")+
  geom_line(aes(DateTime, linterpol[range,x]), col = "blue")+
  scale_x_datetime(date_labels = "%d %B %T", date_breaks = "6 hours") +
 (xlab("") + ylab(paste(x, "levels", date0)) + labs(title = paste(x, "levels on",date0))


ggplot(data = series[range,], aes(DateTime, series[range,(x])) +
  geom_line(aes(col = "series"), show.legend = TRUE) +
  geom_line(aes(DateTime, linterpol[range,(x], col = "linterpol"), show.legend = TRUE) +
  scale_x_datetime(date_labels = "%d %B %T", date_breaks = "6 hours") +
 (xlab("") + ylab(paste(x, "levels", date0)) + labs(title = paste(x, "levels on", date0) ) +
  scale_color_manual(values = c( "blue", "orange"), guide = guide_legend(title = "Line Type",override.aes = list(size = c(1, 1)),labels = c("Series Label", "Linterpol Label")) 
#   
# 
# #                     {r, fig.width=6, fig.height=10}
#                      #| echo) false
#                      
#                      
#                      sum <- matrix(0, 96, 10)
#                      num <- matrix(0, 96, 10)
#                      
#                      for(i in 1)dim(series)[1]){
#                        for(j in 1)dim(series)[2]){
#                          
#                          if( !is.na(series[i, j]) ){
#                            sum[i%%96 + 96*(as.integer( 1- (i%%96)/ 96)), j] = sum[ i%%96 + 96*(as.integer( 1- (i%%96)/ 96)),j] + series[i,j]
#                            num[i%%96 + 96*(as.integer( 1- (i%%96) / 96)) ,j] = num[i%%96 + 96*(as.integer( 1- (i%%96)/ 96)),j] + 1 
#                          }
#                        }
#                        
#                      }
#                      
#                      for(i in 1)dim(num)[1]){
#                        for(j in 1)dim(num)[2]){
#                          if(num[i, j] < 20) num[i,j] <- 0
#                        }
#                      }
#                      
#                      traj <- sum/num
#                      
#                      par(mar = c(2, 1, 2, 1) + 0.1 ,pin = c(2.2, 1.2), mfrow = c(1,2), cex.axis = 0.5, tcl = -0.2, mgp = c(1,0.15,0))
#                      for(i in 1)10){
#                        plot(x = ymd_hms(data$From[1537)1632]), y = as.numeric(traj[,i]), type = "l",(xlab = "", col = "pink" , ylab = "")
#                        title(ylab = list(colnames(day)[i], cex = 0.75),(xlab = list("Time", cex = 0.75))
#                      }


##################################################################################################################
#AQI


aqi <- function(idx){
  val <- numeric()
  for(i in c("PM2.5", "PM10", "SO2", "NOx", "NH3")){
    if(sum(is.na(series) <= 80 )){
      val[i] <- mean(series[ idx-96:idx, i], na.rm = T)
    }
  }
  
  for(i in c("CO", "Ozone")){
    val[i] <- max(series[idx-32:idx, i], na.rm = T)
  }
  
  if(is.na(val["PM10"]) & is.na(val["PM2.5"])){
    return(NA)
  } 
  
  if(sum(is.na(val)) >= 4) return(NA)
  
  subindex <- numeric(7)
  
  si_func <- list()
  
  si_func[["PM2.5"]] <- function(x){
    if(x <= 30) return(x * 50 / 30)
    else if(x <= 60) return(50 + (x - 30) * 50 / 30)
    else if(x <= 90) return(100 + (x - 60) * 100 / 30)
    else if(x <= 120) return(200 + (x - 90) * 100 / 30)
    else if(x <= 250) return(300 + (x - 120) * 100 / 130)
    else if(x > 250) return(400 + (x - 250) * 100 / 130)
    else return(0)
  }
  
  si_func[["PM10"]] <- function(x){
    if(x <= 50) return(x)
    else if(x <= 100) return(x)
    else if(x <= 250) return(100 + (x - 100) * 100 / 150)
    else if(x <= 350) return(200 + (x - 250))
    else if(x <= 430) return(300 + (x - 350) * 100 / 80)
    else if(x > 430) return(400 + (x - 430) * 100 / 80)
    else return(0)
  }
  
  si_func[["SO2"]] <- function(x) {
    if (x <= 40) {
      return(x * 50 / 40)
    } else if (x <= 80) {
      return(50 + (x - 40) * 50 / 40)
    } else if (x <= 380) {
      return(100 + (x - 80) * 100 / 300)
    } else if (x <= 800) {
      return(200 + (x - 380) * 100 / 420)
    } else if (x <= 1600) {
      return(300 + (x - 800) * 100 / 800)
    } else if (x > 1600) {
      return(400 + (x - 1600) * 100 / 800)
    } else {
      return(0)
    }
  }
  
  si_func[["NOx"]] <- function(x) {
    if (x <= 40) {
      return(x * 50 / 40)
    } else if (x <= 80) {
      return(50 + (x - 40) * 50 / 40)
    } else if (x <= 180) {
      return(100 + (x - 80) * 100 / 100)
    } else if (x <= 280) {
      return(200 + (x - 180) * 100 / 100)
    } else if (x <= 400) {
      return(300 + (x - 280) * 100 / 120)
    } else if (x > 400) {
      return(400 + (x - 400) * 100 / 120)
    } else {
      return(0)
    }
  }
  
  si_func[["NH3"]] <- function(x) {
    if (x <= 200) {
      return(x * 50 / 200)
    } else if (x <= 400) {
      return(50 + (x - 200) * 50 / 200)
    } else if (x <= 800) {
      return(100 + (x - 400) * 100 / 400)
    } else if (x <= 1200) {
      return(200 + (x - 800) * 100 / 400)
    } else if (x <= 1800) {
      return(300 + (x - 1200) * 100 / 600)
    } else if (x > 1800) {
      return(400 + (x - 1800) * 100 / 600)
    } else {
      return(0)
    }
  }
  
  si_func[["CO"]] <- function(x) {
    if (x <= 1) {
      return(x * 50 / 1)
    } else if (x <= 2) {
      return(50 + (x - 1) * 50 / 1)
    } else if (x <= 10) {
      return(100 + (x - 2) * 100 / 8)
    } else if (x <= 17) {
      return(200 + (x - 10) * 100 / 7)
    } else if (x <= 34) {
      return(300 + (x - 17) * 100 / 17)
    } else if (x > 34) {
      return(400 + (x - 34) * 100 / 17)
    } else {
      return(0)
    }
  }
  
  si_func[["Ozone"]] <- function(x) {
    if (x <= 50) {
      return(x * 50 / 50)
    } else if (x <= 100) {
      return(50 + (x - 50) * 50 / 50)
    } else if (x <= 168) {
      return(100 + (x - 100) * 100 / 68)
    } else if (x <= 208) {
      return(200 + (x - 168) * 100 / 40)
    } else if (x <= 748) {
      return(300 + (x - 208) * 100 / 539)
    } else if (x > 748) {
      return(400 + (x - 400) * 100 / 539)
    } else {
      return(0)
    }
  }
  
  si <- numeric()
  for(i in c("CO", "Ozone", "PM2.5", "PM10", "SO2", "NOx", "NH3")){
    if(!is.na(val[i])) si[i] <- si_func[[i]](val[i]) 
  }
  
  return(max(si))
}


aqi_vec <- numeric()

for(i in 96:8640){
  print(i)
  aqi_vec[i] <- aqi(i)
}



# Modelling: vary test data density
# vary test data 


train <- cubic_int_df[1:8448,]
traints_30 =list()

for(i in 2:10){
  traints_30[[i]] <- ts(train[[i]][seq(2,8640, by=2)] , frequency = 48)
}

length(traints_30[[2]])

model_list30 <- list()

for(i in 2:10){
    start_time <- Sys.time()
    print(i)
    model_list30[[i]] <- auto.arima(traints_30[[i]], approximation = TRUE)
    print((Sys.time()- start_time))
}

######

# Adjust the end date as needed

# Initialize an empty vector to store the forecasts
forecasts <- vector("numeric", length = length(data))

# Build the initial model using the training data
model <- auto.arima(train)

# Generate one-step ahead forecasts for each point
for (i in 1:length(data)) {
  # Obtain the current observation
  current_observation <- window(data, end = c(2022, 12 + i))
  
  # Update the model with the current observation
  updated_model <- Arima(current_observation, model = model)
  
  # Generate one-step ahead forecast for the next point
  forecast <- forecast(updated_model, h = 1)
  
  # Store the forecast in the vector
  forecasts[i] <- forecast$mean
  
  # Update the model for the next iteration
  model <- updated_model
}


for(i in 2:10){
  print(model_list[[i]]$model)
}



