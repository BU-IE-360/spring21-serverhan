# install the required packages first
require(jsonlite)
require(httr)
require(data.table)

get_token = function(username, password, url_site){
  
  post_body = list(username=username,password=password)
  post_url_string = paste0(url_site,'/token/')
  result = POST(post_url_string, body = post_body)
  
  # error handling (wrong credentials)
  if(result$status_code==400){
    print('Check your credentials')
    return(0)
  }
  else if (result$status_code==201){
    output = content(result)
    token = output$key
  }
  
  return(token)
}

get_data = function(start_date='2020-03-20', token, url_site){
  
  post_body = list(start_date=start_date,username=username,password=password)
  post_url_string = paste0(url_site,'/dataset/')
  
  header = add_headers(c(Authorization=paste('Token',token,sep=' ')))
  result = GET(post_url_string, header, body = post_body)
  output = content(result)
  data = data.table::rbindlist(output)
  data[,event_date:=as.Date(event_date)]
  data = data[order(product_content_id,event_date)]
  return(data)
}


send_submission = function(predictions, token, url_site, submit_now=F){
  
  format_check=check_format(predictions)
  if(!format_check){
    return(FALSE)
  }
  
  post_string="list("
  for(i in 1:nrow(predictions)){
    post_string=sprintf("%s'%s'=%s",post_string,predictions$product_content_id[i],predictions$forecast[i])
    if(i<nrow(predictions)){
      post_string=sprintf("%s,",post_string)
    } else {
      post_string=sprintf("%s)",post_string)
    }
  }
  
  submission = eval(parse(text=post_string))
  json_body = jsonlite::toJSON(submission, auto_unbox = TRUE)
  submission=list(submission=json_body)
  
  print(submission)
  # {"31515569":2.4,"32737302":2.4,"32939029":2.4,"4066298":2.4,"48740784":2.4,"6676673":2.4, "7061886":2.4, "73318567":2.4, "85004":2.4} 
  
  if(!submit_now){
    print("You did not submit.")
    return(FALSE)      
  }
  
  
  header = add_headers(c(Authorization=paste('Token',token,sep=' ')))
  post_url_string = paste0(url_site,'/submission/')
  result = POST(post_url_string, header, body=submission)
  
  if (result$status_code==201){
    print("Successfully submitted. Below you can see the details of your submission")
  } else {
    print("Could not submit. Please check the error message below, contact the assistant if needed.")
  }
  
  print(content(result))
  
}

check_format = function(predictions){
  
  if(is.data.frame(predictions) | is.data.frame(predictions)){
    if(all(c('product_content_id','forecast') %in% names(predictions))){
      if(is.numeric(predictions$forecast)){
        print("Format OK")
        return(TRUE)
      } else {
        print("forecast information is not numeric")
        return(FALSE)                
      }
    } else {
      print("Wrong column names. Please provide 'product_content_id' and 'forecast' columns")
      return(FALSE)
    }
    
  } else {
    print("Wrong format. Please provide data.frame or data.table object")
    return(FALSE)
  }
  
}

# this part is main code
subm_url = 'http://46.101.163.177'
u_name = "Group12"
p_word = "FBXNuZDaEgLQiD6b"
submit_now = FALSE

username = u_name
password = p_word

token = get_token(username=u_name, password=p_word, url=subm_url)
data1 = get_data(token=token,url=subm_url)

predictions=unique(data1[,list(product_content_id)])
predictions[,forecast:=2.3]

send_submission(predictions, token, url=subm_url, submit_now=F)

------------
  
  
########################### Our Code Starts Here
  
require(ggplot2)
library(caTools)
library(xts)
library(zoo)
library(forecast)
library(writexl)
library(gtools)
library(dplyr)
library(corrplot)
library(urca)
library(data.table)

##### Data is gathered and some manipulations are made.
data2 = read.csv("C:/Users/demir/Desktop/ProjectRawData(1).csv")

write.csv(data1[,c(2,3,1,4,5,6,7,8,9,10,11,12,13)],"C:/Users/demir/Desktop/onlinedata.csv", row.names = FALSE)

data3 = read.csv("C:/Users/demir/Desktop/onlinedata.csv")

data3[nrow(data3):1, ]
data4=data3[order(data3$event_date),]
data4=data4[nrow(data4):1, ]
data = rbind(data4,data2)
data=data[nrow(data):1, ]


write_xlsx(data,"C:/Users/demir/Desktop/rbinddata.xlsx")

n =nrow(data)/9
dates = seq(as.Date("2020-05-25"), length = n, by = "days")
test_dates=seq(as.Date("2021-03-01"), length = n-280, by = "days")

##### Products are created

prod1=subset(data, data$product_content_id==48740784)
prod2=subset(data, data$product_content_id==32939029)
prod3=subset(data, data$product_content_id==4066298)
prod4=subset(data, data$product_content_id==85004)
prod5=subset(data, data$product_content_id==6676673)
prod6=subset(data, data$product_content_id==7061886)
prod7=subset(data, data$product_content_id==31515569)
prod8=subset(data, data$product_content_id==73318567)
prod9=subset(data, data$product_content_id==32737302)


######################## 48740784 - Product 1 

plot(dates,prod1$sold_count,type="l", main="Product 1 Sales", xlab= "Dates", ylab= "Sales")

test=ur.kpss(prod1$sold_count)
summary(test)

prod1_xts=xts(prod1,order.by=dates)

prod1_train_xts=prod1_xts[index(prod1_xts)<"2021-03-01" & index(prod1_xts)>="2020-09-29"]
prod1_test_xts=prod1_xts[index(prod1_xts)>="2021-03-01"]

##### Check correlations to add regressors to use them in ARIMA models and regressions

cor(prod1$sold_count, prod1$price)
cor(prod1$sold_count, prod1$favored_count)
cor(prod1$sold_count, prod1$basket_count)
cor(prod1$sold_count, prod1$category_sold)

prod1_xreg1=cbind(prod1$price[128:280],prod1$basket_count[128:280],prod1$category_sold[128:280])
prod1_xreg2=cbind(prod1$price[281:n],prod1$basket_count[281:n], prod1$category_sold[281:n])

##### Checking autocorrelation and partial autocorrelation plots to determine which order we can use in Arima model.

acf(prod1$sold_count)
pacf(prod1$sold_count)

arima_prod1=Arima(as.numeric(prod1_train_xts$sold_count),xreg=as.matrix(prod1_xreg1),order=c(1,0,0))
AIC(arima_prod1)

forecast_arima_prod1=forecast(arima_prod1,xreg=as.matrix(prod1_xreg2))
prod1_arima_fit=xts(forecast_arima_prod1$mean,order.by=test_dates)

plot(dates,prod1$sold_count, type="l")
lines(test_dates,prod1_arima_fit, col ="blue")

#####

prod1_reg_train=prod1[128:280,]
prod1_reg_test=prod1[281:n,]

prod1_reg=lm(sold_count ~ -1+category_sold+basket_count,data=prod1_reg_train)
summary(prod1_reg)

predict_prod1_reg=predict(prod1_reg,prod1_reg_test)
prod1_reg_fit= abs(xts(predict_prod1_reg,order.by=test_dates))

plot(dates,prod1$sold_count, type="l")
lines(test_dates,prod1_reg_fit, col="red")

##### Regressors which we will use in ARIMAX model are also 2 ahead forecasted.

prod1_catsol_xreg=as.numeric(prod1_xts$category_sold)
prod1_catsol_model=auto.arima(prod1_catsol_xreg)
prod1_catsol_forecast=forecast(prod1_catsol_model,h=2)

prod1_basket_xreg=as.numeric(prod1_xts$basket_count)
prod1_basket_model=auto.arima(prod1_basket_xreg)
prod1_basket_forecast=forecast(prod1_basket_model,h=2)

prod1_final_xreg1=cbind(prod1$category_sold, prod1$basket_count)
prod1_final_xreg2=data.table("category_sold"=prod1_catsol_forecast$mean,"basket_count"=prod1_basket_forecast$mean)

##### ARIMA Final 
prod1_final_xts=as.numeric(prod1_xts$sold_count)

prod1_final_arima_model=Arima(prod1_final_xts, xreg=as.matrix(prod1_final_xreg1),order=c(1,0,0))
prod1_final_forecast_arima=forecast(prod1_final_arima_model,xreg=as.matrix(prod1_final_xreg2))

##### Regression Final

prod1_reg_final=lm(sold_count~-1+basket_count+category_sold,data=prod1)

pred_prod1_reg_submodel=predict(prod1_reg_final,prod1_final_xreg2)

##### Mean Absolute Percentage Error check to determine which model we should use to forecast 2 days ahead sales value of that product.

prod1_reg_mae=mean(abs((prod1_reg_fit-as.numeric(prod1_test_xts$sold_count))))
prod1_reg_mae
prod1_arima_mae=mean(abs((prod1_arima_fit-as.numeric(prod1_test_xts$sold_count))))
prod1_arima_mae

##### Combine Them 

prod1_final=(prod1_final_forecast_arima$mean[2]+pred_prod1_reg_submodel[2])/2
prod1_final

######################### 32939029 - Product 2

plot(dates,prod2$sold_count,type="l", main="Product 2 Sales", xlab= "Dates", ylab= "Sales")

test=ur.kpss(prod2$sold_count)
summary(test)

prod2_xts=xts(prod2,order.by=dates)

prod2_train_xts=prod2_xts[index(prod2_xts)<"2021-05-01" & index(prod2_xts)>="2020-05-25"]
prod2_test_xts=prod2_xts[index(prod2_xts)>="2021-05-01"]

test_dates_prod2 = seq(as.Date("2021-05-01"), length = n-341, by = "days")

##### Check correlations to add regressors to use them in ARIMA models and regressions

cor(prod2$sold_count, prod2$price)
cor(prod2$sold_count, prod2$category_favored)
cor(prod2$sold_count, prod2$basket_count)
cor(prod2$sold_count, prod2$category_sold)

prod2_xreg1=cbind(prod2$category_sold[1:341],prod2$basket_count[1:341],prod2$category_favored[1:341])
prod2_xreg2=cbind(prod2$category_sold[342:n],prod2$basket_count[342:n], prod2$category_favored[342:n])

##### Checking autocorrelation and partial autocorrelation plots to determine which order we can use in Arima model.

acf(prod2$sold_count)
pacf(prod2$sold_count)

prod2_arima_model=Arima(as.numeric(prod2_train_xts$sold_count),xreg=as.matrix(prod2_xreg1),order=c(2,0,1))
AIC(prod2_arima_model)

prod2_forecast_arima=forecast(prod2_arima_model,xreg=as.matrix(prod2_xreg2))
prod2_arima_fit=xts(prod2_forecast_arima$mean,order.by=test_dates_prod2)

plot(dates,prod2$sold_count, type="l")
lines(test_dates_prod2,prod2_arima_fit,col="blue")

######################

prod2_reg_train=prod2[1:341,]
prod2_reg_test=prod2[342:n,]

prod2_reg=lm(sold_count~-1+category_sold+basket_count+category_favored,data=prod2_reg_train)
summary(prod2_reg)

pred_prod2_reg=predict(prod2_reg,prod2_reg_test)
prod2_reg_fit=xts(pred_prod2_reg,order.by = test_dates_prod2)

plot(dates,prod2$sold_count, type="l")
lines(test_dates_prod2,prod2_reg_fit,col="red")

##### Regressors which we will use in ARIMAX model are also 2 ahead forecasted.

prod2_catfav_xreg=as.numeric(prod2_xts$category_favored)
prod2_catfav_model=auto.arima(prod2_catfav_xreg)
prod2_catfav_forecast=forecast(prod2_catfav_model,h=2)

prod2_catsol_xreg=as.numeric(prod2_xts$category_sold)
prod2_catsol_model=auto.arima(prod2_catsol_xreg)
prod2_catsol_forecast=forecast(prod2_catsol_model,h=2)

prod2_basket_xreg=as.numeric(prod2_xts$basket_count)
prod2_basket_model=auto.arima(prod2_basket_xreg)
prod2_basket_forecast=forecast(prod2_basket_model,h=2)


prod2_final_xreg1=cbind(prod2$category_sold,prod2$category_favored,prod2$basket_count)
prod2_final_xreg2=data.table("category_sold"=prod2_catsol_forecast$mean,"category_favored"=prod2_catfav_forecast$mean,"basket_count"=prod2_basket_forecast$mean)


###### ARIMA Final
prod2_final_xts=as.numeric(prod2_xts$sold_count)

prod2_final_arima_model=Arima(prod2_final_xts,xreg=as.matrix(prod2_final_xreg1),order=c(2,0,1))
prod2_final_forecast_arima=forecast(prod2_final_arima_model,xreg=as.matrix(prod2_final_xreg2))

######### Regression Final

prod2_reg_final=lm(sold_count~-1+category_sold+category_favored+basket_count,data=prod2)
pred_prod2_reg_submodel=predict(prod2_reg_final,prod2_final_xreg2)

##### Mean Absolute Percentage Error check to determine which model we should use to forecast 2 days ahead sales value of that product.

prod2_arima_mape=mean(abs((prod2_arima_fit-as.numeric(prod2_test_xts$sold_count))/as.numeric(prod2_test_xts$sold_count)))
prod2_arima_mape
prod2_reg_mape=mean(abs((prod2_reg_fit-as.numeric(prod2_test_xts$sold_count))/as.numeric(prod2_test_xts$sold_count)))
prod2_reg_mape

######## Combine Them

prod2_final=(prod2_final_forecast_arima$mean[2]+pred_prod2_reg_submodel[2])/2
prod2_final

########################## 4066298 - Product 3

plot(dates,prod3$sold_count,type="l", main="Product 3 Sales", xlab= "Dates", ylab= "Sales")

test=ur.kpss(prod3$sold_count)
summary(test)

prod3_xts=xts(prod3,order.by=dates)

prod3_train_xts=prod3_xts[index(prod3_xts)<"2021-03-01"]
prod3_test_xts=prod3_xts[index(prod3_xts)>="2021-03-01"]

##### Check correlations to add regressors to use them in ARIMA models and regressions

cor(prod3$sold_count, prod3$price)
cor(prod3$sold_count, prod3$category_favored)
cor(prod3$sold_count, prod3$basket_count)
cor(prod3$sold_count, prod3$category_sold)

prod3_xreg1=cbind(prod3$category_sold[1:280],prod3$basket_count[1:280],prod3$price[1:280], prod3$category_favored[1:280])
prod3_xreg2=cbind(prod3$category_sold[281:n],prod3$basket_count[281:n],prod3$price[281:n], prod3$category_favored[281:n])

##### Checking autocorrelation and partial autocorrelation plots to determine which order we can use in Arima model.

acf(prod3$sold_count)
pacf(prod3$sold_count)

prod3_arima_model=Arima(as.numeric(prod3_train_xts$sold_count),xreg=as.matrix(prod3_xreg1),order=c(1,0,0))
AIC(prod3_arima_model)

forecast_prod3=forecast(prod3_arima_model,xreg=as.matrix(prod3_xreg2))
prod3_arima_fit=xts(forecast_prod3$mean,order.by=test_dates)

plot(dates,prod3$sold_count, type="l")
lines(test_dates,forecast_prod3$mean,col="blue")

#####   

prod3_reg_train=prod3[1:280,]
prod3_reg_test=prod3[281:n,]

prod3_reg=lm(sold_count~-1+category_sold+basket_count+category_favored,data=prod3_reg_train)
summary(prod3_reg)

pred_prod3_reg=predict(prod3_reg,prod3_reg_test)
prod3_reg_fit=xts(pred_prod3_reg,order.by=test_dates)

plot(dates,prod3$sold_count,type="l")
lines(test_dates,prod3_reg_fit,col="red")

##### Regressors which we will use in ARIMAX model are also 2 ahead forecasted.

prod3_catfav_xreg=as.numeric(prod3_xts$category_favored)
prod3_catfav_model=auto.arima(prod3_catfav_xreg)
prod3_catfav_forecast=forecast(prod3_catfav_model,h=2)

prod3_catsol_xreg=as.numeric(prod3_xts$category_sold)
prod3_catsol_model=auto.arima(prod3_catsol_xreg)
prod3_catsol_forecast=forecast(prod3_catsol_model,h=2)

prod3_basket_xreg=as.numeric(prod3_xts$basket_count)
prod3_basket_model=auto.arima(prod3_basket_xreg)
prod3_basket_forecast=forecast(prod3_basket_model,h=2)

prod3_price_xreg=as.numeric(prod3_xts$price)
prod3_price_model=auto.arima(prod3_price_xreg)
prod3_price_forecast=forecast(prod3_price_model,h=2)

##################################################################

prod3_final_xreg1=cbind(prod3$category_sold,prod3$category_favored,prod3$basket_count,prod3$price)
prod3_final_xreg2=data.table("category_sold"=prod3_catsol_forecast$mean,"category_favored"=prod3_catfav_forecast$mean,"basket_count"=prod3_basket_forecast$mean,"price"=prod3_price_forecast$mean)

##### ARIMA Final 

prod3_final_xts=as.numeric(prod3_xts$sold_count)

prod3_final_arima_model=Arima(prod3_final_xts,xreg=as.matrix(prod3_final_xreg1),order=c(1,0,0))
prod3_final_forecast_arima=forecast(prod3_final_arima_model,xreg=as.matrix(prod3_final_xreg2))

##### Regression final

prod3_reg_final=lm(sold_count~-1+category_sold+category_favored+basket_count,data=prod3)

pred_prod3_reg_submodel=predict(prod3_reg_final,prod3_final_xreg2)

##### Mean Absolute Percentage Error check to determine which model we should use to forecast 2 days ahead sales value of that product.

prod3_reg_mape=mean(abs((prod3_reg_fit-as.numeric(prod3_test_xts$sold_count))/as.numeric(prod3_test_xts$sold_count)))
prod3_reg_mape
prod3_arima_mape=mean(abs((prod3_arima_fit-as.numeric(prod3_test_xts$sold_count))/as.numeric(prod3_test_xts$sold_count)))
prod3_arima_mape

##### Combine Them

prod3_final=(prod3_final_forecast_arima$mean[2]+pred_prod3_reg_submodel[2])/2
prod3_final

##################### 85004 - Product 4

plot(dates,prod4$sold_count,type="l", main="Product 4 Sales", xlab= "Dates", ylab= "Sales")

test=ur.kpss(prod4$sold_count)
summary(test)

prod4_xts=xts(prod4,order.by=dates)
prod4_train_xts=prod4_xts[index(prod4_xts)<"2021-03-01"  & index(prod4_xts)>="2020-05-25"]
prod4_test_xts=prod4_xts[index(prod4_xts)>="2021-03-01"]

##### Check correlations to add regressors to use them in ARIMA models and regressions

cor(prod4$sold_count, prod4$price)
cor(prod4$sold_count, prod4$basket_count)
cor(prod4$sold_count, prod4$category_sold)
cor(prod4$sold_count, prod4$category_favored)

prod4_xreg1=cbind(prod4$basket_count[1:280],prod4$category_favored[1:280])
prod4_xreg2=cbind(prod4$basket_count[281:n],prod4$category_favored[281:n])

##### Checking autocorrelation and partial autocorrelation plots to determine which order we can use in Arima model.

acf(prod4$sold_count)
pacf(prod4$sold_count)

prod4_arima_model=auto.arima(as.numeric(prod4_train_xts$sold_count),xreg=as.matrix(prod4_xreg1))
AIC(prod4_arima_model)

prod4_forecast_arima=forecast(prod4_arima_model,xreg=as.matrix(prod4_xreg2))
prod4_arima_fit=xts(prod4_forecast_arima$mean,order.by = test_dates)

plot(dates,prod4$sold_count, type ="l")
lines(test_dates,prod4_arima_fit,col="blue")

#################prod4 Regression

prod4_reg_train=prod4[1:280,]
prod4_reg_test=prod4[281:n,]

prod4_reg=lm(sold_count~-1+basket_count,data=prod4_reg_train)
summary(prod4_reg)

pred_prod4_reg=predict(prod4_reg,prod4_reg_test)
prod4_reg_fit=xts(pred_prod4_reg,order.by=test_dates)

plot(dates,prod4$sold_count,type="l")
lines(test_dates,prod4_reg_fit,col="red")

##### Regressors which we will use in ARIMAX model are also 2 ahead forecasted.

prod4_basket_xreg=as.numeric(prod4_xts$basket_count)
prod4_basket_model=auto.arima(prod4_basket_xreg)
prod4_basket_forecast=forecast(prod4_basket_model,h=2)

prod4_catfav_xreg=as.numeric(prod4_xts$category_favored)
prod4_catfav_model=auto.arima(prod4_catfav_xreg)
prod4_catfav_forecast=forecast(prod4_catfav_model,h=2)

##############

prod4_final_xreg1=cbind(prod4$basket_count,prod4$category_favored)
prod4_final_xreg2=data.table("basket_count"=prod4_basket_forecast$mean,"category_favored"=prod4_catfav_forecast$mean)

### ARIMA Final

prod4_final_xts=as.numeric(prod4_xts$sold_count)

prod4_final_arima_model=auto.arima(prod4_final_xts,xreg=as.matrix(prod4_final_xreg1),)
prod4_final_forecast_arima=forecast(prod4_final_arima_model,xreg=as.matrix(prod4_final_xreg2))

##### Regression Final

prod4_reg_final=lm(sold_count~-1+basket_count,data=prod4)
pred_prod4_reg_submodel=predict(prod4_reg_final,prod4_final_xreg2)

##### Mean Absolute Percentage Error check to determine which model we should use to forecast 2 days ahead sales value of that product.

prod4_ARIMA_MAPE=mean(abs((prod4_arima_fit-as.numeric(prod4_test_xts$sold_count))/as.numeric(prod4_test_xts$sold_count)))
prod4_ARIMA_MAPE
prod4_reg_MAPE=mean(abs((prod4_reg_fit-as.numeric(prod4_test_xts$sold_count))/as.numeric(prod4_test_xts$sold_count)))
prod4_reg_MAPE


##### Combine them

prod4_final=(prod4_final_forecast_arima$mean[2]+pred_prod4_reg_submodel[2])/2
prod4_final

  ########################### 6676673 - Product 5

plot(dates,prod5$sold_count,type="l", main="Product 5 Sales", xlab= "Dates", ylab= "Sales")

test=ur.kpss(prod5$sold_count)
summary(test)

prod5_xts=xts(prod5,order.by=dates)

prod5_train_xts=prod5_xts[index(prod5_xts)>="2020-05-25" & index(prod5_xts)<"2021-03-01"]
prod5_test_xts=prod5_xts[index(prod5_xts)>="2021-03-01"]

##### Check correlations to add regressors to use them in ARIMA models and regressions

cor(prod5$sold_count, prod5$price)
cor(prod5$sold_count, prod5$favored_count)
cor(prod5$sold_count, prod5$basket_count)
cor(prod5$sold_count, prod5$category_sold)
cor(prod5$sold_count, prod5$category_favored)

prod5_xreg1=cbind(prod5$basket_count[1:280])
prod5_xreg2=cbind(prod5$basket_count[281:n])

##### Checking autocorrelation and partial autocorrelation plots to determine which order we can use in Arima model.

acf(prod5$sold_count)
pacf(prod5$sold_count)

prod5_arima_model=Arima(as.numeric(prod5_train_xts$sold_count), xreg=as.matrix(prod5_xreg1),  order=c(0,1,4))
AIC(prod5_arima_model)

prod5_forecast_arima=forecast(prod5_arima_model,xreg=as.matrix(prod5_xreg2))
prod5_arima_fit=xts(prod5_forecast_arima$mean,order.by=test_dates)

plot(dates,prod5$sold_count, type="l")
lines(test_dates,prod5_arima_fit,col="blue")

######

prod5_reg_train=prod5[1:280,]
prod5_reg_test=prod5[281:n,]

prod5_reg=lm(sold_count~-1+basket_count,data=prod5_reg_train)
summary(prod5_reg)

pred_prod5_reg=predict(prod5_reg,prod5_reg_test)
prod5_reg_fit=xts(pred_prod5_reg,order.by=test_dates)

plot(dates,prod5$sold_count, type="l")
lines(test_dates,prod5_reg_fit,col="red")

##### Regressors which we will use in ARIMAX model are also 2 ahead forecasted.

prod5_basket_xreg=as.numeric(prod5_xts$basket_count)
prod5_basket_model=auto.arima(prod5_basket_xreg)
prod5_basket_forecast=forecast(prod5_basket_model,h=2)

#######

prod5_final_xreg1=cbind(prod5$basket_count)
prod5_final_xreg2=data.table("basket_count"=prod5_basket_forecast$mean)

##### Arima Final

prod5_final_xts=as.numeric(prod5_xts$sold_count)
prod5_final_arima_model=Arima(prod5_final_xts,xreg=as.matrix(prod5_final_xreg1),order=c(0,1,4))
prod5_final_forecast_arima=forecast(prod5_final_arima_model,xreg=as.matrix(prod5_final_xreg2))

##### Regression final

prod5_reg_final=lm(sold_count~-1+basket_count,data=prod5)
pred_prod5_reg_submodel=predict(prod5_reg_final,prod5_final_xreg2)

##### Mean Absolute Percentage Error check to determine which model we should use to forecast 2 days ahead sales value of that product.

prod5_reg_MAPE=mean(abs((prod5_reg_fit-as.numeric(prod5_test_xts$sold_count))/as.numeric(prod5_test_xts$sold_count)))
prod5_reg_MAPE
prod5_ARIMA_MAPE=mean(abs((prod5_arima_fit-as.numeric(prod5_test_xts$sold_count))/as.numeric(prod5_test_xts$sold_count)))
prod5_ARIMA_MAPE

##### Combine them

prod5_final=(prod5_final_forecast_arima$mean[2]+pred_prod5_reg_submodel[2])/2
prod5_final

################### 7061886 - Product 6

plot(dates,prod6$sold_count,type="l", main="Product 6 Sales", xlab= "Dates", ylab= "Sales")

test=ur.kpss(prod6$sold_count)
summary(test)

prod6_xts=xts(prod6,order.by=dates)

prod6_train_xts=prod6_xts[index(prod6_xts)<"2021-03-01" & index(prod6_xts)>="2020-05-25"]
prod6_test_xts=prod6_xts[index(prod6_xts)>="2021-03-01"]

##### Check correlations to add regressors to use them in ARIMA models and regressions

cor(prod6$sold_count, prod6$price)
cor(prod6$sold_count, prod6$favored_count)
cor(prod6$sold_count, prod6$basket_count)
cor(prod6$sold_count, prod6$category_sold)

prod6_xreg1=cbind(prod6$basket_count[1:280])
prod6_xreg2=cbind(prod6$basket_count[281:n])

##### Checking autocorrelation and partial autocorrelation plots to determine which order we can use in Arima model.

acf(prod6$sold_count)
pacf(prod6$sold_count)

prod6_arima_model=auto.arima(as.numeric(prod6_train_xts$sold_count),xreg=as.matrix(prod6_xreg1))
AIC(prod6_arima_model)

prod6_forecast_arima=forecast(prod6_arima_model,xreg=as.matrix(prod6_xreg2))
prod6_arima_fit=xts(prod6_forecast_arima$mean,order.by=test_dates)

plot(dates,prod6$sold_count, type="l")
lines(test_dates,prod6_arima_fit,col="blue")

####
prod6_reg_train=prod6[1:280,]
prod6_reg_test=prod6[281:n,]

prod6_reg=lm(sold_count~-1+basket_count,data=prod5_reg_train)
summary(prod6_reg)

pred_prod6_reg=predict(prod6_reg,prod6_reg_test)
prod6_reg_fit=xts(pred_prod6_reg,order.by = test_dates)

plot(dates,prod6$sold_count,type="l")
lines(test_dates,prod6_reg_fit,col="red")

##### Regressors which we will use in ARIMAX model are also 2 ahead forecasted.

prod6_basket_xreg=as.numeric(prod6_xts$basket_count)
prod6_basket_model=auto.arima(prod6_basket_xreg)
prod6_basket_forecast=forecast(prod6_basket_model,h=2)

########

prod6_final_xreg1=cbind(prod6$basket_count)
prod6_final_xreg2=data.table("basket_count"=prod6_basket_forecast$mean)

##### ARIMA final

prod6_final_xts=as.numeric(prod6_xts$sold_count)
prod6_final_arima_model=auto.arima(prod6_final_xts,xreg=as.matrix(prod6_final_xreg1))
prod6_final_forecast_arima=forecast(prod6_final_arima_model,xreg=as.matrix(prod6_final_xreg2))

##### Regression final
prod6_reg_final=lm(sold_count~-1+basket_count,data=prod6)
pred_prod6_reg_submodel=predict(prod6_reg_final,prod6_final_xreg2)

##### Mean Absolute Percentage Error check to determine which model we should use to forecast 2 days ahead sales value of that product.

prod6_reg_MAPE=mean(abs((prod6_reg_fit-as.numeric(prod6_test_xts$sold_count))/as.numeric(prod6_test_xts$sold_count)))
prod6_reg_MAPE
prod6_ARIMA_MAPE=mean(abs((prod6_arima_fit-as.numeric(prod6_test_xts$sold_count))/as.numeric(prod6_test_xts$sold_count)))
prod6_ARIMA_MAPE

##### Combine them
prod6_final=(prod6_final_forecast_arima$mean[2]+pred_prod6_reg_submodel[2])/2
prod6_final

  ################################### 31515569 - Product 7

plot(dates,prod7$sold_count,type="l", main="Product 7 Sales", xlab= "Dates", ylab= "Sales")

test=ur.kpss(prod7$sold_count)
summary(test)

prod7_xts=xts(prod7,order.by=dates)

prod7_train_xts=prod7_xts[index(prod7_xts)<"2021-03-01"&index(prod7_xts)>="2020-05-25"]
prod7_test_xts=prod7_xts[index(prod7_xts)>="2021-03-01"]

##### Check correlations to add regressors to use them in ARIMA models and regressions

cor(prod7$sold_count, prod7$price)
cor(prod7$sold_count, prod7$favored_count)
cor(prod7$sold_count, prod7$basket_count)
cor(prod7$sold_count, prod7$category_sold)

prod7_xreg1=cbind(prod7$basket_count[1:280])
prod7_xreg2=cbind(prod7$basket_count[281:n])

##### Checking autocorrelation and partial autocorrelation plots to determine which order we can use in Arima model.

acf(prod7$sold_count)
pacf(prod7$sold_count)

prod7_arima_model=Arima(as.numeric(prod7_train_xts$sold_count),xreg=as.matrix(prod7_xreg1),order=c(1,2,4))
AIC(prod7_arima_model)

prod7_forecast_arima=forecast(prod7_arima_model,xreg=as.matrix(prod7_xreg2))
prod7_arima_fit=xts(prod7_forecast_arima$mean,order.by=test_dates)

plot(dates,prod7$sold_count,type="l")
lines(test_dates,prod7_arima_fit,col="blue")

####Regression prod7

prod7_reg_train=prod7[1:280,]
prod7_reg_test=prod7[281:n,]

prod7_reg=lm(sold_count~-1+basket_count,data=prod7_reg_train)
summary(prod7_reg)

pred_prod7_reg=predict(prod7_reg,prod7_reg_test)
prod7_reg_fit=xts(pred_prod7_reg,order.by = test_dates)

plot(dates,prod7$sold_count,type="l")
lines(test_dates,prod7_reg_fit,col="red")

##### Regressors which we will use in ARIMAX model are also 2 ahead forecasted.

prod7_basket_xreg=as.numeric(prod7_xts$basket_count)
prod7_basket_model=auto.arima(prod7_basket_xreg)
prod7_basket_forecast=forecast(prod7_basket_model,h=2)

####################

prod7_final_xreg1=cbind(prod7$basket_count)
prod7_final_xreg2=data.table("basket_count"=prod7_basket_forecast$mean )

##### ARIMA Final

prod7_final_xts=as.numeric(prod7_xts$sold_count)
prod7_final_arima_model=Arima(prod7_final_xts,xreg=as.matrix(prod7_final_xreg1),order=c(1,2,4))
prod7_final_forecast_arima=forecast(prod7_final_arima_model,xreg=as.matrix(prod7_final_xreg2))

##### Regression Final

prod7_reg_final=lm(sold_count~-1+basket_count,data=prod7)
pred_prod7_reg_submodel=predict(prod7_reg_final,prod7_final_xreg2)

##### Mean Absolute Percentage Error check to determine which model we should use to forecast 2 days ahead sales value of that product.

prod7_arima_mape=mean(abs((prod7_arima_fit-as.numeric(prod7_test_xts$sold_count))/as.numeric(prod7_test_xts$sold_count)))
prod7_arima_mape
prod7_reg_mape=mean(abs((prod7_reg_fit-as.numeric(prod7_test_xts$sold_count))/as.numeric(prod7_test_xts$sold_count)))
prod7_reg_mape

##### Only use regression model which fits better than ARIMA

prod7_final=pred_prod7_reg_submodel[2]
prod7_final

##################### 73318567 - Product 8

plot(dates,prod8$sold_count,type="l", main="Product 8 Sales", xlab= "Dates", ylab= "Sales")

test=ur.kpss(prod8$sold_count)
summary(test)

prod8_xts=xts(prod8,order.by=dates)
prod8_train_xts=prod8_xts[index(prod8_xts)>="2021-01-23" & index(prod8_xts)<"2021-05-01"]
prod8_test_xts=prod8_xts[index(prod8_xts)>="2021-05-01"]

test_dates_prod8=seq(as.Date("2021-05-01"), length = n-341, by = "days")

##### Check correlations to add regressors to use them in ARIMA models and regressions

cor(prod8$sold_count, prod8$price)
cor(prod8$sold_count, prod8$favored_count)
cor(prod8$sold_count, prod8$basket_count)
cor(prod8$sold_count, prod8$category_sold)

prod8_xreg1=cbind(prod8$favored_count[244:341],prod8$basket_count[244:341])
prod8_xreg2=cbind(prod8$favored_count[342:n], prod8$basket_count[342:n])

##### Checking autocorrelation and partial autocorrelation plots to determine which order we can use in Arima model.

acf(prod8$sold_count)
pacf(prod8$sold_count)

prod8_arima_model=Arima(as.numeric(prod8_train_xts$sold_count),xreg=as.matrix(prod8_xreg1),order=c(2,1,0))
AIC(prod8_arima_model)

prod8_forecast_arima=forecast(prod8_arima_model,xreg=as.matrix(prod8_xreg2))
prod8_arima_fit=xts(prod8_forecast_arima$mean,order.by=test_dates_prod8)

plot(dates,prod8$sold_count,type="l")
lines(test_dates_prod8,prod8_arima_fit,col="blue")

#####

prod8_reg_train=prod8[244:341,]
prod8_reg_test=prod8[342:n,]

prod8_reg=lm(sold_count~favored_count+basket_count,data=prod8_reg_train)

pred_prod8_reg=predict(prod8_reg,prod8_reg_test)
prod8_reg_fit=xts(pred_prod8_reg,order.by = test_dates_prod8)

plot(dates,prod8$sold_count,type="l")
lines(test_dates_prod8,prod8_reg_fit,col="red")

##### Regressors which we will use in ARIMAX model are also 2 ahead forecasted.

prod8_basket_xreg=as.numeric(prod8_xts$basket_count)
prod8_basket_model=auto.arima(prod8_basket_xreg)
prod8_basket_forecast=forecast(prod8_basket_model,h=2)

prod8_favored_xreg=as.numeric(prod8_xts$favored_count)
prod8_favored_model=auto.arima(prod8_favored_xreg)
prod8_favored_forecast=forecast(prod8_favored_model,h=2)

#####

prod8_final_xreg1=cbind(prod8$basket_count,prod8$favored_count)
prod8_final_xreg2=data.table("basket_count"=prod8_basket_forecast$mean,"favored_count"=prod8_favored_forecast$mean)

##### ARIMA Final

prod8_final_xts=as.numeric(prod8_xts$sold_count)
prod8_final_arima_model=Arima(prod8_final_xts,xreg=as.matrix(prod8_final_xreg1),order=c(2,1,0))
prod8_final_forecast_arima=forecast(prod8_final_arima_model,xreg=as.matrix(prod8_final_xreg2))

##### Regression final

prod8_reg_final=lm(sold_count~favored_count+basket_count,data=prod8)
pred_prod8_reg_submodel=predict(prod8_reg_final,prod8_final_xreg2)

##### Mean Absolute Percentage Error check to determine which model we should use to forecast 2 days ahead sales value of that product.

prod8_arima_mape=mean(abs((prod8_arima_fit-as.numeric(prod8_test_xts$sold_count))/as.numeric(prod8_test_xts$sold_count)))
prod8_arima_mape
prod8_reg_mape=mean(abs((prod8_reg_fit-as.numeric(prod8_test_xts$sold_count))/as.numeric(prod8_test_xts$sold_count)))
prod8_reg_mape

##### Only use regression model which fits better than ARIMA

prod8_final=pred_prod8_reg_submodel[2]
prod8_final

######################### 32737302 - Product 9

plot(dates,prod9$sold_count,type="l", main="Product 7 Sales", xlab= "Dates", ylab= "Sales")

test=ur.kpss(prod9$sold_count)
summary(test)

prod9_xts=xts(prod9,order.by=dates)
prod9_train_xts=prod9_xts[index(prod9_xts)>="2021-02-20" & index(prod9_xts)<"2021-05-01"]
prod9_test_xts=prod9_xts[index(prod9_xts)>="2021-05-01"]

test_dates_prod9=seq(as.Date("2021-05-01"), length = n-341, by = "days")

##### Check correlations to add regressors to use them in ARIMA models and regressions

cor(prod9$sold_count, prod9$price)
cor(prod9$sold_count, prod9$favored_count)
cor(prod9$sold_count, prod9$basket_count)
cor(prod9$sold_count, prod9$category_sold)

prod9_xreg1=cbind(prod9$favored_count[272:341],prod9$basket_count[272:341])
prod9_xreg2=cbind(prod9$favored_count[342:n], prod9$basket_count[342:n])

##### Checking autocorrelation and partial autocorrelation plots to determine which order we can use in Arima model.

acf(prod9$sold_count)
pacf(prod9$sold_count)

prod9_arima_model=Arima(as.numeric(prod9_train_xts$sold_count),xreg=as.matrix(prod9_xreg1),order=c(1,0,0))
AIC(prod9_arima_model)

prod9_forecast_arima=forecast(prod9_arima_model,xreg=as.matrix(prod9_xreg2))
prod9_arima_fit=xts(prod9_forecast_arima$mean,order.by=test_dates_prod9)

plot(dates,prod9$sold_count,type="l")
lines(test_dates_prod9,prod9_arima_fit,col="blue")

#####

prod9_reg_train=prod9[272:341,]
prod9_reg_test=prod9[342:n,]

prod9_reg=lm(sold_count~favored_count+basket_count,data=prod9_reg_train)

pred_prod9_reg=predict(prod9_reg,prod9_reg_test)
prod9_reg_fit=xts(pred_prod9_reg,order.by = test_dates_prod9)

plot(dates,prod9$sold_count,type="l")
lines(test_dates_prod9,prod9_reg_fit,col="red")

##### Regressors which we will use in ARIMAX model are also 2 ahead forecasted.

prod9_basket_xreg=as.numeric(prod9_xts$basket_count)
prod9_basket_model=auto.arima(prod9_basket_xreg)
prod9_basket_forecast=forecast(prod9_basket_model,h=2)

prod9_favored_xreg=as.numeric(prod9_xts$favored_count)
prod9_favored_model=auto.arima(prod9_favored_xreg)
prod9_favored_forecast=forecast(prod9_favored_model,h=2)

####################################

prod9_final_xreg1=cbind(prod9$basket_count,prod9$favored_count)
prod9_final_xreg2=data.table("basket_count"=prod9_basket_forecast$mean,"favored_count"=prod9_favored_forecast$mean)

##### ARIMA Final

prod9_final_xts=as.numeric(prod9_xts$sold_count)
prod9_final_arima_model=Arima(prod9_final_xts,xreg=as.matrix(prod9_final_xreg1),order=c(1,0,0))
prod9_final_forecast_arima=forecast(prod9_final_arima_model,xreg=as.matrix(prod9_final_xreg2))

##### Regression final

prod9_reg_final=lm(sold_count~favored_count+basket_count,data=prod9)
pred_prod9_reg_submodel=predict(prod9_reg_final,prod9_final_xreg2)

##### Mean Absolute Percentage Error check to determine which model we should use to forecast 2 days ahead sales value of that product.

prod9_arima_mape=mean(abs((prod9_arima_fit-as.numeric(prod9_test_xts$sold_count))/as.numeric(prod9_test_xts$sold_count)))
prod9_arima_mape
prod9_reg_mape=mean(abs((prod9_reg_fit-as.numeric(prod9_test_xts$sold_count))/as.numeric(prod9_test_xts$sold_count)))
prod9_reg_mape

###### Combine them

prod9_final=(prod9_final_forecast_arima$mean[2]+pred_prod9_reg_submodel[2])/2
prod9_final

############################### Sending Submission

forecasts=list(prod7_final,prod9_final,prod2_final,prod3_final,prod1_final,prod5_final,prod6_final,prod8_final, prod4_final)
product_id=list(31515569,32737302,32939029,4066298,48740784,6676673,7061886,73318567,85004)

table_submisssion=data.table("product_content_id"=product_id, "forecast"=forecasts)
table_submisssion$forecast=as.numeric(table_submisssion$forecast)
check_format(table_submisssion)
write_xlsx(table_submisssion,"/Users/muhammetenesustun/Desktop/ourpreds.xlsx")

send_submission(table_submisssion, token, url=subm_url, submit_now=T)




