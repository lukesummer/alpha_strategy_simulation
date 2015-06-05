#equal weighted portofolio
#train period:2010-11-05 ~ 2014-12-23
#trading period: 2013-10-14 ~ 2014-12-23
setwd("~/accomplishment/work/risk management") 
#basic parameters
risk_free_rate <- 0.03/52
margin_percent <- 0.2 #margin and reserve
init_wealth <- 1 #initial wealth
init_reserve <- 0
adj_period <- 1
z <- 2.33
reduce_percent <- 1

# basic data processing
week_data = read.csv(file="output_st13.csv", header=T,sep=";") 
row_num <- nrow(week_data) 
# c("st7","st10","st11","st12","st13")
st_col_name <- c("st1","st2","st3","st4","st5","st6","st7","st8","st9","st10","st11","st12","st13") 
sh_col_name <- c("sh") 
st13_data <- week_data[1:row_num,st_col_name] 
row_num <- nrow(st13_data) #221
col_num <- ncol(st13_data) #ranges from 1 - 13
sh_data <- week_data[1:row_num,sh_col_name] 
date <- week_data[1:row_num,c("date")] 

# basic index and numbers of time line
ret_row_num <- row_num - 1
train_begin_ind <- 1
train_end_ind <- 140
train_num <- train_end_ind - train_begin_ind + 1  #104-1+1=104
trad_begin_ind <- train_end_ind + 1
trad_end_ind <- ret_row_num - 20  #200
trad_num <- trad_end_ind - trad_begin_ind + 1 #316-105+1=212

# calculate simple return of sh
sh_diff <- diff(sh_data) 
sh_ret <- sh_diff[1:ret_row_num]/sh_data[1:ret_row_num] 
ex_sh_ret <- sh_ret - risk_free_rate #316
# calculate simple return of st
st13_ret <- matrix(c(1:(col_num*ret_row_num)),nrow=ret_row_num) 
ex_st13_ret <- matrix(c(1:(col_num*ret_row_num)),nrow=ret_row_num) 
for (k in 1:col_num)
{
  st_diff <- diff(st13_data[,k]) 
  st13_ret[,k] <- (st_diff[1:ret_row_num])/st13_data[1:ret_row_num,k] 
  ex_st13_ret[,k] <- st13_ret[,k] - risk_free_rate #220
}
st_ret <- c(1:ret_row_num) 
ex_st_ret <- c(1:ret_row_num) 
for (k in 1:ret_row_num)
{
  st_ret[k] <- sum(st13_ret[k,])/col_num 
  ex_st_ret[k] <- st_ret[k]-risk_free_rate 
}
#
adj_num <- ceiling((trad_end_ind - trad_begin_ind + 1)/adj_period) 

#
st_price <- st13_data[2:row_num,] #2:221,
sh_price <- sh_data[2:row_num]#2:221
alpha_vec <- 0 
alpha_mat <- matrix(c(1:(adj_num*col_num)),nrow=adj_num) 
beta_vec <- 0 
beta_mat <- matrix(c(1:(adj_num*col_num)),nrow=adj_num) 
st_wealth <- 0 
sh_wealth <- 0 
wealth <- 0 
ret_vec <- 0 
sigma_st_mat <- matrix(c(1:(trad_num*col_num)),nrow=trad_num) 
sigma_mar_vec <- c(1:trad_num) 
sigma_port_vec <- c(1:trad_num) 
var_vec <- c(1:trad_num) 
var_per_vec <- c(1:trad_num)
var_loss_vec <- c(1:trad_num)
wealth_ret_vec <-c(1:trad_num) 
reserve <- 0 # once exception happens, we should allocation some money into reserve account
total_wealth <- 0
total_ret <- 0
for(k in 1:trad_num)
{
  total_wealth[k] <- 0 
}
for (k in 1:trad_num)
{
  reserve[k] <- 0
}
variance_sum1 <- c(1:trad_num)  
variance_sum2 <- c(1:trad_num) 



###### trading simulation starts #####
for(i in 1 : adj_num)
{
  (trad_end_ind - trad_begin_ind + 1)/adj_period
  out_begin_ind <- adj_period*(i-1)+1
  out_end_ind <- out_begin_ind + train_num - 1
  alpha_sum <- 0
  beta_sum <- 0
  for (k in 1:col_num)
  {
    lm_model <- lm(ex_st13_ret[out_begin_ind:out_end_ind,k]~ex_sh_ret[out_begin_ind:out_end_ind]) 
    alpha_mat[i,k] <- lm_model$coefficients[1] #intercept-alpha
    beta_mat[i,k] <- lm_model$coefficients[2] #slope-beta
    alpha_sum <- alpha_sum + alpha_mat[i,k] 
    beta_sum <- beta_sum + beta_mat[i,k] 
  }
  alpha_vec[i] <- alpha_sum/col_num 
  beta_vec[i] <- beta_sum/col_num 
  #
  in_begin_ind <- (adj_period*(i-1)+1) 
  in_end_ind <- in_begin_ind+adj_period-1 #212
  
  for (j in in_begin_ind : min(in_end_ind,trad_num))# j ranges from 1 to 212, but 4 times each iteration
  {
    beta <- beta_vec[i]
    #construct a portfolio
    if ( j == 1)
    {
      # at the beginning of the week, we have:
      st_wealth[j] <- init_wealth/(margin_percent*beta + 1) 
      sh_wealth[j] <- init_wealth*margin_percent*beta/(margin_percent*beta + 1) 
      # at the end of the week
      wealth[j] = init_wealth+st_wealth[j]*st_ret[j+train_num]-(sh_wealth[j]/margin_percent)*sh_ret[j+train_num] 
      ret_vec[j] = (wealth[j] - init_wealth)/init_wealth 
      wealth_ret_vec[j] <- init_wealth*ret_vec[j]
      total_wealth[j] <- init_reserve*(1+risk_free_rate)+wealth[j]
      total_ret[j] <- (total_wealth[j] - init_wealth)/init_wealth
    }
    else
    {
      st_wealth[j] <- wealth[j-1]/(margin_percent*beta + 1)
      sh_wealth[j] <- wealth[j-1]*margin_percent/(margin_percent + 1/beta) 
      wealth[j] <- wealth[j-1]+st_wealth[j]*st_ret[j+train_num]-(sh_wealth[j]/margin_percent)*sh_ret[j+train_num]
      ret_vec[j] <- (wealth[j]- wealth[j-1])/wealth[j-1]
      wealth_ret_vec[j] <- wealth[j-1]*ret_vec[j]
      ## for risk management strategy
      reserve[j] <- reserve[j-1]*(1+risk_free_rate)
      total_wealth[j] <- wealth[j] + reserve[j]
      total_ret[j] <- (total_wealth[j]-total_wealth[j-1])/total_wealth[j-1]
      ###############################
    }
    # calculate the Value at Risk
#     # 1.static VaR
#     if(j == 1)
#     {
#       ## calculate the sigma of stocks
#       variance_sum1[1] <- 0 
#       variance_sum2[1] <- 0 
#       for(kk in 1:col_num)
#       {
#         sigma_st_mat[1,kk] <- sd(st13_ret[1:(1+train_num-1),kk]) 
#         variance_sum1[1] <- variance_sum1[1] + (sigma_st_mat[1,kk]^2)*((st_wealth[1]/col_num)^2) 
#       }
#       ## calculate the sigma of the market
#       sigma_mar_vec[1] <- sd(sh_ret[1:(1+train_num-1)]) 
#       variance_sum1[1] <- variance_sum1[1]+((sh_wealth[1]/margin_percent)^2)*(sigma_mar_vec[1]^2) 
#       ## calculate sum2
#       for (k in 1:col_num)
#       {
#         variance_sum2[1] <- variance_sum2[1]-2*beta_mat[i,k]*(sigma_mar_vec[1]^2)*st_wealth[1]*sh_wealth[1]/(col_num*margin_percent) 
#       }
#       for (m in 1:(col_num-1))
#       {
#         for (l in (m+1):col_num)
#         {
#           #variance_sum2[1] <- variance_sum2[1]+2*((st_wealth[1]/col_num)^2)*sigma_st_mat[1,m]*sigma_st_mat[1,l]*Correlation(st13_ret[1:(1+train_num-1),m],st13_ret[1:(1+train_num-1),l]) 
#           variance_sum2[1] <- variance_sum2[1]+2*((st_wealth[1]/col_num)^2)*(sigma_mar_vec[1]^2)*beta_mat[i,m]*beta_mat[i,l] 
#         }
#       }
#       ## calculate VaR
#       sigma_port_vec[1] <- sqrt(variance_sum1[1] + variance_sum2[1]) 
#       var_vec[1] <- sigma_port_vec[1]*z - alpha_vec[i]*init_wealth 
#       
#       for (k in 1:(trad_num-1))
#       {
#         var_vec[k+1] = var_vec[k] 
#       }
#     }
    
    
        # 2.dynamic VaR
        ## calculate the sigma of stocks
        variance_sum1[j] <- 0 
        variance_sum2[j] <- 0 
        for(k in 1:col_num)
        {
          sigma_st_mat[j,k] <- sd(st13_ret[j:(j+train_num-1),k]) 
          variance_sum1[j] <- variance_sum1[j] + (sigma_st_mat[j,k]^2)*((st_wealth[j]/col_num)^2) 
        }
        ## calculate the sigma of the market
        sigma_mar_vec[j] <- sd(sh_ret[j:(j+train_num-1)]) 
        variance_sum1[j] <- variance_sum1[j]+((sh_wealth[j]/margin_percent)^2)*(sigma_mar_vec[j]^2) 
        ## calculate sum2
        for (k in 1:col_num)
        {
          variance_sum2[j] <- variance_sum2[j] -2*beta_mat[i,k]*(sigma_mar_vec[j]^2)*st_wealth[j]*sh_wealth[j]/(col_num*margin_percent) 
        }
        for (m in 1:(col_num-1))
        {
          for (l in (m+1):col_num)
          {
            #variance_sum2[j] <- variance_sum2[j]+2*((st_wealth[j]/col_num)^2)*sigma_st_mat[j,m]*sigma_st_mat[j,l]*Correlation(st13_ret[j:(j+train_num-1),m],st13_ret[j:(j+train_num-1),l]) 
            variance_sum2[j] <- variance_sum2[j]+2*((st_wealth[j]/col_num)^2)*(sigma_mar_vec[j]^2)*beta_mat[i,m]*beta_mat[i,l] 
          }
        }
        ## calculate VaR
        sigma_port_vec[j] <- sqrt(variance_sum1[j] + variance_sum2[j]) 
        if (j ==1)
        {
          var_vec[j] <- sigma_port_vec[j]*z+alpha_vec[i]*init_wealth+(1-beta_vec[i])*st_wealth[j]*risk_free_rate
          var_loss_vec[j] <- -var_vec[j]
          var_per_vec[j] <-var_loss_vec[j]/init_wealth
        }
        else
        {
          var_vec[j] <- sigma_port_vec[j]*z+alpha_vec[i]*wealth[j]+(1-beta_vec[i])*st_wealth[j]*risk_free_rate
          var_loss_vec[j] <- -var_vec[j]
          var_per_vec[j] <- var_loss_vec[j]/wealth[j-1]
        }
        #risk management strategy based on VaR
        if (wealth_ret_vec[j]<(-var_vec[j]))
        {
          temp_wealth <- wealth[j]
          wealth[j] <- wealth[j]*reduce_percent
          reserve[j] <- ifelse(j==1,0,reserve[j-1])+temp_wealth*(1-reduce_percent)
        }
        else
        {
          reserve[j] <- ifelse(j==1,0,reserve[j-1])*(1+risk_free_rate)
        }
  } 
}

##### trading simulation ends #####


plot(1:length(sigma_port_vec),sigma_port_vec,'l') 
plot(1:length(total_wealth),wealth,'l') 
plot(1:length(alpha_vec),alpha_vec,'l') 
plot(1:length(beta_vec),beta_vec,'l')

plot(1:length(ret_vec),ret_vec,'l') 

# graph 1
plot(c(1:trad_num),ret_vec,ylim=c(min(ret_vec,var_per_vec),max(ret_vec,var_per_vec)),col='red','l')
points(c(1:trad_num),var_per_vec,col='green','l')

# graph 2
plot(c(1:trad_num),total_wealth,ylim=c(min(total_wealth,(sh_price[trad_begin_ind:trad_end_ind]/sh_price[trad_begin_ind-1])),max(total_wealth,(sh_price[trad_begin_ind:trad_end_ind]/sh_price[trad_begin_ind-1]))),'l') 
points(c(1:trad_num),sh_price[trad_begin_ind:trad_end_ind]/sh_price[trad_begin_ind-1],col='red','l')
total_wealth

# for chi test
for (i in 10:length(wealth_ret_vec))
{
  result[i] <- chi_test(ret_vec[1:i],var_per_vec[1:i])
  if (result[i] == 1 && first_oberve_ind==0)
  {
    first_oberve_ind <- i
  }
}

# calculate monthly return
month_ret_vec <- 0 
for (k in 1:(trad_num/4))
{
  month_ret_vec[k] <- mean(ret_vec[(4*(k-1)+1):(4*k)])*12 
}

# the function to calculate the correlation
Correlation<- function(x,y) {
  len<-length(x)
  if( len != length(y))
    stop("length not equal!")
  
  x2 <- unlist(lapply(x,function(a) return(a^2)))
  y2 <- unlist(lapply(y,function(a) return(a^2)))
  xy <- x*y
  
  a <- sum(xy)*len - sum(x)*sum(y)
  b <- sqrt(sum(x2)*len - sum(x)^2)*sqrt(sum(y2)*len - sum(y)^2)
  if( b == 0)
    stop("data is incorrect!")
  return(a/b)
}

# the function for chi-square test
# x <- wealth_ret_vec
# y <- var_loss_vec
chi_test <- function(x,y) 
{
  #x <- wealth_ret_vec[1:10]
  #y <- var_loss_vec[1:10]
  p <- 0.01
  chi_test_result <- 0
  len<-length(x)
  T <- len
  T00 <- 0
  T11 <- 0
  T10 <- 0
  T01 <- 0
  index <- 0
  index <- ifelse(x<y,1,0)
  N <- sum(index)
  if( len != length(y))
    stop("length not equal!")
  for (i in 2:len)
  {
    if (index[i] == 1)
    {
      if (index[i-1] == 1)
      {
        T11 <- T11 + 1
      }
      if (index[i-1] == 0)
      {
        T01 <- T01 + 1
      }
    }
    if (index[i] == 0)
    {
      if (index[i-1] == 0)
      {
        T00 <- T00 + 1
      }
      else
      {
        T10 <- T10 + 1
      }
    }
  }
  a <- T10 + T11
  b <- T01 + T11
  uc_1 <- ((1-p)^(T-N))*(p^N)
  uc_2 <- ((1-N/T)^(T-N))*((N/T)^N)
  lr_uc <- -2*(ifelse(uc_1==0,-1000,log(uc_1)))+2*(ifelse(uc_2==0,-1000,log(uc_2)))
  lr_ind <- 0
  pi1 <- ifelse(a==0,0,(T11/a))
  pi0 <- T01/(T00+T01)
  pi <- b/(T00+T01+T10+T11)
  if(pi != 0 &&pi1 !=0 && pi0!=0)
  {
    ind_1 <- ((1-pi)^(T00+T10))*(pi^(T01+T10))
    ind_2 <- ((1-pi0)^T00)*(pi0^T01)*(1-pi1)^T10*(pi1^T11)
    lr_ind <- -2*ifelse(ind_1==0,-1000,log(ind_1))+2*ifelse(ind_2==0,-1000,log(ind_2))  
  }
  ratio=lr_uc+lr_ind
  print(ratio)
  #   if (lr_uc > 3.84)#3.84
  #   {
  #     chi_test_result <- 1
  #   }
  if(ratio > 5.99)
  {
    chi_test_result <- 1
  }  
  return(chi_test_result)
}
first_oberve_ind <- 0
result <-0 
result[1:9] <- 0

# variables to be observed
first_oberve_ind
total_wealth

# write to xlsx
# library(xlsx)
# ifelse(ret_vec[1:60]<var_per_vec[1:60],1,0)
# return_data <- data.frame(ret_vec,-var_per_vec,(ret_vec<var_per_vec))
# write.xlsx(return_data,"~/accomplishment/work/risk management/return_data.xlsx")
