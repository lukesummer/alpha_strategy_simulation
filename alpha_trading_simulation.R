#train period:2008-05-20 ~ 2010-05-20
#trading period: 2010-05-21 ~ 2014-12-10
setwd("~/accomplishment/work/risk management");
#basic parameters
margin_percent <- 0.01; #margin and reserve
init_wealth <- 1; #initial wealth
adj_period <- 1;

week_data = read.csv(file="output_st13.csv", header=T,sep=";");
# c("st7","st10","st11","st12","st13")
st_col_name <- c("st7","st10","st11","st12","st13");
sh_col_name <- c("sh");
row_num <- nrow(week_data);#221
st13_data <- week_data[1:row_num,st_col_name];
st13_col_num <- ncol(st_data);
sh_data <- week_data[1:row_num,sh_col_name];
st_data <- 0;
for ( k in 1:row_num)
{
  st_data[k] <- sum(st13_data[k,])
}
ret_row_num <- row_num - 1;
train_begin_ind <- 1;
train_end_ind <- 180;
train_num <- train_end_ind - train_begin_ind + 1; #104-1+1=104
trad_begin_ind <- train_end_ind + 1;  #
trad_end_ind <- ret_row_num - 20; #200
trad_num <- trad_end_ind - trad_begin_ind + 1;#316-105+1=212
risk_free_rate <- 0.03/52;
# calculate simple return of sh
sh_diff <- diff(sh_data);
sh_ret <- sh_diff[1:ret_row_num]/sh_data[1:ret_row_num];
ex_sh_ret <- sh_ret - risk_free_rate;#316
# calculate simple return of st
st_diff <- diff(st_data);
st_ret <- st_diff[1:ret_row_num]/st_data[1:ret_row_num];
ex_st_ret <- st_ret - risk_free_rate;#220
#
st_price <- st_data[2:row_num];#2:221
sh_price <- sh_data[2:row_num]#2:221
alpha_vec <- 0;
beta_vec <- 0;
st_share_vec <- 0;
sh_share_vec <- 0;
st_wealth <- 0;
sh_wealth <- 0;
wealth <- 0;
ret_vec <- 0;

for(i in 1 : ceiling((trad_end_ind - trad_begin_ind + 1)/adj_period)) #212/4=53, iteration monthly
{
  out_begin_ind <- adj_period*(i-1)+1;
  out_end_ind <- out_begin_ind + train_num - 1;
  lm_model <- lm(ex_st_ret[out_begin_ind:out_end_ind]~ex_sh_ret[out_begin_ind:out_end_ind]);
  alpha_vec[i] <- lm_model$coefficients[1] #intercept-alpha
  beta_vec[i] <- lm_model$coefficients[2] #slope-beta
  in_begin_ind <- (adj_period*(i-1)+1);
  in_end_ind <- in_begin_ind+adj_period-1;#212
  for (j in in_begin_ind : min(in_end_ind,trad_num))# j ranges from 1 to 212, but 4 times each iteration
  {
    beta <- beta_vec[i];
    #construct a portfolio
    if ( j == 1)
    {
      st_wealth[j] <- init_wealth/(margin_percent*beta + 1);
      sh_wealth[j] <- init_wealth*margin_percent/(margin_percent + 1/beta);
    }
    else
    {
      st_wealth[j] <- wealth[j-1]/(margin_percent*beta + 1);
      sh_wealth[j] <- wealth[j-1]*margin_percent/(margin_percent + 1/beta);
    }
    st_share_vec[j] <- st_wealth/st_price[j+train_end_ind-1];
    sh_share_vec[j] <- (sh_wealth/margin_percent)/sh_price[j+train_end_ind-1];
    wealth[j] = st_share_vec[j]*st_price[j+train_end_ind] + sh_wealth[j] - (sh_wealth[j]/margin_percent)*sh_ret[j];
    if (j == 1)
    {
      ret_vec[j] = (wealth[j] - init_wealth)/init_wealth;
    }
    else
    {
      ret_vec[j] = (wealth[j]- wealth[j-1])/wealth[j-1];
    }
  } 
}

plot(1:length(st_share_vec),st_share_vec,'l');
plot(1:length(lm_model$residuals),lm_model$residuals,'l');

plot(1:length(alpha_vec),alpha_vec,'l');
plot(1:length(ret_vec),ret_vec,'l')
plot(1:length(sh_price[trad_begin_ind:trad_end_ind]),sh_price[trad_begin_ind:trad_end_ind]/sh_price[trad_begin_ind-1],'l')
plot(1:length(st_price[trad_begin_ind:trad_end_ind]),st_price[trad_begin_ind:trad_end_ind]/st_price[trad_begin_ind-1],'l')
plot(1:length(wealth),wealth,'l');
wealth
length(wealth)






