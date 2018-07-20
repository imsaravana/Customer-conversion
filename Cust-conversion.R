library(tidyverse)
# read csv
p_data = read_csv("paywall_data.csv")
p_payment = read_csv("paywall_payment_data.csv")

#left join on id
p_joined = left_join(p_data, p_payment)

#converting to posixct format using lubridate package

library(lubridate)
for(i in 3:19){
p_joined[i] = mdy_hm(p_joined[i])
}

#converting to epoch time format
for(i in 3:19){
p_joined[i]= as.numeric[i]
}

#Getting only paid users from the dataset for analysis using dplyr
pay_info <- p_joined %>% filter(p_joined$`Paid?` ==1)


#Subtracting each paywall from First payment date and replacing all NA and negative values 
#using a large number which can be ignored later during conversion rate calculation
#USing row wise will be significantly faster
for(j in 2:nrow(pay_info)){
  for(i in 4:18){
    if((pay_info[j,19]-pay_info[j,i]) >0 & !is.na(pay_info[j,i])){
  pay_info[j,i] <- pay_info[j,19]-pay_info[j,i]}
    else{
      pay_info[j,i] <- 111111111111111111111
    }
    
  }
}


#normal
#analysis<- pay_info
#for(i in 4:18){
  #analysis[,i] = analysis[,19]-analysis[,i]
#}


#Finding minimum value using dplyr and filtering out the values containing 111111111111111111111
pay_info <- pay_info  %>% rowwise() %>% mutate(paywall=min(c("column names go here"))) %>%filter(paywall < 111111111111111111111 )

#Finds the column name of the minimum value which gives the paywall name 
pay_info$min_paywall = colnames(pay_info[4:18])[apply(pay_info[4:18],1,which.min)]

#Grouping by min_paywall and calculating paywall count and revenue
final <- pay_info %>% group_by(min_paywall) %>% summarise(n = n(), conversion_revenue= sum(`First Payment Value`)) 

#Finding total count of paywall and converting it to long format from wide
final_1 <- gather(p_joined[4:18] %>% summarise_each(funs(n_distinct)))

#left join on min_paywall and key
conversion = left_join(final, final_1, by = c("min_paywall" = "key"))

#renaming min_paywall, n and value
conversion <- conversion %>% rename(paywall= min_paywall, paywall_count=n, total_count=value) %>% mutate(conversion_rate=(paywall_count/total_count)*100) %>% arrange(desc(conversion_revenue,conversion_rate))
