# set up 
setwd("/home/tck9844/segmentation")
library(data.table)
ordprod <- fread("ordprod.csv")
lookup <- fread("lookup.csv")
order <- fread("order.csv")
order$dlv_dt <- as.Date(order$dlv_dt,"%d%b%Y")

# merge files
########
# order_product level file
library(dplyr)
ord1 <- ordprod %>%
  dplyr::inner_join(lookup, by = "pod_id") %>%
  dplyr::inner_join(order, by = "ord_id") %>%
  dplyr::select(cnsm_id.y, ord_id, dlv_dt,ord_seq_num,it_dmnd_qy,pod_id,volume,it_qy,it_pr_qy,tot_pr_qy,it_long_name_tx,brnd_desc,owner,minor_cat)
summary(ord1)
head(ord1)

# # subset orders with unilever products
# unilever_ord <- ord1 %>%
#   dplyr::filter(owner == "UNILEVER") %>%
#   dplyr::select(cnsm_id.y, ord_id, dlv_dt, it_dmnd_qy,pod_id,it_qy,it_pr_qy,tot_pr_qy,it_long_name_tx,brnd_desc,minor_cat) %>% distinct %>%
#   dplyr::mutate(t_week = as.numeric(as.Date("2013/6/25") - dlv_dt)/7, 
#                 major_cat = ifelse(grepl("HB", minor_cat,fixed = T), "hb", "food"))
# 
# # food vs hb  food: $913481, hb:$288273
# unilever_ord %>%
#   dplyr::group_by(major_cat) %>%
#   dplyr::summarise(revenue = sum(it_qy*it_pr_qy), n_cust = n_distinct(cnsm_id.y)) 
# 
# ## choose t 
# #######
# # unilever customers - orders with unilever products, inter-purchase time, avg spending
# rfm_uni <- unilever_ord %>%
#   dplyr::group_by(cnsm_id.y) %>%
#   dplyr::summarise(tof=max(t_week), r = min(t_week), ford=n_distinct(ord_id), m=sum(it_dmnd_qy), lambda = (tof-r)/(ford-1)) 
# summary(rfm_uni$lambda)  # median 5.556  - t = monthly
# 
# ### calculate p0,p1
# ############
# t1_start_date <- c(paste0("2012-", 7:12, "-01"), paste0("2013-", 1:4, "-01"))
# t1_stop_date <- c(paste0("2012-", 8:12, "-01"), paste0("2013-", 1:5, "-01"))
# t2_start_date <- c(paste0("2012-", 8:12, "-01"), paste0("2013-", 1:5, "-01"))
# t2_stop_date <- c(paste0("2012-", 9:12, "-01"), paste0("2013-", 1:6, "-01"))
# 
# t1_buy <- t1_no_buy_2_buy <- t1_buy_2_buy <- t1_no_buy <- rep(NA, length(t1_start_date))
# 
# for(i in 1:length(t1_start_date)){
#   unilever_cust <- unilever_ord %>%
#     dplyr::filter(dlv_dt < as.Date(t1_stop_date[i])) %>%
#     dplyr::select(cnsm_id.y) %>% distinct 
# 
#   # calculate p0 & p1 
#   unilever_t1 <- unilever_ord %>%
#     dplyr::filter(dlv_dt >= as.Date(t1_start_date[i]) & dlv_dt < as.Date(t1_stop_date[i])) %>%
#     dplyr::select(1) %>% distinct 
#   
#   unilever_t2 <- unilever_ord %>%
#     dplyr::filter(dlv_dt >= as.Date(t2_start_date[i]) & dlv_dt < as.Date(t2_stop_date[i])) %>%
#     dplyr::select(1) %>% distinct #9332 customers
#   
#   buy_t1 <- ifelse(unilever_cust$cnsm_id.y %in% unilever_t1$cnsm_id.y,1,0)
#   buy_t2 <- ifelse(unilever_cust$cnsm_id.y %in% unilever_t2$cnsm_id.y,1,0)
#   
#   # retention_rate - buy to buy
#   t1_buy_2_buy[i] <- sum(buy_t1 == 1 & buy_t2 == 1)
#   t1_buy[i] <- sum(buy_t1 == 1)
#   
#   # reactivation_rate - no buy to buy
#   t1_no_buy_2_buy[i] <- sum(buy_t1 == 0 & buy_t2 == 1)
#   t1_no_buy[i] <- sum(buy_t1 == 0)
# }
# 
# sum(t1_buy_2_buy) / sum(t1_buy)       # 0.5035
# sum(t1_no_buy_2_buy) / sum(t1_no_buy) # 0.177
# 
# cbind(t1_start_date, t1_buy_2_buy, t1_buy, t1_no_buy_2_buy, t1_no_buy, t1_buy_2_buy/ t1_buy, t1_no_buy_2_buy/ t1_no_buy)
# 
# #check seasonality
# library(lubridate)
# unilever_ord %>%
#   dplyr::group_by(year(dlv_dt), month(dlv_dt)) %>%
#   dplyr::summarise(sum(it_qy*it_pr_qy)) 
# 
# ### calculate average spending of a typical customer in a target period
# uni_avg <- unilever_ord %>%
#   dplyr::group_by(yr = year(dlv_dt), month = month(dlv_dt)) %>%
#   dplyr::summarise(n_cust = n_distinct(cnsm_id.y), m_sum = sum(it_qy*it_pr_qy)) %>%
#   dplyr::mutate(avg_m = m_sum/n_cust) 
# uni_avg <- uni_avg[-c(1,13),]
# sum(uni_avg$m_sum)/sum(uni_avg$n_cust)  #9.10

# unilever customers inter-purchase time, spending - baseline of grocery shopping
unilever_cust1 <- unilever_ord %>%
  dplyr::select(cnsm_id.y) %>% distinct # 40253 unique customers

unilever_totord <- unilever_cust1 %>%
  dplyr::left_join(order, by = c("cnsm_id.y" = "cnsm_id")) %>%
  dplyr::filter(dlv_dt >= as.Date("2012-06-17")) %>%
  dplyr::select(-c(5:6))

# calculate p0 and p1 for unilever customers - an order to another order (overall)
t1_start_date <- c(paste0("2012-", 7:12, "-01"), paste0("2013-", 1:4, "-01"))
t1_stop_date <- c(paste0("2012-", 8:12, "-01"), paste0("2013-", 1:5, "-01"))
t2_start_date <- c(paste0("2012-", 8:12, "-01"), paste0("2013-", 1:5, "-01"))
t2_stop_date <- c(paste0("2012-", 9:12, "-01"), paste0("2013-", 1:6, "-01"))

t1_buy_tot <- t1_no_buy_2_buy_tot <- t1_buy_2_buy_tot <- t1_no_buy_tot <- rep(NA, length(t1_start_date))

for(i in 1:length(t1_start_date)){
  unilever_cust <- unilever_totord %>%
    dplyr::filter(dlv_dt < as.Date(t1_stop_date[i])) %>%
    dplyr::select(cnsm_id.y) %>% distinct 
  
  # calculate p0 & p1 
  unilever_t1 <- unilever_totord %>%
    dplyr::filter(dlv_dt >= as.Date(t1_start_date[i]) & dlv_dt < as.Date(t1_stop_date[i])) %>%
    dplyr::select(1) %>% distinct 
  
  unilever_t2 <- unilever_totord %>%
    dplyr::filter(dlv_dt >= as.Date(t2_start_date[i]) & dlv_dt < as.Date(t2_stop_date[i])) %>%
    dplyr::select(1) %>% distinct
  
  buy_t1_tot <- ifelse(unilever_cust$cnsm_id.y %in% unilever_t1$cnsm_id.y,1,0)
  buy_t2_tot <- ifelse(unilever_cust$cnsm_id.y %in% unilever_t2$cnsm_id.y,1,0)
  
  # retention_rate - buy to buy
  t1_buy_2_buy_tot[i] <- sum(buy_t1_tot == 1 & buy_t2_tot == 1)
  t1_buy_tot[i] <- sum(buy_t1_tot == 1)
  
  # reactivation_rate - no buy to buy
  t1_no_buy_2_buy_tot[i] <- sum(buy_t1_tot == 0 & buy_t2_tot == 1)
  t1_no_buy_tot[i] <- sum(buy_t1_tot == 0)
}

sum(t1_buy_2_buy_tot) / sum(t1_buy_tot)       # 0.7269104
sum(t1_no_buy_2_buy_tot) / sum(t1_no_buy_tot) # 0.224308

cbind(t1_start_date, t1_buy_2_buy_tot, t1_buy_tot, t1_no_buy_2_buy_tot, t1_no_buy_tot, t1_buy_2_buy_tot/ t1_buy_tot, t1_no_buy_2_buy_tot/ t1_no_buy_tot)

# unilever customers avg spending on overall products in a target period
unitot_avg <- unilever_totord %>%
  dplyr::group_by(yr = year(dlv_dt), month = month(dlv_dt)) %>%
  dplyr::summarise(n_cust = n_distinct(cnsm_id.y), m_sum = sum(it_dmnd_qy)) %>%
  dplyr::mutate(avg_m = m_sum/n_cust) 
unitot_avg <- unitot_avg[-c(1,13),]
sum(unitot_avg$m_sum)/sum(unitot_avg$n_cust)  # $314.09

# segment p0 & p1 analysis
#######
## prepare segment cluster dataframe
# 1-LazyEd -2767
# 2-Frugal Vijay - 3711  
# 3-Snacker John - 1558 
# 4-Fresh Tony - 1837
# 5-Loyal Christ - 4813 
# 6-Home-Loving Jenna - 3618 
seg <- fread("grocery_segment.csv")
unilever_cust_seg <- unilever_cust1 %>%
  dplyr::inner_join(seg, by = c("cnsm_id.y"= "cnsm_id")) %>%
  dplyr::select(c(1,59)) %>% distinct

# 
unilever_ord1 <- unilever_ord %>%
  dplyr::inner_join(unilever_cust_seg, by = "cnsm_id.y") %>% distinct

# cluster with unilever orders
# calculate p0, p1 for all segment customers with unilever orders - chage line 176, 181, 185
t1_start_date <- c(paste0("2012-", 7:12, "-01"), paste0("2013-", 1:4, "-01"))
t1_stop_date <- c(paste0("2012-", 8:12, "-01"), paste0("2013-", 1:5, "-01"))
t2_start_date <- c(paste0("2012-", 8:12, "-01"), paste0("2013-", 1:5, "-01"))
t2_stop_date <- c(paste0("2012-", 9:12, "-01"), paste0("2013-", 1:6, "-01"))

t1_buy <- t1_no_buy_2_buy <- t1_buy_2_buy <- t1_no_buy <- rep(NA, length(t1_start_date))

for(i in 1:length(t1_start_date)){
  unilever_cust <- unilever_ord1 %>%
    dplyr::filter(dlv_dt < as.Date(t1_stop_date[i]) & CLUSTER == 1) %>%
    dplyr::select(cnsm_id.y) %>% distinct 
  
  # calculate p0 & p1 
  unilever_t1 <- unilever_ord1 %>%
    dplyr::filter(dlv_dt >= as.Date(t1_start_date[i]) & dlv_dt < as.Date(t1_stop_date[i]) & CLUSTER ==1) %>%
    dplyr::select(1) %>% distinct 
  
  unilever_t2 <- unilever_ord1 %>%
    dplyr::filter(dlv_dt >= as.Date(t2_start_date[i]) & dlv_dt < as.Date(t2_stop_date[i]) & CLUSTER == 1) %>%
    dplyr::select(1) %>% distinct #9332 customers
  
  buy_t1 <- ifelse(unilever_cust$cnsm_id.y %in% unilever_t1$cnsm_id.y,1,0)
  buy_t2 <- ifelse(unilever_cust$cnsm_id.y %in% unilever_t2$cnsm_id.y,1,0)
  
  # retention_rate - buy to buy
  t1_buy_2_buy[i] <- sum(buy_t1 == 1 & buy_t2 == 1)
  t1_buy[i] <- sum(buy_t1 == 1)
  
  # reactivation_rate - no buy to buy
  t1_no_buy_2_buy[i] <- sum(buy_t1 == 0 & buy_t2 == 1)
  t1_no_buy[i] <- sum(buy_t1 == 0)
  
}
sum(t1_buy_2_buy) / sum(t1_buy)       # 0.4975
sum(t1_no_buy_2_buy) / sum(t1_no_buy) # 0.245

cbind(t1_start_date, t1_buy_2_buy, t1_buy, t1_no_buy_2_buy, t1_no_buy, t1_buy_2_buy/ t1_buy, t1_no_buy_2_buy/ t1_no_buy)


## calculate p0 and p1 for unilever customers of different segments with overall orders
unilever_totord1 <- unilever_totord %>%
  dplyr::inner_join(unilever_cust_seg, by = "cnsm_id.y") %>% distinct
#########

t1_start_date <- c(paste0("2012-", 7:12, "-01"), paste0("2013-", 1:4, "-01"))
t1_stop_date <- c(paste0("2012-", 8:12, "-01"), paste0("2013-", 1:5, "-01"))
t2_start_date <- c(paste0("2012-", 8:12, "-01"), paste0("2013-", 1:5, "-01"))
t2_stop_date <- c(paste0("2012-", 9:12, "-01"), paste0("2013-", 1:6, "-01"))

t1_buy_tot <- t1_no_buy_2_buy_tot <- t1_buy_2_buy_tot <- t1_no_buy_tot <- rep(NA, length(t1_start_date))

for(i in 1:length(t1_start_date)){
  unilever_cust <- unilever_totord1 %>%
    dplyr::filter(dlv_dt < as.Date(t1_stop_date[i]) & CLUSTER == 1) %>%
    dplyr::select(cnsm_id.y) %>% distinct 
  
  # calculate p0 & p1 
  unilever_t1 <- unilever_totord1 %>%
    dplyr::filter(dlv_dt >= as.Date(t1_start_date[i]) & dlv_dt < as.Date(t1_stop_date[i])) %>%
    dplyr::select(1) %>% distinct 
  
  unilever_t2 <- unilever_totord1 %>%
    dplyr::filter(dlv_dt >= as.Date(t2_start_date[i]) & dlv_dt < as.Date(t2_stop_date[i])) %>%
    dplyr::select(1) %>% distinct
  
  buy_t1_tot <- ifelse(unilever_cust$cnsm_id.y %in% unilever_t1$cnsm_id.y,1,0)
  buy_t2_tot <- ifelse(unilever_cust$cnsm_id.y %in% unilever_t2$cnsm_id.y,1,0)
  
  # retention_rate - buy to buy
  t1_buy_2_buy_tot[i] <- sum(buy_t1_tot == 1 & buy_t2_tot == 1)
  t1_buy_tot[i] <- sum(buy_t1_tot == 1)
  
  # reactivation_rate - no buy to buy
  t1_no_buy_2_buy_tot[i] <- sum(buy_t1_tot == 0 & buy_t2_tot == 1)
  t1_no_buy_tot[i] <- sum(buy_t1_tot == 0)
}

sum(t1_buy_2_buy_tot) / sum(t1_buy_tot)       
sum(t1_no_buy_2_buy_tot) / sum(t1_no_buy_tot) 

cbind(t1_start_date, t1_buy_2_buy_tot, t1_buy_tot, t1_no_buy_2_buy_tot, t1_no_buy_tot, t1_buy_2_buy_tot/ t1_buy_tot, t1_no_buy_2_buy_tot/ t1_no_buy_tot)


# rfm_tot <- unilever_totord %>%
#   dplyr::group_by(cnsm_id.y) %>%
#   dplyr::summarise(tof=max(t_week), r = min(t_week), ford=n_distinct(ord_id), m=sum(it_dmnd_qy), lambda = (tof-r)/(ford-1)) 
# summary(rfm_tot$lambda)  # median 4.139


# 5-Loyal Christ - 4813 
# 6-Home-Loving Jenna - 3618 
# most bought product - 2353 orders
unionly_ord_seg %>%
  dplyr::filter(CLUSTER == 6) %>%
  dplyr::group_by(pod_id, brnd_desc, it_long_name_tx) %>%
  dplyr::summarise(n_ord = n_distinct(ord_id), n_cust = n_distinct(cnsm_id.y), n_item = sum(it_qy),
                   m_sum = sum(it_qy*it_pr_qy)) %>%
  dplyr::arrange(-n_ord)

# 42,963
unionly_ord_seg %>%
  dplyr::filter(CLUSTER == 5) %>%
  dplyr::group_by(ord_id) %>%
  dplyr::summarise(m = sum(it_qy*it_pr_qy))

948/4813 # 0.1969665

# % of home-loving jenna buy hellman / Q-tips
592/3618  # 0.1636263
417/3618  # 0.115257

# upsell in mayonaise
unionly_ord_seg %>%
  dplyr::filter(CLUSTER == 5) %>%
  dplyr::filter(grepl("Mayonnaise",it_long_name_tx, fixed = T) == T) %>%
  dplyr::group_by(pod_id, brnd_desc, it_long_name_tx, it_pr_qy) %>%
  dplyr::summarise(n_ord = n_distinct(ord_id), n_cust = n_distinct(cnsm_id.y), n_item = sum(it_qy),
                   m_sum = sum(it_qy*it_pr_qy)) %>%
  dplyr::arrange(-n_ord)

# cross-sell
ketchup <- ord1 %>%
  dplyr::left_join(seg[,c(1,59)], by = c("cnsm_id.y" = "cnsm_id")) %>%
  dplyr::filter(CLUSTER == 5) %>%
  dplyr::filter(grepl("Ketchup",it_long_name_tx, fixed = T) == T) %>%
  dplyr::group_by(ord_id) %>%
  dplyr::summarise(m_sum = sum(it_qy*it_pr_qy))
dim(ketchup)  #8169 orders

length(unique(ord1$ord_id))   # 443733   1.8% 
# calculation
8169/443733

mayo <- ord1 %>%
  dplyr::left_join(seg[,c(1,59)], by = c("cnsm_id.y" = "cnsm_id")) %>%
  dplyr::filter(CLUSTER == 5) %>%
  dplyr::filter(grepl("Mayo",it_long_name_tx, fixed = T) == T) %>%
  dplyr::group_by(ord_id) %>%
  dplyr::summarise(m_sum = sum(it_qy*it_pr_qy))
dim(mayo)  #7902 orders

ketchup_mayo <- mayo %>%
  dplyr::left_join(ord1, by = "ord_id") %>%
  dplyr::filter(grepl("Ketchup",it_long_name_tx, fixed = T) == T) %>%
  dplyr::group_by(ord_id) %>%
  dplyr::summarise(m_sum = sum(it_qy*it_pr_qy))
dim(ketchup_mayo)  # 1424 orders
1424/7902   # 18% of the transactions 

unionly_ord_seg %>%
  dplyr::filter(CLUSTER == 5) %>%
  dplyr::filter(grepl("Mayonnaise",it_long_name_tx, fixed = T) == T) %>%
  dplyr::group_by(pod_id, brnd_desc, it_long_name_tx, it_pr_qy) %>%
  dplyr::summarise(n_ord = n_distinct(ord_id), n_cust = n_distinct(cnsm_id.y), n_item = sum(it_qy),
                   m_sum = sum(it_qy*it_pr_qy)) %>%
  dplyr::arrange(-n_ord)


