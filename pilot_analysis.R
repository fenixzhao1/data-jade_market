##### Data preparation #####
# load packages
rm(list = ls())
library(here)
library(ggplot2)
library(dplyr)
library(haven)
library(xtable)

# add paths to files
your_path<-here("df/")
path = list.files(path=your_path, pattern="*.csv")

# import the data
full_data<-NULL
for(i in seq(along=path)){
  d<-read.csv(paste(your_path, '/', path[i],sep=""),header=T, stringsAsFactors = FALSE)
  full_data<-rbind(full_data,d)
}
rm(d)

# remove practice rounds
full_data = full_data %>% filter(subsession.is_practice_round == 0)

# gen group id
full_data = full_data %>% mutate(
  group_id = paste(session.code, subsession.round_number, group.id_in_subsession, sep = '_'),
  is_overbid = ifelse(player.bid_amount>player.item_final_value, 1, 0),
  bid2_higher = ifelse(is.na(player.bid_amount2-player.bid_amount1), NA, ifelse(
    player.bid_amount2-player.bid_amount1>0, 1, 0)),
  bid_type = ifelse(subsession.red_bid==1, 'RedBid', 'WhiteBid'),
  treatment = paste(bid_type, subsession.auction, sep = '_')
)

# unique treatments
uniquetreat = unique(full_data$treatment)
uniquegroup = unique(full_data$group_id)

# generate the group level data frame
table = matrix(0, nrow = length(uniquegroup), ncol = 15)
rownames(table) = uniquegroup
colnames(table) = c('winner profit', 'winner value', 'highest value', 'lowest value',
                    'buyer profit', 'seller revenue', 'treatment', 
                    'is positive profit', 'is efficient', 'efficiency',
                    'overbid', 'retake', 'bid2_higher', 'bid1_mean', 'bid1_max')
table = data.frame(table)
for (i in 1:length(uniquegroup)){
  df = filter(full_data, group_id == uniquegroup[i])
  table$winner.profit[i] = (df$player.item_final_value - df$player.bid_amount)[df$player.is_winner==1]
  table$winner.value[i] = df$player.item_final_value[df$player.is_winner==1]
  table$highest.value[i] = max(df$player.item_final_value[df$player.role_set=='bidder'])
  table$lowest.value[i] = min(df$player.item_final_value[df$player.role_set=='bidder'])
  table$buyer.profit[i] = mean(df$player.payoff[df$player.role_set=='bidder'])
  table$seller.revenue[i] = df$player.payoff[df$player.role_set=='auctioneer']
  table$is.positive.profit[i] = ifelse(table[i,1]>0, 1, 0)
  table$is.efficient[i] = ifelse(table[i,2]==table[i,3], 1, 0)
  table$treatment[i] = df$treatment[1]
  effi = (table$winner.value[i]-table$lowest.value[i])/(table$highest.value[i]-table$lowest.value[i])
  table$efficiency[i] = ifelse(is.nan(effi), 1, effi)
  table$overbid[i] = mean(df$is_overbid[df$player.role_set=='bidder'])
  table$retake[i] = df$player.retake[df$player.role_set=='auctioneer']
  table$bid2_higher[i] = mean(df$bid2_higher[df$player.role_set=='bidder'])
  table$bid1_mean[i] = mean(df$player.bid_amount1[df$player.role_set=='bidder'])
  table$bid1_max[i] = max(df$player.bid_amount1[df$player.role_set=='bidder'])
}


##### Efficiency and Winners' Curse #####
# generate the treatment level table
table2 = matrix(0, nrow = length(uniquetreat), ncol = 8)
rownames(table2) = uniquetreat
colnames(table2) = c('% of positive profit', '% of highest value', 'efficiency',
                     'buyer profit', 'seller revenue', '% of overbidding', 
                     '% of retake', '% of higher bid2')
for (i in 1:length(uniquetreat)){
  df = filter(table, treatment == uniquetreat[i])
  table2[i,1] = mean(df$is.positive.profit)
  table2[i,2] = mean(df$is.efficient)
  table2[i,3] = mean(df$efficiency)
  table2[i,4] = mean(df$buyer.profit)
  table2[i,5] = mean(df$seller.revenue)
  table2[i,6] = mean(df$overbid)
  table2[i,7] = mean(df$retake)
  table2[i,8] = mean(df$bid2_higher, na.rm = TRUE)
}

# show results
xtable(table2, digits=2)
rm(df)


##### Conditional retake #####
table3 = matrix(0, nrow = length(uniquetreat), ncol = 4)
rownames(table3) = uniquetreat
colnames(table3) = c('max if retake', 'mean if retake',
                     'max if not retake', 'mean if not retake')
for (i in 1:length(uniquetreat)){
  df = filter(table, treatment == uniquetreat[i])
  table3[i,1] = mean(df$bid1_max[df$retake==1])
  table3[i,2] = mean(df$bid1_mean[df$retake==1])
  table3[i,3] = mean(df$bid1_max[df$retake==0])
  table3[i,4] = mean(df$bid1_mean[df$retake==0])
}

# show results
xtable(table3, digits=2)
rm(df)
