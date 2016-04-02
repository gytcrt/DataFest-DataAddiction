library(dplyr)
library(data.table)
library(stringr)

adwords <- fread("approved_adwords_v3.csv") %>% 
  as.data.frame() %>%
  tbl_df()

purchase <- fread("approved_data_purchase-v5.csv") %>% 
  as.data.frame() %>%
  tbl_df()

ga <- fread("approved_ga_data_v2.csv") %>% 
  as.data.frame() %>%
  tbl_df()

#########################
########Adwords##########
#########################

names(adwords)

adwords_sample <- sample_n(adwords, 100000)

#percentage with adgrounp_id
sum(table(adwords['adgroup_id']))/nrow(adwords)

keyword_freq <- adwords %>% 
  group_by(keyword) %>%
  summarise(total = n())

View(group_by(adwords, account, ad_group, campaign) %>% summarise(count=n()))

campaign <- adwords %>%
  group_by(campaign) %>%
  summarise( total = n())

adgrounps <- adwords %>%
  group_by(ad_group) %>%
  summarise( total = n())

ad_ca_key <- adwords %>%
  group_by(campaign, ad_group, keyword)
View(summarise(ad_ca_key))


#filter out adwords with adgroup id
ad_with_adid <- adwords %>% 
  filter(!is.na(adgroup_id)) %>%
  group_by(adgroup_id) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) #812097 rows
#percentage of rows with adgroup id
nrow(ad_with_adid)/nrow(adwords)  #0.5593005

#filter out adwords with campaign id
ad_with_campid <- adwords %>% 
  filter(!is.na(campaign_id)) %>%
  group_by(campaign_id) %>%
  summarise(count = n()) %>%
  arrange(desc(count))#1172684 rows

#first 100 campaigns take more than 80% counts, maybe we should only focus on them.
sum(ad_with_campid$count[1:100])/sum(ad_with_campid$count)

#percentage of rows with campaign id
nrow(ad_with_camp)/nrow(adwords)  #0.8076408

#ad camp
ad_with_camp <- adwords %>% 
  filter(!is.na(campaign)) %>%
  group_by(campaign) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
sum(ad_with_camp$count[1:200])/sum(ad_with_camp$count)
#0.8162119

#ad group
ad_with_adg <- adwords %>% 
  filter(!is.na(ad_group)) %>%
  group_by(ad_group) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
sum(ad_with_adg$count[1:1000])/sum(ad_with_adg$count)
#0.7233219

####Match.type and max_cpc
match_type_fee <- adwords %>%
  group_by(match.type, max_cpc) %>%
  summarise_each(funs(mean))


#filter out "ZZ - Bob's Automation Shop"
zz <- adwords %>% filter(account == "ZZ - Bob's Automation Shop")
zz_summary <- zz %>% group_by()


other <- adwords %>% filter(account != "ZZ - Bob's Automation Shop")

#########################
########Purchase#########
#########################

purchase_sample <- sample_n(purchase, 100000)
names(purchase)

#filter out ga with adgroup id
ga_with_adid <- filter(ga, adgroup_id != "")  #144771 rows
#percentage of rows with adgroup id
nrow(ga_with_adid)/nrow(ga)  #0.03426163  pretty low


#filter out ga with campaign id
ga_with_camp <- filter(ga, campaign_id != "")  #144771 rows
#percentage of rows with adgroup id
nrow(ga_with_camp)/nrow(ga)  #0.03426163  the same

by_cat <- purchase %>%
    group_by(primary_act_name, secondary_act_name) %>%
    summarise( total = n(),
               mean_price = mean(trans_face_val_amt))

#########################
############GA###########
#########################
ga_sample <- sample_n(ga, 100000)
