## A boot strap analysis of the LDT 

library(MKinfer)

# two_non_bootstrap 

ppt_list = unique(p_data$ppt) 
list = list()

for (i in 1:length(ppt_list))
{
tdf = p_data %>% 
  filter(ppt == ppt_list[i]) %>% 
  filter(type == "two_way_cognate"| type == "non_cognate")

res = boot.t.test(log_rt~type, data = tdf)

list[[i]] = data.frame(boot = res[["boot.p.value"]], reg = res[["p.value"]], ppt = ppt_list[i], comp = "two_non")
}

two_df = do.call(rbind, list) %>% 
  as.data.frame()


three_list = list()
for (i in 1:length(ppt_list))
{
  tdf = p_data %>% 
    filter(ppt == ppt_list[i]) %>% 
    filter(type == "three_way_cognate"| type == "non_cognate")
  
  res = boot.t.test(log_rt~type, data = tdf)
  
  three_list[[i]] = data.frame(boot = res[["boot.p.value"]], reg = res[["p.value"]], ppt = ppt_list[i], comp = "three_non")
}

three_df = do.call(rbind, three_list) %>% 
  as.data.frame()




list_l2_l3= list()

for (i in 1:length(ppt_list))
{
  tdf = p_data %>% 
    filter(ppt == ppt_list[i]) %>% 
    filter(type == "three_way_cognate"| type == "two_way_cognate")
  
  res = boot.t.test(log_rt~type, data = tdf)
  
  list_l2_l3[[i]] = data.frame(boot = res[["boot.p.value"]], reg = res[["p.value"]], ppt = ppt_list[i], comp = "two_three")
}

two_three_df = do.call(rbind, list_l2_l3) %>% 
  as.data.frame()

#####
all = rbind(three_df,two_three_df, two_df)