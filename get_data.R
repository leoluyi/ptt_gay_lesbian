library(PTTr)
library(data.table)
library(magrittr)
gat_dt <- get_all_posts("gay", max_post = 30000)
#les_dt <- get_all_posts("lesbian", max_post = 30000)

gat_dt %>% fwrite("ptt_gay_20170227.csv")
#les_dt %>% fwrite("ptt_lesbain_20170227.csv")

