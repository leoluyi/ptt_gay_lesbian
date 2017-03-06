library(magrittr)
library(data.table)
library(dplyr)
library(dtplyr)
library(readr)
library(stringr)
library(tm) # install.packages("slam", type = "binary")
library(text2vec)
library(jiebaR) # word segmentation
library(wordcloud2)
# library(topicmodels)
# http://stackoverflow.com/questions/24172188/how-can-i-install-topicmodels-package-in-r
library(ldatuning) # Select number of topics for LDA model # sudo apt install libmpfr-dev
library(wordVectors) # devtools::install_github("bmschmidt/wordVectors")
library(ggplot2)
library(feather)
library(DT)
library(corrr) # for corr plot
library(Matrix) # for Sparse Matrix
library(slam)
library(lubridate)
library(viridis)
invisible(
  lapply(list.files("utils", pattern = "\\.[Rr]$", full.names = TRUE), 
         function(x) {source(x, encoding = "UTF-8"); invisible()})
)
# devtools::install_github("qinwf/ropencc") # 繁簡轉換


## Data Cleansing

gay_dt <- fread("data/ptt_gay_20170227.csv")
lesbian_dt <- fread("data/ptt_lesbian_20170227.csv")
ptt_dt <- rbindlist(list(gay_dt, lesbian_dt))
rm(gay_dt, lesbian_dt)

# Data Cleansing

# Add category
ptt_dt[, category := 
         stringr::str_extract(title, "\\[([^]]+?)\\]")]

# time
ptt_dt[, `:=`(post_time = post_time %>% 
                strptime("%a %b %d %T %Y", tz = "ROC") %>% 
                as.POSIXct)]
ptt_dt[, `:=`(post_time = ifelse(
  post_time >= as.POSIXct("1990-01-01","ROC") %>% as.numeric(),
  post_time, NA))]
ptt_dt[, `:=`(post_time = post_time %>% 
                as.POSIXct(tz = "ROC", origin = "1970-01-01"))]
# Clean text
ptt_dt[, post_text := post_text %>% str_replace_all("※ 引述.+?之銘言：\n", "")]
ptt_dt[title %>% str_detect("^Fw:"), 
       post_text := post_text %>%
         str_replace_all("(?m)^(?:※ \\[本文轉錄|作者:|標題:|時間:).*$", "") %>%
         str_trim()]
ptt_dt[title %>% str_detect("^Re:"), 
       post_text := post_text %>%
         str_replace_all("(?m)^:(.*)$", "") %>% str_trim()]
# ptt_dt[, post_text := post_text %>%
#          str_replace("(?ism).*Ctrl\\s?[+]\\s?Y.*?$", "")]
ptt_dt[category=="[活動]", post_text := post_text %>%
         str_replace("(?s)^.*Y 刪除[\\s=]+", "")]
ptt_dt[category=="[尋人]", post_text := post_text %>%
         str_replace("(?s)^.*可快速刪除整行文字\\)\\s*", "")]
ptt_dt[, post_text := post_text %>%
         str_replace_all("[^\\w\\s\u4E00-\u9FD5]", " ")]
ptt_dt[, push_text := push_text %>%
         str_replace_all("[^\\w\\s\u4E00-\u9FD5]", " ")]

# remove url
url_re <- "((([A-Za-z]{3,9}:(?:\\/\\/)?)(?:[\\-;:&=\\+\\$,\\w]+@)?[A-Za-z0-9\\.\\-]+|(?:www\\.|[\\-;:&=\\+\\$,\\w]+@)[A-Za-z0-9\\.\\-]+)((?:\\/[\\+~%\\/\\.\\w\\-_]*)?\\??(?:[\\-\\+=&;%@\\.\\w_]*)#?(?:[\\.\\!\\/\\\\\\w]*))?)"
ptt_dt[, post_text := post_text %>% str_replace_all(url_re, "")]
ptt_dt[, push_text := push_text %>% str_replace_all(url_re, "")]


# filter time
min_time <- ptt_dt[, .(min_time = min(post_time, na.rm=T)), by = .(board)][
  , max(min_time)]
ptt_dt <- ptt_dt[post_time >= min_time]

# ----------------------------------------------

mix_seg <- worker(type = "mix",
                  user = "utils/user_dict_utf8.txt",
                  stop_word = "utils/stop_utf8.txt",
                  symbol = FALSE,
                  encoding = "UTF-8")
# hmm_seg <- worker(type = "hmm",
#                   user = "utils/user_dict_utf8.txt",
#                   stop_word = "utils/stop_utf8.txt",
#                   symbol = FALSE,
#                   encoding = "UTF-8")
mix_seg <= ptt_dt[, post_text][1000] # try first post
# hmm_seg <= ptt_dt[, post_text][1]

# self-made filter (built-in perl's regular expression has bug)
cutter <- function (text, worker) {
  # text = "馬英九去世新大學演講"
  if (text %in% c(".", "")) {
    return(NA_character_)
  }
  
  filter_words = c(
    "推文", "站內信", "其他", "推",
    "我.?","他.?","你.?", "想說",
    "所以","可以","沒有","不過","因為",
    "還是","覺得","大家","比較","感覺","時候","現在","時間",
    "可能","東西","然後","而且","自己","有點",
    "這邊","那.","發現","雖然","不要","還是",
    "一樣","知道","看到","真的","今天","就是","這樣","如果",
    "不會","什麼","後來","問題","之前","只是","或是","的話",
    "其他","這麼","已經","很多","出來","整個","但是","卻",
    "偏偏","如果","不過","因此","或","又","也","其實",
    "希望","結果","怎麼","當然","有些","以上","另外","此外",
    "以外","裡面","部分","直接","剛好","由於",
    "原本","標題","時間","日期","作者","這種","表示","看見",
    "似乎","一半","一堆","反正","常常","幾個","目前","上次",
    "公告","只好","哪裡","一.","怎麼","好像","結果",
    "而已", "居然", "謝謝","請問","大大","小弟", "文章代碼",
    "po","xd","應該","最後","有沒有","sent","from","my",
    "Android", "JPTT", "如提","如題","編輯","引述","銘言","站內信",
    "記者",
    "中心","之.","指出","朋友",
    "了","也","的","在","與","及","等","是","the","and",
    "月", "年", "日", "時", "NA",
    "\\s",
    "[a-zA-Z]",
    "[0-9]"
  )
  pattern <- sprintf("^%s", paste(filter_words, collapse = "|^"))
  tryCatch({
    text_seg <- mix_seg <= text
  }, error = function(e) {
    stop('"', text, '" >> ', e)
  })
  filter_seg <- text_seg[!stringr::str_detect(text_seg, pattern)]
  filter_seg
}

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl, {
  library(stringr)
  library(jiebaR)
  mix_seg <- worker(type = "mix",
                    user = "utils/user_dict_utf8.txt",
                    stop_word = "utils/stop_utf8.txt",
                    symbol = FALSE,
                    encoding = "UTF-8")
})
clusterExport(cl, list("cutter"))

# 平權 ----------------------------------------------------------------------
# 
# doc.list <- ptt_dt[category == "[平權]", post_text] %>% 
#   parLapply(cl, ., cutter) %>% 
#   parLapply(cl, ., function(x) x[!is.na(x)])
# stopCluster(cl)
# 
# dtm <- doc.list %>% seglist_to_dtm %>% filter_tfidf_dtm
# 
# # https://cran.r-project.org/web/packages/ldatuning/vignettes/topics.html
# tic <- Sys.time()
# result <- FindTopicsNumber(
#   dtm,
#   topics = c(seq(2, 10, by = 2),
#              seq(10, 50, by = 5),
#              seq(60, 100, by = 10),
#              seq(120, 200, by = 20)
#   ),
#   metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010"),
#   method = "Gibbs",
#   control = list(seed = Sys.time() %>% as.numeric()),
#   mc.cores = 3L,
#   verbose = TRUE
# )
# Sys.time() - tic
# save(result, file = "models/lda_sim_result_pingquan.RData")
# 

# All ---------------------------------------------------------------------

doc.list <- ptt_dt[
  !category %in% c("[公告]", "[尋人]", "[自介]", "[她介]", "[他介]",
                   "[創作]", "[租屋]", "[Line]", "[line]", "[群組]",
                   "[問卷]", "[Line群]", "[徵人]", "[活動]"), 
  paste(post_text, push_text)] %>% 
  parLapply(cl, ., cutter) %>% 
  parLapply(cl, ., function(x) x[!is.na(x)])
stopCluster(cl)
# doc.list <- ptt_dt[, post_text] %>% 
#   mclapply(cutter, worker = mix_seg, mc.cores = 3) %>% 
#   mclapply(function(x) x[!is.na(x)], mc.cores = 3) 
dtm <- doc.list %>% seglist_to_dtm %>% filter_tfidf_dtm

# https://cran.r-project.org/web/packages/ldatuning/vignettes/topics.html
tic <- Sys.time()
result <- FindTopicsNumber(
  dtm,
  topics = c(seq(2, 10, by = 2),
             #seq(10, 60, by = 5),
             seq(20, 100, by = 10),
             seq(120, 300, by = 50)
  ),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010"),
  method = "Gibbs",
  control = list(seed = Sys.time() %>% as.numeric()),
  mc.cores = 3L,
  verbose = TRUE
)
Sys.time() - tic
save(result, file = "models/lda_sim_result_all.RData")


