# Sentiment analysis for Chinese text
# Cheng-Jun Wang


############
"Algorithm"
############

##############
"read dict"
##############
library(plyr)
library(stringr)
library(e1071)
library(Rwordseg)
require(rJava)
library(tm)
library(slam)
Sys.setlocale(locale="Chinese")

setwd("D:/Dropbox/sentimentCN/dict/")

# positive: combine 正面情感词语（中文）,正面评价词语（中文）, ntusd-positive
posdict = read.csv("positive.txt", header = FALSE, stringsAsFactors = FALSE)[,1]
# negative: combine 负面情感词语（中文）,负面评价词语（中文）, ntusd-negative
negdict = read.csv("negative.txt", header = FALSE, stringsAsFactors = FALSE)[,1]

# split 程度级别词语（中文）.txt into 6 extent term most, very, more, ish, insufficient, inverse (over)
mostdict = read.csv("./most.txt", header = FALSE, stringsAsFactors = FALSE)[,1]
verydict = read.csv("./very.txt", header = FALSE, stringsAsFactors = FALSE)[,1]
moredict = read.csv("./more.txt", header = FALSE, stringsAsFactors = FALSE)[,1]
ishdict = read.csv("./ish.txt", header = FALSE, stringsAsFactors = FALSE)[,1]
insufficientdict = read.csv("./insufficient.txt", header = FALSE, stringsAsFactors = FALSE)[,1]
inversedict = read.csv("./inverse.txt", header = FALSE, stringsAsFactors = FALSE)[,1]
inversedict = c("不","不是","没","没有", inversedict)

##################
"sentiment function"
##################
# "4. 定义判断基数偶数的函数。在判断否定词时使用。"
judgeodd = function(num){
  judge = num %% 2 # 1 is odd, 0 is even
  return (judge)
}

# clean data
dataset = c("这手机的画面不是很好，不过操作却比较流畅。拍照真的太烂了！系统也不好。", 
            "我喜欢苹果手机！非常好用。就是太贵了。")

dataset = gsub("。", "。 ", dataset, fixed = T)
dataset = gsub("！", "！ ", dataset, fixed = T)
dataset = gsub("？", "？ ", dataset, fixed = T)

# cut sentences
cut_sentence = function(x){
  ht1 = strsplit(x, " ", fixed = T)
  return(ht1)
}

cuted_data = cut_sentence(dataset)


#   cuted_data = c()
#   for (cell in dataset){
#     cuted_data = c(cuted_data, cut_sentence(cell) )
#   }

# word segment
dataset <- lapply(1:length(dataset), 
                  function(i) segmentCN(dataset[i], nosymbol = FALSE, nature = FALSE))

# sentiment_score_list
sentiment_score_list = function(cuted_data ){
  count1 = c()
  count2 = c()
  for (sents in cuted_data){ #循环遍历每一个评论
    for (sent in sents){ #循环遍历评论中的每一个分句
      # sent = cuted_data[[1]][1] # Testing with this sent
      segtmp = segmentCN(sent, nosymbol = FALSE, nature = FALSE)  #把句子进行分词，以列表的形式返回
      i = 1 #记录扫描到的词的位置
      a = 1 #记录情感词的位置
      poscount = 0 #积极词的第一次分值
      poscount2 = 0 #积极词反转后的分值
      poscount3 = 0 #积极词的最后分值（包括叹号的分值）
      negcount = 0
      negcount2 = 0
      negcount3 = 0
      for (word in segtmp){
        if (word %in% posdict){#判断词语是否是情感词
          poscount = poscount + 1                
          c = 0 # 记录分句中否定词数量
          for (w in segtmp[a:i]){#扫描情感词前的程度词
            if (w %in% mostdict){
              poscount = poscount*4.0
            }
            else if (w %in% verydict){
              poscount = poscount*3.0
            }
            else if (w %in% moredict){
              poscount = poscount*2.0  
            }
            else if (w %in% ishdict){
              poscount = poscount/2.0
            }
            else if (w %in% insufficientdict){
              poscount = poscount/4.0
            }
            else if (w %in% inversedict){
              c = c + 1
            }         
          }
          if (judgeodd(c) == 1){ # "odd"==1
            poscount = poscount*(-1.0)
            poscount2 = poscount2 + poscount
            poscount = 0
            poscount3 = poscount + poscount2 + poscount3
            poscount2 = 0
          } #扫描情感词前的否定词数
          else{
            poscount3 = poscount + poscount2 + poscount3
            poscount = 0
          }
          a = i + 1 #情感词的位置变化  
        } 
        else if (word %in% negdict){#消极情感的分析，与上面一致
          negcount = negcount + 1
          d = 0
          for (w in segtmp[a:i]){#扫描情感词前的程度词
            if (w %in% mostdict){
              negcount = negcount*4.0
            }
            else if (w %in% verydict){
              negcount = negcount*3.0
            }
            else if (w %in% moredict){
              negcount = negcount*2.0  
            }
            else if (w %in% ishdict){
              negcount = negcount/2.0
            }
            else if (w %in% insufficientdict){
              negcount = negcount/4.0
            }
            else if (w %in% inversedict){
            d = d + 1
            }
          }
          if (judgeodd(c) == 1){
            negcount = negcount*(-1.0)
            negcount2 = negcount2 + negcount
            negcount = 0
            negcount3 = negcount + negcount2 + negcount3
            negcount2 = 0
          } #扫描情感词前的否定词数
          else{
            negcount3 = negcount + negcount2 + negcount3
            negcount = 0
          }
          a = i + 1 #情感词的位置变化 
        }
        else if (word == '！'| word == '!'){
          for (w2 in segtmp[1:length(segtmp)-1]){#扫描感叹号前的情感词，发现后权值+2，然后退出循环
            if (w2 %in% c(posdict[,1], negdict[,1]) ){
              poscount3 = poscount3 + 2
              negcount3 = poscount3 + 2
              break 
            }  
          } 
            
        } ##判断句子是否有感叹号
        i = i + 1 #扫描词位置前移
      }
      #以下是防止出现负数的情况
      pos_count = 0
      neg_count = 0
      if (poscount3 < 0 & negcount3 > 0){
        neg_count = neg_count + negcount3 - poscount3
        pos_count = 0
      }
      else if (negcount3 < 0 & poscount3 > 0){
        pos_count = poscount3 - negcount3
        neg_count = 0
      }
      else if (poscount3 < 0 & negcount3 < 0){
        neg_count = -poscount3
        pos_count = -negcount3
      }
      else{
        pos_count = poscount3
        neg_count = negcount3
        count1 = c(count1, pos_count, neg_count)
        count2 = c(count2, count1)
        count1 = c()
      }
    }
  }  
  return (count2)  
}

sentiment_score_list(cuted_data)


# sent = cuted_data[[1]][1] # Testing with this sent
segtmp = segmentCN(sent, nosymbol = FALSE, nature = FALSE)  #把句子进行分词，以列表的形式返回
for (word in segtmp){
  i = 1 #记录扫描到的词的位置
  cat(segtmp[i])
  a = 1 #记录情感词的位置
  cat(segtmp[a])
  poscount = 0 #积极词的第一次分值
  poscount2 = 0 #积极词反转后的分值
  poscount3 = 0 #积极词的最后分值（包括叹号的分值）
  negcount = 0
  negcount2 = 0
  negcount3 = 0
  if (word %in% posdict){#判断词语是否是情感词
    poscount = poscount + 1                
    c = 0 # 记录分句中否定词数量
    cat(c)
    for (w in segtmp[a:i]){#扫描情感词前的程度词
      cat("this is", w)
      if (w %in% mostdict){
        poscount = poscount*4.0
      }
      else if (w %in% verydict){
        poscount = poscount*3.0
      }
      else if (w %in% moredict){
        poscount = poscount*2.0  
      }
      else if (w %in% ishdict){
        poscount = poscount/2.0
      }
      else if (w %in% insufficientdict){
        poscount = poscount/4.0
      }
      else if (w %in% inversedict){
        c <<- c + 1
        cat(c)
      }   
      
    }
    a = i + 1
    cat(c)
  }
  i = i + 1
}

