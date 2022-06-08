library(tidyverse)
library(tictoc)
library(data.table)
library(prophet)


final <- fread('kaggle/predict_future_sales/data/final_train.csv') 
# data.table is formattted by data[i, j, by]
# translates to where (dataset) --> i (order by) and select --> k (update/calculate) --> by (group by)

ex <- final[date_block_num > 23]
ex[, .(month_yr, Count)]
