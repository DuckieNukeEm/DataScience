# Data Set location:
# https://archive.ics.uci.edu/ml/datasets/human+activity+recognition+using+smartphones
# Source of knowledge
# https://www.analyticsvidhya.com/blog/2018/05/24-ultimate-data-science-projects-to-boost-your-knowledge-and-skills/

#######
#
# Loading Packages
#
#######

library(tidyverse)
library(randomForest)

#######
#
# Loading data and splitting it up
#
#######


col_labels <- readLines("features.txt")

train_all = bind_cols(read_table("train/y_train.txt", col_names = 'Labels'),
                        read_table("train/X_train.txt", col_names = col_labels)) %>%
              mutate(Labels = factor(Labels))

split_samp = sample(1:nrow(train_all), nrow(train_all) * 0.7)

train_sub = train_all[split_samp,]
test_sub = train_all[-split_samp,]

test_final = bind_cols(read_table("test/y_test.txt", col_names = 'Labels'),
                        read_table("test/X_test.txt", col_names = col_labels)) 


#######
#
# There's a lot of data so I'm going to PCA the shit out of it!
#
######





prin_comp = prcomp(train_sub[,-1], scale. = T)
prop_varex <- prin_comp$sdev^2/sum(prin_comp$sdev^2)

plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")


plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")



train_pca =   bind_cols(Labels = train_sub$Labels,
             data.frame( predict(prin_comp, newdata = train_sub[,-1] ) )
              )

train_pca = train_pca[,1:90]



#######
#
# Random Forest on the PCA
#
#######

rf_df = randomForest(Labels ~., data= train_pca)

acc_tbl = table(predict(rf_df, newdata = train_pca), train_pca$Labels)

acc_tbl

sum(diag(acc_tbl))/sum(acc_tbl)

test_pca =   bind_cols(Labels = test_sub$Labels,
                        data.frame( predict(prin_comp, newdata = test_sub[,-1] ) )
)

test_pca = test_pca[,1:90]

acc_tbl = table(predict(rf_df, newdata = test_pca), test_pca$Labels)

acc_tbl

sum(diag(acc_tbl))/sum(acc_tbl)


######
#
# Random forest on the normal data
#
######

## gotta make the variables play Nice
train_sub_hotswap = train_sub
names(train_sub_hotswap) = c('Labels',paste('X_',seq(1:561),  sep ='') )


rf_nonPCA_df = randomForest(Labels ~., data = train_sub_hotswap)


acc_tbl = table(predict(rf_nonPCA_df, newdata = train_sub_hotswap), train_sub_hotswap$Labels)

acc_tbl

sum(diag(acc_tbl))/sum(acc_tbl)



test_sub_hotswap = test_sub
names(test_sub_hotswap) = c('Labels',paste('X_',seq(1:561),  sep ='') )

acc_tbl = table(predict(rf_nonPCA_df, newdata = test_sub_hotswap), test_sub_hotswap$Labels)

acc_tbl

sum(diag(acc_tbl))/sum(acc_tbl)





