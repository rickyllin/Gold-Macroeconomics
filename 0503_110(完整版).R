rm(list = ls())
library(dplyr)
###讀取行業資料###
data_110 <- read.csv("/Users/linzili/研究工作/政府調查/Y110S0204.csv")
names(data_110) <- c("NSML","UNIT", "IND", "ORG", "YEAR", "010", "EMP", "SAL", "INVESTAS","ASSET", "REV", 
                     "COST", "COMP1", "COMP2","COMP3", "COMP4","COMP5","COMP6", "BRAND", "TRIREV", "TRICOST","HEMP",
                     "SH7", "CSID", "RENTAS", "RES","OUTSALE", "REVGRP", "EMPGRP","NTAX")
data_110 <- data_110[data_110$NTAX != "O", ]

data <- read.csv("/Users/linzili/研究工作/政府調查/town_group1.csv")
data_110 <- left_join(data_110, data, by = c("SH7" = "村里代碼"))

y110_5611 <- subset(data_110, IND == 4541)
y110_5611 <- y110_5611 %>% mutate(SEQ = row_number())
y110_5611$SH4 <- ifelse(substr(y110_5611$SH7, 1, 2) %in% c("63", "64", "65", "66", "67", "68"), 1,
                        ifelse((nchar(y110_5611$SH7) == 7 & substr(y110_5611$SH7, 1, 2) %in% c("18")) |
                                 (nchar(y110_5611$SH7) == 6 & substr(y110_5611$SH7, 1, 1) %in% c("4", "7")), 2,
                               ifelse(substr(y110_5611$SH7, 1, 2) %in% c("16", "71", "72"), 3, 4)))

y110_5611$YEAR <- 110 - y110_5611$YEAR
y110_5611 <- y110_5611 %>% mutate(Company_ID = as.character(row_number()))
####################################################

###將COMP2、COMP3、COMP4變數合併成COMP###
y110_5611$COMP2[y110_5611$COMP2 == 0] <- 2
y110_5611$COMP3[y110_5611$COMP3 == 0] <- 2
y110_5611$COMP4[y110_5611$COMP4 == 0] <- 2
y110_5611$COMP5[y110_5611$COMP5 == 0] <- 2 # comp5 6 不納入
y110_5611$COMP6[y110_5611$COMP6 == 0] <- 2

# 合併SH(如果有需要的話)
# y110_5611 <- y110_5611 %>%
#   mutate(SH = case_when(
#     SH %in% c(1, 2) ~ 1,
#     SH %in% c(3, 4) ~ 2,
#     SH %in% c(5, 6) ~ 3,
#     SH %in% c(7, 8) ~ 4,
#     TRUE ~ SH  # 其他值保持不變
#   ))
####################################

###該行業未申報及有申報但須納入抽樣店家資料###
nrow(y110_5611[y110_5611$NTAX == "N" & y110_5611$NSML == "0", ])

###處理變數###
factor_vars <- c("SH", "ORG", "UNIT", "RES", "BRAND", 
                 "COMP1", "COMP2", "COMP3", "COMP4", "COMP5", "COMP6", "SH7","RENTAS","OUTSALE","NSML", "SH4")

y110_5611[factor_vars] <- lapply(y110_5611[factor_vars], factor)

###輔助變數選取(#製造、批發、零售業有+OUTSALE+TRIREV+TRICOST #F020300僅製造業有 #若是該資料某變數只有一個值需移除)###
costlm<-lm(COST~ORG+YEAR+EMP+SAL+RENTAS+RES+BRAND+COMP1+COMP2+COMP3+COMP4+SH4,data = y110_5611)
costvar<-step(costlm,scope = list(upper=costlm), direction="both")
revlm<-lm(REV~ORG+YEAR+EMP+SAL+RENTAS+RES+BRAND+COMP1+COMP2+COMP3+COMP4+SH4,data = y110_5611)
revvar<-step(revlm,scope = list(upper=revlm), direction="both")
costs<-unlist(strsplit(strsplit(as.character(unlist(costvar$call$formula)),"~")[[3]]," "))
revs<-unlist(strsplit(strsplit(as.character(unlist(revvar$call$formula)),"~")[[3]]," "))
varselect<-unique(c(costs[-which(costs%in%"+")],revs[-which(revs%in%"+")]))
varselect<-c("COST","REV",varselect)
comp_group <- c("COMP1", "COMP2", "COMP3", "COMP4")# 放跑回歸有的
if (any(comp_group %in% varselect)) {
  varselect <- unique(c(varselect, comp_group))
} else {
  varselect <- setdiff(varselect, comp_group)
}
#######################################################################################

###REV設CV處理全查層資料(#CV越小全查層店家數越多)###
library(stratification)
revtakeall<-strata.LH(y110_5611$REV,CV=0.05,takeall = 1)
#################################

###未申報店家扣掉全查層剩餘店家資料###
y110_5611_new<-y110_5611[-which(revtakeall$stratumID==3),]

###處理全查層變數###
y110_5611[which(revtakeall$stratumID == 3), "STRATA"] <- 0
y110_5611[which(revtakeall$stratumID == 3), "dom"] <- 1 #為了之後合併

###分層變數cost,asset,emp目標變數rev之bb法分層抽樣###
misnum_b<-length(y110_5611_new$SEQ)
library(memoise)
library(SamplingStrata)
y110_5611_new$dom <- 1
summary.all<-list()
numsam.all<-list()
data.all<-list()

###設定目標變數之cv值###
treatment<-c(0.005)
layerCount <- 4  
summary.all <- list()
numsam.all <- list()
data.all <- list()
sample_results <- list()

for (j in 1:8) {  
  framet <- buildFrameDF(
    y110_5611_new[y110_5611_new$SH == j, ],
    id = "SEQ",
    domainvalue = "dom",
    X = c("COST", "EMP"),
    Y = c("REV")
  )
  
  for (i in treatment) {
    set.seed(109)
    data("errors")
    errors[1, 2] <- i
    errors[1, 4] <- 1
    
    solutiont <- optimStrata(
      method = "continuous",
      errors = errors[1, c(1, 2, 4)],
      framesamp = framet,
      nStrata = layerCount,
      showPlot = FALSE
    )
    
    strsummary <- summaryStrata(solutiont$framenew, solutiont$aggr_strata)
    numsam <- sum(strsummary$Allocation)
    y110_5611_mix3 <- y110_5611_new[y110_5611_new$SH == j, ]
    y110_5611_mix3$STRATA <- solutiont$indices$X1
    
    temp_data <- y110_5611_mix3[y110_5611_mix3$SH == j, ]
    
    sampled_indices <- list()  
    for (stratum in 1:layerCount) {
      base_sample <- sample(
        which(temp_data$STRATA == stratum),
        strsummary$Allocation[stratum]
      )
      additional_sample <- which(
        temp_data$STRATA == stratum &
          (temp_data$UNIT == 8 | temp_data$YEAR <= 5 | temp_data$NTAX == "T" | (temp_data$NTAX == "N" & temp_data$NSML != "0"))
      )
      sampled_indices[[stratum]] <- union(base_sample, additional_sample)
    }
    
    col_mix3 <- unlist(sampled_indices)
    del_mix3 <- temp_data$SEQ[-col_mix3]
    
    temp_data$COST[which(temp_data$SEQ %in% del_mix3)] <- NA
    temp_data$REV[which(temp_data$SEQ %in% del_mix3)] <- NA
    
    key <- paste0("SH", j, "_T", which(treatment %in% i))
    summary.all[[key]] <- strsummary
    numsam.all[[key]] <- numsam
    data.all[[key]] <- temp_data
    
    sample_results[[key]] <- setNames(sampled_indices, paste0("x", 1:layerCount))
  }
}

combined_data <- bind_rows(data.all, .id = "source")
combined_list <- split(combined_data, combined_data$SH)

for (i in 1:8) {
  current_data <- combined_list[[i]]
  
  抽出來的樣本數 <- sapply(1:layerCount, function(x) {
    length(sample_results[[paste0("SH", i, "_T1")]][[paste0("x", x)]])
  })
  
  母體中已申報及未申報之必查 <- sapply(1:layerCount, function(x) {
    length(which(current_data$STRATA == x &
                   (current_data$UNIT == 8 | current_data$YEAR <= 5 | current_data$NTAX == "T" | (current_data$NTAX == "N" & current_data$NSML != "0"))))
  })
  
  可抽值 <- summary.all[[paste0("SH", i, "_T1")]]$Population - 母體中已申報及未申報之必查
  
  從可抽抽出來的樣本數 <- sapply(1:layerCount, function(x) {
    length(setdiff(
      sample_results[[paste0("SH", i, "_T1")]][[paste0("x", x)]],
      which(current_data$STRATA == x &
              (current_data$UNIT == 8 | current_data$YEAR <= 5 | current_data$NTAX == "T" | (current_data$NTAX == "N" & current_data$NSML != "0")))
    ))
  })
  
  需插補 <- 可抽值 - 從可抽抽出來的樣本數
  
  summary.all[[paste0("SH", i, "_T1")]] <- summary.all[[paste0("SH", i, "_T1")]] %>%
    mutate(
      實際抽出來的樣本數 = 抽出來的樣本數,
      母體中已申報及未申報之必查 = 母體中已申報及未申報之必查,
      可抽 = 可抽值,
      從可抽抽出來的樣本數 = 從可抽抽出來的樣本數,
      需插補 = 需插補
    )
}
summary.all
#############################################################################
###抽樣數###
library(purrr)
library(tibble)
table <- tibble(
  Population = nrow(y110_5611),
  全查= length(which(revtakeall$stratumID == 3)),
  必查 = sum(map_dbl(summary.all, ~ sum(.x$母體中已申報及未申報之必查, na.rm = TRUE))),
  可抽 = sum(map_dbl(summary.all, ~ sum(.x$可抽, na.rm = TRUE))),
  從可抽抽出來的樣本數 = sum(map_dbl(summary.all, ~ sum(.x$從可抽抽出來的樣本數, na.rm = TRUE))),
  Allocation = sum(map_dbl(summary.all, ~ sum(.x$Allocation, na.rm = TRUE))),
  SamplingRate1 = (全查 + Allocation) / Population,
  SamplingRate2 = 從可抽抽出來的樣本數/ 可抽
)
table


###進行插補 ###
###因應不同CV值 名稱格式為#imp.插補方法_CV值# #插補方法_CV值_total_var_list# ###vn == variable name###

###設定需計算總和與變異數欄位###
columns <- c("COST", "REV")

###平均值插補法###
for(i in 1:length(treatment)){
  
  # 因應更改CV動態修改變數名稱
  imp_mean_vn <- paste0("imp.mean_", treatment[i]) # imp.mean_CV值
  mean_var_list_name <- paste0("mean_", treatment[i], "_total_var_list") # mean_CV值_total_var_list
  mean_sum_list_name <- paste0("mean_", treatment[i], "_total_sum_list") # mean_CV值_total_sum_list
  assign(imp_mean_vn, list()) # 初始化imp_mean_CV值
  
  y110_5611_mean_list <- list()
  for (j in 1:length(data.all)){
    temp_mean <- data.all[[j]]
    ###分層插補###
    for (k in 1:layerCount){
      temp_mean$COST[which(is.na(temp_mean$COST)=="TRUE"&temp_mean$STRATA==k)]<-mean(temp_mean$COST[temp_mean$STRATA==k],na.rm = T)
      temp_mean$REV[which(is.na(temp_mean$REV)=="TRUE"&temp_mean$STRATA==k)]<-mean(temp_mean$REV[temp_mean$STRATA==k],na.rm = T)
    }
    y110_5611_mean_list[[j]] <- temp_mean  
  }
  
  y110_5611_mean <- do.call(rbind, y110_5611_mean_list)
  y110_5611_mean <- y110_5611_mean_list %>%
    bind_rows() %>%
    bind_rows(
      y110_5611 %>%
        filter(revtakeall$stratumID == 3)
    ) %>%
    mutate(
      SH7 = as.character(SH7),
      SH22 = if_else(nchar(SH7) == 6, substr(SH7, 1, 1), substr(SH7, 1, 2))
    )
  tmp <- get(imp_mean_vn)
  tmp[[i]] <- y110_5611_mean
  assign(imp_mean_vn, tmp) # 插補結果給imp.mean_CV值
}

mean_var_list <- list()
mean_sum_list <- list()
for (j in 1:length(data.all)) {
  mean_var_list[[paste0("group", j)]] <- sapply(y110_5611_mean_list[[j]][columns], var)
  mean_sum_list[[paste0("group", j)]] <- sapply(y110_5611_mean_list[[j]][columns], sum)
}
assign(mean_var_list_name, mean_var_list)
assign(mean_sum_list_name, mean_sum_list)
#############################################################

###中位數插補法###
for(i in 1:length(treatment)){
  
  imp_median_vn <- paste0("imp.median_", treatment[i])
  median_var_list_name <- paste0("median_", treatment[i], "_total_var_list")
  median_sum_list_name <- paste0("median_", treatment[i], "_total_sum_list")
  assign(imp_median_vn, list())
  
  y110_5611_median_list <- list()
  ###分層插補###
  for (j in 1:length(data.all)){
    temp_median <- data.all[[j]]
    ###分層插補###
    for (k in 1:layerCount){
      temp_median$COST[which(is.na(temp_median$COST)=="TRUE"&temp_median$STRATA==k)]<-median(temp_median$COST[temp_median$STRATA==k],na.rm = T)
      temp_median$REV[which(is.na(temp_median$REV)=="TRUE"&temp_median$STRATA==k)]<-median(temp_median$REV[temp_median$STRATA==k],na.rm = T)
    }
    y110_5611_median_list[[j]] <- temp_median  
  }
  
  y110_5611_median <- y110_5611_median_list %>%
    bind_rows() %>%
    bind_rows(
      y110_5611 %>%
        filter(revtakeall$stratumID == 3)
    ) %>%
    mutate(
      SH7 = as.character(SH7),
      SH22 = if_else(nchar(SH7) == 6, substr(SH7, 1, 1), substr(SH7, 1, 2))
    )
  tmp <- get(imp_median_vn)
  tmp[[i]] <- y110_5611_median
  assign(imp_median_vn, tmp)
}

median_var_list <- list()
median_sum_list <- list()
for (j in 1:length(data.all)) {
  median_var_list[[paste0("group", j)]] <- sapply(y110_5611_median_list[[j]][columns], var)
  median_sum_list[[paste0("group", j)]] <- sapply(y110_5611_median_list[[j]][columns], sum)
}
assign(median_var_list_name, median_var_list)
assign(median_sum_list_name, median_sum_list)
#############################################################

###熱卡插補法###
library(VIM)
set.seed(109)

for(i in 1:length(treatment)){
  
  imp_hotdeck_vn <- paste0("imp.hotdeck_", treatment[i])
  hotdeck_var_list_name <- paste0("hotdeck_", treatment[i], "_total_var_list")
  hotdeck_sum_list_name <- paste0("hotdeck_", treatment[i], "_total_sum_list")
  assign(imp_hotdeck_vn, list())
  
  y110_5611_hotdeck_list <- list()
  for (j in 1:length(data.all)){
    temp_hotdeck<-data.all[[j]]
    ### 分層插補 ###
    for(k in 1:layerCount){
      temp_hotdeck[which(temp_hotdeck$STRATA==k),]<-hotdeck(temp_hotdeck[which(temp_hotdeck$STRATA==k),],variable=c("COST","REV"))[,1:length(temp_hotdeck)]
    }
    y110_5611_hotdeck_list[[j]] <- temp_hotdeck
  }
  
  y110_5611_hotdeck <- y110_5611_hotdeck_list %>%
    bind_rows() %>%
    bind_rows(
      y110_5611 %>%
        filter(revtakeall$stratumID == 3)
    ) %>%
    mutate(
      SH7 = as.character(SH7),
      SH22 = if_else(nchar(SH7) == 6, substr(SH7, 1, 1), substr(SH7, 1, 2))
    )
  tmp <- get(imp_hotdeck_vn)
  tmp[[i]] <- y110_5611_hotdeck
  assign(imp_hotdeck_vn, tmp)
}

hotdeck_var_list <- list()
hotdeck_sum_list <- list()
for (j in 1:length(data.all)) {
  hotdeck_var_list[[paste0("group", j)]] <- sapply(y110_5611_hotdeck_list[[j]][columns], var)
  hotdeck_sum_list[[paste0("group", j)]] <- sapply(y110_5611_hotdeck_list[[j]][columns], sum)
}
assign(hotdeck_var_list_name, hotdeck_var_list)
assign(hotdeck_sum_list_name, hotdeck_sum_list)
#########################################################################

###多重插補前處理全查層變數名稱###
y110_5611_less <- y110_5611[, varselect]# 先刪
#########################################################################

###貝氏迴歸插補法(五組插補資料取平均)###
library(mice)
set.seed(109)

for(i in 1:length(treatment)){
  imp_norm5_vn <- paste0("imp.norm5_", treatment[i])
  norm5_var_list_name <- paste0("norm5_", treatment[i], "_total_var_list")
  norm5_sum_list_name <- paste0("norm5_", treatment[i], "_total_sum_list")
  assign(imp_norm5_vn, list())
  
  y110_5611_norm5_list <- list()
  complete_mice_norm5_list <- list()
  for (k in 1:length(data.all)){
    
    temp_norm5<-data.all[[k]]
    mice_norm5<-list()
    
    ###分層插補###
    for(j in 1:layerCount){
      mice_norm5[[j]]<-mice(data=temp_norm5[which(temp_norm5$STRATA==j),varselect],method = "norm",m=5)
    }
    
    # 將分層的插補結果合併
    complete_mice_norm5 <- vector("list", 5)
    for(m in 1:5) { 
      complete_mice_norm5[[m]] <- do.call(rbind, lapply(1:layerCount, function(j) {
        complete(mice_norm5[[j]], m)
      }))
      complete_mice_norm5[[m]]$REV[is.na(complete_mice_norm5[[m]]$REV)] <- 0
      complete_mice_norm5[[m]]$COST[is.na(complete_mice_norm5[[m]]$COST)] <- 0
      complete_mice_norm5[[m]] <- complete_mice_norm5[[m]] %>%
        bind_rows(y110_5611_less %>% filter(revtakeall$stratumID == 3))
      
      complete_mice_norm5_list[[k]] <- complete_mice_norm5
    }
    temp_norm5 <- temp_norm5 %>% bind_rows(y110_5611 %>% filter(revtakeall$stratumID == 3))
    temp_norm5$COST <- rowMeans(sapply(1:5, function(m) complete_mice_norm5[[m]]$COST))
    temp_norm5$REV <- rowMeans(sapply(1:5, function(m) complete_mice_norm5[[m]]$REV))
    
    y110_5611_norm5_list[[k]] <- temp_norm5
  }
  y110_5611_norm5 <- y110_5611_norm5_list %>%
    bind_rows() %>%
    distinct() %>%
    mutate(
      SH7 = as.character(SH7),
      SH22 = if_else(nchar(SH7) == 6, substr(SH7, 1, 1), substr(SH7, 1, 2))
    )
  tmp <- get(imp_norm5_vn)
  tmp[[i]] <- y110_5611_norm5
  assign(imp_norm5_vn, tmp)
}

norm5_means_list <- vector("list", length(data.all))
norm5_within_var_list <- vector("list", length(data.all))
norm5_within_var_means <- vector("list", length(data.all))
norm5_between_var_list <- vector("list", length(data.all))
assign(norm5_var_list_name, list())
norm5_var_list <- get(norm5_var_list_name)
norm5_sum_list <- list()

for (k in 1:length(data.all)) {
  complete_mice_norm5 <- complete_mice_norm5_list[[k]]  
  
  # 對每個欄位計算m次插補結果的平均值
  norm5_means_list[[k]] <- lapply(columns, function(col) {
    sapply(1:5, function(m) {
      mean(complete_mice_norm5[[m]][[col]], na.rm = TRUE)  
    })
  })
  names(norm5_means_list[[k]]) <- columns
  
  # 計算插補內變異（每組插補數據集內的變異數）
  norm5_within_var_list[[k]] <- lapply(columns, function(col) {
    sapply(1:5, function(m) {
      var(complete_mice_norm5[[m]][[col]])
    })
  })
  names(norm5_within_var_list[[k]]) <- columns
  
  # 計算每個欄位的變異數平均值
  norm5_within_var_means[[k]] <- sapply(norm5_within_var_list[[k]], mean)
  
  # 計算每個欄位的平均值之間的變異數
  norm5_between_var_list[[k]] <- sapply(norm5_means_list[[k]], var)
  
  # 計算總變異數（Rubin's Rules）
  norm5_var_list[[paste0("group", k)]] <- norm5_within_var_means[[k]] + (1 + 1/5) * norm5_between_var_list[[k]]
  
  norm5_sum_list[[paste0("group", k)]] <- sapply(y110_5611_norm5_list[[k]][columns], sum)
}

assign(norm5_var_list_name, norm5_var_list)
assign(norm5_sum_list_name, norm5_sum_list)
###################################################################################

###貝氏迴歸插補法(十組插補資料取平均)###
library(mice)
set.seed(109)

for(i in 1:length(treatment)){
  imp_norm10_vn <- paste0("imp.norm10_", treatment[i])
  norm10_var_list_name <- paste0("norm10_", treatment[i], "_total_var_list")
  norm10_sum_list_name <- paste0("norm10_", treatment[i], "_total_sum_list")
  assign(imp_norm10_vn, list())
  
  y110_5611_norm10_list <- list()
  complete_mice_norm10_list <- list()
  for (k in 1:length(data.all)){
    
    temp_norm10<-data.all[[k]]
    mice_norm10<-list()
    
    ###分層插補###
    for(j in 1:layerCount){
      mice_norm10[[j]]<-mice(data=temp_norm10[which(temp_norm10$STRATA==j),varselect],method = "norm",m=10)
    }
    
    # 將分層的插補結果合併
    complete_mice_norm10 <- vector("list", 10)
    for(m in 1:10) { 
      complete_mice_norm10[[m]] <- do.call(rbind, lapply(1:layerCount, function(j) {
        complete(mice_norm10[[j]], m)
      }))
      complete_mice_norm10[[m]]$REV[is.na(complete_mice_norm10[[m]]$REV)] <- 0
      complete_mice_norm10[[m]]$COST[is.na(complete_mice_norm10[[m]]$COST)] <- 0
      complete_mice_norm10[[m]] <- complete_mice_norm10[[m]] %>%
        bind_rows(y110_5611_less %>% filter(revtakeall$stratumID == 3))
      
      complete_mice_norm10_list[[k]] <- complete_mice_norm10
    }
    temp_norm10 <- temp_norm10 %>% bind_rows(y110_5611 %>% filter(revtakeall$stratumID == 3))
    temp_norm10$COST <- rowMeans(sapply(1:10, function(m) complete_mice_norm10[[m]]$COST))
    temp_norm10$REV <- rowMeans(sapply(1:10, function(m) complete_mice_norm10[[m]]$REV))
    
    y110_5611_norm10_list[[k]] <- temp_norm10
  }
  y110_5611_norm10 <- y110_5611_norm10_list %>%
    bind_rows() %>%
    distinct() %>%
    mutate(
      SH7 = as.character(SH7),
      SH22 = if_else(nchar(SH7) == 6, substr(SH7, 1, 1), substr(SH7, 1, 2))
    )
  tmp <- get(imp_norm10_vn)
  tmp[[i]] <- y110_5611_norm10
  assign(imp_norm10_vn, tmp)
}

norm10_means_list <- vector("list", length(data.all))
norm10_within_var_list <- vector("list", length(data.all))
norm10_within_var_means <- vector("list", length(data.all))
norm10_between_var_list <- vector("list", length(data.all))
assign(norm10_var_list_name, list())
norm10_var_list <- get(norm10_var_list_name)
norm10_sum_list <- list()

for (k in 1:length(data.all)) {
  complete_mice_norm10 <- complete_mice_norm10_list[[k]]  
  
  # 對每個欄位計算m次插補結果的平均值
  norm10_means_list[[k]] <- lapply(columns, function(col) {
    sapply(1:10, function(m) {
      mean(complete_mice_norm10[[m]][[col]], na.rm = TRUE)  
    })
  })
  names(norm10_means_list[[k]]) <- columns
  
  # 計算插補內變異（每組插補數據集內的變異數）
  norm10_within_var_list[[k]] <- lapply(columns, function(col) {
    sapply(1:10, function(m) {
      var(complete_mice_norm10[[m]][[col]])
    })
  })
  names(norm10_within_var_list[[k]]) <- columns
  
  # 計算每個欄位的變異數平均值
  norm10_within_var_means[[k]] <- sapply(norm10_within_var_list[[k]], mean)
  
  # 計算每個欄位的平均值之間的變異數
  norm10_between_var_list[[k]] <- sapply(norm10_means_list[[k]], var)
  
  # 計算總變異數（Rubin's Rules）
  norm10_var_list[[paste0("group", k)]] <- norm10_within_var_means[[k]] + (1 + 1/10) * norm10_between_var_list[[k]]
  
  norm10_sum_list[[paste0("group", k)]] <- sapply(y110_5611_norm10_list[[k]][columns], sum)
}

assign(norm10_var_list_name, norm10_var_list)
assign(norm10_sum_list_name, norm10_sum_list)
###################################################################################

###預測均值配對插補法(五組插補資料取平均)###
library(mice)
set.seed(109)

for(i in 1:length(treatment)){
  imp_pmm5_vn <- paste0("imp.pmm5_", treatment[i])
  pmm5_var_list_name <- paste0("pmm5_", treatment[i], "_total_var_list")
  pmm5_sum_list_name <- paste0("pmm5_", treatment[i], "_total_sum_list")
  assign(imp_pmm5_vn, list())
  
  y110_5611_pmm5_list <- list()
  complete_mice_pmm5_list <- list()
  for (k in 1:length(data.all)){
    
    temp_pmm5<-data.all[[k]]
    mice_pmm5<-list()
    
    ###分層插補###
    for(j in 1:layerCount){
      mice_pmm5[[j]]<-mice(data=temp_pmm5[which(temp_pmm5$STRATA==j),varselect],method = "pmm",m=5)
    }
    
    # 將分層的插補結果合併
    complete_mice_pmm5 <- vector("list", 5)
    for(m in 1:5) { 
      complete_mice_pmm5[[m]] <- do.call(rbind, lapply(1:layerCount, function(j) {
        complete(mice_pmm5[[j]], m)
      }))
      complete_mice_pmm5[[m]]$REV[is.na(complete_mice_pmm5[[m]]$REV)] <- 0
      complete_mice_pmm5[[m]]$COST[is.na(complete_mice_pmm5[[m]]$COST)] <- 0
      complete_mice_pmm5[[m]] <- complete_mice_pmm5[[m]] %>%
        bind_rows(y110_5611_less %>% filter(revtakeall$stratumID == 3 ))
      
      complete_mice_pmm5_list[[k]] <- complete_mice_pmm5
    }
    temp_pmm5 <- temp_pmm5 %>% bind_rows(y110_5611 %>% filter(revtakeall$stratumID == 3))
    temp_pmm5$COST <- rowMeans(sapply(1:5, function(m) complete_mice_pmm5[[m]]$COST))
    temp_pmm5$REV <- rowMeans(sapply(1:5, function(m) complete_mice_pmm5[[m]]$REV))
    
    y110_5611_pmm5_list[[k]] <- temp_pmm5
  }
  y110_5611_pmm5 <- y110_5611_pmm5_list %>%
    bind_rows() %>%
    distinct() %>%
    mutate(
      SH7 = as.character(SH7),
      SH22 = if_else(nchar(SH7) == 6, substr(SH7, 1, 1), substr(SH7, 1, 2))
    )
  tmp <- get(imp_pmm5_vn)
  tmp[[i]] <- y110_5611_pmm5
  assign(imp_pmm5_vn, tmp)
}

pmm5_means_list <- vector("list", length(data.all))
pmm5_within_var_list <- vector("list", length(data.all))
pmm5_within_var_means <- vector("list", length(data.all))
pmm5_between_var_list <- vector("list", length(data.all))
assign(pmm5_var_list_name, list())
pmm5_var_list <- get(pmm5_var_list_name)
pmm5_sum_list <- list()

for (k in 1:length(data.all)) {
  complete_mice_pmm5 <- complete_mice_pmm5_list[[k]]  
  
  # 對每個欄位計算m次插補結果的平均值
  pmm5_means_list[[k]] <- lapply(columns, function(col) {
    sapply(1:5, function(m) {
      mean(complete_mice_pmm5[[m]][[col]], na.rm = TRUE)  
    })
  })
  names(pmm5_means_list[[k]]) <- columns
  
  # 計算插補內變異（每組插補數據集內的變異數）
  pmm5_within_var_list[[k]] <- lapply(columns, function(col) {
    sapply(1:5, function(m) {
      var(complete_mice_pmm5[[m]][[col]])
    })
  })
  names(pmm5_within_var_list[[k]]) <- columns
  
  # 計算每個欄位的變異數平均值
  pmm5_within_var_means[[k]] <- sapply(pmm5_within_var_list[[k]], mean)
  
  # 計算每個欄位的平均值之間的變異數
  pmm5_between_var_list[[k]] <- sapply(pmm5_means_list[[k]], var)
  
  # 計算總變異數（Rubin's Rules）
  pmm5_var_list[[paste0("group", k)]] <- pmm5_within_var_means[[k]] + (1 + 1/5) * pmm5_between_var_list[[k]]
  
  pmm5_sum_list[[paste0("group", k)]] <- sapply(y110_5611_pmm5_list[[k]][columns], sum)
}

assign(pmm5_var_list_name, pmm5_var_list)
assign(pmm5_sum_list_name, pmm5_sum_list)
###################################################################################

###預測均值配對插補法(十組插補資料取平均)###
library(mice)
set.seed(109)

for(i in 1:length(treatment)){
  imp_pmm10_vn <- paste0("imp.pmm10_", treatment[i])
  pmm10_var_list_name <- paste0("pmm10_", treatment[i], "_total_var_list")
  pmm10_sum_list_name <- paste0("pmm10_", treatment[i], "_total_sum_list")
  assign(imp_pmm10_vn, list())
  
  y110_5611_pmm10_list <- list()
  complete_mice_pmm10_list <- list()
  for (k in 1:length(data.all)){
    
    temp_pmm10<-data.all[[k]]
    mice_pmm10<-list()
    
    ###分層插補###
    for(j in 1:layerCount){
      mice_pmm10[[j]]<-mice(data=temp_pmm10[which(temp_pmm10$STRATA==j),varselect],method = "pmm",m=10)
    }
    
    # 將分層的插補結果合併
    complete_mice_pmm10 <- vector("list", 10)
    for(m in 1:10) { 
      complete_mice_pmm10[[m]] <- do.call(rbind, lapply(1:layerCount, function(j) {
        complete(mice_pmm10[[j]], m)
      }))
      complete_mice_pmm10[[m]]$REV[is.na(complete_mice_pmm10[[m]]$REV)] <- 0
      complete_mice_pmm10[[m]]$COST[is.na(complete_mice_pmm10[[m]]$COST)] <- 0
      complete_mice_pmm10[[m]] <- complete_mice_pmm10[[m]] %>%
        bind_rows(y110_5611_less %>% filter(revtakeall$stratumID == 3))
      
      complete_mice_pmm10_list[[k]] <- complete_mice_pmm10
    }
    temp_pmm10 <- temp_pmm10 %>% bind_rows(y110_5611 %>% filter(revtakeall$stratumID == 3))
    temp_pmm10$COST <- rowMeans(sapply(1:10, function(m) complete_mice_pmm10[[m]]$COST))
    temp_pmm10$REV <- rowMeans(sapply(1:10, function(m) complete_mice_pmm10[[m]]$REV))
    
    y110_5611_pmm10_list[[k]] <- temp_pmm10
  }
  y110_5611_pmm10 <- y110_5611_pmm10_list %>%
    bind_rows() %>%
    distinct() %>%
    mutate(
      SH7 = as.character(SH7),
      SH22 = if_else(nchar(SH7) == 6, substr(SH7, 1, 1), substr(SH7, 1, 2))
    )
  tmp <- get(imp_pmm10_vn)
  tmp[[i]] <- y110_5611_pmm10
  assign(imp_pmm10_vn, tmp)
}

pmm10_means_list <- vector("list", length(data.all))
pmm10_within_var_list <- vector("list", length(data.all))
pmm10_within_var_means <- vector("list", length(data.all))
pmm10_between_var_list <- vector("list", length(data.all))
assign(pmm10_var_list_name, list())
pmm10_var_list <- get(pmm10_var_list_name)
pmm10_sum_list <- list()

for (k in 1:length(data.all)) {
  complete_mice_pmm10 <- complete_mice_pmm10_list[[k]]  
  
  # 對每個欄位計算m次插補結果的平均值
  pmm10_means_list[[k]] <- lapply(columns, function(col) {
    sapply(1:10, function(m) {
      mean(complete_mice_pmm10[[m]][[col]], na.rm = TRUE)  
    })
  })
  names(pmm10_means_list[[k]]) <- columns
  
  # 計算插補內變異（每組插補數據集內的變異數）
  pmm10_within_var_list[[k]] <- lapply(columns, function(col) {
    sapply(1:10, function(m) {
      var(complete_mice_pmm10[[m]][[col]])
    })
  })
  names(pmm10_within_var_list[[k]]) <- columns
  
  # 計算每個欄位的變異數平均值
  pmm10_within_var_means[[k]] <- sapply(pmm10_within_var_list[[k]], mean)
  
  # 計算每個欄位的平均值之間的變異數
  pmm10_between_var_list[[k]] <- sapply(pmm10_means_list[[k]], var)
  
  # 計算總變異數（Rubin's Rules）
  pmm10_var_list[[paste0("group", k)]] <- pmm10_within_var_means[[k]] + (1 + 1/10) * pmm10_between_var_list[[k]]
  
  pmm10_sum_list[[paste0("group", k)]] <- sapply(y110_5611_pmm10_list[[k]][columns], sum)
}

assign(pmm10_var_list_name, pmm10_var_list)
assign(pmm10_sum_list_name, pmm10_sum_list)
####################################################################################

###條件2:絕對數(收入-支出)/收入<=0.8(記錄不滿足之筆數)###
condition2<-list()
for (i in 1:length(treatment)){
  
  b1<-length(which(abs(get(paste0("imp.mean_", treatment[i]))[[i]]$REV-get(paste0("imp.mean_", treatment[i]))[[i]]$COST)/get(paste0("imp.mean_", treatment[i]))[[i]]$REV>0.8))
  b2<-length(which(abs(get(paste0("imp.median_", treatment[i]))[[i]]$REV-get(paste0("imp.median_", treatment[i]))[[i]]$COST)/get(paste0("imp.median_", treatment[i]))[[i]]$REV>0.8))
  b3<-length(which(abs(get(paste0("imp.hotdeck_", treatment[i]))[[i]]$REV-get(paste0("imp.hotdeck_", treatment[i]))[[i]]$COST)/get(paste0("imp.hotdeck_", treatment[i]))[[i]]$REV>0.8))
  b4<-length(which(abs(get(paste0("imp.norm5_", treatment[i]))[[i]]$REV-get(paste0("imp.norm5_", treatment[i]))[[i]]$COST)/get(paste0("imp.norm5_", treatment[i]))[[i]]$REV>0.8))
  b5<-length(which(abs(get(paste0("imp.norm10_", treatment[i]))[[i]]$REV-get(paste0("imp.norm10_", treatment[i]))[[i]]$COST)/get(paste0("imp.norm10_", treatment[i]))[[i]]$REV>0.8))
  b6<-length(which(abs(get(paste0("imp.pmm5_", treatment[i]))[[i]]$REV-get(paste0("imp.pmm5_", treatment[i]))[[i]]$COST)/get(paste0("imp.pmm5_", treatment[i]))[[i]]$REV>0.8))
  b7<-length(which(abs(get(paste0("imp.pmm10_", treatment[i]))[[i]]$REV-get(paste0("imp.pmm10_", treatment[i]))[[i]]$COST)/get(paste0("imp.pmm10_", treatment[i]))[[i]]$REV>0.8))
  b8<-length(which(abs(y110_5611_new$REV-y110_5611_new$COST)/y110_5611_new$REV>0.8))
  con2<-cbind(b1,b2,b3,b4,b5,b6,b7,b8)
  colnames(con2)<-c("mean","median","hotdeck","norm_5","norm_10","pmm_5","pmm_10","original")  
  condition2[[i]]<-con2
}
condition2
#########################################################################################

###條件3:總支出>=僱用員工薪資###必滿足
condition3<-list()
for (i in 1:length(treatment)){
  
  a1<-length(which(get(paste0("imp.mean_", treatment[i]))[[i]]$COST<get(paste0("imp.mean_", treatment[i]))[[i]]$SAL))
  a2<-length(which(get(paste0("imp.median_", treatment[i]))[[i]]$COST<get(paste0("imp.median_", treatment[i]))[[i]]$SAL))
  a3<-length(which(get(paste0("imp.hotdeck_", treatment[i]))[[i]]$COST<get(paste0("imp.hotdeck_", treatment[i]))[[i]]$SAL))
  a4<-length(which(get(paste0("imp.norm5_", treatment[i]))[[i]]$COST<get(paste0("imp.norm5_", treatment[i]))[[i]]$SAL))
  a5<-length(which(get(paste0("imp.norm10_", treatment[i]))[[i]]$COST<get(paste0("imp.norm10_", treatment[i]))[[i]]$SAL))
  a6<-length(which(get(paste0("imp.pmm5_", treatment[i]))[[i]]$COST<get(paste0("imp.pmm5_", treatment[i]))[[i]]$SAL))
  a7<-length(which(get(paste0("imp.pmm10_", treatment[i]))[[i]]$COST<get(paste0("imp.pmm10_", treatment[i]))[[i]]$SAL))
  a8<-length(which(y110_5611_new$COST<y110_5611_new$SAL))
  con3<-cbind(a1,a2,a3,a4,a5,a6,a7,a8)
  colnames(con3)<-c("mean","median","hotdeck","norm_5","norm_10","pmm_5","pmm_10","original")  
  condition3[[i]]<-con3
}
condition3
#########################################################################################

###條件4:三角貿易收入-三角貿易支出<=總收入###必滿足
condition4<-list()
for (i in 1:length(treatment)){
  
  e1<-length(which((get(paste0("imp.mean_", treatment[i]))[[i]]$TRIREV-get(paste0("imp.mean_", treatment[i]))[[i]]$TRICOST)>get(paste0("imp.mean_", treatment[i]))[[i]]$REV))
  e2<-length(which((get(paste0("imp.median_", treatment[i]))[[i]]$TRIREV-get(paste0("imp.median_", treatment[i]))[[i]]$TRICOST)>get(paste0("imp.median_", treatment[i]))[[i]]$REV))
  e3<-length(which((get(paste0("imp.hotdeck_", treatment[i]))[[i]]$TRIREV-get(paste0("imp.hotdeck_", treatment[i]))[[i]]$TRICOST)>get(paste0("imp.hotdeck_", treatment[i]))[[i]]$REV))
  e4<-length(which((get(paste0("imp.norm5_", treatment[i]))[[i]]$TRIREV-get(paste0("imp.norm5_", treatment[i]))[[i]]$TRICOST)>get(paste0("imp.norm5_", treatment[i]))[[i]]$REV))
  e5<-length(which((get(paste0("imp.norm10_", treatment[i]))[[i]]$TRIREV-get(paste0("imp.norm10_", treatment[i]))[[i]]$TRICOST)>get(paste0("imp.norm10_", treatment[i]))[[i]]$REV))
  e6<-length(which((get(paste0("imp.pmm5_", treatment[i]))[[i]]$TRIREV-get(paste0("imp.pmm5_", treatment[i]))[[i]]$TRICOST)>get(paste0("imp.pmm5_", treatment[i]))[[i]]$REV))
  e7<-length(which((get(paste0("imp.pmm10_", treatment[i]))[[i]]$TRIREV-get(paste0("imp.pmm10_", treatment[i]))[[i]]$TRICOST)>get(paste0("imp.pmm10_", treatment[i]))[[i]]$REV))
  e8<-length(which((y110_5611_new$TRIREV-y110_5611_new$TRICOST)>y110_5611_new$REV))
  con4<-cbind(e1,e2,e3,e4,e5,e6,e7,e8)
  colnames(con4)<-c("mean","median","hotdeck","norm_5","norm_10","pmm_5","pmm_10","original")  
  condition4[[i]]<-con4
}
condition4
#########################################################################################

###新誤差率表###
options(scipen = 999)
options(digits = 3)
y110_5611_1 <- y110_5611[, c("INVESTAS", "ASSET", "REV", "COST", "Company_ID")]
y110_5611_new_1 <- y110_5611_new[, c("INVESTAS","ASSET","REV","COST", "Company_ID")]
y110_5611_new_all <- bind_rows(
  y110_5611_new_1,
  y110_5611 %>%
    filter(revtakeall$stratumID == 3) %>%
    select(REV, COST, Company_ID)
)
APE<-function(pred,true){sum(abs(pred-true))/sum(true)}

imp_APE <- function(data) {
  APElist <- list()
  for (i in 1:length(data)) {
    merged_data <- inner_join(data[[i]], y110_5611_new_all, by = "Company_ID", suffix = c("_pred", "_true"))
    COST <- APE(merged_data$COST_pred, merged_data$COST_true)
    REV <- APE(merged_data$REV_pred, merged_data$REV_true)
    ASSET <- APE(merged_data$ASSET_pred, merged_data$ASSET_true)
    INVESTAS <- APE(merged_data$INVESTAS_pred, merged_data$INVESTAS_true)
    alist <- cbind(COST, REV)
    APElist[[i]] <- alist
  }
  return(APElist)
}

for(i in 1:length(treatment)){
  mean_ape <- imp_APE(get(paste0("imp.mean_", treatment[i])))
  median_ape<-imp_APE(get(paste0("imp.median_", treatment[i])))
  hotdeck_ape<-imp_APE(get(paste0("imp.hotdeck_", treatment[i])))
  norm5_ape<-imp_APE(get(paste0("imp.norm5_", treatment[i])))
  norm10_ape<-imp_APE(get(paste0("imp.norm10_", treatment[i])))
  pmm5_ape<-imp_APE(get(paste0("imp.pmm5_", treatment[i])))
  pmm10_ape<-imp_APE(get(paste0("imp.pmm10_", treatment[i])))
  a_table<-rbind(mean_ape[[i]],median_ape[[i]],hotdeck_ape[[i]],norm5_ape[[i]],norm10_ape[[i]],pmm5_ape[[i]],pmm10_ape[[i]])
  rownames(a_table)<-c("mean","median","hotdeck","norm_5","norm_10","pmm_5","pmm_10")
  new_ape_vn <- paste0("new_ape_", treatment[i])
  assign(new_ape_vn, list())
  assign(new_ape_vn, a_table)
}
#指定table名稱為new_ape_CV值 例 new_ape_0.0052
#################################################################################

###絕對誤差率表###
options(scipen = 999)
options(digits = 3)
APE<-function(pred,true){abs(sum(pred)-sum(true))/sum(true)}

imp_APE <- function(data) {
  APElist <- list()
  for (i in 1:length(data)) {
    merged_data <- inner_join(data[[i]], y110_5611_new_all, by = "Company_ID", suffix = c("_pred", "_true"))
    COST <- APE(merged_data$COST_pred, merged_data$COST_true)
    REV <- APE(merged_data$REV_pred, merged_data$REV_true)
    ASSET <- APE(merged_data$ASSET_pred, merged_data$ASSET_true)
    INVESTAS <- APE(merged_data$INVESTAS_pred, merged_data$INVESTAS_true)
    alist <- cbind(COST, REV)
    APElist[[i]] <- alist
  }
  return(APElist)
}

for(i in 1:length(treatment)){
  mean_ape <- imp_APE(get(paste0("imp.mean_", treatment[i])))
  median_ape<-imp_APE(get(paste0("imp.median_", treatment[i])))
  hotdeck_ape<-imp_APE(get(paste0("imp.hotdeck_", treatment[i])))
  norm5_ape<-imp_APE(get(paste0("imp.norm5_", treatment[i])))
  norm10_ape<-imp_APE(get(paste0("imp.norm10_", treatment[i])))
  pmm5_ape<-imp_APE(get(paste0("imp.pmm5_", treatment[i])))
  pmm10_ape<-imp_APE(get(paste0("imp.pmm10_", treatment[i])))
  a_table<-rbind(mean_ape[[i]],median_ape[[i]],hotdeck_ape[[i]],norm5_ape[[i]],norm10_ape[[i]],pmm5_ape[[i]],pmm10_ape[[i]])
  rownames(a_table)<-c("mean","median","hotdeck","norm_5","norm_10","pmm_5","pmm_10")
  ape_vn <- paste0("ape_", treatment[i])
  assign(ape_vn, list())
  assign(ape_vn, a_table)
}
#指定table名稱為ape_CV值 例 ape_0.0052
#################################################################################

###ks.test表###
imp_ks<-function(data){
  kslist<-list()
  for(i in 1:length(treatment)){
    merged_data <- inner_join(data[[i]], y110_5611_new_all, by = "Company_ID", suffix = c("_pred", "_true"))
    COST <- ks.test(merged_data$COST_pred, merged_data$COST_true)
    REV <- ks.test(merged_data$REV_pred, merged_data$REV_true)
    ASSET <- ks.test(merged_data$ASSET_pred, merged_data$ASSET_true)
    INVESTAS <- ks.test(merged_data$INVESTAS_pred, merged_data$INVESTAS_true)
    klist<-cbind(COST$p.value,REV$p.value)
    kslist[[i]] <-klist
  }
  return(kslist)
}

for(i in 1:length(treatment)){
  mean_ks <- imp_ks(get(paste0("imp.mean_", treatment[i])))
  median_ks<-imp_ks(get(paste0("imp.median_", treatment[i])))
  hotdeck_ks<-imp_ks(get(paste0("imp.hotdeck_", treatment[i])))
  norm5_ks<-imp_ks(get(paste0("imp.norm5_", treatment[i])))
  norm10_ks<-imp_ks(get(paste0("imp.norm10_", treatment[i])))
  pmm5_ks<-imp_ks(get(paste0("imp.pmm5_", treatment[i])))
  pmm10_ks<-imp_ks(get(paste0("imp.pmm10_", treatment[i])))
  k_table<-rbind(mean_ks[[i]],median_ks[[i]],hotdeck_ks[[i]],norm5_ks[[i]],norm10_ks[[i]],pmm5_ks[[i]],pmm10_ks[[i]])
  rownames(k_table)<-c("mean","median","hotdeck","norm_5","norm_10","pmm_5","pmm_10")
  colnames(k_table)<-c("COST","REV")
  ks_vn <- paste0("ks_", treatment[i])
  assign(ks_vn, list())
  assign(ks_vn, k_table)
}
#指定table名稱為ks_CV值 例 ks_0.0028
#############################################################################################################

###計算母體資料前處理###
y110_5611_new$STRATA <- 0
after_combine <- y110_5611_new %>%
  bind_rows(
    y110_5611 %>%
      filter(revtakeall$stratumID == 3)
  ) 

###總變異數表###
population_total_var_list <- sapply(after_combine[columns], var)
var_table <- data.frame(
  mean = get(mean_var_list_name),
  median = get(median_var_list_name),
  hotdeck = get(hotdeck_var_list_name),
  norm5 = get(norm5_var_list_name),
  norm10 = get(norm10_var_list_name),
  pmm5 = get(pmm5_var_list_name),
  pmm10 = get(pmm10_var_list_name),
  population = population_total_var_list
)
var_vn <- paste0("var_", treatment[1])
assign(var_vn, t(var_table))
#指定table名稱為var_CV值 例 var_0.0055
#################################################################################

###總和表###
population_total_sum_list <- sapply(after_combine[columns], sum)
sum_table <- data.frame(
  mean = get(mean_sum_list_name),
  median = get(median_sum_list_name),
  hotdeck = get(hotdeck_sum_list_name),
  norm5 = get(norm5_sum_list_name),
  norm10 = get(norm10_sum_list_name),
  pmm5 = get(pmm5_sum_list_name),
  pmm10 = get(pmm10_sum_list_name),
  population = population_total_sum_list
)
sum_vn <- paste0("sum_", treatment[1])
assign(sum_vn, t(sum_table))
#指定table名稱為sum_CV值 例 sum_0.0028
#################################################################################

###比較數據###
methods <- c("mean", "median", "hotdeck", "norm5", "norm10", "pmm5", "pmm10")

cp_list <- list()

for (m in methods) {
  cp_tmp <- list()
  for (i in 1:length(treatment)) {
    imp <- get(paste0("imp.", m, "_", treatment[i]))[[i]] 
    total_var_list <- get(paste0(m, "_", treatment[i], "_total_var_list")) 
    
    # 針對每個群組分開計算
    for (g in 1:length(data.all)) {  
      n_g <- nrow(y110_5611_new[y110_5611_new$SH == g, ])  
      
      COST <- (mean(imp$COST[imp$SH == g]) - mean(y110_5611_new$COST[y110_5611_new$SH == g]))^2 * n_g / total_var_list[[g]][["COST"]]
      REV <- (mean(imp$REV[imp$SH == g]) - mean(y110_5611_new$REV[y110_5611_new$SH == g]))^2 * n_g / total_var_list[[g]][["REV"]]
      
      # 儲存該群的 WSE
      cp_tmp[[paste0("group", g)]] <- cbind(COST, REV)
    }
    cp_list[[m]] <- cp_tmp
  }
}

compare_table <- NULL
for (method in names(cp_list)) {
  for (group in names(cp_list[[method]])) {
    method_result <- cp_list[[method]][[group]]
    method_result <- cbind(Method = method, Group = group, method_result)
    compare_table <- rbind(compare_table, method_result)
  }
}
rownames(compare_table) <- compare_table[,1]
compare_table <- compare_table[,-1]
compare_vn <- paste0("compare_", treatment[1])
assign(compare_vn, compare_table)  
#指定table名稱為compare_CV值 例 compare_0.0055
#################################################################################

###計算縣市差距###
library(tidyr)
library(ggplot2)

after_combine <- after_combine %>%
  mutate(SH7 = as.character(SH7)) %>%
  mutate(SH22 = if_else(nchar(SH7) == 6, substr(SH7, 1, 1), substr(SH7, 1, 2)))

before <- after_combine %>%
  group_by(SH22) %>%
  summarise(
    total_COST = sum(COST, na.rm = TRUE),
    total_REV = sum(REV, na.rm = TRUE)
  )

###依照前述結果取出三組前三名 將這九個方法_CV放入 繪製四張縣市比較盒狀圖###
method_names <- c("median_0.005","mean_0.005") 
diff_list <- list()

for (method in method_names) {
  # 動態生成對應的變數名稱
  imp_data <- get(paste0("imp.", method))[[1]]
  
  after <- imp_data %>%
    group_by(SH22) %>%
    summarise(
      total_COST = sum(COST, na.rm = TRUE),
      total_REV = sum(REV, na.rm = TRUE)
    )
  
  diff_result <- after %>%
    inner_join(before, by = "SH22", suffix = c("_after", "_before")) %>%
    mutate(
      diff_COST = total_COST_after - total_COST_before,
      # diff_ASSET = total_ASSET_after - total_ASSET_before,
      diff_REV = total_REV_after - total_REV_before,
      # diff_INVESTAS = total_INVESTAS_after - total_INVESTAS_before
    )
  
  diff_list[[method]] <- diff_result
}

diff_vars <- c("diff_COST", "diff_REV")
y_axis_labels <- c(
  "diff_COST" = "COST",
  "diff_REV" = "REV"
)
plots <- list()


for (v in diff_vars) {
  diff_data <- lapply(names(diff_list), function(method) {
    data.frame(
      SH22 = diff_list[[method]]$SH22,
      diff_value = diff_list[[method]][[v]],
      method = method
    )
  }) %>%
    bind_rows()
  
  p <- ggplot(diff_data, aes(x = method, y = diff_value, fill = method)) +
    geom_boxplot() +
    labs(title = paste("Comparison of", v, "Across Methods"),
         x = "Method",
         y = y_axis_labels[v]) +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 50),     # 標題字體大小
      axis.title.x = element_text(size = 40),                  # x軸標籤字體大小
      axis.title.y = element_text(size = 40),                  # y軸標籤字體大小
      axis.text = element_text(size = 30)                      # 座標軸文字大小
    )
  
  plots[[v]] <- p
  
  ggsave(
    filename = paste0("plot_", v, ".png"),
    plot = p,
    width = 16,
    height = 8,
    dpi = 300,
    bg = "white"
  )
}

plots
#################################################################################不用ASSET跟INVESTAS了
##### 細緻分類評估 #####

## 原始母體總和
y110_5611

##### 大行業交叉村里 #####
y110_5611_cross_village <- y110_5611 %>%
  select(縣市名稱, 鄉鎮市區名稱, REV, COST) %>%
  group_by(縣市名稱, 鄉鎮市區名稱) %>%
  summarise(REV = sum(REV),
            COST = sum(COST))

##### 中行業交叉鄉鎮 #####
y110_5611_cross_town <- y110_5611 %>%
  select(縣市名稱, REV, COST) %>%
  group_by(縣市名稱) %>%
  summarise(REV = sum(REV),
            COST = sum(COST))

##### 中行業交叉人數規模別 ######
y110_5611_cross_emp <- y110_5611 %>%
  select(EMPGRP, REV, COST) %>%
  group_by(EMPGRP) %>%
  summarise(REV = sum(REV),
            COST = sum(COST))
y110_5611_cross_emp

##### 中行業交叉收入規模別 #####
y110_5611_cross_rev <- y110_5611 %>%
  select(REVGRP, REV, COST) %>%
  group_by(REVGRP) %>%
  summarise(REV = sum(REV),
            COST = sum(COST))
y110_5611_cross_rev

###########
## 插補總和
impu <- imp.median_0.005[[1]] # cv=0.05, 插補方式：median

##### 大行業交叉村里 #####
imp.median_0.05_village <- bind_rows(
  impu %>% 
    select(縣市名稱, 鄉鎮市區名稱, REV, COST)
) %>%
  
  group_by(縣市名稱, 鄉鎮市區名稱) %>%
  summarise(
    REV = sum(REV, na.rm = TRUE),
    COST = sum(COST, na.rm = TRUE),
    .groups = "drop"
  )



##### 中行業交叉鄉鎮 #####
imp.median_0.05_cross_town <- bind_rows(
  impu %>% 
    select(縣市名稱, REV, COST)
) %>%
  
  group_by(縣市名稱) %>%
  summarise(
    REV = sum(REV, na.rm = TRUE),
    COST = sum(COST, na.rm = TRUE),
    .groups = "drop"
  )


##### 中行業交叉人數規模別 ######
imp.median_0.05_cross_emp <- bind_rows(
  impu %>% 
    select(EMPGRP, REV, COST)
) %>%
  
  group_by(EMPGRP) %>%
  summarise(
    REV = sum(REV, na.rm = TRUE),
    COST = sum(COST, na.rm = TRUE),
    .groups = "drop"
  )
imp.median_0.05_cross_emp

##### 中行業交叉收入規模別 #####
imp.median_0.05_cross_rev <- bind_rows(
  impu %>% 
    select(REVGRP, REV, COST)
) %>%
  
  group_by(REVGRP) %>%
  summarise(
    REV = sum(REV, na.rm = TRUE),
    COST = sum(COST, na.rm = TRUE),
    .groups = "drop"
  )
imp.median_0.05_cross_rev
###########################################################################################

##### 母體、插補總值 #####
create_summary_table <- function(df1, df2, var_names, df1_name = "df1", df2_name = "df2") {
  sums <- numeric(length(var_names) * 2)
  vars <- numeric(length(var_names) * 2)
  datasets <- character(length(var_names) * 2)
  variables <- character(length(var_names) * 2)
  
  for (i in seq_along(var_names)) {
    sums[i] <- sum(df1[[var_names[i]]], na.rm = TRUE)
    vars[i] <- var(df1[[var_names[i]]], na.rm = TRUE)
    datasets[i] <- df1_name
    variables[i] <- var_names[i]
  }
  
  for (i in seq_along(var_names)) {
    sums[i + length(var_names)] <- sum(df2[[var_names[i]]], na.rm = TRUE)
    vars[i + length(var_names)] <- var(df2[[var_names[i]]], na.rm = TRUE)
    datasets[i + length(var_names)] <- df2_name
    variables[i + length(var_names)] <- var_names[i]
  }
  
  summary_table <- data.frame(
    Dataset = datasets,
    Variable = variables,
    Sum = sums,
    Variance = vars
  )
  
  return(summary_table)
}

# y110_5611 是原始的， check 是插補的
var_names <- c("REV", "COST")
result_table <- create_summary_table(y110_5611, impu, var_names, "y110_5611", "imputation")
result_table


###################################################################################最後輸出以下已經整理好的表（原始合併插補）

###########村里誤差###########
merged_data_village <- inner_join(
  imp.median_0.05_village,
  y110_5611_cross_village,
  by = c("縣市名稱", "鄉鎮市區名稱"),
  suffix = c("_imp", "_y110")  
)

error_rate_village <- merged_data_village %>%
  mutate(
    COST_error_rate = ifelse(COST_y110 == 0, NA, abs((COST_imp - COST_y110) / COST_y110)),  
    REV_error_rate = ifelse(REV_y110 == 0, NA, abs((REV_imp - REV_y110) / REV_y110)),     
    COST_below_10 = COST_error_rate < 0.1,                    
    REV_below_10 = REV_error_rate < 0.1,
    REV_below_10 = if_else(is.na(REV_below_10),REV_imp == REV_y110,REV_below_10),
    COST_below_10 = if_else(is.na(COST_below_10),COST_imp == COST_y110,COST_below_10)
  ) %>%
  summarise(
    cost_below_10_count = sum(COST_below_10, na.rm = TRUE),    
    rev_below_10_count = sum(REV_below_10, na.rm = TRUE),
    target = nrow(merged_data_village),
    diff_cost = target-cost_below_10_count,
    diff_rev = target-rev_below_10_count
    
  )
error_rate_village

###########鄉鎮誤差###########
merged_data_town <- inner_join(
  imp.median_0.05_cross_town,
  y110_5611_cross_town,
  by = c("縣市名稱"),
  suffix = c("_imp", "_y110")  
)

error_rate_town <- merged_data_town %>%
  mutate(
    COST_error_rate = ifelse(COST_y110 == 0, NA, abs((COST_imp - COST_y110) / COST_y110)),  
    REV_error_rate = ifelse(REV_y110 == 0, NA, abs((REV_imp - REV_y110) / REV_y110)),     
    COST_below_10 = COST_error_rate < 0.1,                    
    REV_below_10 = REV_error_rate < 0.1,
    REV_below_10 = if_else(is.na(REV_below_10),REV_imp == REV_y110,REV_below_10),
    COST_below_10 = if_else(is.na(COST_below_10),COST_imp == COST_y110,COST_below_10)
  ) %>%
  summarise(
    cost_below_10_count = sum(COST_below_10, na.rm = TRUE),    
    rev_below_10_count = sum(REV_below_10, na.rm = TRUE),
    target = nrow(merged_data_town),
    diff_cost = target-cost_below_10_count,
    diff_rev = target-rev_below_10_count
    
  )
error_rate_town

###########人數規模別誤差###########
merged_data_emp <- inner_join(
  imp.median_0.05_cross_emp,
  y110_5611_cross_emp,
  by = c("EMPGRP"),
  suffix = c("_imp", "_y110")  
)

error_rate_emp <- merged_data_emp %>%
  mutate(
    COST_error_rate = abs((COST_imp - COST_y110) / COST_y110),  
    REV_error_rate = abs((REV_imp - REV_y110) / REV_y110),     
    COST_below_10 = COST_error_rate < 0.1,                    
    REV_below_10 = REV_error_rate < 0.1                     
  ) 
error_rate_emp

###########規模別誤差###########
merged_data_rev <- inner_join(
  imp.median_0.05_cross_rev,
  y110_5611_cross_rev,
  by = c("REVGRP"),
  suffix = c("_imp", "_y110")  
)

error_rate_rev <- merged_data_rev %>%
  mutate(
    COST_error_rate = abs((COST_imp - COST_y110) / COST_y110),  
    REV_error_rate = abs((REV_imp - REV_y110) / REV_y110),     
    COST_below_10 = COST_error_rate < 0.1,                    
    REV_below_10 = REV_error_rate < 0.1                     
  ) 
error_rate_rev

best <- inner_join(
  y110_5611 %>%
    select(Company_ID, 縣市名稱, 鄉鎮市區名稱, REV, COST, EMPGRP, REVGRP),
  impu %>%
    select(Company_ID, REV, COST),
  by = "Company_ID",
  suffix = c("_y110", "_imp")
) %>%
  select(縣市名稱, 鄉鎮市區名稱, REV_y110, COST_y110, REV_imp, COST_imp, EMPGRP, REVGRP)

library(writexl)
sheets <- list(
  "最佳插補" = best, 
  "大行業交叉村里" = merged_data_village,
  "中行業交叉鄉鎮" = merged_data_town,
  "中行業交叉收入規模" = merged_data_rev,
  "中行業交叉人數規模" = merged_data_emp
)
write_xlsx(sheets, path = paste0("綜合表_5611_", treatment,".xlsx"))
####################################################################################
###########################合併大行業###########################
####################################################################################


library(dplyr)
library(purrr)
library(readxl)
base_path <- "/Users/linzili/研究工作/政府調查"  # 改為你的實際資料夾路徑，如果檔案在當前目錄就設為 "" 
# 匯入所有細行業差補結果最好的工作表
files <- c("綜合表_5620_0.005.xlsx", "綜合表_5590_0.9.xlsx", "綜合表_5611_0.005.xlsx")# 匯入所有細行業差補結果最好的工作表
files <- if (base_path == "") files else file.path(base_path, files)
sheet <- 1
source_labels <- c("5620", "5590", "5611")# 按照上面讀取檔案的順序

tables <- map2(files, source_labels, ~read_excel(.x, sheet = sheet) %>%
                 mutate(source = .y,  
                        across(c(REV_imp, COST_imp, REV_y110, COST_y110), as.numeric)))

vill_stacked <- bind_rows(tables)

vill <- vill_stacked %>%
  group_by(縣市名稱, 鄉鎮市區名稱) %>%
  summarise(
    REV_imp = sum(REV_imp, na.rm = TRUE),
    COST_imp = sum(COST_imp, na.rm = TRUE),
    REV_y110 = sum(REV_y110, na.rm = TRUE),
    COST_y110 = sum(COST_y110, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    COST_error_rate = ifelse(COST_y110 == 0, NA, abs((COST_imp - COST_y110) / COST_y110)),
    REV_error_rate = ifelse(REV_y110 == 0, NA, abs((REV_imp - REV_y110) / REV_y110)),
    COST_below_10 = COST_error_rate < 0.1,
    REV_below_10 = REV_error_rate < 0.1
  ) %>%
  select("縣市名稱", "鄉鎮市區名稱", "REV_imp", "COST_imp", "REV_y110", "COST_y110", 
         "COST_error_rate", "REV_error_rate", "COST_below_10", "REV_below_10")
#########################
# 合併後村里誤差通過的家數
vill %>%
  summarise(
    cost_below_10_count = sum(COST_below_10, na.rm = TRUE),    
    rev_below_10_count = sum(REV_below_10, na.rm = TRUE),
    target = nrow(vill),
    diff_cost = target-cost_below_10_count,
    diff_rev = target-rev_below_10_count
  )
#############################如果村里真的沒辦法100%通過####################################################
above_10_vill <- vill %>%
  filter(REV_below_10 == FALSE | COST_below_10 == FALSE) %>%
  select("縣市名稱", "鄉鎮市區名稱") %>%
  distinct()

vill_stacked_adjusted<- vill_stacked %>%
  mutate(
    to_update = paste(縣市名稱, 鄉鎮市區名稱) %in%
      paste(above_10_vill$縣市名稱, above_10_vill$鄉鎮市區名稱),
    REV_imp = if_else(to_update, REV_y110, REV_imp),
    COST_imp = if_else(to_update, COST_y110, COST_imp)
  ) 

vill_stacked_adjusted$source_prefix <- ifelse(
  nchar(vill_stacked_adjusted$source) == 3,  
  substr(vill_stacked_adjusted$source, 1, 1),  
  ifelse(
    nchar(vill_stacked_adjusted$source) == 4,  
    substr(vill_stacked_adjusted$source, 1, 2),
    NA  
  )
)
###########################################新的表
#####大行業
# 1. 縣市名稱 + 鄉鎮市區名稱
sum_by_area <- vill_stacked_adjusted %>%
  group_by(縣市名稱, 鄉鎮市區名稱) %>%
  summarise(
    REV_imp = sum(REV_imp, na.rm = TRUE),
    COST_imp = sum(COST_imp, na.rm = TRUE),
    REV_y110 = sum(REV_y110, na.rm = TRUE),
    COST_y110 = sum(COST_y110, na.rm = TRUE),
    .groups = "drop"
  )

error_by_area <- sum_by_area %>%
  mutate(
    REV_error_rate = ifelse(REV_y110 == 0, NA, abs((REV_imp - REV_y110) / REV_y110)),
    COST_error_rate = ifelse(COST_y110 == 0, NA, abs((COST_imp - COST_y110) / COST_y110)),
    REV_below_10 = REV_error_rate < 0.1,
    COST_below_10 = COST_error_rate < 0.1,
    REV_below_10 = if_else(is.na(REV_below_10),REV_imp == REV_y110,REV_below_10),
    COST_below_10 = if_else(is.na(COST_below_10),COST_imp == COST_y110,COST_below_10)
  )%>%
  ungroup() %>%
  summarise(
    REV_below_10_count = sum(REV_below_10, na.rm = TRUE),
    COST_below_10_count = sum(COST_below_10, na.rm = TRUE),
    target = nrow(sum_by_area),
    diff_REV = target-REV_below_10_count,
    diff_COST = target - COST_below_10_count
  )

error_by_area

########################################
#####中行業
# 2. 鄉鎮市區名稱
sum_by_city <- vill_stacked_adjusted %>%
  group_by(source_prefix, 縣市名稱) %>%
  summarise(
    REV_y110 = sum(REV_y110, na.rm = TRUE),
    COST_y110 = sum(COST_y110, na.rm = TRUE),
    REV_imp = sum(REV_imp, na.rm = TRUE),
    COST_imp = sum(COST_imp, na.rm = TRUE),
    .groups = "drop"
  )

error_by_city <- sum_by_city %>%
  group_by(source_prefix) %>%
  mutate(
    REV_error_rate = ifelse(REV_y110 == 0, NA, abs((REV_imp - REV_y110) / REV_y110)),
    COST_error_rate = ifelse(COST_y110 == 0, NA, abs((COST_imp - COST_y110) / COST_y110)),
    REV_below_10 = REV_error_rate < 0.1,
    COST_below_10 = COST_error_rate < 0.1,
    REV_below_10 = if_else(is.na(REV_below_10), REV_imp == REV_y110, REV_below_10),
    COST_below_10 = if_else(is.na(COST_below_10), COST_imp == COST_y110, COST_below_10)
  ) %>%
  summarise(
    REV_below_10_count = sum(REV_below_10, na.rm = TRUE),
    COST_below_10_count = sum(COST_below_10, na.rm = TRUE),
    target = n(),
    diff_REV = target - REV_below_10_count,
    diff_COST = target - COST_below_10_count
  )

error_by_city_split <- error_by_city %>%
  group_split(source_prefix)
names(error_by_city_split) <- error_by_city$source_prefix
for (src in names(error_by_city_split)) {
  cat(sprintf("\n%s：\n", src))
  print(error_by_city_split[[src]])
}

########################################
# 3. REVGRP
sum_by_revgrp <- vill_stacked_adjusted %>%
  group_by(source_prefix, REVGRP) %>%
  summarise(
    REV_y110 = sum(REV_y110, na.rm = TRUE),
    COST_y110 = sum(COST_y110, na.rm = TRUE),
    REV_imp = sum(REV_imp, na.rm = TRUE),
    COST_imp = sum(COST_imp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    REV_error_rate = ifelse(REV_y110 == 0, NA, abs((REV_imp - REV_y110) / REV_y110)),
    COST_error_rate = ifelse(COST_y110 == 0, NA, abs((COST_imp - COST_y110) / COST_y110)),
    REV_below_10 = REV_error_rate < 0.1,
    COST_below_10 = COST_error_rate < 0.1
  )

error_by_revgrp <- sum_by_revgrp %>%
  group_by(source_prefix) %>%
  summarise(
    REV_below_10_count = sum(REV_below_10, na.rm = TRUE),
    COST_below_10_count = sum(COST_below_10, na.rm = TRUE),
    target = n(),
    diff_REV = target - REV_below_10_count,
    diff_COST = target - COST_below_10_count
  )

error_by_revgrp_split <- error_by_revgrp %>%
  group_split(source_prefix)
names(error_by_revgrp_split) <- error_by_revgrp$source_prefix
for (src in names(error_by_revgrp_split)) {
  cat(sprintf("\n%s：\n", src))
  print(error_by_revgrp_split[[src]])
}

########################################
# 4. EMPGRP
sum_by_empgrp <- vill_stacked_adjusted %>%
  group_by(source_prefix, EMPGRP) %>%
  summarise(
    REV_y110 = sum(REV_y110, na.rm = TRUE),
    COST_y110 = sum(COST_y110, na.rm = TRUE),
    REV_imp = sum(REV_imp, na.rm = TRUE),
    COST_imp = sum(COST_imp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    REV_error_rate = ifelse(REV_y110 == 0, NA, abs((REV_imp - REV_y110) / REV_y110)),
    COST_error_rate = ifelse(COST_y110 == 0, NA, abs((COST_imp - COST_y110) / COST_y110)),
    REV_below_10 = REV_error_rate < 0.1,
    COST_below_10 = COST_error_rate < 0.1
  )

error_by_empgrp <- sum_by_empgrp %>%
  group_by(source_prefix) %>%
  summarise(
    REV_below_10_count = sum(REV_below_10, na.rm = TRUE),
    COST_below_10_count = sum(COST_below_10, na.rm = TRUE),
    target = n(),
    diff_REV = target - REV_below_10_count,
    diff_COST = target - COST_below_10_count
  )

error_by_empgrp_split <- error_by_empgrp %>%
  group_split(source_prefix)
names(error_by_empgrp_split) <- error_by_empgrp$source_prefix
for (src in names(error_by_empgrp_split)) {
  cat(sprintf("\n%s：\n", src))
  print(error_by_empgrp_split[[src]])
}

vill_stacked_adjusted %>%
  group_by(source) %>%
  summarise(
    COST_y110 = sum(COST_y110, na.rm = TRUE),
    COST_imp = sum(COST_imp, na.rm = TRUE),
    REV_y110 = sum(REV_y110, na.rm = TRUE),
    REV_imp = sum(REV_imp, na.rm = TRUE),
    
  )

library(writexl)
sheets <- list(
  "大行業交叉村里" = sum_by_area,
  "中行業交叉鄉鎮" = sum_by_city,
  "中行業交叉收入規模" = sum_by_revgrp,
  "中行業交叉人數規模" = sum_by_empgrp
)

library(openxlsx)
wb <- createWorkbook()
addWorksheet(wb, "大行業交叉村里")
writeData(wb, "大行業交叉村里", sheets[["大行業交叉村里"]])

split_sheets <- c("中行業交叉鄉鎮", "中行業交叉收入規模", "中行業交叉人數規模")
for (sheet_name in split_sheets) {
  df <- sheets[[sheet_name]]
  
  df_split <- df %>%
    group_split(source_prefix)
  
  source_values <- unique(df$source_prefix)
  names(df_split) <- source_values
  
  for (src in source_values) {
    ws_name <- paste0(sheet_name, "_", src)
    addWorksheet(wb, ws_name)
    writeData(wb, ws_name, df_split[[src]])
  }
}


output_file <- paste0("綜合表_I_", "_new.xlsx")
saveWorkbook(wb, output_file, overwrite = TRUE)



