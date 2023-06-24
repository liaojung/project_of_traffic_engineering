library(ggplot2)
library(dplyr)

setwd("C:/Users/yingj/OneDrive - 國立陽明交通大學/作業/一下/交工/交工實作五")
my_data = read.csv("路口臨近路段速率登錄表.csv")
data_adj = my_data/cospi(1/6)

road_A = merge(table(cut(data_adj[,1], seq(0,95,5))),
              table(cut(data_adj[,2], seq(0,95,5))),
              by = "Var1", sort = FALSE)
road_B = merge(table(cut(data_adj[,3], seq(0,95,5))),
               table(cut(data_adj[,4], seq(0,95,5))),
               by = "Var1", sort = FALSE)
road_C = merge(table(cut(data_adj[,5], seq(0,95,5))),
               table(cut(data_adj[,6], seq(0,95,5))),
               by = "Var1", sort = FALSE)
write.table(cbind(road_A, road_B, road_C), file = "次數分配表.csv",
            sep = ",", col.names = TRUE, row.names = FALSE)


data2 = data.frame(road_A_bike = cut(data_adj[,1], breaks = c(-Inf, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, Inf)),
                   road_A_car = cut(data_adj[,2], breaks = c(-Inf, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, Inf)),
                   road_B_bike = cut(data_adj[,3], breaks = c(-Inf, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, Inf)),
                   road_B_car = cut(data_adj[,4], breaks = c(-Inf, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, Inf)),
                   road_C_bike = cut(data_adj[,5], breaks = c(-Inf, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, Inf)),
                   road_C_car = cut(data_adj[,6], breaks = c(-Inf, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, Inf)))

data3 = data.frame(road_A_bike = c(sum(table(data_adj[,1])),
                                   mean(data_adj[,1], na.rm = TRUE),
                                   quantile(data_adj[,1], c(0, 1, 0.15, 0.25, 0.5), na.rm = TRUE),
                                   names(table(data_adj[,1]))[which.max(table(data_adj[,1]))],
                                   quantile(data_adj[,1], c( 0.75, 0.85), na.rm = TRUE)),
                   road_A_car = c(sum(table(data_adj[,2])),
                                  mean(data_adj[,2], na.rm = TRUE),
                                  quantile(data_adj[,2], c(0, 1, 0.15, 0.25, 0.5), na.rm = TRUE),
                                  names(table(data_adj[,2]))[which.max(table(data_adj[,2]))],
                                  quantile(data_adj[,2], c( 0.75, 0.85), na.rm = TRUE)),
                   road_B_bike = c(sum(table(data_adj[,3])),
                                   mean(data_adj[,3], na.rm = TRUE),
                                   quantile(data_adj[,3], c(0, 1, 0.15, 0.25, 0.5), na.rm = TRUE),
                                   names(table(data_adj[,3]))[which.max(table(data_adj[,3]))],
                                   quantile(data_adj[,3], c( 0.75, 0.85), na.rm = TRUE)),
                   road_B_car = c(sum(table(data_adj[,4])),
                                  mean(data_adj[,4], na.rm = TRUE),
                                  quantile(data_adj[,4], c(0, 1, 0.15, 0.25, 0.5), na.rm = TRUE),
                                  names(table(data_adj[,4]))[which.max(table(data_adj[,4]))],
                                  quantile(data_adj[,4], c( 0.75, 0.85), na.rm = TRUE)),
                   road_C_bike = c(sum(table(data_adj[,5])),
                                   mean(data_adj[,5], na.rm = TRUE),
                                   quantile(data_adj[,5], c(0, 1, 0.15, 0.25, 0.5), na.rm = TRUE),
                                   names(table(data_adj[,5]))[which.max(table(data_adj[,5]))],
                                   quantile(data_adj[,5], c( 0.75, 0.85), na.rm = TRUE)),
                   road_C_car = c(sum(table(data_adj[,6])),
                                  mean(data_adj[,6], na.rm = TRUE),
                                  quantile(data_adj[,6], c(0, 1, 0.15, 0.25, 0.5), na.rm = TRUE),
                                  names(table(data_adj[,6]))[which.max(table(data_adj[,6]))],
                                  quantile(data_adj[,6], c( 0.75, 0.85), na.rm = TRUE))

)

write.table(data3, file = "各行車方向汽、機車現點速率分配之各項量數表.csv",
            sep = ",", col.names = TRUE, row.names = FALSE)

data4 = data.frame(road_A_bike = cut(data_adj[,1],breaks = seq(0,95,5)),
                   road_A_car = cut(data_adj[,2], breaks = seq(0,95,5)),
                   road_B_bike = cut(data_adj[,3], breaks = seq(0,95,5)),
                   road_B_car = cut(data_adj[,4], breaks = seq(0,95,5)),
                   road_C_bike = cut(data_adj[,5], breaks = seq(0,95,5)),
                   road_C_car = cut(data_adj[,6], breaks = seq(0,95,5)))
data4[,1] = as.numeric(data4[,1])*5-2.5
data4[,2] = as.numeric(data4[,2])*5-2.5
data4[,3] = as.numeric(data4[,3])*5-2.5
data4[,4] = as.numeric(data4[,4])*5-2.5
data4[,5] = as.numeric(data4[,5])*5-2.5
data4[,6] = as.numeric(data4[,6])*5-2.5

data5 = data.frame(road_A_bike = c(sum(table(data4[,1])),
                                   mean(data4[,1], na.rm = TRUE),
                                   quantile(data4[,1], c(0, 1, 0.15, 0.25, 0.5), na.rm = TRUE),
                                   names(table(data4[,1]))[which.max(table(data4[,1]))],
                                   quantile(data4[,1], c( 0.75, 0.85), na.rm = TRUE)),
                   road_A_car = c(sum(table(data4[,2])),
                                  mean(data4[,2], na.rm = TRUE),
                                  quantile(data4[,2], c(0, 1, 0.15, 0.25, 0.5), na.rm = TRUE),
                                  names(table(data4[,2]))[which.max(table(data4[,2]))],
                                  quantile(data4[,2], c( 0.75, 0.85), na.rm = TRUE)),
                   road_B_bike = c(sum(table(data4[,3])),
                                   mean(data4[,3], na.rm = TRUE),
                                   quantile(data4[,3], c(0, 1, 0.15, 0.25, 0.5), na.rm = TRUE),
                                   names(table(data4[,3]))[which.max(table(data4[,3]))],
                                   quantile(data4[,3], c( 0.75, 0.85), na.rm = TRUE)),
                   road_B_car = c(sum(table(data4[,4])),
                                  mean(data4[,4], na.rm = TRUE),
                                  quantile(data4[,4], c(0, 1, 0.15, 0.25, 0.5), na.rm = TRUE),
                                  names(table(data4[,4]))[which.max(table(data4[,4]))],
                                  quantile(data4[,4], c( 0.75, 0.85), na.rm = TRUE)),
                   road_C_bike = c(sum(table(data4[,5])),
                                   mean(data4[,5], na.rm = TRUE),
                                   quantile(data4[,5], c(0, 1, 0.15, 0.25, 0.5), na.rm = TRUE),
                                   names(table(data4[,5]))[which.max(table(data4[,5]))],
                                   quantile(data4[,5], c( 0.75, 0.85), na.rm = TRUE)),
                   road_C_car = c(sum(table(data4[,6])),
                                  mean(data4[,6], na.rm = TRUE),
                                  quantile(data4[,6], c(0, 1, 0.15, 0.25, 0.5), na.rm = TRUE),
                                  names(table(data4[,6]))[which.max(table(data4[,6]))],
                                  quantile(data4[,6], c( 0.75, 0.85), na.rm = TRUE))
                   
)

write.table(data5, file = "各行車方向汽、機車現點速率分配之各項量數表_區間.csv",
            sep = ",", col.names = TRUE, row.names = FALSE)