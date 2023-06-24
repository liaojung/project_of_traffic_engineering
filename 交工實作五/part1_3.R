png("汽、機車次數分佈長條圖_路段A.png", width = 10, height = 4.5, units = 'in', res = 200)
barplot(t(matrix(c(table(data2[,1]),table(data2[,2])), nrow = 14)), 
        beside = TRUE, 
        main = "汽、機車次數分佈長條圖( 路段 A )",
        xlab = "速率( 公里/時 )",
        ylab = "車    輛     數",
        names.arg = c("20-", "20~25", "25~30", "30~35","35~40", "40~45", "45~50",
                      "50~55", "55~60", "60~65", "65~70", "70~75", "75~80", "80+"),
        col = c("limegreen", "steelblue2"))
dev.off()

png("汽、機車次數分佈長條圖_路段B.png", width = 10, height = 4.5, units = 'in', res = 200)
barplot(t(matrix(c(table(data2[,3]),table(data2[,4])), nrow = 14)), 
        beside = TRUE, 
        main = "汽、機車次數分佈長條圖( 路段 B )",
        xlab = "速率( 公里/時 )",
        ylab = "車    輛     數",
        names.arg = c("20-", "20~25", "25~30", "30~35","35~40", "40~45", "45~50",
                      "50~55", "55~60", "60~65", "65~70", "70~75", "75~80", "80+"),
        col = c("limegreen", "steelblue2"))
dev.off()


png("汽、機車次數分佈長條圖_路段C.png", width = 10, height = 4.5, units = 'in', res = 200)
barplot(t(matrix(c(table(data2[,5]),table(data2[,6])), nrow = 14)), 
        beside = TRUE, 
        main = "汽、機車次數分佈長條圖( 路段 C )",
        xlab = "速率( 公里/時 )",
        ylab = "車    輛     數",
        names.arg = c("20-", "20~25", "25~30", "30~35","35~40", "40~45", "45~50",
                      "50~55", "55~60", "60~65", "65~70", "70~75", "75~80", "80+"),
        col = c("limegreen", "steelblue2"))
dev.off()

png("五項量數盒鬚圖_區間.png", width = 10, height = 5.5, units = 'in', res = 200)
boxplot(data4,
        main = " 現點速率五項量數盒鬚圖 ")
dev.off()