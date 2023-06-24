distri_bike_A = cbind(upper = seq(0, 90, 5),
                      lower = seq(5, 95, 5),
                      mid = seq(2.5, 92.5, 5),
                      n = road_A[,2],
                      rate = 100*road_A[,2]/sum(road_A[,2]),
                      cumrate = 100*cumsum(road_A[,2])/sum(road_A[,2]))
distri_bike_A = cbind(distri_bike_A, 
                      s = distri_bike_A[,3]*distri_bike_A[,4], 
                      "s^2" = (distri_bike_A[,3]^2)*distri_bike_A[,4])
distri_bike_A = rbind(distri_bike_A,
                      c("-","Total","-", 
                        sum(distri_bike_A[,4]),
                        sum(distri_bike_A[,5]),
                        "-",
                        sum(distri_bike_A[,7]), 
                        sum(distri_bike_A[,8])))
write.table(distri_bike_A, file = "速率次數分配表_路段A機車.csv",sep = ",", col.names = TRUE, row.names = FALSE)

distri_car_A = cbind(upper = seq(0, 90, 5),
                     lower = seq(5, 95, 5),
                     mid = seq(2.5, 92.5, 5),
                     n = road_A[,3],
                     rate = 100*road_A[,3]/sum(road_A[,3]),
                     cumrate = 100*cumsum(road_A[,3])/sum(road_A[,3]))
distri_car_A = cbind(distri_car_A, 
                      s = distri_car_A[,3]*distri_car_A[,4], 
                      "s^2" = (distri_car_A[,3]^2)*distri_car_A[,4])
distri_car_A = rbind(distri_car_A,
                      c("-","Total","-",
                        sum(distri_car_A[,4]),
                        sum(distri_car_A[,5]),
                        "-",
                        sum(distri_car_A[,7]), 
                        sum(distri_car_A[,8])))
write.table(distri_car_A, file = "速率次數分配表_路段A汽車.csv",sep = ",", col.names = TRUE, row.names = FALSE)

distri_bike_B = cbind(upper = seq(0, 90, 5),
                      lower = seq(5, 95, 5),
                      mid = seq(2.5, 92.5, 5),
                      n = road_B[,2],
                      rate = 100*road_B[,2]/sum(road_B[,2]),
                      cumrate = 100*cumsum(road_B[,2])/sum(road_B[,2]))
distri_bike_B = cbind(distri_bike_B, 
                      s = distri_bike_B[,3]*distri_bike_B[,4], 
                      "s^2" = (distri_bike_B[,3]^2)*distri_bike_B[,4])
distri_bike_B = rbind(distri_bike_B,
                      c("-","Total","-", 
                        sum(distri_bike_B[,4]),
                        sum(distri_bike_B[,5]),
                        "-",
                        sum(distri_bike_B[,7]), 
                        sum(distri_bike_B[,8])))
write.table(distri_bike_B, file = "速率次數分配表_路段B機車.csv",sep = ",", col.names = TRUE, row.names = FALSE)

distri_car_B = cbind(upper = seq(0, 90, 5),
                     lower = seq(5, 95, 5),
                     mid = seq(2.5, 92.5, 5),
                     n = road_B[,3],
                     rate = 100*road_B[,3]/sum(road_B[,3]),
                     cumrate = 100*cumsum(road_B[,3])/sum(road_B[,3]))
distri_car_B = cbind(distri_car_B,
                     s = distri_car_B[,3]*distri_car_B[,4], 
                     "s^2" = (distri_car_B[,3]^2)*distri_car_B[,4])
distri_car_B = rbind(distri_car_B,
                     c("-","Total","-",
                       sum(distri_car_B[,4]),
                       sum(distri_car_B[,5]),
                       "-",
                       sum(distri_car_B[,7]), 
                       sum(distri_car_B[,8])))
write.table(distri_car_B, file = "速率次數分配表_路段B汽車.csv",sep = ",", col.names = TRUE, row.names = FALSE)

distri_bike_C = cbind(upper = seq(0, 90, 5),
                      lower = seq(5, 95, 5),
                      mid = seq(2.5, 92.5, 5),
                      n = road_C[,2],
                      rate = 100*road_C[,2]/sum(road_C[,2]),
                      cumrate = 100*cumsum(road_C[,2])/sum(road_C[,2]))
distri_bike_C = cbind(distri_bike_C, 
                      s = distri_bike_C[,3]*distri_bike_C[,4], 
                      "s^2" = (distri_bike_C[,3]^2)*distri_bike_C[,4])
distri_bike_C = rbind(distri_bike_C,
                      c("-","Total","-", 
                        sum(distri_bike_C[,4]),
                        sum(distri_bike_C[,5]),
                        "-",
                        sum(distri_bike_C[,7]), 
                        sum(distri_bike_C[,8])))
write.table(distri_bike_C, file = "速率次數分配表_路段C機車.csv",sep = ",", col.names = TRUE, row.names = FALSE)

distri_car_C = cbind(upper = seq(0, 90, 5),
                     lower = seq(5, 95, 5),
                     mid = seq(2.5, 92.5, 5),
                     n = road_C[,3],
                     rate = 100*road_C[,3]/sum(road_C[,3]),
                     cumrate = 100*cumsum(road_C[,3])/sum(road_C[,3]))
distri_car_C = cbind(distri_car_C, 
                     s = distri_car_C[,3]*distri_car_C[,4], 
                     "s^2" = (distri_car_C[,3]^2)*distri_car_C[,4])
distri_car_C = rbind(distri_car_C,
                     c("-","Total","-",
                       sum(distri_car_C[,4]),
                       sum(distri_car_C[,5]),
                       "-",
                       sum(distri_car_C[,7]), 
                       sum(distri_car_C[,8])))
write.table(distri_car_C, file = "速率次數分配表_路段C汽車.csv",sep = ",", col.names = TRUE, row.names = FALSE)


