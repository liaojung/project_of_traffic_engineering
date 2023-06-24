chisq_car_B = matrix(as.numeric(distri_car_B[-20,1:4]), ncol = 4)
chisq_car_B = chisq_car_B[,-3]
chisq_car_B[1,1] = -Inf
chisq_car_B[19,2] = Inf

mean_car_B = as.numeric(distri_car_B[20,7])/as.numeric(distri_car_B[20,4])
sd_car_B = (as.numeric(distri_car_B[20,8])/as.numeric(distri_car_B[20,4]) - mean_car_B^2)^0.5
#4
chisq_car_B = cbind(chisq_car_B, chisq_car_B[,2]-mean_car_B)
#5
chisq_car_B = cbind(chisq_car_B, (chisq_car_B[,2]-mean_car_B)/sd_car_B)
#6
chisq_car_B = cbind(chisq_car_B, round(pnorm(chisq_car_B[,5]),5))
#7
a = c(pnorm(chisq_car_B[1,5]))
for (i in c(2:19)){
  a[i] = pnorm(chisq_car_B[i,5])-pnorm(chisq_car_B[i-1,5])
}
chisq_car_B = cbind(chisq_car_B, round(a,5))
#8
chisq_car_B = cbind(chisq_car_B, chisq_car_B[,7]*as.numeric(distri_car_B[20,4]))
#9
on = c(NA, NA, NA, NA, NA, NA, NA,sum(chisq_car_B[1:9,3]), chisq_car_B[10:15,3], sum(chisq_car_B[15:19,3]), NA, NA, NA)
chisq_car_B = cbind(chisq_car_B, on)
#10
fn = c(NA, NA, NA, NA, NA, NA, NA,sum(chisq_car_B[1:9,8]), chisq_car_B[10:15,8], sum(chisq_car_B[15:19,8]), NA, NA, NA)
chisq_car_B = cbind(chisq_car_B, fn)
#11
chisq_car_B = cbind(chisq_car_B, round((chisq_car_B[,9]-chisq_car_B[,10])^2/chisq_car_B[,10],5))

#chisq
chi_value = sum(chisq_car_B[,11], na.rm = TRUE)
free = (15-9+1)-2-1
critical = 9.488
normal_dis = chi_value < critical
chisq_car_B = rbind(chisq_car_B, c(chi_value, free, critical,as.character(normal_dis) , NA, NA, NA, NA, NA, NA, NA))

write.table(chisq_car_B, file = "常態分配_路段B汽車.csv",sep = ",", col.names = FALSE, row.names = FALSE)