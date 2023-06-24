chisq_car_C = matrix(as.numeric(distri_car_C[-20,1:4]), ncol = 4)
chisq_car_C = chisq_car_C[,-3]
chisq_car_C[1,1] = -Inf
chisq_car_C[19,2] = Inf

mean_car_C = as.numeric(distri_car_C[20,7])/as.numeric(distri_car_C[20,4])
sd_car_C = (as.numeric(distri_car_C[20,8])/as.numeric(distri_car_C[20,4]) - mean_car_C^2)^0.5
#4
chisq_car_C = cbind(chisq_car_C, chisq_car_C[,2]-mean_car_C)
#5
chisq_car_C = cbind(chisq_car_C, (chisq_car_C[,2]-mean_car_C)/sd_car_C)
#6
chisq_car_C = cbind(chisq_car_C, round(pnorm(chisq_car_C[,5]),5))
#7
a = c(pnorm(chisq_car_C[1,5]))
for (i in c(2:19)){
  a[i] = pnorm(chisq_car_C[i,5])-pnorm(chisq_car_C[i-1,5])
}
chisq_car_C = cbind(chisq_car_C, round(a,5))
#8
chisq_car_C = cbind(chisq_car_C, chisq_car_C[,7]*as.numeric(distri_car_C[20,4]))
#9
on = c(NA, NA, NA, NA, NA, NA, sum(chisq_car_C[1:7,3]), chisq_car_C[8:9,3], sum(chisq_car_C[10:19,3]), NA, NA, NA, NA, NA, NA, NA, NA, NA)
chisq_car_C = cbind(chisq_car_C, on)
#10
fn = c(NA, NA, NA, NA, NA, NA, sum(chisq_car_C[1:7,8]), chisq_car_C[8:9,8], sum(chisq_car_C[10:19,8]), NA, NA, NA, NA, NA, NA, NA, NA, NA)
chisq_car_C = cbind(chisq_car_C, fn)
#11
chisq_car_C = cbind(chisq_car_C, round((chisq_car_C[,9]-chisq_car_C[,10])^2/chisq_car_C[,10],5))

#chisq
chi_value = sum(chisq_car_C[,11], na.rm = TRUE)
free = (10-7+1)-2-1
critical = 3.841
normal_dis = chi_value < critical
chisq_car_C = rbind(chisq_car_C, c(chi_value, free, critical,as.character(normal_dis) , NA, NA, NA, NA, NA, NA, NA))

write.table(chisq_car_C, file = "常態分配_路段C汽車.csv",sep = ",", col.names = FALSE, row.names = FALSE)