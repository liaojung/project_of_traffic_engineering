chisq_car_A = matrix(as.numeric(distri_car_A[-20,1:4]), ncol = 4)
chisq_car_A = chisq_car_A[,-3]
chisq_car_A[1,1] = -Inf
chisq_car_A[19,2] = Inf

mean_car_A = as.numeric(distri_car_A[20,7])/as.numeric(distri_car_A[20,4])
sd_car_A = (as.numeric(distri_car_A[20,8])/as.numeric(distri_car_A[20,4]) - mean_car_A^2)^0.5
#4
chisq_car_A = cbind(chisq_car_A, chisq_car_A[,2]-mean_car_A)
#5
chisq_car_A = cbind(chisq_car_A, (chisq_car_A[,2]-mean_car_A)/sd_car_A)
#6
chisq_car_A = cbind(chisq_car_A, round(pnorm(chisq_car_A[,5]),5))
#7
a = c(pnorm(chisq_car_A[1,5]))
for (i in c(2:19)){
  a[i] = pnorm(chisq_car_A[i,5])-pnorm(chisq_car_A[i-1,5])
}
chisq_car_A = cbind(chisq_car_A, round(a,5))
#8
chisq_car_A = cbind(chisq_car_A, chisq_car_A[,7]*as.numeric(distri_car_A[20,4]))
#9
on = c(NA, NA, NA, NA, NA, NA, NA,sum(chisq_car_A[1:8,3]), chisq_car_A[9:14,3], sum(chisq_car_A[15:19,3]), NA, NA, NA, NA)
chisq_car_A = cbind(chisq_car_A, on)
#10
fn = c(NA, NA, NA, NA, NA, NA, NA,sum(chisq_car_A[1:8,8]), chisq_car_A[9:14,8], sum(chisq_car_A[15:19,8]), NA, NA, NA, NA)
chisq_car_A = cbind(chisq_car_A, fn)
#11
chisq_car_A = cbind(chisq_car_A, round((chisq_car_A[,9]-chisq_car_A[,10])^2/chisq_car_A[,10],5))

#chisq
chi_value = sum(chisq_car_A[,11], na.rm = TRUE)
free = (15-8+1)-2-1
critical = 11.07
normal_dis = chi_value < critical
chisq_car_A = rbind(chisq_car_A, c(chi_value, free, critical,as.character(normal_dis) , NA, NA, NA, NA, NA, NA, NA))

write.table(chisq_car_A, file = "常態分配_路段A汽車.csv",sep = ",", col.names = FALSE, row.names = FALSE)