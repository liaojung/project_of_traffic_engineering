chisq_bike_A = matrix(as.numeric(distri_bike_A[-20,1:4]), ncol = 4)
chisq_bike_A = chisq_bike_A[,-3]
chisq_bike_A[1,1] = -Inf
chisq_bike_A[19,2] = Inf

mean_bike_A = as.numeric(distri_bike_A[20,7])/as.numeric(distri_bike_A[20,4])
sd_bike_A = (as.numeric(distri_bike_A[20,8])/as.numeric(distri_bike_A[20,4]) - mean_bike_A^2)^0.5
#4
chisq_bike_A = cbind(chisq_bike_A, chisq_bike_A[,2]-mean_bike_A)
#5
chisq_bike_A = cbind(chisq_bike_A, (chisq_bike_A[,2]-mean_bike_A)/sd_bike_A)
#6
chisq_bike_A = cbind(chisq_bike_A, round(pnorm(chisq_bike_A[,5]),5))
#7
a = c(pnorm(chisq_bike_A[1,5]))
for (i in c(2:19)){
  a[i] = pnorm(chisq_bike_A[i,5])-pnorm(chisq_bike_A[i-1,5])
}
chisq_bike_A = cbind(chisq_bike_A, round(a,5))
#8
chisq_bike_A = cbind(chisq_bike_A, chisq_bike_A[,7]*as.numeric(distri_bike_A[20,4]))
#9
on = c(NA, NA, NA, NA, NA, NA, sum(chisq_bike_A[1:7,3]), chisq_bike_A[8:14,3], sum(chisq_bike_A[15:19,3]), NA, NA, NA, NA)
chisq_bike_A = cbind(chisq_bike_A, on)
#10
fn = c(NA, NA, NA, NA, NA, NA, sum(chisq_bike_A[1:7,8]), chisq_bike_A[8:14,8], sum(chisq_bike_A[15:19,8]), NA, NA, NA, NA)
chisq_bike_A = cbind(chisq_bike_A, fn)
#11
chisq_bike_A = cbind(chisq_bike_A, round((chisq_bike_A[,9]-chisq_bike_A[,10])^2/chisq_bike_A[,10],5))

#chisq
chi_value = sum(chisq_bike_A[,11], na.rm = TRUE)
free = (15-7+1)-2-1
critical = 16.92
normal_dis = chi_value < critical
chisq_bike_A = rbind(chisq_bike_A, c(chi_value, free, critical,as.character(normal_dis) , NA, NA, NA, NA, NA, NA, NA))

write.table(chisq_bike_A, file = "常態分配_路段A機車.csv",sep = ",", col.names = FALSE, row.names = FALSE)