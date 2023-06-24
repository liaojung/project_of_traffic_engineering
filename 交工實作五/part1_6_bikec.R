chisq_bike_C = matrix(as.numeric(distri_bike_C[-20,1:4]), ncol = 4)
chisq_bike_C = chisq_bike_C[,-3]
chisq_bike_C[1,1] = -Inf
chisq_bike_C[19,2] = Inf

mean_bike_C = as.numeric(distri_bike_C[20,7])/as.numeric(distri_bike_C[20,4])
sd_bike_C = (as.numeric(distri_bike_C[20,8])/as.numeric(distri_bike_C[20,4]) - mean_bike_C^2)^0.5
#4
chisq_bike_C = cbind(chisq_bike_C, chisq_bike_C[,2]-mean_bike_C)
#5
chisq_bike_C = cbind(chisq_bike_C, (chisq_bike_C[,2]-mean_bike_C)/sd_bike_C)
#6
chisq_bike_C = cbind(chisq_bike_C, round(pnorm(chisq_bike_C[,5]),5))
#7
a = c(pnorm(chisq_bike_C[1,5]))
for (i in c(2:19)){
  a[i] = pnorm(chisq_bike_C[i,5])-pnorm(chisq_bike_C[i-1,5])
}
chisq_bike_C = cbind(chisq_bike_C, round(a,5))
#8
chisq_bike_C = cbind(chisq_bike_C, chisq_bike_C[,7]*as.numeric(distri_bike_C[20,4]))
#9
on = c(NA, NA, NA, NA, NA, sum(chisq_bike_C[1:6,3]), chisq_bike_C[7:11,3], sum(chisq_bike_C[12:19,3]), NA, NA, NA, NA, NA, NA, NA)
chisq_bike_C = cbind(chisq_bike_C, on)
#10
fn = c(NA, NA, NA, NA, NA, sum(chisq_bike_C[1:6,8]), chisq_bike_C[7:11,8], sum(chisq_bike_C[12:19,8]), NA, NA, NA, NA, NA, NA, NA)
chisq_bike_C = cbind(chisq_bike_C, fn)
#11
chisq_bike_C = cbind(chisq_bike_C, round((chisq_bike_C[,9]-chisq_bike_C[,10])^2/chisq_bike_C[,10],5))

#chisq
chi_value = sum(chisq_bike_C[,11], na.rm = TRUE)
free = (12-6+1)-2-1
critical = 9.488
normal_dis = chi_value < critical
chisq_bike_C = rbind(chisq_bike_C, c(chi_value, free, critical,as.character(normal_dis) , NA, NA, NA, NA, NA, NA, NA))

write.table(chisq_bike_C, file = "常態分配_路段C機車.csv",sep = ",", col.names = FALSE, row.names = FALSE)