chisq_bike_B = matrix(as.numeric(distri_bike_B[-20,1:4]), ncol = 4)
chisq_bike_B = chisq_bike_B[,-3]
chisq_bike_B[1,1] = -Inf
chisq_bike_B[19,2] = Inf

mean_bike_B = as.numeric(distri_bike_B[20,7])/as.numeric(distri_bike_B[20,4])
sd_bike_B = (as.numeric(distri_bike_B[20,8])/as.numeric(distri_bike_B[20,4]) - mean_bike_B^2)^0.5
#4
chisq_bike_B = cbind(chisq_bike_B, chisq_bike_B[,2]-mean_bike_B)
#5
chisq_bike_B = cbind(chisq_bike_B, (chisq_bike_B[,2]-mean_bike_B)/sd_bike_B)
#6
chisq_bike_B = cbind(chisq_bike_B, round(pnorm(chisq_bike_B[,5]),5))
#7
a = c(pnorm(chisq_bike_B[1,5]))
for (i in c(2:19)){
  a[i] = pnorm(chisq_bike_B[i,5])-pnorm(chisq_bike_B[i-1,5])
}
chisq_bike_B = cbind(chisq_bike_B, round(a,5))
#8
chisq_bike_B = cbind(chisq_bike_B, chisq_bike_B[,7]*as.numeric(distri_bike_B[20,4]))
#9
on = c(NA, NA, NA, NA, NA, NA, NA,sum(chisq_bike_B[1:8,3]), chisq_bike_B[9:15,3], sum(chisq_bike_B[16:19,3]), NA, NA, NA)
chisq_bike_B = cbind(chisq_bike_B, on)
#10
fn = c(NA, NA, NA, NA, NA, NA, NA,sum(chisq_bike_B[1:8,8]), chisq_bike_B[9:15,8], sum(chisq_bike_B[16:19,8]), NA, NA, NA)
chisq_bike_B = cbind(chisq_bike_B, fn)
#11
chisq_bike_B = cbind(chisq_bike_B, round((chisq_bike_B[,9]-chisq_bike_B[,10])^2/chisq_bike_B[,10],5))

#chisq
chi_value = sum(chisq_bike_B[,11], na.rm = TRUE)
free = (16-8+1)-2-1
critical = 12.59
normal_dis = chi_value < critical
chisq_bike_B = rbind(chisq_bike_B, c(chi_value, free, critical,as.character(normal_dis) , NA, NA, NA, NA, NA, NA, NA))

write.table(chisq_bike_B, file = "常態分配_路段B機車.csv",sep = ",", col.names = FALSE, row.names = FALSE)