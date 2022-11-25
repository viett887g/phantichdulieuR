update.packages("tools")
install.packages("ggplot2", lib="C:\\Program Files\\R\\R-4.1.0")
update.packages("ggplot2")
update.packages("data.table")
update.packages("forcats")
update.packages("dplyr")
update.packages("ggrepel")
library(ggplot2)
library("readxl")
setwd("C:/Users/ADMIN/Downloads")
my_data= read_excel("Hotel.xlsx")
df1= na.omit(my_data)
df= data.frame(df1)
#Tinh cac gia tri
fert<-df$Reviews
table(fert)
#Do dai
dodai<-length(fert)
print(dodai)
#Trung binh  
trung_binh<-mean(fert)
print(trung_binh)
#Yeu vi
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
yeu_vi<-getmode(fert)
print(yeu_vi)

#Trung vi
trung_vi<- median(fert)
print(trung_vi)

#Phuong sai
phuong_sai<-var(fert)
print(phuong_sai)

#Do lech chuan
do_lech_chuan<-sd(fert)
print(do_lech_chuan)

#Sai so chuan
sai_so_chuan = sd(fert)/sqrt(length(fert))
print(sai_so_chuan)
#gia tri nho nhat
gt_nho_nhat=min(fert)
print(gt_nho_nhat)
#gia tri lon nhat
gt_lon_nhat=max(fert)
print(gt_lon_nhat)

#Ve bieu do
hh=df$Terrible
hh
plot(fert,type="l",col='red',main = "dữ liệu khách sạn biểu đồ plot",ylab = "count")
lines(hh,type="l",col='blue')
#box
boxplot(fert,hh,
        main = "dữ liệu khách sạn tại Reviews và Terrible",
        xlab = "count",
        ylab = "Reviews và Terrible",
        col = "lightblue",
        border = "brown",horizontal = TRUE)
