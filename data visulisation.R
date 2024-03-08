library(tidyr)
install.packages("vcd")
library(magrittr)
library(dplyr)
library(GGally)
library(skimr)
data_true<-read.csv("group_23 .csv") %>%
  filter(Semer=="CL0")%>%
  select(-Semer)
data_group<-data_true%>%
  group_by(Amyl)
summary<-data_group%>%
  summarise(n())
data_true$Amyl[data_true$Amyl=="CL3"|data_true$Amyl=="CL4"|data_true$Amyl=="CL5"|data_true$Amyl=="CL6"]<-"CLcom"
cross_table <- table(data_true$Amyl, data_true$Age)
cross_table_df <- as.data.frame(cross_table)
names(cross_table_df) <- c("drug_last_use_time", "Age", "frequency")
ggplot(cross_table_df, aes(x = drug_last_use_time, y = frequency, fill = Age)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Barplot of drug last use time vs age",
       x = "drug last use time", y = "Frequency") +
  theme_minimal()
ggplot(data_true,aes(x=Amyl,group=Age,fill=Age))+geom_bar()
library(vcd)
# 安装和加载 FactoMineR 包
install.packages("FactoMineR")
library(FactoMineR)
result <- chisq.test(data_true$Age, data_true$Amyl)
print(result)
# 进行多重对应分析
mca_results <- MCA(data_true, graph = FALSE)
# 查看结果和图形
print(mca_results)
plot(mca_results)


