#패키지
install.packages("dplyr") #데이터 편집
install.packages("ggplot2") #그래프
install.packages("tidyverse") #문자열 편집
install.packages("lubridate") #시간데이터 편집

#라이브러리
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)

#데이터 불러오기
###쿨링로드 with metro
not_cl_metro = read.csv("/Users/kihun/Downloads/s-dot-metro.csv",fileEncoding = "euc-kr")
not_cl_num <- not_cl_metro
list_not_metro <- unique(c(not_cl_metro$시리얼번호))
not_cl_num <- not_cl_num %>% rename(시리얼=시리얼번호)

dt_4_not_cl_metro = NULL
for(i in 1:dim(dt_4)[1]){
  if(dt_4[i,2] %in% list_not_metro){
    {
#      if(dt_4[i,2] != "V02Q1940745"){
        temp = dt_4[i,]
        dt_4_not_cl_metro = rbind(dt_4_not_cl_metro, temp)
#      }
    }
  }
} 
not_metro_join <- left_join(dt_4_not_cl_metro,not_cl_num,by="시리얼",relationship="many-to-many")
not_mean_by_buffer <- not_metro_join %>% group_by(역명) %>% summarize(PM_mean = mean(미세먼지보정))

metro_data <- read_excel("/Users/kihun/Downloads/metro_union_new.xlsx")
not_metro_final <- left_join(metro_data,not_mean_by_buffer,by="역명")
write.csv(not_metro_final,"/Users/kihun/Downloads/metro_union_final.csv",fileEncoding = "euc-kr")
#
ggplot(data=metro_join) + geom_line(mapping=aes(x=등록일자,y=미세먼지보정,group=시리얼,color=시리얼))+
  labs(x='Time', y='PM10') + 
  ggtitle('4/27 Cooling-Road PM10 / Time') +
  theme(legend.position="none")

"V02Q1940745"
'OC3CL200026'

###metro without Cooling-Road
cl_metro = read.csv("//Users/kihun/Downloads/metro_union_new.xlsx",fileEncoding = "euc-kr")
cl_num <- cl_metro[,c("번호","시리얼번호")]
list_metro <- c(cl_metro$시리얼번호)
cl_num = cl_num %>% rename(시리얼=시리얼번호)

dt_4_cl_metro = NULL
for(i in 1:dim(dt_4)[1]){
  if(dt_4[i,2] %in% list_metro){
    {
      if(dt_4[i,2] != "V02Q1940745"){
        temp = dt_4[i,]
        dt_4_cl_metro = rbind(dt_4_cl_metro, temp)
      }
    }
  }
} 
metro_join <- inner_join(dt_4_cl_metro,cl_num,by="시리얼")
mean_by_buffer <- metro_join %>% group_by(번호) %>% summarize(PM_mean = mean(미세먼지보정))
write.csv(mean_by_buffer,"metro_PM_mean.csv",fileEncoding = "euc-kr")
#
ggplot(data=metro_join) + geom_line(mapping=aes(x=등록일자,y=미세먼지보정,group=시리얼,color=시리얼))+
  labs(x='Time', y='PM10') + 
  ggtitle('4/27 Cooling-Road PM10 / Time') +
  theme(legend.position="none")

"V02Q1940745"
'OC3CL200026'


############3월29일########
dt_3=read.csv("/Users/kihun/Documents/졸업작품/졸업작품-데이터/S-DoT_NATURE_2021/S-DoT_NATURE_2021.03.29-04.04.csv", fileEncoding="euc-kr")
dt_3=read.csv("/Users/kihun/Documents/졸업작품/졸업작품-데이터/S-DoT_NATURE_2021/S-DoT_NATURE_2021.11.16-11.22.csv", fileEncoding="euc-kr")
dt_3=read.csv("/Users/kihun/Documents/졸업작품/졸업작품-데이터/S-DoT_NATURE_2021/S-DoT_NATURE_2021.05.03-05.09.csv", fileEncoding="euc-kr")
dt_3=read.csv("/Users/kihun/Downloads/S-DoT_NATURE_2022/S-DoT_NATURE_2022.04.25-05.01.csv", fileEncoding = "euc-kr")
##
dt_3=read.csv("/Users/kihun/Downloads/S-DoT_NATURE_2022/S-DoT_NATURE_2022.02.28-03.06.csv", fileEncoding = "euc-kr")
dt_3$X.4 = ymd_hm(dt_3$X.4)
dt_3 <- dt_3 %>% filter(!is.na(dt_3[,31]))
dt_3_trim = NULL
for(i in 1:dim(dt_3)[1]){
    if(str_detect(dt_3[i,31],"2022-03-05")){
      dt_3_trim = rbind(dt_3_trim,dt_3[i,])
  }
}

dt_3_demo = dt_3_trim[,c(2,23,31)]
dt_3_demo <- dt_3_demo %>% rename(시리얼=모델명, 미세먼지보정=초미세먼지.보정.....,등록일자=X.4)
dt_3_29 = dt_3_demo
##
dt_3$등록일자=ymd_hm(dt_3$등록일자)
dt_3_demo = dt_3[,c(3,6,24,26)]
dt_3_demo <- rename(dt_3_demo, 미세먼지 = 미세먼지.....)
dt_3_demo <- rename(dt_3_demo, 미세먼지보정 = 미세먼지.보정.....)
dt_3_demo[,4]

dt_3_29 = NULL
for(i in 1:dim(dt_3_demo)[1]){
  if(str_detect(dt_3_demo[i,4],"2021-03-29")){
    dt_3_29 = rbind(dt_3_29, dt_3_demo[i,])
  }
}

dt_3_cl = NULL
for(i in 1:dim(dt_3_29)[1]){
  if(dt_3_29[i,1] %in% lst){
        temp = dt_3_29[i,]
        dt_3_cl = rbind(dt_3_cl, temp)
  }
} 

ggplot(data=dt_3_cl) + geom_line(mapping=aes(x=등록일자,y=미세먼지보정,group=시리얼,color=시리얼))+
  labs(x='Time', y='PM10') + 
  ggtitle('3/29 Cooling-Road PM10 / Time') +
  theme(legend.position="none")

dt_3_rd = NULL
for(i in 1:dim(dt_3_29)[1]){
  if(dt_3_29[i,1] %in% rd_select){
    if(dt_3_29[i,1] != "V02Q1940871" & dt_3_29[i,1] != "V02Q1940474" & dt_3_29[i,1] != "V02Q1940859" ){
          temp = dt_3_29[i,]
          dt_3_rd = rbind(dt_3_rd, temp)
    }
  }
}

ggplot(data=dt_3_rd) + geom_line(mapping=aes(x=등록일자,y=미세먼지보정,group=시리얼,color=시리얼))+
  labs(x='Time', y='PM10') + 
  ggtitle('3/29 Random Select PM10 / Time') +
  theme(legend.position="none")

dt_3_cl$cls <- as.factor(rep("Cooling Road",1080))
dt_3_rd$cls <- as.factor(rep("Random Select",973))

dt_3_all = NULL
dt_3_fin = rbind(dt_3_rd, dt_3_cl)
dt_3_fin$cls=as.factor(dt_3_fin$cls)
dt_3_all <- dt_3_fin %>% filter(!is.na(dt_3_fin[,2]))

#그래프
ggplot(data=dt_3_all) + geom_line(mapping=aes(x=등록일자,y=미세먼지보정,group=시리얼,color=cls))

#평균비교
dt_3_summary=aggregate(x=dt_3_all, by=list(dt_3_all$등록일자,dt_3_all$cls), FUN="mean")
ggplot(data=dt_3_summary) + geom_line(mapping=aes(x=Group.1,y=미세먼지보정,group=Group.2,color=Group.2)) + 
  labs(x="Time", y="PM10") + ggtitle('3/29 - PM10 Difference')

#t-test
res <- t.test(미세먼지보정 ~ cls, data = dt_3_all)
res

############4월############
dt_4 = read.csv("/Users/kihun/Downloads/data(2022.04.27).csv",fileEncoding="euc-kr") #4월27일
dt_4$등록일자=ymd_hm(dt_4$등록일자) #날짜처리
dt_4$등록일자=hour(dt_4$등록일자)
#쿨링로드 ; 버퍼 500
lst = c("V02Q1940130","V02Q1940104","OC3CL200182","OC3CL200067","OC3CL200184",
        "OC3CL200026","OC3CL200027","V02Q1940194","V02Q1940384","V02Q1940952","V02Q1940301",
        "OC3CL200079","V02Q1940642","V02Q1940112","OC3CL200164","V02Q1940793","V02Q1940757",
        "V02Q1940745","V02Q1940867","V02Q1940943","V02Q1940766","V02Q1940794","V02Q1940806",
        "V02Q1940574","V02Q1940336","V02Q1940326","OC3CL200035","OC3CL200204","V02Q1940719",
        "V02Q1940839","OC3CL200210","OC3CL200211","OC3CL200194","V02Q1940541","V02Q1940053",
        "V02Q1940566","V02Q1940604","V02Q1940567","V02Q1940780","V02Q1940935","OC3CL200041",
        "OC3CL200212","V02Q1940311","V02Q1940727","V02Q1940944") 

lst_cl = read.csv("/Users/kihun/Documents/지하철역_5179ver/s-dot-500/s-dot-500.csv", fileEncoding = "euc-kr")
lst_cl = lst_cl[,c(1,2,8,9)]
lst_cl_select = unique(lst_cl$시리얼번호)
#쿨링로드 ; 버퍼 1000 - 500
lst_km = read.csv("/Users/kihun/Documents/쿨링로드_위치_버퍼/s_dot_point_1000-500/s-dot_point_cl_road_1000-500.csv",fileEncoding="euc-kr")
lst_km_select = as.data.frame(unique(lst_km$시리얼번호))
lst_km_select <- rename(lst_km_select, 시리얼 = "unique(lst_km$시리얼번호)")
##쿨링로드
dt_4_cl = NULL
for(i in 1:dim(dt_4)[1]){
  if(dt_4[i,2] %in% lst){
    temp = dt_4[i,]
    dt_4_cl = rbind(dt_4_cl, temp)
  }
} 
#이상치제거
dt_4_cl_eli = NULL
for(i in 1:dim(dt_4_cl)[1]){
  if(dt_4_cl[i,2] != "V02Q1940745" ){
    dt_4_cl_eli = rbind(dt_4_cl_eli, dt_4_cl[i,])
  }
}
######
dt = dt_4_cl_eli
dt$등록일자 = hour(dt$등록일자)
######
ggplot(data=dt) + geom_line(mapping=aes(x=factor(등록일자),y=미세먼지보정,group=시리얼,color=시리얼))+
  labs(x='Time', y='PM10 (μg/m^3 )')+ 
  ggtitle('4/27 Cooling-Road PM10 / Time') +
  theme(legend.position="none", axis.title = element_text(size=14))

#4월27일 쿨링로드 그래프
ggplot(data=dt_4_cl) + geom_line(mapping=aes(x=등록일자,y=미세먼지보정,group=시리얼,color=시리얼))+
  labs(x='Time', y='PM10 (μg/m^3 )')+ 
  ggtitle('4/27 Cooling-Road PM10 / Time') +
  theme(legend.position="none")
#4월 27일 이상치 제거 그래프
ggplot(data=dt_4_cl_eli) + geom_line(mapping=aes(x=등록일자,y=미세먼지보정,group=시리얼,color=시리얼))+
  labs(x='Time', y='PM10') + 
  ggtitle('4/27 Cooling-Road PM10 / Time') +
  theme(legend.position="none", axis.text.y=element_text(angle=90))


##랜덤선택
random_select = read.csv("/Users/kihun/Downloads/new_select45_sdot/new_select.csv",fileEncoding="euc-kr")
random_select = random_select[,c(1,2,8,9)]
rd_select <- unique(random_select$시리얼)
dt_4_rd = NULL
for(i in 1:dim(dt_4)[1]){
  if(dt_4[i,2] %in% rd_select){
    temp = dt_4[i,]
    dt_4_rd = rbind(dt_4_rd, temp)
  }
}
ggplot(dt_4_rd) + geom_line(mapping=aes(x=등록일자, y=미세먼지보정, group=시리얼, color=시리얼)) + theme(legend.position="none")
#이상치제거 - 미세먼지 200 초과, 0
dt_4_rd_eli = NULL
for(i in 1:dim(dt_4_rd)[1]){
  if(dt_4_rd[i,2] != "V02Q1940471"  & dt_4_rd[i,2] != "V02Q1940871" ){
    dt_4_rd_eli = rbind(dt_4_rd_eli, dt_4_rd[i,])
  }
}
#그래프 그리기
ggplot(data=dt_4_rd_eli) + geom_line(mapping=aes(x=등록일자,y=미세먼지보정,group=시리얼,color=시리얼))+
  labs(x='Time', y='PM10') + 
  ggtitle('4/27 Random Select - Eliminate PM10 / Time') +
  theme(legend.position="none")

ggplot(data=dt_4_rd_eli) + geom_line(mapping=aes(x=factor(등록일자),y=미세먼지보정,group=시리얼,color=시리얼))+
  labs(x='Time', y='PM10 (μg/m^3 )')+ 
  ggtitle('4/27 Random Select PM10 / Time') +
  theme(legend.position="none", axis.title = element_text(size=14))


##버퍼 1km - 500m
dt_4_buf = NULL
for(i in 1:dim(dt_4)[1]){
  if(dt_4[i,2] %in% lst_km_select){
    temp = dt_4[i,]
    dt_4_buf = rbind(dt_4_buf, temp)
  }
}
ggplot(dt_4_buf) + geom_line(mapping=aes(x=등록일자, y=미세먼지보정, group=시리얼, color=시리얼)) + theme(legend.position="none")
#이상치제거 - 미세먼지 200 초과, 0; "V02Q1940241" , OC3CL200014, V02Q1940297, V02Q1940865
dt_4_buf_eli = NULL
for(i in 1:dim(dt_4_buf)[1]){
  if(dt_4_buf[i,2] != "V02Q1940241" & dt_4_buf[i,2] != "OC3CL200014" & dt_4_buf[i,2] !="V02Q1940297" & dt_4_buf[i,2] != "V02Q1940865" & dt_4_buf[i,2] != "V02Q1940438" ){
    dt_4_buf_eli = rbind(dt_4_buf_eli, dt_4_buf[i,])
  }
}
#그래프 그리기
dt_4_rd_eli$등록일자=hour(dt_4_rd_eli$등록일자)
ggplot(data=dt_4_buf_eli) + geom_line(mapping=aes(x=등록일자,y=미세먼지보정,group=시리얼,color=시리얼))+
  labs(x='Time', y='PM10') + 
  ggtitle('4/27 500-1000(m) - Eliminate PM10 / Time') +
  theme(legend.position="none")



##구분자 생성
dt_4_cl_eli$Group <- as.factor(rep("Cooling Road",940))
dt_4_rd_eli$Group <- as.factor(rep("Random Select",936))
dt_4_buf_eli$cls <- as.factor(rep(3,1310))
#데이터 통합
dt_all = NULL
dt_all = rbind(dt_4_rd_eli, dt_4_cl_eli)
dt_all$Group=as.factor(dt_all$Group)

dt_cl_buf_all = NULL
dt_cl_buf_all = rbind(dt_4_cl_eli, dt_4_buf_eli)
dt_cl_buf_all$cls = as.factor(dt_cl_buf_all$cls)
#그래프
dt_all$등록일자 = hour(dt_all$등록일자)
ggplot(data=dt_all) + geom_line(mapping=aes(x=factor(등록일자),y=미세먼지보정,group=시리얼,color=Group)) + labs(x="Time",y='PM10 (μg/m^3)' )+ ggtitle("4/27 - PM10 Difference") + theme(axis.title= element_text(size=14))
ggplot(data=dt_cl_buf_all) + geom_line(mapping=aes(x=등록일자,y=미세먼지보정,group=시리얼,color=cls))
#평균비교
dt_summary=aggregate(x=dt_all, by=list(dt_all$등록일자,dt_all$Group), FUN="mean")
dt_summary_1=aggregate(x=dt_cl_buf_all, by=list(dt_cl_buf_all$등록일자,dt_cl_buf_all$cls), FUN="mean")
ggplot(data=dt_summary) + geom_line(mapping=aes(x=factor(Group.1),y=미세먼지보정,group=Group.2,color=Group.2)) + labs(x="Time",y='PM10 (μg/m^3)' ) + ggtitle("4/27 - PM10 Difference")+theme(axis.title = element_text(size=14))
ggplot(data=dt_summary_1) + geom_line(mapping=aes(x=Group.1,y=미세먼지보정,group=Group.2,color=Group.2))
#t-test
res_1 <- t.test(미세먼지보정 ~ cls, data = dt_cl_buf_all)
res_1
res <- t.test(미세먼지보정 ~ Group, data = dt_all)
res


########################################8월####################################################
dt_8 = read.csv("/Users/kihun/Documents/졸업작품/졸업작품-데이터/S-DoT_NATURE_2022/S-DoT_NATURE_2022.08.01-08.07.csv", fileEncoding = "euc-kr") #8월5일
dt_8 = dt_8[,c(3,7,26)]
dt_8 <- rename(dt_8, 기온 = 기온...)
dt_8$등록일자=ymd_hm(dt_8$등록일자)

dt_8_2nd = read.csv("/Users/kihun/Documents/졸업작품/졸업작품-데이터/S-DoT_NATURE_2022/S-DoT_NATURE_2022.08.08-08.14.csv", fileEncoding = "euc-kr") #8월14일
dt_8_2nd = dt_8_2nd[,c(3,7,26)]
dt_8_2nd <- rename(dt_8_2nd, 기온 = 기온...)
dt_8_2nd$등록일자=ymd_hm(dt_8_2nd$등록일자)

##8월 5일  기온
dt_8_5 = NULL
for(i in 1:dim(dt_8)[1]){
  if(str_detect(dt_8[i,3],"2022-08-05")){
    dt_8_5 = rbind(dt_8_5, dt_8[i,])
  }
}

#쿨링로드 설치 장소
dt_8_cl = NULL
for(i in 1:dim(dt_8_5)[1]){
  if(dt_8_5[i,1] %in% lst){
    dt_8_cl = rbind(dt_8_cl, dt_8_5[i,])
  }
}
ggplot(dt_8_cl) + geom_line(mapping=aes(x=등록일자,y=기온,group=시리얼,color=시리얼))+
  labs(x='Time', y='Temperature') + 
  ggtitle('8/5 Cooling-Road TMP / Time') +
  theme(legend.position="none")

#랜덤 장소
dt_8_rd = NULL
for(i in 1:dim(dt_8_5)[1]){
  if(dt_8_5[i,1] %in% rd_select & dt_8_5[i,1] != "V02Q1940954"){
    dt_8_rd = rbind(dt_8_rd, dt_8_5[i,])
  }
}
ggplot(dt_8_rd) + geom_line(mapping=aes(x=등록일자,y=기온,group=시리얼,color=시리얼))+
  labs(x='Time', y='Temperature') + 
  ggtitle('8/5 Random Select TMP / Time') +
  theme(legend.position="none")


##구분자 생성
dt_8_cl$cls <- as.factor(rep(1,405))
dt_8_rd$cls <- as.factor(rep(2,427))
#데이터 통합
dt_8_5_all = NULL
dt_8_5_all = rbind(dt_8_cl, dt_8_rd)
dt_8_5_all$cls=as.factor(dt_8_5_all$cls)

#그래프
ggplot(data=dt_8_5_all) + geom_line(mapping=aes(x=등록일자,y=기온,group=시리얼,color=cls))

#평균비교
dt_8_5_summary=aggregate(x=dt_8_5_all, by=list(dt_8_5_all$등록일자,dt_8_5_all$cls), FUN="mean")
ggplot(data=dt_8_5_summary) + geom_line(mapping=aes(x=Group.1,y=기온,group=Group.2,color=Group.2)) + 
  labs(x="Time", y="PM10") + ggtitle('PM10 Difference')


#t-test
res_8 <- t.test(기온 ~ cls, data = dt_8_5_all)
res_8



##8월 12일  기온
dt_8_12 = NULL
for(i in 1:dim(dt_8_2nd)[1]){
  if(str_detect(dt_8_2nd[i,3],"2022-08-12")){
    dt_8_12 = rbind(dt_8_12, dt_8_2nd[i,])
  }
}

#쿨링로드 설치 장소
dt_8_12_cl = NULL
for(i in 1:dim(dt_8_12)[1]){
  if(dt_8_12[i,1] %in% lst){
    if(!is.na(dt_8_12[i,2])){
      dt_8_12_cl = rbind(dt_8_12_cl, dt_8_12[i,])
    }
  }
}
ggplot(dt_8_12_cl) + geom_line(mapping=aes(x=등록일자,y=기온,group=시리얼,color=시리얼))+
  labs(x='Time', y='Temperature') + 
  ggtitle('8/12 Cooling-Road TMP / Time')  + theme(legend.position="none")

#랜덤 장소
dt_8_12_rd = NULL
for(i in 1:dim(dt_8_12)[1]){
  if(dt_8_12[i,1] %in% rd_select){
    if(!is.na(dt_8_12[i,2])){
      dt_8_12_rd = rbind(dt_8_12_rd, dt_8_12[i,])
    }
  }
}
ggplot(dt_8_12_rd) + geom_line(mapping=aes(x=등록일자,y=기온,group=시리얼,color=시리얼))+
  labs(x='Time', y='Temperature') + 
  ggtitle('8/12 Random Select TMP / Time') +
  theme(legend.position="none")

##구분자 생성
dt_8_12_cl$cls <- as.factor(rep(1,971))
dt_8_12_rd$cls <- as.factor(rep(2,1019))
#데이터 통합
dt_8_12_all = NULL
dt_8_12_all = rbind(dt_8_12_cl, dt_8_12_rd)
dt_8_12_all$cls=as.factor(dt_8_12_all$cls)

#그래프
ggplot(data=dt_8_12_all) + geom_line(mapping=aes(x=등록일자,y=기온,group=시리얼,color=cls))

#평균비교
dt_8_12_summary=aggregate(x=dt_8_12_all, by=list(dt_8_12_all$등록일자,dt_8_12_all$cls), FUN="mean")
ggplot(data=dt_8_12_summary) + geom_line(mapping=aes(x=Group.1,y=기온,group=Group.2,color=Group.2))

#t-test
res_8_12 <- t.test(기온 ~ cls, data = dt_8_12_all)
res_8_12

##8월 4일  기온
dt_8_4 = NULL
for(i in 1:dim(dt_8)[1]){
  if(str_detect(dt_8[i,3],"2022-08-04")){
    dt_8_4 = rbind(dt_8_5, dt_8[i,])
  }
}

#쿨링로드 설치 장소
dt_8_4_cl = NULL
for(i in 1:dim(dt_8_4)[1]){
  if(dt_8_4[i,1] %in% lst){
    dt_8_4_cl = rbind(dt_8_4_cl, dt_8_4[i,])
  }
}
ggplot(dt_8_4_cl) + geom_line(mapping=aes(x=등록일자,y=기온,group=시리얼,color=시리얼))+
  labs(x='Time', y='Temperature') + 
  ggtitle('8/4 Cooling-Road TMP / Time') +
  theme(legend.position="none")

#랜덤 장소
dt_8_4_rd = NULL
for(i in 1:dim(dt_8_4)[1]){
  if(dt_8_4[i,1] %in% rd_select){
    if(!is.na(dt_8_4[i,2])){
      dt_8_4_rd = rbind(dt_8_4_rd, dt_8_4[i,])
    }
  }
}
ggplot(dt_8_4_rd) + geom_line(mapping=aes(x=등록일자,y=기온,group=시리얼,color=시리얼))+
  labs(x='Time', y='Temperature') + 
  ggtitle('8/5 Random Select TMP / Time') +
  theme(legend.position="none")


##구분자 생성
dt_8_4_cl$cls <- as.factor(rep(1,405))
dt_8_4_rd$cls <- as.factor(rep(2,427))
#데이터 통합
dt_8_4_all = NULL
dt_8_4_all = rbind(dt_8_4_cl, dt_8_4_rd)
dt_8_4_all$cls=as.factor(dt_8_4_all$cls)

#그래프
ggplot(data=dt_8_4_all) + geom_line(mapping=aes(x=등록일자,y=기온,group=시리얼,color=cls))

#평균비교
dt_8_4_summary=aggregate(x=dt_8_4_all, by=list(dt_8_4_all$등록일자,dt_8_4_all$cls), FUN="mean")
ggplot(data=dt_8_4_summary) + geom_line(mapping=aes(x=Group.1,y=기온,group=Group.2,color=Group.2))

#t-test
res_8 <- t.test(기온 ~ cls, data = dt_8_4_all)
res_8

#######################종속변수 구축
dt_4_cl_eli_mean <- dt_4_cl_eli %>% group_by(시리얼) %>% summarise(mean=mean(미세먼지보정))
dt_4_rd_eli_mean <- dt_4_rd_eli %>% group_by(시리얼) %>% summarise(mean=mean(미세먼지보정))

#########################지도 위 시각화
install.packages("raster")
install.packages("ggmap")
install.packages("rgeos")
install.packages("maptools")
install.packages("rgdal")

library(raster)
library(ggmap)
library(rgeos)
library(maptools)
library(rgdal)

dt_4_cl_mean <- dt_4_cl_eli_mean %>% left_join(lst_cl, by=c("시리얼" = "시리얼번호"))
dt_4_rd_mean <- dt_4_rd_eli_mean %>% left_join(random_select, by=c("시리얼" = "시리얼"))

map <- shapefile("/Users/kihun/Documents/졸업작품/졸업작품-데이터/_census_data_2022_bnd_dong_bnd_dong_11_2022_2022/bnd_dong_11_2022_2022_2Q.shp")
df_map <- fortify(map, region="ADM_CD")
map_v <-ggplot() + geom_polygon(data = df_map, mapping=aes(x=long,y=lat,group=group),color="white",fill="grey")

dt_4_cl_mean$cls <- rep(1,dim(dt_4_cl_mean)[1])
dt_4_rd_mean$cls <- rep(2,dim(dt_4_rd_mean)[1])
dt_4_cl_mean$dif <- dt_4_cl_mean$mean - mean(dt_4_rd_mean$mean)

mean_all = rbind(dt_4_cl_mean, dt_4_rd_mean)

map_all <- map_v + geom_point(data=mean_all, aes(x=경도, y=위도,size=mean, color=cls), alpha=0.7) # cl + rd

map_v + geom_point(data=dt_4_cl_mean, aes(x=경도, y=위도,size=1000*dif), color="blue", alpha=0.7) #dif

map_cl <- map_v + geom_point(data=dt_4_cl_mean, aes(x=경도, y=위도, size=dif, color="pink"),alpha=0.7) #Cl
map_rd <- map_cl + geom_point(data=dt_4_rd_mean, aes(x=경도, y=위도, size=mean, color="blue"),alpha=0.7) #rd

###########4/18 bldg, link#######################
link_data = read.csv("/Users/kihun/Documents/new_link/link_data.csv",fileEncoding = "euc-kr")
cl_surf <- link_data %>% group_by(지하철역) %>% summarize(surf = mean(ROAD_BT * ROAD_LT))
write.csv(cl_surf,"/Users/kihun/Documents/cl_link_surf.csv",fileEncoding="euc-kr")

new_link_data = read.csv("/Users/kihun/Documents/new_link/new_select_link.csv",fileEncoding = "euc-kr")
rd_surf <- new_link_data %>% group_by(시리얼) %>% summarize(surf = mean(ROAD_BT * ROAD_LT))
write.csv(rd_surf,"/Users/kihun/Documents/rd_link_surf.csv",fileEncoding="euc-kr")

bldg_data = read.csv("/Users/kihun/Documents/new_bldg_gdf/final/bldg.csv",fileEncoding="euc-kr")
cl_bldg_surf <- bldg_data %>% group_by(지하철역) %>% summarize(surf = mean(area * GRO_FLO_CO))
write.csv(cl_bldg_surf,"/Users/kihun/Documents/cl_bldg_surf.csv",fileEncoding="euc-kr")

new_bldg_data = read.csv("/Users/kihun/Documents/new_bldg_gdf/final/new_select_bldg.csv",fileEncoding="euc-kr")
rd_bldg_surf <- new_bldg_data %>% group_by(시리얼) %>% summarize(surf = mean(area * GRO_FLO_CO))
write.csv(rd_bldg_surf,"/Users/kihun/Documents/rd_bldg_surf.csv",fileEncoding="euc-kr")

bldg_s_dot_data = read.csv("/Users/kihun/Documents/new_bldg_gdf/final/bldg_s_dot.csv",fileEncoding="euc-kr")
cl_s_dot_bldg_surf <- bldg_s_dot_data %>% group_by(시리얼번호) %>% summarize(surf = mean(area * GRO_FLO_CO))
write.csv(cl_s_dot_bldg_surf,"/Users/kihun/Documents/cl_s_dot_bldg_surf.csv",fileEncoding="euc-kr")

link_s_dot_data = read.csv("/Users/kihun/Documents/new_bldg_gdf/final/link_s_dot.csv",fileEncoding="euc-kr")
cl_s_dot_link_surf <- link_s_dot_data %>% group_by(시리얼번호) %>% summarize(surf = mean(ROAD_BT * ROAD_LT))
write.csv(cl_s_dot_link_surf,"/Users/kihun/Documents/cl_s_dot_link_surf.csv",fileEncoding="euc-kr")
