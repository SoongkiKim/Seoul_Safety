library(dplyr)
library(ggplot2)
library(ggthemes)
library(corrplot)
library(reshape2)

Sys.setlocale("LC_ALL", locale="Korean")

# 전국 범죄 발생 건수


##############################################
# 전국 범죄 발생 추이 2016-2021
kor_criminal <- read.csv("전국범죄발생건수_2016_2021.csv", fileEncoding = 'cp949')

kor_criminal %>% ggplot(aes(x=factor(연도),y=범죄발생건수, group=1))+
  geom_line(col='red', size=1)+geom_point(col='red', size=3)+
  labs(x = "연도",y = "범죄발생빈도",
                    title = "연도별 전국 범죄 발생 건수")+
  theme(plot.title = element_text(hjust = 0.5,size=15,face='bold'))

# 전국 인구 수
kor_pop <- read.csv("전국인구_범죄발생건수.csv", fileEncoding = 'cp949')

kor_pop %>% ggplot(aes(reorder(지역, 총인구), y=총인구, fill=지역))+
  geom_bar(stat = 'identity')+labs(x = "지역",y = "총인구",
                                   title = "지역별 인구수")+
  theme(legend.position = "none")+
  geom_text(aes(지역, 총인구, label =총인구),vjust = 0.7)+
  theme(plot.title = element_text(hjust = 0.5,size=15,face='bold'))+coord_flip()

# 지역별 범죄 발생 건수
kor_pop %>% ggplot(aes(reorder(지역, 범죄발생), y=범죄발생, fill=지역))+
  geom_bar(stat = 'identity')+labs(x = "지역",y = "범죄발생",
                                   title = "지역별 범죄 발생 건수")+
  theme(legend.position = "none")+
  geom_text(aes(지역, 범죄발생, label =범죄발생),vjust = 0.7)+
  theme(plot.title = element_text(hjust = 0.5,size=15,face='bold'))+coord_flip()

# 요일별 범죄발생 2017 - 2021
day_of_cri_2017_2021 <- read.csv("범죄_발생요일_2017_2021.csv", fileEncoding = 'cp949',
                                 stringsAsFactors = T)

day_of_cri_2017_2021$연도 <- as.factor(day_of_cri_2017_2021$연도)

day_of_cri_2017_2021$요일 <- factor(day_of_cri_2017_2021$요일, 
                             levels=c("월요일","화요일","수요일","목요일",
                                      "금요일","토요일","일요일"))

day_of_cri_2017_2021 %>% ggplot(aes(x=요일,
                            y=발생건수,group=연도,color=연도))+
  geom_line()+geom_point(aes(x=요일, y=발생건수,group=연도,color=연도))+
  labs(x = "발생 요일",y = "발생 건수",
      title = "요일별 범죄 발생 건수 2017-2021")+
  theme(plot.title = element_text(hjust = 0.5,size=15,face='bold'))

# 시간별 범죄 발생 2017-2021
time_of_cri_2017_2021 <- read.csv("범죄발생_시간_2017_2021.csv", fileEncoding = 'cp949',
                                 stringsAsFactors = T)

time_of_cri_2017_2021$연도 <- as.factor(time_of_cri_2017_2021$연도)

time_of_cri_2017_2021 %>% ggplot(aes(x=시간대,
                                    y=발생건수,group=연도,color=연도))+
  geom_line()+geom_point(aes(x=시간대, y=발생건수,group=연도,color=연도))+
  labs(x = "발생 시간대",y = "발생 건수",
       title = "시간대별 범죄 발생 건수 2017-2021")+ 
  theme(plot.title = element_text(hjust = 0.5,size=15,face='bold'))

##############################################
# 서울시 범죄 & 행복지수 데이터셋 불러오기.
df <- read.csv("서울_범죄_평균행복지수.csv", fileEncoding = 'cp949')
df2 <- read.csv("서울시_범죄.csv", fileEncoding = 'cp949') 

# 2016 - 2021 서울시 범죄발생 추이
criminal <- df%>% select(연도, 범죄발생건수) %>% 
  melt(id="연도")
criminal <- criminal[,-2]
ggplot(criminal, aes(x=factor(연도), y=value, group=1))+geom_line(col='midnight blue',
                                                                size=1.2)+
  geom_point(col='midnight blue',size=3)+labs(x = "연도",y = "발생 건수",
                          title = "서울시 범죄 발생 건수 2016-2021")+
  theme(plot.title = element_text(hjust = 0.5,size=15,face='bold'))

# 2016 - 2021 서울시 종합행복지수 추이
sat <- df%>% select(연도, 종합행복지수) %>% 
  melt(id="연도")
sat <- sat[,-2]
ggplot(sat, aes(x=factor(연도), y=value, group=1))+geom_line(col='dark violet'
                                                           ,size=1.2)+
  geom_point(col='dark violet', size=3)+labs(x = "연도",y = "종합행복지수",
                          title = "서울시 종합 행복 지수 2016-2021")+ 
  theme(plot.title = element_text(hjust = 0.5,size=15,face='bold'))

# 서울시 범죄
cr_2018 <- read.csv("서울시_5대범죄_2018.csv",fileEncoding = 'cp949')
cr_2019 <- read.csv("서울시_5대범죄_2019.csv",fileEncoding = 'cp949')
cr_2020 <- read.csv("서울시_5대범죄_2020.csv",fileEncoding = 'cp949')
cr_2021 <- read.csv("서울시_5대범죄_2021.csv",fileEncoding = 'cp949')
cr_sums_2018 <- sum(cr_2018$소계)
cr_sums_2019 <- sum(cr_2019$소계)
cr_sums_2020 <- sum(cr_2020$소계)
cr_sums_2021 <- sum(cr_2021$소계)

#서울시 시민행복도
hp_2018 <- read.csv("서울_시민행복지수_2018.csv",fileEncoding = 'cp949')
hp_2019 <- read.csv("서울_시민행복지수_2019.csv",fileEncoding = 'cp949')
hp_2020 <- read.csv("서울_시민행복지수_2020.csv",fileEncoding = 'cp949')
hp_2021 <- read.csv("서울_시민행복지수_2021.csv",fileEncoding = 'cp949')


# 코로나 이전, 이후 범죄
cr_bf_cv <- cr_sums_2018+cr_sums_2019
cr_af_cv <- cr_sums_2020+cr_sums_2021
count_cr <- t(data.frame(c(cr_bf_cv, cr_af_cv)))
rownames(count_cr) <- "count"
colnames(count_cr) <- c("before_covid","after_covid")
bar <- barplot(count_cr, beside=T, ylim=c(0, 230000), col = c("indian red", "steel blue"),
        main="서울시 코로나19 이전/이후 범죄발생건수",
        ylab="범죄 발생 건수")
text(x=bar,y=count_cr,labels=count_cr,pos=3,col="black")

# 코로나 이전, 이후 평균행복지수
sat_18_19 <- mean(sat[,2][1:2])
sat_20_21 <- mean(sat[,2][3:4])
average_sat <- t(data.frame(c(sat_18_19, sat_20_21)))
rownames(average_sat) <- "average"
colnames(average_sat) <- c("sat_before_covid","sat_after_covid")
bar2 <- barplot(average_sat, ylim=c(0, 7.5), beside=T, 
                col = c("pale green", "lavender"),
                main="서울시 코로나19 이전/이후 시민 행복 지수",
                ylab="시민 행복 지수")
text(x=bar1,y=average_sat,labels=average_sat,pos=3,col="black")



# 코로나 이전 서울 구별 범죄발생건수
cr_18_19 <- data.frame(cr_2018$소계+cr_2019$소계)
colnames(cr_18_19)[1] <-'코로나이전범죄건수' 
cr_2018 <- cbind(cr_2018,cr_18_19)

ggplot(cr_2018, aes(reorder(자치구, 코로나이전범죄건수), y=코로나이전범죄건수, 
                    fill=자치구))+geom_bar(stat = 'identity')+
  labs(x = "연도",  title = "코로나 이전 서울시 범죄 발생 건수")+
 coord_flip()+theme(legend.position = "none")+
  geom_text(aes(자치구, 코로나이전범죄건수, label =코로나이전범죄건수), 
            vjust = 0.7)+ theme(plot.title = element_text(hjust = 0.5,size=15,
                                                           face='bold'))
 

# 코로나 이후 서울 구별 범죄발생건수
cr_20_21 <- data.frame(cr_2020$소계+cr_2021$소계)
colnames(cr_20_21)[1] <-'코로나이후범죄건수' 
cr_2021 <- cbind(cr_2021,cr_20_21)
ggplot(cr_2021, aes(reorder(자치구, 코로나이후범죄건수), y=코로나이후범죄건수, 
                    fill=자치구))+geom_bar(stat = 'identity')+
  labs(x='자치구', title='서울시 코로나 이후 범죄 발생 건수')+
  coord_flip()+
  theme(legend.position = "none")+
  geom_text(aes(자치구, 코로나이후범죄건수, label =코로나이후범죄건수),vjust = 0.7)+
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = 'bold'))

# 코로나 이전 서울 구별 시민행복지수
hp_18_19 <- data.frame((hp_2018$시민행복지수+hp_2019$시민행복지수)/2)
colnames(hp_18_19)[1] <-'코로나이전시민행복지수' 
hp_2018 <- cbind(hp_2018,hp_18_19)
ggplot(hp_2018, aes(reorder(자치구, 코로나이전시민행복지수), y=코로나이전시민행복지수, 
                    fill=자치구))+geom_bar(stat = 'identity')+
  labs(x='자치구', title='코로나 이전 시민 행복 지수')+coord_flip()+
  theme(legend.position = "none")+geom_text(aes(자치구, 코로나이전시민행복지수, 
                                                label =코로나이전시민행복지수),
                                            vjust = 0.7)+
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = 'bold'))

# 코로나 이후 서울 구별 시민행복지수
hp_20_21 <- data.frame((hp_2020$시민행복지수+hp_2021$시민행복지수)/2)
colnames(hp_20_21)[1] <-'코로나이후시민행복지수' 
hp_2021 <- cbind(hp_2021,hp_20_21)
ggplot(hp_2021, aes(reorder(자치구, 코로나이후시민행복지수), y=코로나이후시민행복지수, 
                    fill=자치구))+geom_bar(stat = 'identity')+
  labs(x='자치구', title='코로나 이후 시민 행복 지수')+coord_flip()+
  theme(legend.position = "none")+geom_text(aes(자치구, 코로나이후시민행복지수, 
                                                 label =코로나이후시민행복지수),
                                             vjust = 0.7)+
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = 'bold'))

# 서울시 인구&범죄
g1 <- ggplot(df2,aes(x=인구수, y=범죄발생건수 ))+
  geom_point(aes(color=자치구))+geom_smooth()+geom_text(aes(label=자치구), 
                                                     vjust=-0.2)+
  theme_bw()+theme(legend.position = "none")+
  ggtitle('인구 수 & 범죄 발생 건수')+
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = 'bold'))


# 범죄 & 평균연령
g2 <- ggplot(df2,aes(x=평균연령, y=범죄발생건수 ))+
  geom_point(aes(color=자치구))+geom_smooth()+
  geom_text(aes(label=자치구), vjust=-0.2)+
  theme(legend.background = element_rect(size=1))+
  theme_bw()+theme(legend.position = "none")+
  ggtitle('평균연령 & 범죄 발생 건수')+
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = 'bold'))


# 범죄 & CCTV 수
g3 <- ggplot(df2,aes(x=CCTV수, y=범죄발생건수 ))+
  geom_point(aes(color=자치구))+geom_smooth()+
  geom_text(aes(label=자치구), vjust=-0.2)+
  theme_bw()+theme(legend.position = "none")+
  ggtitle('CCTV 수 & 범죄 발생 건수')+
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = 'bold'))


# 범죄 & 유흥업소 수
g4 <- ggplot(df2,aes(x=유흥업소수, y=범죄발생건수 ))+
  geom_point(aes(color=자치구))+geom_smooth()+
  geom_text(aes(label=자치구), vjust=-0.2)+
  theme_bw()+theme(legend.position = "none")+
  ggtitle('유흥업소 수 & 범죄 발생 건수')+
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = 'bold'))

# 인구. 평균연령, CCTV 수, 유흥업소 수
library(gridExtra)

grid.arrange(g1,g2,g3,g4, nrow=2, ncol=2)

# 시민행복지수 합치기
hp_cri <- hp_2021 %>% select(자치구, 시민행복지수)
df2 <- merge(df2,hp_cri, by='자치구')

# 범죄 & 시민행복지수
h1 <- df2 %>% ggplot(aes(x=범죄발생건수, y=시민행복지수 ))+
  geom_point(aes(color=자치구))+geom_smooth()+
  geom_text(aes(label=자치구), vjust=-1)+
  theme_bw()+theme(legend.position = "none")+
  ggtitle('시민 행복 지수 & 범죄 발생 건수')+
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = 'bold'))

# 살인 & 시민행복지수
h2 <- df2 %>% ggplot(aes(x=살인, y=시민행복지수 ))+
  geom_point(aes(color=자치구))+geom_smooth()+
  geom_text(aes(label=자치구), vjust=-1)+ theme_bw()+theme(legend.position = "none")+
  ggtitle('시민 행복 지수 & 살인')+
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = 'bold'))

# 강도 & 시민행복지수
h3 <- df2 %>% ggplot(aes(x=강도, y=시민행복지수 ))+
  geom_point(aes(color=자치구))+geom_smooth()+
  geom_text(aes(label=자치구), vjust=-1)+ theme_bw()+theme(legend.position = "none")+
  ggtitle('시민 행복 지수 & 강도')+
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = 'bold'))

# 강간.강제추행 & 시민행복지수
h4 <- df2 %>% ggplot(aes(x=강간.강제추행, y=시민행복지수 ))+
  geom_point(aes(color=자치구))+geom_smooth()+
  geom_text(aes(label=자치구), vjust=-1)+ theme_bw()+theme(legend.position = "none")+
  ggtitle('시민 행복 지수 & 강간.강제추행')+
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = 'bold'))

# 절도 & 시민행복지수
h5 <- df2 %>% ggplot(aes(x=절도, y=시민행복지수 ))+
  geom_point(aes(color=자치구))+geom_smooth()+
  geom_text(aes(label=자치구), vjust=-1)+theme_bw()+theme(legend.position = "none")+
  ggtitle('시민 행복 지수 & 절도')+
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = 'bold'))

# 폭행 & 시민행복지수
h6 <- df2 %>% ggplot(aes(x=폭력, y=시민행복지수 ))+
  geom_point(aes(color=자치구))+geom_smooth()+
  geom_text(aes(label=자치구), vjust=-1)+theme_bw()+theme(legend.position = "none")+
  ggtitle('시민 행복 지수 & 폭행')+
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = 'bold'))

# 서울 시민 행복 지수 & 범죄
grid.arrange(h1, h2, h3, h4,h5, h6, nrow=3, ncol=2)


# 강남구 범죄유형별 빈도수 비교
gangnam <- df2 %>% filter(자치구 == '강남구') %>% select(c(자치구, 살인,
                                                     강도, 강간.강제추행, 절도,
                                                     폭력)) %>% melt('자치구')
ggplot(gangnam, aes(x=reorder(variable, value), y=value, fill=variable))+
  geom_bar(stat='identity')+geom_text(aes(label=value), vjust=-1)+
  theme(legend.position = "none")+
  labs(x='범죄유형', y='빈도수', title='강남구 범죄유형별 빈도수')+
  theme(plot.title = element_text(hjust = 0.5, size=15, face='bold'))

# 서울시 구별 유흥업소 수 
df2 %>% select(c(자치구, 유흥업소수)) %>% melt('자치구') %>% 
  ggplot(aes(reorder(x=자치구, value), y=value, fill=자치구))+
  geom_bar(stat = 'identity')+geom_text(aes(label=value), vjust=0.5)+
  theme_bw()+theme(legend.position = "none")+
  labs(x='자치구', y='유흥업소 수', title='서울시 구별 유흥업소 수')+
  theme(plot.title = element_text(hjust =0.5, size=15, face='bold'))+
  coord_flip()

# 요일별 폭력범죄
violenceOfDay <- read.csv("요일별_폭력범죄.csv", fileEncoding = 'cp949',
                           stringsAsFactors = T)

violenceOfDay$발생요일 <- factor(violenceOfDay$발생요일, 
                             levels=c("월요일","화요일","수요일","목요일",
                                      "금요일","토요일","일요일"))

violenceOfDay %>% ggplot(aes(x=발생요일, y=발생건수, group=1))+
  geom_line(size=1, col='orange red')+geom_point(size=3, col='orange red')+
  labs(x = "범죄 발생 요일",y = "폭력범죄 발생건수",
                                      title = "요일별 폭력범죄 발생건수")+ 
  theme(plot.title = element_text(hjust = 0.5,size=15,face='bold'))


# 시간대별 폭력범죄 발생 
violenceOfTime <- read.csv("시간대별_폭력범죄.csv", fileEncoding = 'cp949',
                           stringsAsFactors = T)

violenceOfTime %>% ggplot(aes(x=factor(범죄발생시간), y=발생건수, group=1))+
  geom_line(size=1, col='purple')+geom_point(size=3, col='purple')+
  labs(x = "범죄발생시간",y = "폭력범죄 발생건수",
                          title = "시간대별 폭력범죄 발생건수")+ 
  theme(plot.title = element_text(hjust = 0.5,size=15,face='bold'))

# 강남구 시간대별 유동인구
popPerTime <- read.csv("강남구_시간대별_유동인구.csv", fileEncoding = 'cp949',
                       stringsAsFactors = T)

popPerTime %>% ggplot(aes(x=시간대, y=유동인구수, group=1))+
  geom_line(col='dark green', size=1)+geom_point(col='dark green', size=3)+
  labs(x = "시간대",y = "유동인구 수",
                                      title = "강남구 시간대별 유동인구 수")+ 
  theme(plot.title = element_text(hjust = 0.5,size=15,face='bold'))
