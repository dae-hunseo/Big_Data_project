#################### index ####################
# 1. brfss
# 2. brfss vs. checkup
# 3. region
## 3-1. region raw
## 3-2. region / 주민등록인구
## 3-3. region / 심장질환사망자
## 3-4. region / 심장장애
# 4. 심장질환위험군 vs. heart
## 4-1. 심장질환위험군 vs. 심장질환사망자 vs. 심장장애
## 4-2. 심장질환위험군(인구대비) vs. 심장질환사망자 vs. 심장장애
# 5. region / 심장질환위험군
## 5-1. region / 심장질환위험군
## 5-2. region / 심장질환위험군(인구대비)

###### 1. brfss ###### 
library(ggplot2)
library(gridExtra)
library(dplyr)
library(rlang)

plot_brfss <- function(xcol) {
  brfss %>% 
    group_by(!!rlang::sym(xcol), HEARTDISEASE) %>%
    summarise(count=n()) %>%
    mutate(prop = round(count/sum(count), 3)) %>%
    ggplot(aes(x=!!rlang::sym(xcol), y=prop, fill=HEARTDISEASE)) + 
    geom_bar(stat="identity", show.legend=F) +
    geom_text(aes(label=paste(prop*100, "%")), size=2, position=position_stack(.5)) +
    scale_x_discrete(labels=c("0"="no", "1"="yes")) +
    theme(axis.title.y=element_blank(), 
          axis.title.x=element_text(size=8),
          axis.text.y=element_text(size=6),
          axis.text.x=element_text(size=6)) -> p
  
  return(p)
}

brfss <- read.csv("./data/brfss.csv", header=T)

for (n in names(brfss)) {
  brfss[,n] <- factor(brfss[,n])
}
str(brfss)

b1 <- plot_brfss("DEAF")
b2 <- plot_brfss("BLIND")
b3 <- plot_brfss("DIABETE")
b4 <- plot_brfss("HYPERTENSION")
b5 <- plot_brfss("KIDNEY")
b6 <- plot_brfss("SMOKE")

b <- grid.arrange(b1, b2, b3, b4, b5, b6, ncol=3, nrow=2)
ggsave(filename="./2 visualize/brfss.png", plot=b, 
       width=6, height=4, units="in", dpi=500)



##### 2. brfss vs. checkup(건강검진) #####
library(ggplot2)
library(gridExtra)
library(dplyr)
library(rlang)

plot_brfss_checkup <- function(fillcol) {
  health %>%
    group_by(type, !!rlang::sym(fillcol)) %>%
    summarise(count=n()) %>%
    mutate(prop=round(count/sum(count), 3)) %>%
    ggplot(aes(x=type, y=prop, fill=!!rlang::sym(fillcol))) +
    geom_bar(stat="identity") +
    geom_text(aes(label=paste(prop*100, "%")), size=2, position=position_stack(.5)) +
    scale_fill_discrete(name=fillcol, labels=c("yes", "no")) +
    theme(axis.title.y=element_blank(), 
          axis.title.x=element_blank(),
          axis.text.x=element_text(size=6),
          axis.text.y=element_text(size=6),
          legend.title=element_text(size=4),
          legend.text=element_text(size=6)) -> p
  
  return(p)
}

brfss <- read.csv("./data/brfss.csv", header=T)
brfss <- brfss[-1]
brfss$type <- "brfss"
for (n in names(brfss)) {
  brfss[,n] <- factor(brfss[,n])
}
checkup <- read.csv("./data/checkup_221227.csv", header=T)
checkup <- checkup[-1]
checkup$type <- "checkup"
for (n in names(checkup)) {
  checkup[,n] <- factor(checkup[,n])
}
health <- rbind(brfss, checkup)

h1 <- plot_brfss_checkup("DEAF")
h2 <- plot_brfss_checkup("BLIND")
h3 <- plot_brfss_checkup("DIABETE")
h4 <- plot_brfss_checkup("HYPERTENSION")
h5 <- plot_brfss_checkup("KIDNEY")
h6 <- plot_brfss_checkup("SMOKE")

h <- grid.arrange(h1, h2, h3, h4, h5, h6, ncol=3, nrow=2)
ggsave(filename="./2 visualize/brfss vs checkup.png", plot=h, 
       width=6, height=4, units="in", dpi=500)



##### 3. region #####
library(ggplot2)
library(dplyr)
library(gridExtra)
library(tidyr)
library(rlang)
library(stringr)

plot_region <- function(startcol, endcol) {
  region_v %>%
    gather(key="kind", value="count", !!rlang::sym(startcol):!!rlang::sym(endcol), factor_key=T) %>%
    ggplot(aes(x=지역, y=count, fill=str_wrap(kind, 8))) +
    geom_bar(position=position_dodge(0.5), stat="identity", size=.2, width=.4) +
    theme(axis.title.y=element_blank(), 
          axis.title.x=element_blank(), 
          axis.text=element_text(size=6), 
          legend.title=element_blank(),
          legend.text=element_text(size=6)) -> p
  return(p)
}



##### 3-2. region raw #####
region_v <- read.csv("./data/region_v.csv", header=T)
region_v$지역 <- factor(region_v$지역)
str(region_v)

r1 <- plot_region("심장질환사망자", "심장장애")
r2 <- plot_region("상급종합병원", "심장혈관흉부외과의원")
r3 <- plot_region("일반의", "심장혈관흉부외과전문의")
r4 <- plot_region("입원병상", "중환병실")
r5 <- plot_region("구급차", "구급대원")

grid1 <- grid.arrange(r1, r2, r3, r4, r5, ncol=2, nrow=3)
ggsave(filename="./2 visualize/region_raw.png", plot=grid1, 
       width=12, height=4, units="in", dpi=500)
grid2 <- grid.arrange(r2, r3, r4, r5, ncol=2, nrow=2)
ggsave(filename="./2 visualize/region_raw_medical.png", plot=grid2, 
       width=12, height=4, units="in", dpi=500)
ggsave(filename="./2 visualize/region_raw_heart.png", plot=r1,
       width=6, height=4, units="in", dpi=500)



##### 3-2. region / 주민등록인구 #####
region_v <- read.csv("./data/region_v.csv", header=T)
region_v$지역 <- factor(region_v$지역)
str(region_v)

region_v$주민등록인구 <- region_v$주민등록인구 / 100000
for (n in names(region_v)[3:19]) {
  region_v[,n] <- region_v[,n]/region_v$주민등록인구
}

r1 <- plot_region("심장질환사망자", "심장장애")
r2 <- plot_region("상급종합병원", "심장혈관흉부외과의원")
r3 <- plot_region("일반의", "심장혈관흉부외과전문의")
r4 <- plot_region("입원병상", "중환병실")
r5 <- plot_region("구급차", "구급대원")

grid1 <- grid.arrange(r1, r2, r3, r4, r5, ncol=2, nrow=3)
ggsave(filename="./2 visualize/region_per_pop.png", plot=grid1, 
       width=12, height=4, units="in", dpi=500)
grid2 <- grid.arrange(r2, r3, r4, r5, ncol=2, nrow=2)
ggsave(filename="./2 visualize/region_per_pop_medical.png", plot=grid2, 
       width=12, height=4, units="in", dpi=500)
ggsave(filename="./2 visualize/region_per_pop_heart.png", plot=r1,
       width=6, height=4, units="in", dpi=500)



##### 3-3. region / 심장질환사망자 #####
region_v <- read.csv("./data/region_v.csv", header=T)
region_v$지역 <- factor(region_v$지역)
str(region_v)

for (n in names(region_v)[5:19]) {
  region_v[,n] <- region_v[,n] / region_v$심장질환사망자
}
r2 <- plot_region("상급종합병원", "심장혈관흉부외과의원")
r3 <- plot_region("일반의", "심장혈관흉부외과전문의")
r4 <- plot_region("입원병상", "중환병실")
r5 <- plot_region("구급차", "구급대원")

grid2 <- grid.arrange(r2, r3, r4, r5, ncol=2, nrow=2)
ggsave(filename="./2 visualize/region_per_death_medical.png", plot=grid2, 
       width=12, height=4, units="in", dpi=500)



##### 3-4. region / 심장장애 #####
region_v <- read.csv("./data/region_v.csv", header=T)
region_v$지역 <- factor(region_v$지역)
str(region_v)

# 심장장애로 나누기
for (n in names(region_v)[5:19]) {
  region_v[,n] <- region_v[,n] / region_v$심장장애
}

r2 <- plot_region("상급종합병원", "심장혈관흉부외과의원")
r3 <- plot_region("일반의", "심장혈관흉부외과전문의")
r4 <- plot_region("입원병상", "중환병실")
r5 <- plot_region("구급차", "구급대원")

grid2 <- grid.arrange(r2, r3, r4, r5, ncol=2, nrow=2)
ggsave(filename="./2 visualize/region_per_disability_medical.png", plot=grid2, 
       width=12, height=4, units="in", dpi=500)



##### 4. 심장질환위험군 vs. heart #####
library(ggplot2)
library(dplyr)
library(gridExtra)
library(tidyr)
library(rlang)
library(stringr)

plot_risk_heart <- function(startcol, endcol) {
  all_v %>%
    gather(key="kind", value="count", !!rlang::sym(startcol):!!rlang::sym(endcol), factor_key=T) %>%
    ggplot(aes(x=지역, y=count, color=str_wrap(kind, 8), group=str_wrap(kind, 8))) +
    geom_line() +
    theme(axis.title.y=element_blank(), 
          axis.title.x=element_blank(), 
          axis.text.y=element_blank(), 
          axis.ticks.y=element_blank(),
          axis.text.x=element_text(size=6), 
          legend.title=element_blank(),
          legend.text=element_text(size=6)) -> p
  return(p)
}



##### 4-1. 심장질환위험군 vs. 심장질환사망자 vs. 심장장애 #####
all_v <- read.csv("./data/region_with_risk.csv", header=T)
all_v$지역 <- factor(all_v$지역)
all_v <- all_v[, c("지역", "심장질환위험군", "심장질환사망자", "심장장애")]
str(all_v)

all_v[2:4] <- sapply(all_v[2:4], function(x) {
  return((x - min(x)) / (max(x) - min(x)))
})

r1 <- plot_risk_heart("심장질환위험군", "심장장애")
ggsave(filename="./2 visualize/risk vs heart.png", plot=r1,
       width=6, height=4, units="in", dpi=500)



##### 4-2. 심장질환위험군(인구대비) vs. heart #####
all_v <- read.csv("./data/region_with_risk.csv", header=T)
all_v$지역 <- factor(all_v$지역)
for (n in c("심장질환위험군", "심장질환사망자", "심장장애")) {
  all_v[,n] <- all_v[,n] / all_v$주민등록인구.10만.
}
all_v <- all_v[, c("지역", "심장질환위험군", "심장질환사망자", "심장장애")]
all_v[2:4] <- sapply(all_v[2:4], function(x) {
  return((x - min(x)) / (max(x) - min(x)))
})
str(all_v)

r1 <- plot_risk_heart("심장질환위험군", "심장장애")
ggsave(filename="./2 visualize/risk_per_pop vs heart.png", plot=r1,
       width=6, height=4, units="in", dpi=500)



##### 5. 심장질환위험군 vs. region #####
library(ggplot2)
library(dplyr)
library(gridExtra)
library(tidyr)
library(rlang)
library(stringr)

plot_risk_region <- function(startcol, endcol) {
  all_v %>%
    gather(key="kind", value="count", !!rlang::sym(startcol):!!rlang::sym(endcol), factor_key=T) %>%
    ggplot(aes(x=지역, y=count, fill=str_wrap(kind, 8))) +
    geom_bar(position=position_dodge(0.5), stat="identity", size=.2, width=.4) +
    theme(axis.title.y=element_blank(), 
          axis.title.x=element_blank(), 
          axis.text=element_text(size=6), 
          legend.title=element_blank(),
          legend.text=element_text(size=6)) -> p
  return(p)
}



##### 5-1. region / 심장질환위험군 #####
all_v <- read.csv("./data/region_with_risk.csv", header=T)
all_v$지역 <- factor(all_v$지역)
str(all_v)

for (n in names(all_v)[4:20]) {
  all_v[,n] <- all_v[,n] / all_v$심장질환위험군
}

r1 <- plot_risk_region("심장질환사망자", "심장장애")
r2 <- plot_risk_region("상급종합병원", "심장혈관흉부외과의원")
r3 <- plot_risk_region("일반의", "심장혈관흉부외과전문의")
r4 <- plot_risk_region("입원병상", "중환병실")
r5 <- plot_risk_region("구급차", "구급대원")

grid1 <- grid.arrange(r1, r2, r3, r4, r5, ncol=2, nrow=3)
ggsave(filename="./2 visualize/region_per_risk.png", plot=grid1, 
       width=12, height=4, units="in", dpi=500)
grid2 <- grid.arrange(r2, r3, r4, r5, ncol=2, nrow=2)
ggsave(filename="./2 visualize/region_per_risk_medical.png", plot=grid2, 
       width=12, height=4, units="in", dpi=500)
ggsave(filename="./2 visualize/region_per_risk_heart.png", plot=r1,
       width=6, height=4, units="in", dpi=500)



##### 5-2. region / 심장질환위험군(인구대비) #####
all_v <- read.csv("./data/region_with_risk.csv", header=T)
all_v$지역 <- factor(all_v$지역)
str(all_v)

for (n in names(all_v)[4:20]) {
  all_v[,n] <- all_v[,n] / all_v$심장질환위험군.인구대비.
}

r1 <- plot_risk_region("심장질환사망자", "심장장애")
r2 <- plot_risk_region("상급종합병원", "심장혈관흉부외과의원")
r3 <- plot_risk_region("일반의", "심장혈관흉부외과전문의")
r4 <- plot_risk_region("입원병상", "중환병실")
r5 <- plot_risk_region("구급차", "구급대원")

grid1 <- grid.arrange(r1, r2, r3, r4, r5, ncol=2, nrow=3)
ggsave(filename="./2 visualize/region_per_risk_per_pop.png", plot=grid1, 
       width=12, height=4, units="in", dpi=500)
grid2 <- grid.arrange(r2, r3, r4, r5, ncol=2, nrow=2)
ggsave(filename="./2 visualize/region_per_risk_per_pop_medical.png", plot=grid2, 
       width=12, height=4, units="in", dpi=500)
ggsave(filename="./2 visualize/region_per_risk_per_pop_heart.png", plot=r1,
       width=6, height=4, units="in", dpi=500)

