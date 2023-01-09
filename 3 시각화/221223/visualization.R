###### brfss ###### 
library(ggplot2)
library(gridExtra)
library(dplyr)
library(rlang)

brfss <- read.csv("./data/brfss.csv", header=T)

for (n in names(brfss)[-3:-4]) {
  brfss[,n] <- factor(brfss[,n])
}
str(brfss)

plot_brfss <- function(xcol) {
  brfss %>% 
    group_by(!!rlang::sym(xcol), HEARTDISEASE) %>%
    summarise(count=n()) %>%
    mutate(prop = round(count/sum(count), 3)) %>%
    ggplot(aes(x=!!rlang::sym(xcol), y=prop, fill=HEARTDISEASE)) + 
    geom_bar(stat="identity", show.legend=F) +
    geom_text(aes(label=paste(prop*100, "%")), size=3, position=position_stack(.5)) +
    theme(axis.title.y=element_blank()) -> p
  
  if (xcol == "ALCOHOL") {
    return(p + scale_x_discrete(labels=c("0"="no", "1"="yes", "9"="unknown")))
  } else {
    return(p + scale_x_discrete(labels=c("0"="no", "1"="yes")))
  }
}

b1 <- plot_brfss("DEAF")
b2 <- plot_brfss("BLIND")
b3 <- plot_brfss("DIABETE")
b4 <- plot_brfss("HYPERTENSION")
b5 <- plot_brfss("KIDNEY")
b6 <- plot_brfss("ALCOHOL")
b7 <- plot_brfss("SMOKE")

grid.arrange(b1, b2, b3, b4, b5, b6, b7, ncol=3, nrow=3)
###############################################


##### brfss + checkup(건강검진) #####
library(ggplot2)
library(gridExtra)
library(dplyr)
library(rlang)

# add brfss and checkup
brfss <- read.csv("./data/brfss.csv", header=T)
brfss <- brfss[-3:-4]
brfss <- brfss[-1]
brfss$type <- "brfss"
for (n in names(brfss)) {
  brfss[,n] <- factor(brfss[,n])
}
checkup <- read.csv("./data/checkup.csv", header=T)
checkup <- checkup[-3:-4]
checkup <- checkup[-1]
checkup$type <- "checkup"
for (n in names(checkup)) {
  checkup[,n] <- factor(checkup[,n])
}
health <- rbind(brfss, checkup)

# function for plot
plot_brfss_checkup <- function(fillcol) {
  health %>%
    group_by(type, !!rlang::sym(fillcol)) %>%
    summarise(count=n()) %>%
    mutate(prop=round(count/sum(count), 3)) %>%
    ggplot(aes(x=type, y=prop, fill=!!rlang::sym(fillcol))) +
    geom_bar(stat="identity") +
    geom_text(aes(label=paste(prop*100, "%")), size=3, position=position_stack(.5)) +
    theme(axis.title.y=element_blank(), axis.title.x=element_blank()) -> p
  
  if (fillcol == "ALCOHOL") {
    return(p + scale_fill_discrete(name=fillcol, labels=c("yes", "no", "unknown")))
  } else {
    return(p + scale_fill_discrete(name=fillcol, labels=c("yes", "no", "unknown")))
  }
}

# plot
h1 <- plot_brfss_checkup("DEAF")
h2 <- plot_brfss_checkup("BLIND")
h3 <- plot_brfss_checkup("DIABETE")
h4 <- plot_brfss_checkup("HYPERTENSION")
h5 <- plot_brfss_checkup("KIDNEY")
h6 <- plot_brfss_checkup("ALCOHOL")
h7 <- plot_brfss_checkup("SMOKE")

grid.arrange(h1, h2, h3, h4, h5, h6, h7, ncol=3, nrow=3)
###############################################


##### region 그대로 #####
library(ggplot2)
library(dplyr)
library(gridExtra)
library(tidyr)
library(rlang)

region_v <- read.csv("./data/region_v.csv", header=T)
region_v$지역 <- factor(region_v$지역)
str(region_v)

names(region_v)

plot_region <- function(startcol, endcol) {
  region_v %>%
    gather(key="kind", value="count", !!rlang::sym(startcol):!!rlang::sym(endcol), factor_key=T) %>%
    ggplot(aes(x=지역, y=count, fill=kind)) +
    geom_bar(position=position_dodge(0.5), stat="identity", size=.2, width=.4) +
    theme(axis.title.y=element_blank(), axis.title.x=element_blank(), legend.title=element_blank())
}

r <- plot_region("심장질환사망자", "구급대원")
r1 <- plot_region("심장질환사망자", "심장장애")
r2 <- plot_region("상급종합병원", "심장혈관흉부외과의원")
r3 <- plot_region("일반의", "심장혈관흉부외과전문의")
r4 <- plot_region("입원병상", "중환병실")
r5 <- plot_region("구급차", "구급대원")

grid.arrange(r1, r2, r3, r4, r5, ncol=2, nrow=3)
###############################################


##### region 인구 비례 #####
library(ggplot2)
library(dplyr)
library(gridExtra)
library(tidyr)
library(rlang)

region_v <- read.csv("./data/region_v.csv", header=T)
region_v$지역 <- factor(region_v$지역)
str(region_v)

# 주민등록인구로 나누기
region_v$주민등록인구 <- region_v$주민등록인구 / 100000

for (n in names(region_v)[3:19]) {
  region_v[,n] <- region_v[,n]/region_v$주민등록인구
}

r <- plot_region("심장질환사망자", "구급대원")
r1 <- plot_region("심장질환사망자", "심장장애")
r2 <- plot_region("상급종합병원", "심장혈관흉부외과의원")
r3 <- plot_region("일반의", "심장혈관흉부외과전문의")
r4 <- plot_region("입원병상", "중환병실")
r5 <- plot_region("구급차", "구급대원")

grid.arrange(r1, r2, r3, r4, r5, ncol=2, nrow=3)
###############################################


##### region 심장질환사망자 비례 #####
library(ggplot2)
library(dplyr)
library(gridExtra)
library(tidyr)
library(rlang)

region_v <- read.csv("./data/region_v.csv", header=T)
region_v$지역 <- factor(region_v$지역)
str(region_v)

# 심장질환사망자로 나누기
for (n in names(region_v)[5:19]) {
  region_v[,n] <- region_v[,n] / region_v$심장질환사망자
}

# r <- plot_region("심장질환사망자", "구급대원")
# r1 <- plot_region("심장질환사망자", "심장장애")
r2 <- plot_region("상급종합병원", "심장혈관흉부외과의원")
r3 <- plot_region("일반의", "심장혈관흉부외과전문의")
r4 <- plot_region("입원병상", "중환병실")
r5 <- plot_region("구급차", "구급대원")

grid.arrange(r2, r3, r4, r5, ncol=2, nrow=2)
###############################################


##### region 심장질환사망자 비례 #####
library(ggplot2)
library(dplyr)
library(gridExtra)
library(tidyr)
library(rlang)

region_v <- read.csv("./data/region_v.csv", header=T)
region_v$지역 <- factor(region_v$지역)
str(region_v)

# 심장장애로 나누기
for (n in names(region_v)[5:19]) {
  region_v[,n] <- region_v[,n] / region_v$심장장애
}

# r <- plot_region("심장질환사망자", "구급대원")
# r1 <- plot_region("심장질환사망자", "심장장애")
r2 <- plot_region("상급종합병원", "심장혈관흉부외과의원")
r3 <- plot_region("일반의", "심장혈관흉부외과전문의")
r4 <- plot_region("입원병상", "중환병실")
r5 <- plot_region("구급차", "구급대원")

grid.arrange(r2, r3, r4, r5, ncol=2, nrow=2)
###############################################
