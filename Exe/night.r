# 
#  LT for TokyoR
#                    
#                                                          2018.4.27 新規
#-----------------------------------------------------------------------------------

rm(list=ls())

# > sessionInfo()
# R version 3.4.1 (2017-06-30)
# Platform: x86_64-apple-darwin15.6.0 (64-bit)
# Running under: macOS High Sierra 10.13.4

#---- install packages ----

# https://github.com/Rdatatable/data.table/issues/2409
# install.packages("data.table",type='binary')
# install.packages("tidyverse",dependencies = T)
# install.packages("rstan",dependencies = T)
# install.packages("shinystan",dependencies = T)
# install.packages("viridis")

#---- load pakages ----

library("dplyr")
library("ggplot2")
library("ggridges")
library("viridis")
library("data.table")
library("stringr")
require("shinystan")
library("rstan")

#---- read pnt data ----

pnt <- c("47615_utunomiya.tsv", "47624_maebashi.tsv",
         "47629_mito.tsv","47638_koufu.tsv",
         "47662_tokyo.tsv","47670_yokohama.tsv")

df <- data.frame()
for(ipnt in pnt){
  df_tmp <- fread(paste0("../Data/IN/",ipnt))
  df <- rbind(df,df_tmp)
}

colnames(df) <- c("date","sfc_no","temp","temp_rm",
                  "cla","cla_rm","rain","rain_rm") 

ymdh <- df$date %>%
  str_replace(pattern="t", replacement="-") %>%
  str_split(pattern = "-",simplify=TRUE) %>%
  as.data.frame()

ymdh$V4 <- ymdh$V4 %>%
  str_replace(pattern="00", replacement="") 

ymdh <- ymdh %>%
  mutate_at(vars(V1:V4),as.character) %>%
  mutate_at(vars(V1:V4),as.integer)

df_ymdh <- df %>%
  bind_cols(ymdh) 

colnames(df_ymdh) <- c("date","sfc_no","temp","temp_rm","cla","cla_rm",
                       "rain","rain_rm","yyyy","mm","dd","hh")

#---- read sonde data ----

df_s <- fread("../Data/IN/sonde08-17.tsv")

ymdh <- df_s$`TOSHI-TSUKI-NITI-JI` %>%
  str_replace(pattern="t", replacement="-") %>%
  str_split(pattern = "-",simplify=TRUE) %>%
  as.data.frame()

ymdh <- ymdh %>%
  mutate_at(vars(V1:V4),as.character) %>%
  mutate_at(vars(V1:V4),as.integer)

df_s_ymdh <- df_s %>%
  bind_cols(ymdh)

colnames(df_s_ymdh) <- c("date","upp_no","spl","alt","temp","hum",
                       "wdr","wsp","pastm","yyyy","mm","dd","hh")

#---- make temp diff for target----

tdif_set <- df_ymdh %>%
  arrange(sfc_no,yyyy,mm,dd,hh) %>%
  filter(hh %in% c(21,6)) %>%
  select(yyyy,mm,dd,hh,sfc_no,temp,temp_rm) %>%
  mutate(temp_lag = lag(temp), temp_rm_lag = lag(temp_rm)) %>%
  mutate(temp_dif = temp - temp_lag) %>%
  filter(hh == 6 & temp_rm==8 & temp_rm_lag==8) %>%
  select(yyyy,mm,dd,hh,sfc_no,temp_dif)

#---- make hare set from cla ----

hare_set <- df_ymdh %>%
  arrange(sfc_no,yyyy,mm,dd,hh) %>%
  filter(hh %in% c(3,21)) %>%
  select(yyyy,mm,dd,hh,sfc_no,cla) %>% 
  mutate(hare=if_else(cla<=8,1,0)) %>% 
  mutate(hare_lag = lag(hare)) %>%
  filter(hh==3) %>%
  mutate(flg=if_else(hare==hare_lag,1,0)) %>% 
  filter(flg==1) %>% 
  select(yyyy,mm,dd,hh,sfc_no,hare)
  
#---- make rainy days ----

rain_set <- df_ymdh %>%
  arrange(sfc_no,yyyy,mm,dd,hh) %>%
  filter(hh %in% c(1:6,21:24)) %>%
  select(yyyy,mm,dd,hh,sfc_no,rain,rain_rm) %>% 
  mutate(pm_flg=if_else(hh %in% 21:24,1,0)) %>%
  group_by(sfc_no,yyyy,mm,dd,pm_flg) %>% 
  summarise(rain_max=max(rain),rain_max_rm=min(rain_rm)) %>%
  ungroup() %>% 
  mutate(rmax_lag=lag(rain_max),rmax_rm_lag=lag(rain_max_rm)) %>% 
  filter(pm_flg==0 & rain_max==0 & rmax_lag==0 &
           rain_max_rm>=6 & rmax_rm_lag>=6)

#---- make temp advection at 925 hPa ----

sonde_set <- df_s_ymdh %>% 
  filter(spl==9250) %>% 
  select(yyyy:hh,spl,temp) %>%
  mutate(temp_lag = lag(temp)) %>%
  mutate_at(vars(temp,temp_lag),as.integer) %>%
  arrange(yyyy,mm,dd,hh) %>%
  mutate(tadv = temp - temp_lag) %>%
  filter(!is.na(temp) & !is.na(temp_lag)) %>%
  filter(hh == 9) %>%
  select(yyyy:hh,tadv) 

#---- make mean wind at 850 hPa ----

wind_set <- df_s_ymdh %>% 
  filter(spl==8500) %>% 
  select(yyyy:hh,spl,wdr,wsp) %>%
  mutate_at(vars(wdr,wsp),as.numeric) %>%
  mutate(arg=wdr*pi/180) %>%
  mutate(n_wind=wsp*cos(arg),e_wind=wsp*sin(arg)) %>%
  mutate(n_wind_lag=lag(n_wind),e_wind_lag=lag(e_wind)) %>% 
  filter(hh==9) %>%
  mutate(n_sum=(n_wind+n_wind_lag)/2,e_sum=(e_wind+e_wind_lag)/2) %>% 
  mutate(wind_mean=sqrt(n_sum^2 + e_sum^2)) %>% 
  mutate(n_dif=(n_wind-n_wind_lag),e_dif=(e_wind-e_wind_lag)) %>%
  mutate(wind_dif=sqrt(n_dif^2 + e_dif^2)) %>%
  select(yyyy:dd,wind_mean,wind_dif) %>% 
  filter(wind_mean <= 100 & wind_dif <= 100)

#---- merge temp diff and cla ----

df_all <- tdif_set %>%
  inner_join(hare_set, c("yyyy","mm","dd","sfc_no")) %>%
  inner_join(sonde_set,c("yyyy","mm","dd")) %>%
  inner_join(rain_set, c("yyyy","mm","dd","sfc_no")) %>%
  inner_join(wind_set, c("yyyy","mm","dd")) %>%
  select(yyyy,mm,dd,sfc_no,temp_dif,hare,tadv)

#---- chenge date to sin cos ----

df_ang <- df_all %>%
  mutate(yy_tmp=yyyy,mm_tmp=1,dd_tmp=15) %>%
  mutate(ymd=str_c(yyyy,mm,dd,sep="-"),ymd_tmp=str_c(yy_tmp,mm_tmp,dd_tmp,sep="-")) %>%
  mutate_if(is.character,as.Date) %>%
  mutate(cnt=ymd - ymd_tmp)  %>%
  mutate_at(vars(cnt),as.numeric) %>%
  mutate(ang=2*pi*cnt/365) %>% 
  mutate(wt_sm=cos(ang),sp_at=sin(ang),target=temp_dif) %>%
  select(target,hare,wt_sm,sp_at,tadv,sfc_no) %>%
  mutate_at(vars(target,hare,tadv),as.numeric) %>% 
  mutate(target=target/10.0)

#---- model check ----

imgdir <-"../Data/Check"

pnt_no <- c(47615, #utunomiya.tsv", 
         47624, #maebashi.tsv",
         47629, #mito.tsv",
         47638, #koufu.tsv",
         47662, #tokyo.tsv",
         47670) #yokohama.tsv")

for (i in pnt_no) {
  df_check <- df_ang %>%
    filter(sfc_no == i)
  fm = "target ~ hare + wt_sm + sp_at + tadv"
  result <- lm(formula(fm),data=df_check)
  y_hat <- predict(result)
  R_2 <- summary(result)$r.squared
  Coef <- result$coefficients
  
  y_hat <- y_hat %>%
    as.data.frame %>%
    mutate(grp="fst")
  
  # check hist
  y_obs <- df_check %>%
    select(target) %>%
    mutate(grp="obs") %>%
    mutate_at(vars(target),as.numeric)
  
  colnames(y_hat) <- c("temp_dif","grp")
  colnames(y_obs) <- c("temp_dif","grp")
  
  gg_check <- rbind(y_obs,y_hat)
  
  g <- ggplot(gg_check,aes(x=temp_dif, fill =grp)) +
    geom_histogram(position="identity",alpha=0.7) +
    ggtitle(paste0("pnt= ",i,
                   " R^2=",round(R_2, digits = 2),
                   " Coef=",round(Coef[1],digits = 1),
                   ", ",round(Coef[2],digits = 1),
                   ", ",round(Coef[3],digits = 1),
                   ", ",round(Coef[4],digits = 1),
                   ", ",round(Coef[5],digits = 1)
                   ))
  
  plot(g)
  
  ggsave(filename=paste0(imgdir,"/hist_",i,".png"),
         width=6,height=4,dpi=200)
  
  df_smp <- cbind(y_obs,y_hat)
  colnames(df_smp) <- c("obs","grp","fcst","grpf")
  acc <- cor(y_obs$temp_dif,y_hat$temp_dif)
  
  g <- ggplot(df_smp,aes(x=obs,y=fcst))+
    theme_gray () +
    xlim(-7,2)+ylim(-7,2) +
    geom_point(size=2,alpha=0.6) +
    ggtitle(paste0("pnt= ",i,
                   " acc= ",round(acc, digits = 2)))
  
  plot(g)
  
  ggsave(filename=paste0(imgdir,"/smp_",i,".png"),
         width=6,height=4,dpi=200)
}

##### MCMC ####
#---- make ID list ---

df_mc <- df_ang %>%
  mutate(RID=if_else(sfc_no==47615,1,0)) %>% 
  mutate(RID=if_else(sfc_no==47624,2,RID)) %>%
  mutate(RID=if_else(sfc_no==47629,3,RID)) %>%
  mutate(RID=if_else(sfc_no==47638,4,RID)) %>%
  mutate(RID=if_else(sfc_no==47662,5,RID)) %>%
  mutate(RID=if_else(sfc_no==47670,6,RID)) 

datastan <- list(N=nrow(df_mc), R=6,
                 Y=df_mc$target,
                 X1=df_mc$hare, X2=df_mc$wt_sm,
                 X3=df_mc$sp_at, X4=df_mc$tadv,
                 RID=df_mc$RID)

#---- stanmodel compile ---

stanmodel <- stan_model(file='../Model/kaiso.stan')

#---- sampling ---

fit <- sampling(stanmodel, data=datastan,
                chains = 2, iter = 1000,
                seed=1234)

#---- Convergence diagnosis ---

#launch_shinystan(fit)

#---- draw Coef ----
#---- 季節の寄与 ---

outdir <-"../Data/OUT/"

b<-rstan::extract(fit)$b%>%as.numeric()
c<-rstan::extract(fit)$c%>%as.numeric()
res<-data.frame(sample=c(b),pars=rep(c("冬夏"),each=length(b))) %>% 
  rbind(data.frame(sample=c(c),pars=rep(c("春秋"),each=length(c))))

Color=c("tomato","lightseagreen")
g <- ggplot(res,aes(x=sample,y=..density..,colour=pars, fill=pars))+
  theme_gray (base_family = "HiraKakuPro-W3") +
  geom_histogram(position="identity",alpha=0.7)+
  geom_density(position="identity",alpha=0.4)+
  scale_color_manual(values=Color)+
  scale_fill_manual(values=Color) +
  theme(axis.text.x = element_text(size=15),axis.text.y = element_text(size=15))+
  theme(axis.title.x = element_text(size=15),axis.title.y = element_text(size=15)) +
  xlab("季節の寄与") +ylab("denstiy") 

plot(g)

ggsave(filename=paste0(outdir,"season_hist.png"),
       width=5,height=4,dpi=200)

#---- 放射の寄与 ---

a_pnt<-rstan::extract(fit)$a%>%as.numeric()
res<-data.frame(sample=c(a_pnt),
                pars=rep(c("宇都宮","前橋","水戸",
                           "甲府","東京","横浜"),each=1000)) 
g <- ggplot(res, aes(x = sample, y = pars, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
  theme_gray (base_family = "HiraKakuPro-W3") +
  scale_fill_gradientn(
    colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF")) +
  theme(axis.text.x = element_text(size=15),axis.text.y = element_text(size=15))+
  theme(axis.title.x = element_text(size=15),axis.title.y = element_text(size=15)) +
  xlab("放射の寄与") +ylab("地点") 

plot(g)

ggsave(filename=paste0(outdir,"housya_hist.png"),
       width=5,height=4,dpi=200)





















  
