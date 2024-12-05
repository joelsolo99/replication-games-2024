#### SPACE ####
library('dplyr')
library('sjstats')
library('ggplot2')
library('foreach')
library('doParallel')
library("eyetrackingR")
## Set working directory ##
thisFilePath<-rstudioapi::getSourceEditorContext()$path
thisDir=dirname(thisFilePath)
setwd(thisDir)
## Set parallel computation ##
options(mc.cores=parallel::detectCores())
rstan::rstan_options(auto_write=TRUE) 
cores=detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)
## Misc 
options(contrasts=c('contr.helmert','contr.poly'))
#
#
#### FUNCTIONS ####

# Utilitaire #
refresh_period<-(1/60)*1000 # Duration of eye-tracking samples according to the 60Hz refresh rate
`%notin%` <- Negate(`%in%`)
get_age <- function(f,return_date='weeks'){
  f_age=gsub(".*-(.*)\\.topd.*", "\\1", f)
  month=as.integer(substr(f_age, 1, 2))
  day=as.integer(substr(f_age, 3, 4))
  if(return_date=='weeks') age=round((month*4)+(day/7),2)
  if(return_date=='months') age=round(month+(day/30),2)
  return(age)
}

# Informative time window
ET_diff_ITW <- function(df,agg_trial=T) {
  # group_df=age_group_seq
  # group1='adults_up'
  # group2='adults_inv'
  if(!agg_trial){
    if(!all) group_df <- subset(df,group%in%c(group1,group2)) else
      group_df <- df
    group_df%>%select(group,age,ID,Trial,AOI,Time,Prop)->group_df
    group_df%>%tidyr::spread(AOI,Prop)->diff_df
    # group_df%>%select(group,age,ID,AOI,Time,SamplesInAOI)->group_df
    # group_df%>%tidyr::spread(AOI,SamplesInAOI)->diff_df
    # diff_df%>%mutate(Difference=(facing-away)/(facing+away),Null=0)->diff_df
    diff_df%>%mutate(Difference=(facing-away)/(facing+away))->diff_df
    diff_df%>%select(-c(facing,away))->diff_df
    diff_df$Difference[is.nan(diff_df$Difference)] <- 0 # Difference with first method if commented
    # diff_df%>%tidyr::gather(AOI, Difference, Difference:Null)->diff_df_long
    diff_df%>%tidyr::gather(AOI,Difference,Difference)->diff_df_long
    nrow(diff_df_long)*2-nrow(group_df)
    
    # Data tricks to fool the eyetrackinR #
    ET_df <- eyetrackingR::make_time_sequence_data(
      data=ET.data.clean,predictor_columns=c('group','age'),aois=c('away','facing'),summarize_by=c('ID','Trial'),time_bin_size=tbin)
    ET_df <- subset(ET_df,group%in%unique(df$group))
    # ET_df%>%mutate(AOI=ifelse(AOI=='facing','Difference','Null'))->ET_df
    ET_df%>%filter(AOI=='facing')->ET_df
    ET_df$AOI<-ET_df$group
    ET_df%>%group_by(group,age,ID,Trial,Time)%>%
      left_join(diff_df_long, by=c("ID","Trial","Time","group","age"))->ET_df_sub
    # ET_df_sub%>%mutate(Prop=Difference)->ET_df_sub
    # ET_df_sub%>%mutate(AOI=group, Prop=Difference)->ET_df_sub
    sum(ET_df$Prop-ET_df_sub$Prop) # Must be 0 if same order
    ET_df$Prop<-ET_df_sub$Difference
    
    # Plot #
    ET_df%>%group_by(AOI,Time)%>% 
      dplyr::summarise(N=n(),M=mean(Prop,na.rm=T),sd=sd(Prop,na.rm=T),sem=goeveg::sem(Prop,na.rm=T))%>%
      dplyr::rename(Prop=M) -> plot_df
    plot_df%>%ungroup()%>%filter(!is.na(Prop), AOI!='Null')->plot_df
    plot_df$AOI <- as.factor(plot_df$AOI)
    print(ggplot(plot_df) + 
            geom_line(aes(x=Time, y=Prop, linetype=AOI), stat='identity', size=1.2) +
            geom_ribbon(aes(ymin=Prop+sem, ymax=Prop-sem, x=Time, fill=AOI), alpha=0.3) +
            # annotate("rect", xmin=2618, xmax=3842, ymin=-Inf, ymax=Inf, alpha=.1) + # interaction
            geom_hline(yintercept=0, linetype="dashed", color="black") +
            # geom_vline(xintercept= 493, color="black") +
            # geom_vline(xintercept= 3876, color="black") +
            # xlim(limits=c(200,2500))+
            # ylim(limits=c(-0.5,0.5))+
            scale_fill_grey(start=0.2, end=0.5) +
            theme_minimal(base_size=20) + guides(size=F, alpha=F))
  }
  if(agg_trial){
    group_df <- df
    group_df%>%select(group,age,ID,AOI,Time,Prop)->group_df
    group_df%>%tidyr::spread(AOI,Prop)->diff_df
    # group_df%>%select(group,age,ID,AOI,Time,SamplesInAOI)->group_df
    # group_df%>%tidyr::spread(AOI,SamplesInAOI)->diff_df
    diff_df%>%mutate(Difference=(images-elsewhere)/(images+elsewhere))->diff_df
    diff_df%>%select(-c(images,elsewhere))->diff_df
    diff_df$Difference[is.nan(diff_df$Difference)] <- 0 # Difference with first method if commented
    diff_df%>%tidyr::gather(AOI,Difference,Difference)->diff_df_long
    nrow(diff_df_long)*2-nrow(group_df)
    
    # Data tricks to fool the eyetrackinR #
    ET_df <- eyetrackingR::make_time_sequence_data(
      data=age_group,predictor_columns=c('group','age'),aois=c('elsewhere','images'),summarize_by=c('ID'),time_bin_size=tbin)
    ET_df <- subset(ET_df,group%in%unique(group_df$group))
    ET_df%>%filter(AOI=='images')->ET_df
    ET_df%>%group_by(group,age,ID,Time)%>%
      left_join(diff_df_long, by=c("ID","Time","group","age"))->ET_df_sub
    sum(ET_df$Prop-ET_df_sub$Prop) # Must be 0 if same order
    ET_df$Prop<-ET_df_sub$Difference
    
    # Plot #
    ET_df%>%group_by(AOI,Time)%>% 
      dplyr::summarise(N=n(),M=mean(Prop),sd=sd(Prop),sem=goeveg::sem(Prop))%>%
      dplyr::rename(Prop=M) -> plot_df
    plot_df%>%ungroup()%>%filter(!is.na(Prop),AOI!='Null')->plot_df
    plot_df$AOI <- as.factor(plot_df$AOI)
    print(ggplot(plot_df) + 
            geom_line(aes(x=Time, y=Prop, linetype=AOI), stat='identity', size=1.2) +
            geom_ribbon(aes(ymin=Prop+sem, ymax=Prop-sem, x=Time, fill=AOI), alpha=0.3) +
            # annotate("rect", xmin=2618, xmax=3842, ymin=-Inf, ymax=Inf, alpha=.1) + # interaction
            geom_hline(yintercept=0, linetype="dashed", color="black") +
            # geom_vline(xintercept= 493, color="black") +
            # geom_vline(xintercept= 3876, color="black") +
            # xlim(limits=c(200,2500))+
            # ylim(limits=c(-0.5,0.5))+
            scale_fill_grey(start=0.2, end=0.5) +
            theme_minimal(base_size=20) + guides(size=F, alpha=F))
  }
  return(ET_df)
}

# Compute difference time course
ET_diff_group1_group2 <- function(df,group1,group2) {
  # df=ET.data.clean.prop.seq
  
  # Difference facing vs. away 7-mo from common.df #
  facing.group1 <- subset(df, group==group1) %>%group_by(Time, ID) %>%
    select(AOI, ID, group, Time, Prop) %>%dplyr::filter(AOI == 'facing' & group == group1)
  away.group1 <- subset(df, group==group1) %>%group_by(Time) %>%
    select(AOI, ID, group, Time, Prop) %>%dplyr::filter(AOI == 'away' & group == group1)
  difDF.group1 <- droplevels(cbind.data.frame(facing.group1[,1:4],(facing.group1$Prop-away.group1$Prop)/(facing.group1$Prop+away.group1$Prop)))
  difDF.group1$facing_prop=facing.group1$Prop
  difDF.group1$away_prop=away.group1$Prop
  # difDF.group1 <- droplevels(cbind.data.frame(facing.group1[,1:4],(facing.group1$Prop-away.group1$Prop)))
  difDF.group1[is.na(difDF.group1)] <- 0
  colnames(difDF.group1)[5] <- c('differences')
  difDF.group1 %>%group_by(Time, ID, AOI) %>%
    dplyr::summarise(`differences`=mean(`differences`)) %>%mutate(group=group1)-> difDF.group1
  # difDF.group1 <- select(subset(ET.data.group1.clean.prop.seq.TOI.diff.df, AOI == 'away'), c(Time, ID, Prop, AOI))
  # difDF.group1 %>% group_by(Time, ID) %>% mutate(group=group2, `differences`=mean(Prop)) -> difDF.group1
  difDF.group1 %>% mutate(AOI='away') %>% mutate(`differences`=0) -> difDF.neutral
  difDF.group1 <- rbind.data.frame(difDF.group1, difDF.neutral)
  
  # Difference facing vs. away adults from common.df #
  facing.group2 <- subset(df, group==group2) %>%group_by(Time, ID) %>%
    select(AOI, ID, group, Time, Prop) %>%dplyr::filter(AOI == 'facing' & group == group2)
  away.group2 <- subset(df, group==group2) %>%group_by(Time) %>%
    select(AOI, ID, group, Time, Prop) %>%dplyr::filter(AOI == 'away' & group == group2)
  difDF.group2 <- droplevels(cbind.data.frame(facing.group2[,1:4],(facing.group2$Prop-away.group2$Prop)/(facing.group2$Prop+away.group2$Prop)))
  # difDF.group2 <- droplevels(cbind.data.frame(facing.group2[,1:4],(facing.group2$Prop-away.group2$Prop)))
  difDF.group2[is.na(difDF.group2)] <- 0
  colnames(difDF.group2)[5] <- c('differences')
  difDF.group2 %>%group_by(Time, ID, AOI) %>%
    dplyr::summarise(`differences`=mean(`differences`)) %>%mutate(group=group2) -> difDF.group2
  # difDF.group2 <- select(subset(ET.data.group2.clean.prop.seq.TOI.diff.df, AOI == 'away'), c(Time, ID, Prop, AOI))
  # difDF.group2 %>% group_by(Time, ID) %>% mutate(group='adults', `differences`=mean(Prop)) -> difDF.group2
  difDF.group2 %>% mutate(AOI='away') %>% mutate(`differences`=0) -> difDF.neutral
  difDF.group2 <- rbind.data.frame(difDF.group2, difDF.neutral)
  
  # Merge informations #
  common.difDf <- rbind.data.frame(difDF.group1, difDF.group2)
  
  # Data tricks to fool the eyetrack packgroup #
  df2 <- make_time_sequence_data(data=ET.data.clean,
                                 predictor_columns='group',aois=c('away','facing'),
                                 summarize_by=c('ID'),time_bin_size=tbin)
  df2 %>%group_by(ID, Time) %>%left_join(common.difDf, by=c("ID","Time","AOI","group")) %>%
    mutate(Prop=`differences`) %>%
    filter(is.na(`differences`) == F, AOI != 'away') %>%mutate(AOI=group) -> df2
  df2 %>%group_by(AOI, Time) %>%
    mutate(sem=goeveg::sem(`differences`),
           upper=quantile(`differences`)[4],
           lower=quantile(`differences`)[2],
           `Differences`=mean(`differences`)) -> df2.plot
  # df2.plot <- subset(df2.plot, AOI == '7mo_bodies')
  df2.plot$AOI <- as.factor(df2.plot$AOI)
  df2.plot$AOI <- factor(df2.plot$AOI,levels(df2.plot$AOI)[c(2,1)])
  plot <-  ggplot(df2.plot) + 
    geom_line(aes(x=Time, y=`Differences`, linetype=AOI), stat='identity', size=1.3) +
    geom_ribbon(aes(ymin=Differences+sem, ymax=Differences-sem, x=Time, fill=AOI), alpha=0.3) +
    # annotate("rect", xmin=391, xmax=969, ymin=-Inf, ymax=Inf, alpha=.1) + # interaction
    geom_hline(yintercept=0, linetype="dashed", color="black") +
    # geom_vline(xintercept= 493, color="black") +
    # geom_vline(xintercept= 3876, color="black") +
    # xlim(limits=c(200,2500))+
    ylim(limits=c(-0.5,0.5))+
    scale_fill_grey(start=0.2, end=0.5) + theme_minimal(base_size=20) + guides(size=F, alpha=F)
  print(plot)
  
  return(df2)
}
ET_diff_new <- function(df,group1=NA,group2=NA,agg_trial=T,all=T,return_plot=F,plot_split=F) {
  # group_df=ET.data.clean.prop.seq
  # group1='adults_up'
  # group2='adults_inv'
  # agg_trial=T
  # all=T
  # return_plot=F
  if(!agg_trial){
    if(!all) group_df <- subset(df,group%in%c(group1,group2)) else
      group_df <- df
    group_df%>%select(group,age,ID,Trial,AOI,Time,Prop)->group_df
    group_df%>%tidyr::spread(AOI,Prop)->diff_df
    # group_df%>%select(group,age,ID,Trial,AOI,Time,SamplesInAOI)->group_df
    # group_df%>%tidyr::spread(AOI,SamplesInAOI)->diff_df
    # diff_df%>%mutate(Difference=(facing-away)/(facing+away),Null=0)->diff_df
    diff_df%>%mutate(Difference=(facing-away)/(facing+away))->diff_df
    diff_df%>%select(-c(facing,away))->diff_df
    diff_df$Difference[is.nan(diff_df$Difference)] <- 0 # Difference with first method if commented
    # diff_df%>%tidyr::gather(AOI, Difference, Difference:Null)->diff_df_long
    diff_df%>%tidyr::gather(AOI,Difference,Difference)->diff_df_long
    nrow(diff_df_long)*2-nrow(group_df)
    
    # Data tricks to fool the eyetrackinR #
    ET_df <- eyetrackingR::make_time_sequence_data(
      data=ET.data.clean,predictor_columns=c('group','age'),aois=c('away','facing'),summarize_by=c('ID','Trial'),time_bin_size=tbin)
    ET_df <- subset(ET_df,group%in%unique(df$group))
    # ET_df%>%mutate(AOI=ifelse(AOI=='facing','Difference','Null'))->ET_df
    ET_df%>%filter(AOI=='facing')->ET_df
    ET_df$AOI<-ET_df$group
    ET_df%>%group_by(group,age,ID,Trial,Time)%>%
      left_join(diff_df_long, by=c("ID","Trial","Time","group","age"))->ET_df_sub
    # ET_df_sub%>%mutate(Prop=Difference)->ET_df_sub
    # ET_df_sub%>%mutate(AOI=group, Prop=Difference)->ET_df_sub
    sum(ET_df$Prop-ET_df_sub$Prop) # Must be 0 if same order
    ET_df$Prop<-ET_df_sub$Difference
    
    # Plot #
    ET_df%>%group_by(AOI,Time)%>% 
      dplyr::summarise(N=n(),M=mean(Prop,na.rm=T),sd=sd(Prop,na.rm=T),sem=goeveg::sem(Prop,na.rm=T))%>%
      dplyr::rename(Prop=M) -> plot_df
    plot_df%>%ungroup()%>%filter(!is.na(Prop), AOI!='Null')->plot_df
    plot_df$AOI <- as.factor(plot_df$AOI)
    print(ggplot(plot_df) + ylab('Differences') +
            geom_line(aes(x=Time, y=Prop, linetype=AOI), stat='identity', size=1.2) +
            geom_ribbon(aes(ymin=Prop+sem, ymax=Prop-sem, x=Time, fill=AOI), alpha=0.2) +
            geom_hline(yintercept=0, linetype="dashed", color="black") +
            # scale_fill_grey(start=0.2, end=0.5) +
            theme_minimal(base_size=20) + guides(size=F, alpha=F))
  }
  if(agg_trial){
    if(!all) group_df <- subset(df,group%in%c(group1,group2)) else
      group_df <- df
    group_df%>%select(group,age,ID,AOI,Time,Prop)->group_df
    group_df%>%tidyr::spread(AOI,Prop)->diff_df
    # group_df%>%select(group,age,ID,AOI,Time,SamplesInAOI)->group_df
    # group_df%>%tidyr::spread(AOI,SamplesInAOI)->diff_df
    # diff_df%>%mutate(Difference=(facing-away)/(facing+away),Null=0)->diff_df
    diff_df%>%mutate(Difference=(facing-away)/(facing+away))->diff_df
    # diff_df%>%mutate(Difference=facing-away)->diff_df
    diff_df%>%select(-c(facing,away))->diff_df
    diff_df$Difference[is.nan(diff_df$Difference)] <- 0 # Difference with first method if commented
    # diff_df%>%tidyr::gather(AOI, Difference, Difference:Null)->diff_df_long
    diff_df%>%tidyr::gather(AOI,Difference,Difference)->diff_df_long
    nrow(diff_df_long)*2-nrow(group_df)
    # ggplot(diff_df_long) + ylab('Differences') +
    #   geom_line(aes(x=Time,y=Difference,linetype=group,color=group),stat='summary',size=1.2) +
    #   # geom_ribbon(aes(ymin=Prop+sem, ymax=Prop-sem, x=Time, fill=AOI),alpha=0.2) +
    #   geom_hline(yintercept=0,linetype="dashed",color="black") +
    #   geom_vline(xintercept=20,linetype="dashed",color="black") +
    #   geom_vline(xintercept=25,linetype="solid",color="black") +
    #   geom_vline(xintercept=30,linetype="dashed",color="black") +
    #   # scale_fill_grey(start=0.2, end=0.5) +
    #   guides(size=F,alpha=F) + theme_minimal(base_size=20)
    # diff_df_long%>%filter(group=='1yo_bodies'&Time>20&Time<30)->time_window_df
    # time_window_df%>%group_by(group,Time)%>%dplyr::summarise(Difference=mean(Difference))->time_window_df
    # diff_df_long%>%filter(group=='1yo_bodies'&Time==25)->time_point_df
    # mean(time_point_df$Difference)
    
    # Data tricks to fool the eyetrackinR #
    ET_df <- eyetrackingR::make_time_sequence_data(
      data=ET.data.clean,predictor_columns=c('group','age'),aois=c('away','facing'),summarize_by=c('ID'),time_bin_size=tbin)
    ET_df <- subset(ET_df,group%in%unique(df$group))
    # ET_df%>%mutate(AOI=ifelse(AOI=='facing','Difference','Null'))->ET_df
    ET_df%>%filter(AOI=='facing')->ET_df
    ET_df$AOI<-ET_df$group
    ET_df%>%group_by(group,age,ID,Time)%>%
      left_join(diff_df_long, by=c("ID","Time","group","age"))->ET_df_sub
    # ET_df_sub%>%mutate(Prop=Difference)->ET_df_sub
    # ET_df_sub%>%mutate(AOI=group, Prop=Difference)->ET_df_sub
    sum(ET_df$Prop-ET_df_sub$Prop) # Must be 0 if same order
    ET_df$Prop<-ET_df_sub$Difference
    
    # Plot #
    ET_df%>%group_by(AOI,Time)%>% 
      dplyr::summarise(N=n(),M=mean(Prop),sd=sd(Prop),sem=goeveg::sem(Prop))%>%
      dplyr::rename(Prop=M) -> plot_df
    plot_df%>%ungroup()%>%filter(!is.na(Prop), AOI!='Null')->plot_df
    plot_df$AOI <- as.factor(plot_df$AOI)
    # levels(plot_df$AOI) <- c('7','10      ','15','18')
    if(plot_split){
      print(plot <- ggplot(plot_df) + 
              facet_grid(AOI~.) + ylab('Differences') +
              geom_line(aes(x=Time,y=Prop,linetype=AOI,color=AOI),stat='identity',size=1.2) +
              geom_ribbon(aes(ymin=Prop+sem, ymax=Prop-sem, x=Time, fill=AOI),alpha=0.2) +
              geom_hline(yintercept=0,linetype="dashed",color="black") +
              # scale_fill_grey(start=0.2, end=0.5) +
              scale_y_continuous(breaks=seq(-.2,.2,.2)) + coord_cartesian(ylim=c(-.3,.3),xlim=c(12,288))+
              guides(size=F,alpha=F) + theme_minimal(base_size=20))
    }
    else {
      print(plot <- ggplot(plot_df) + ylab('Differences') +
              geom_line(aes(x=Time,y=Prop,linetype=AOI,color=AOI),stat='identity',size=1.2) +
              geom_ribbon(aes(ymin=Prop+sem, ymax=Prop-sem, x=Time, fill=AOI),alpha=0.2) +
              geom_hline(yintercept=0,linetype="dashed",color="black") +
              # scale_fill_grey(start=0.2, end=0.5) +
              scale_y_continuous(breaks=seq(-.2,.2,.2)) + coord_cartesian(ylim=c(-.3,.3),xlim=c(12,288))+
              guides(size=F,alpha=F) + theme_minimal(base_size=20))
    }
    
  }
  if(return_plot) return(plot) else return(ET_df)
}

# Time course cluster mass analysis
perm_test_ttest <- function(df,threshold,n_samples,paired,alternative,chance,mu,gap,tbin) {
  # df=droplevels(subset(ET_diff_ITW_group,grepl('adu',group)))
  # threshold=1.5
  # alternative='two.sided'
  # paired=F
  # n_samples=50
  # gap=0
  # tbin=1
  # chance=T
  # mu=0
  
  # Functions #
  get_stat_time_course <- function(df,paired,alternative,chance,mu) {
    # time_sample=unique(df$Time)[30]
    aois <- unique(df$AOI)
    foreach (time_sample=unique(df$Time),.combine=rbind) %do% {
      # for (time_sample in unique(df$Time)) {
      # print(time_sample)
      cond1 <- subset(df,Time == time_sample & AOI == aois[1])$Prop
      cond2 <- subset(df,Time == time_sample & AOI == aois[2])$Prop
      if(length(cond1)==1|length(cond2)==1){
        stat_stats <- NA
        stat_pval <- NA
      } else if(length(unique(cond1-cond2))==1){
        stat_stats <- NA
        stat_pval <- NA
      } else if(length(unique(c(cond1,cond2)))==1){
        stat_stats <- NA
        stat_pval <- NA
      } else if(paired==T&length(cond1)!=length(cond2)){
        stat_stats <- NA
        stat_pval <- NA
      } else {
        if (chance==F) {
          test_rslt <- t.test(cond1,cond2,paired=paired,alternative=alternative,var.equal=T)
          stat_stats <- test_rslt$statistic
          stat_pval <- test_rslt$p.value
        } 
        else {
          if(any(cond1!=0)) test_rslt <- t.test(cond1,mu=mu,alternative=alternative) else
            test_rslt <- t.test(cond2,mu=mu,alternative=alternative)
          stat_stats <- test_rslt$statistic
          stat_pval <- test_rslt$p.value
        }
      }
      cbind(time_sample,stat_stats,stat_pval)} -> stat_rslts_list
    stat_rslts_list <- as.data.frame(stat_rslts_list)
    stat_rslts_list%>%arrange(time_sample)->stat_rslts_list
    stat_rslts_list$clust_start <- ifelse(abs(stat_rslts_list$stat_stats) > threshold & lag(abs(stat_rslts_list$stat_stats)) < threshold |
                                            abs(stat_rslts_list$stat_stats) > threshold & is.na(lag(abs(stat_rslts_list$stat_stats))),1,0)
    stat_rslts_list$clust_start[1]<- ifelse(abs(stat_rslts_list$stat_stats)[1] > threshold,1,0)
    stat_rslts_list$clust_sum <- cumsum(ifelse(is.na(stat_rslts_list$clust_start),0,stat_rslts_list$clust_start))
    stat_rslts_list$clust_n <- ifelse(abs(stat_rslts_list$stat_stats)>threshold,stat_rslts_list$clust_sum,NA)
    if(alternative=='greater') stat_rslts_list$clust_n[stat_rslts_list$stat_stats<0] <- NA
    if(alternative=='less') stat_rslts_list$clust_n[stat_rslts_list$stat_stats>0] <- NA
    return(dplyr::select(stat_rslts_list,-c(clust_start,clust_sum)))}
  get_permutated_time_course <- function(perm_df,paired,alternative,chance,mu) {
    # perm_df=df
    perm_df$ID <- as.factor(perm_df$ID)
    perm_df$AOI <- as.factor(perm_df$AOI)
    if (paired) {
      foreach (i=levels(perm_df$ID),.combine='rbind',.packages=c('dplyr','doParallel','foreach')) %do% {
        sub_perm_df <- subset(perm_df,ID == i)
        aois <- sample(unique(perm_df$AOI))
        levels(sub_perm_df$AOI) <- aois
        sub_perm_df} -> perm_df}
    if (chance) {
      original_distrib <- aggregate(AOI~ID,data=perm_df,FUN=unique)
      original_distrib$ID <- sample(original_distrib$ID,replace=F)
      original_distrib$shuffle_sign <- rep(sample(c(-1,1)),length(unique(perm_df$ID)))[1:length(unique(perm_df$ID))]
      # original_distrib$shuffle_sign <- sample(rep(c(-1,1),length(unique(perm_df$ID))/2),replace=F) # Shuffle condition assignation (within)
      # original_distrib$shuffle_sign <- sample(c(-1,1),length(unique(perm_df$ID)),replace=T)
      perm_df <- merge(perm_df,original_distrib,by.x=c('AOI','ID'),by.y=c('AOI','ID'))
      perm_df$Prop <- perm_df$Prop*perm_df$shuffle_sign}
    if (!paired&!chance) {
      original_distrib <- aggregate(AOI ~ ID,data=perm_df,FUN=unique)
      original_distrib$ID <- sample(original_distrib$ID,replace=F)
      perm_df <- merge(perm_df,original_distrib,by.x="ID",by.y='ID')
      names(perm_df)[names(perm_df) == "AOI.y"] <- "AOI"
    }
    perm_df_stat_time_course <- get_stat_time_course(perm_df,paired,alternative,chance,mu)
    return(perm_df_stat_time_course)
  }
  get_clust_stats <- function(df,gap) {
    # df=empirical_stat_time_course
    df_clust <- subset(df,!is.na(clust_n))
    # for (clust in unique(df_clust$clust_n)) {
    foreach (clust=unique(df_clust$clust_n),.combine=rbind) %do% {
      sub_agg_df <- subset(df_clust,clust_n == clust)
      data.frame(cluster_n=clust,
                 start=sub_agg_df$time_sample[1],
                 end=sub_agg_df$time_sample[nrow(sub_agg_df)]+tbin,
                 stat=sum(sub_agg_df$stat_stats))} -> clust_stat_df
    if(!is.null(clust_stat_df)&gap>0){
      clust_stat_df%>%mutate(new_start=ifelse(
        row_number()>1 & lag(end+gap)>=start & sign(stat)==lag(sign(stat)),0,1))->clust_stat_df
      clust_stat_df$cluster_n<-cumsum(clust_stat_df$new_start)
      clust_stat_df%>%group_by(cluster_n)%>%
        dplyr::summarise(start=min(start),end=max(end),
                         stat=sum(stat),.groups='drop_last')->clust_stat_df
    }
    return(data.frame(clust_stat_df))
  }
  
  # Empirical distribution #
  empirical_stat_time_course <- get_stat_time_course(df,paired,alternative,chance,mu)
  empirical_cluster <- get_clust_stats(empirical_stat_time_course,gap)
  main_clust <- empirical_cluster[which.max(abs(empirical_cluster$stat)),]
  # p1 <- ggplot(empirical_stat_time_course) + theme_minimal(base_size=25) + xlab('Time') + ylab('-log(p-value)') +
  #   geom_line(aes(x=time_sample,y=stat_stats)) + geom_hline(yintercept=threshold,linetype="dashed") +
  #   geom_vline(xintercept=c(main_clust$start-tbin,main_clust$end),linetype="dashed")
  p2 <- ggplot(empirical_stat_time_course) + theme_minimal(base_size=25) + xlab('Time') + ylab('Statistic') +
    geom_line(aes(x=time_sample,y=stat_stats)) + geom_hline(yintercept=c(threshold,-threshold),linetype="dashed") +
    geom_vline(xintercept=c(main_clust$start-tbin,main_clust$end),linetype="dashed")
  gridExtra::grid.arrange(p2,nrow=1)
  
  # Permutations
  foreach(sample=1:n_samples,.combine=c,.packages=c('doParallel','foreach','dplyr')) %dopar% {
    # for (time_sample in 1:n_samples) {
    perm_stat_time_course <- get_permutated_time_course(df,paired,alternative,chance,mu)
    perm_cluster <- get_clust_stats(perm_stat_time_course,gap)
    # print(perm_cluster)
    if (ncol(perm_cluster)>0) max_clust <- perm_cluster$stat[which.max(abs(perm_cluster$stat))] else max_clust <- 0
  } -> max_stat_list
  empirical_cluster$p_value <- lapply(empirical_cluster$stat,function(X) length(which(max_stat_list > abs(X)))/n_samples)
  print(empirical_cluster)
  # Save results #
  perm_test_rslts <- list(clusters=empirical_cluster,
                          parameters=c(alpha=alpha,threshold=threshold,n_samples=n_samples,paired=paired,alternative=alternative,chance=chance,mu=mu,gap=gap,tbin=tbin))
  return(perm_test_rslts)
}
perm_test_lm <- function(df,to_test='group',to_shuffle='condition',alpha=.05,thres=NA,n_samples=10,gap=0,coll_clust=F,random=F,bayes=F,robust_lm=F,categorical_VI=F,tbin=1) {
  # df=ET_diff_df
  # to_test='age'
  # to_shuffle='condition'
  # alpha=.1
  # thres=NA
  # n_samples=50
  # random=F
  # gap=0
  # coll_clust=F
  # bayes=F
  # robust_lm=F
  # categorical_VI=F
  
  # Check compatibility of arguments #
  set.seed(Sys.time())
  if(to_test%notin%c('group','age')) stop ('Wrong argument to_test')
  if(to_shuffle%notin%c('ID','condition')) stop ('Wrong argument to_shuffle')
  if(to_test=='age'&to_shuffle=='ID') stop ('Cannot test age by shuffling subject group')
  if(to_test=='age'&categorical_VI) stop ('Age is not categorical')
  if(categorical_VI&random) print('LMM with categorical VI not implemented')
  if(categorical_VI&bayes) print('Bayesian regression with categorical VI not implemented')
  if(categorical_VI&robust_lm) print('Robust regression with categorical VI not implemented')
  
  # Set cluster threshold #
  if(is.na(thres)){
    N=length(levels(ET_diff_df$ID))
    k=length(levels(ET_diff_df$group))
    if(categorical_VI) {
      stat_name='F'
      thres=qf(p=alpha,df1=k-1,df2=N-k,lower.tail=F)}
    else if(robust_lm) {
      stat_name='Wald F'
      thres=qf(p=alpha,df1=1,df2=N-k,lower.tail=F)}
    else {
      stat_name='t'
      thres=qt(p=alpha/2,df=N-2,lower.tail=F)} # Threshold corresponding to how pvalues are calculated in lm summary
  }
  
  # Functions #
  get_stat_time_course <- function(df,random,chance,to_test,bayes,robust_lm=robust_lm,categorical_VI=categorical_VI) {
    # time_sample=unique(df$Time)[61]
    foreach (time_sample=unique(df$Time),.combine=rbind) %do% {
      # for (time_sample in unique(df$Time)) {
      time_sub_df <- subset(df,Time==time_sample)
      if(to_test=='group'){
        if(random){
          if(categorical_VI) print('LMM with categorical VI not implemented')
          else{
            if(bayes){
              time_sub_df$num_group <- as.numeric(time_sub_df$group)
              prop_model <- BayesFactor::lmBF(Prop~num_group+ID,data=time_sub_df,whichRandom="ID")}
            else prop_model <- lmerTest::lmer(Prop~poly(as.numeric(group),1)+(1|group),time_sub_df)
          }
        }
        else{
          if(categorical_VI){
            if(bayes|robust_lm) print('Bayesian or robust regression with categorical VI not implemented')
            else {
              time_sub_df$group <- factor(time_sub_df$group,ordered=F)
              contrasts(time_sub_df$group) <- contr.helmert(length(levels(time_sub_df$group)))
              prop_model <- aov(Prop~group,time_sub_df)
            }
          } 
          else{
            if(bayes){
              time_sub_df$num_group <- as.numeric(time_sub_df$group)
              prop_model <- BayesFactor::lmBF(Prop~num_group,data=time_sub_df)
            }
            else{
              if(robust_lm) prop_model <- MASS::rlm(Prop~poly(as.numeric(group),1),time_sub_df)
              else prop_model <- lm(Prop~poly(as.numeric(group),1),time_sub_df)
            }
          }
        }
      } 
      if(to_test=='age'){
        if(random){
          if(bayes){
            time_sub_df$num_age <- as.numeric(as.character(time_sub_df$age))
            prop_model <- BayesFactor::lmBF(Prop~num_group+ID,data=time_sub_df,whichRandom="ID")             
          } 
          else prop_model <- lmerTest::lmer(Prop~as.numeric(as.character(age))+(1|group),time_sub_df)
        }
        else{
          if(bayes){
            time_sub_df$num_age <- as.numeric(as.character(time_sub_df$age))
            prop_model <- BayesFactor::lmBF(Prop~num_age,data=time_sub_df)     
          } 
          else{
            if(robust_lm) {
              prop_model <- MASS::rlm(Prop~age,time_sub_df)
              # if(nrow(time_sub_df)>5) prop_model <- MASS::rlm(Prop~age,time_sub_df)
              # else {
              #   prop_model <- NA
              #   stat_stats <- NA
              #   stat_pval <- NA
              # }
            }
            else prop_model <- lm(Prop~age,time_sub_df)
          }
        }
      }
      if(bayes){
        test_rslt <- BayesFactor::extractBF(prop_model)
        stat_stats <- test_rslt$bf
        stat_pval <- NA}
      else {
        if(categorical_VI){
          test_rslt <- summary(prop_model)[[1]]
          stat_stats <- test_rslt$`F value`[1]
          stat_pval <- test_rslt$`Pr(>F)`[1]
        }
        else {
          if(robust_lm){
            # test_rslt <- data.frame(summary(prop_model)$coefficients)
            # stat_stats <- test_rslt$t.value[2]
            # stat_pval <- pt(q=stat_stats,df=length(unique(time_sub_df$ID))-length(unique(time_sub_df$group)),lower.tail=F)
            # https://stats.stackexchange.com/questions/205614/p-values-and-significance-in-rlm-mass-package-r
            # if(nrow(time_sub_df)>5) {
            if(to_test=='age') test_rslt <- sfsmisc::f.robftest(prop_model,var='age')
            else test_rslt <- sfsmisc::f.robftest(prop_model,var='poly(as.numeric(group), 1)')
            stat_stats <- as.numeric(test_rslt$statistic)
            stat_pval <- as.numeric(test_rslt$p.value)
            # }
          }
          else{
            test_rslt <- data.frame(summary(prop_model)$coefficients)
            stat_stats <- test_rslt$t.value[2]
            stat_pval <- test_rslt$Pr...t..[2]
          }
        }
      }
      cbind(time_sample,stat_stats,stat_pval)} -> stat_rslts_list
    stat_rslts_list <- as.data.frame(stat_rslts_list)
    if(bayes){
      stat_rslts_list$clust_start <- ifelse(stat_rslts_list$stat_stats > thres & lag(stat_rslts_list$stat_stats) < thres |
                                              stat_rslts_list$stat_stats > thres & is.na(lag(stat_rslts_list$stat_stats)),1,0)
      stat_rslts_list$clust_start[1]<- ifelse(stat_rslts_list$stat_stats[1] > thres,1,0) 
      stat_rslts_list$clust_sum <- cumsum(ifelse(is.na(stat_rslts_list$clust_start),0,stat_rslts_list$clust_start))
      stat_rslts_list$clust_n <- ifelse(stat_rslts_list$stat_stats > thres,stat_rslts_list$clust_sum,NA)
    } else {
      stat_rslts_list$clust_start <- ifelse(stat_rslts_list$stat_pval < alpha & lag(stat_rslts_list$stat_pval) > alpha |
                                              stat_rslts_list$stat_pval < alpha & is.na(lag(stat_rslts_list$stat_pval)),1,0)
      stat_rslts_list$clust_start[1]<- ifelse(stat_rslts_list$stat_pval[1] < alpha,1,0) 
      stat_rslts_list$clust_sum <- cumsum(ifelse(is.na(stat_rslts_list$clust_start),0,stat_rslts_list$clust_start))
      stat_rslts_list$clust_n <- ifelse(stat_rslts_list$stat_pval < alpha,stat_rslts_list$clust_sum,NA)
    }
    return(dplyr::select(stat_rslts_list,-c(clust_start,clust_sum)))
  }
  get_permutated_time_course <- function(df,random,chance,to_shuffle) {
    # perm_df=empirical_stat_time_course
    perm_df <- df
    if(to_shuffle=='ID') {
      original_distrib <- aggregate(group~ID,data=perm_df,FUN=unique)
      original_distrib$ID <- sample(original_distrib$ID,replace=F)
      perm_df <- merge(df,original_distrib,by.x="ID",by.y='ID') # Shuffle group (between)
      perm_df$group <- factor(perm_df$group.y,ordered=T)}
    if(to_shuffle=='condition') {
      original_distrib <- aggregate(group~ID,data=perm_df,FUN=unique)
      original_distrib$ID <- sample(original_distrib$ID,replace=F)
      # original_distrib$shuffle_sign <- sample(c(-1,1),length(unique(perm_df$ID)),replace=T) # Shuffle condition assignation (within)
      # original_distrib$shuffle_sign <- sample(rep(c(-1,1),length(unique(perm_df$ID))/2),replace=F) # Shuffle condition assignation (within)
      original_distrib$shuffle_sign <- rep(sample(c(-1,1)),length(unique(perm_df$ID)))[1:length(unique(perm_df$ID))]
      perm_df <- merge(df,original_distrib,by.x=c('group','ID'),by.y=c('group','ID'))
      perm_df$Prop <- perm_df$Prop*perm_df$shuffle_sign}
    return(perm_df)
  }
  get_clust_stats <- function(df,gap,coll_clust) {
    # df=empirical_stat_time_course
    df_clust <- subset(df,!is.na(clust_n))
    # for (clust in unique(df_clust$clust_n)) {
    foreach (clust=unique(df_clust$clust_n),.combine=rbind) %do% {
      sub_agg_df <- subset(df_clust,clust_n == clust)
      data.frame(cluster_n=clust,
                 start=sub_agg_df$time_sample[1],
                 end=sub_agg_df$time_sample[nrow(sub_agg_df)]+tbin,
                 stat=sum(sub_agg_df$stat_stats))
    } -> clust_stat_df
    if(!is.null(clust_stat_df)&gap>0){
      clust_stat_df%>%mutate(new_start=ifelse(
        row_number()>1 & lag(end+gap)>=start & sign(stat)==lag(sign(stat)),0,1))->clust_stat_df
      clust_stat_df$cluster_n<-cumsum(clust_stat_df$new_start)
      clust_stat_df%>%group_by(cluster_n)%>%
        dplyr::summarise(start=min(start),end=max(end),
                         stat=sum(stat),.groups='drop_last')->clust_stat_df
    }
    if(!is.null(clust_stat_df)&coll_clust){
      clust_stat_df$sign <- as.factor(sign(clust_stat_df$stat))
      clust_stat_df%>%group_by(sign)%>%summarise(cluster_n=min(cluster_n),start=min(start),end=max(end),stat=sum(stat))->clust_stat_df
      clust_stat_df%>%select(-sign)->clust_stat_df
    }
    return(data.frame(clust_stat_df))
  }
  
  # Empirical distribution #
  empirical_stat_time_course <- get_stat_time_course(df,random,chance,to_test,bayes,robust_lm,categorical_VI)
  empirical_cluster <- get_clust_stats(empirical_stat_time_course,gap,coll_clust)
  main_clust <- empirical_cluster[which.max(abs(empirical_cluster$stat)),]
  p1 <- ggplot(empirical_stat_time_course) + theme_minimal(base_size=25) + xlab('Time') + ylab('Statistic') +
    geom_line(aes(x=time_sample,y=stat_stats)) + geom_hline(yintercept=c(thres,-thres),linetype="dashed") +
    geom_vline(xintercept=c(main_clust$start-tbin,main_clust$end-tbin),linetype="dashed")
  p2 <- ggplot(empirical_stat_time_course) + theme_minimal(base_size=25) + xlab('Time') + ylab('P-value') +
    geom_line(aes(x=time_sample,y=stat_pval)) + geom_hline(yintercept=c(alpha),linetype="dashed") +
    geom_vline(xintercept=c(main_clust$start-tbin,main_clust$end-tbin),linetype="dashed")
  gridExtra::grid.arrange(p1,p2,nrow=1)
  
  # Permutations test #
  if(!bayes){
    foreach(sample=1:n_samples,.combine=c,.packages=c('doParallel','foreach','dplyr')) %dopar% {
      # for (time_sample in 1:200) {
      perm_df <- get_permutated_time_course(df,random,chance,to_shuffle)
      perm_stat_time_course <- get_stat_time_course(perm_df,random,chance,to_test,bayes=F,robust_lm,categorical_VI)
      perm_cluster <- get_clust_stats(perm_stat_time_course,gap,coll_clust)
      # print(perm_cluster)
      if (ncol(perm_cluster)>0) max_clust <- perm_cluster$stat[which.max(abs(perm_cluster$stat))] else max_clust <- 0
    } -> max_stat_list
    empirical_cluster$p_value <- lapply(empirical_cluster$stat,function(X) length(which(max_stat_list > abs(X)))/n_samples)
    print(empirical_cluster)}
  
  # Save results #
  perm_test_rslts <- list(
    clusters=empirical_cluster,
    parameters=c(to_test=to_test,to_shuffle=to_shuffle,tbin=tbin,
                 statistic=stat_name,alpha=alpha,thres=thres,n_samples=n_samples,
                 gap=gap,coll_clust=coll_clust,random=random,bayes=bayes,categorical_VI=categorical_VI))
  return(perm_test_rslts)
}
timecourse_age_LT <- function(time_prop_df,alpha=.05,thres=NA,clustermass=F,n_samples=5,age_continuous=T){
  # time_prop_df<-ET_diff_df
  ## Factor definition
  time_prop_df%>%mutate(AOI='Difference',age=as.numeric(age))->time_prop_df
  print(plot(time_prop_df,predictor='group')+theme_minimal(base_size=15))
  ## ANOVA
  # http://ordination.okstate.edu/MULTIPLE.htm
  # df = N participant - N predictor in regression, i.e. number of age group
  if(is.na(thres)) thres=qt(p=alpha/2,df=length(unique(time_prop_df$ID))-length(unique(time_prop_df$group)),lower.tail=F)
  if(!age_continuous){
    time_clust_df<-eyetrackingR::make_time_cluster_data(
      data=time_prop_df,
      aoi='Difference',predictor_column='group',
      test='lm',formula=Prop~group,treatment_level='.L',
      threshold=thres)
    print(plot(time_clust_df)+theme_minimal(base_size=15))
    if(clustermass){
      tc_analysis <- eyetrackingR::analyze_time_clusters(
        data=time_clust_df,
        shuffle_by=c("group"),within_subj=F,n_samples=n_samples,treatment_level='.L',parallel=T)
      return(tc_analysis)}
    else{return(time_clust_df)}
  } else {
    time_clust_df<-eyetrackingR::make_time_cluster_data(
      data=time_prop_df,
      aoi='Difference',predictor_column='age',
      test='lm',formula=Prop~age,treatment_level='',
      threshold=thres)
    print(plot(time_clust_df)+theme_minimal(base_size=15))
    if(clustermass){
      tc_analysis <- eyetrackingR::analyze_time_clusters(
        data=time_clust_df,
        shuffle_by=c("age"),within_subj=F,n_samples=n_samples,parallel=T)
      return(tc_analysis)}
    else{return(time_clust_df)}
  }
}
#
#
#### DATA ####

## Stimuli ##
stim_size <- read.table(paste0(getwd(), '/STIMULI/PLT/HUMAN/bodies_distance.csv'),header=T,sep=',')
t.test(stim_size$facing,stim_size$nonfacing,var.equal=T)

## 7mo bodies ##
filenames <- list.files(paste0(getwd(), '/data_raw/plt_7mo'), pattern='*.topd')
foreach (f=filenames, .combine=rbind.data.frame,.packages='dplyr') %dopar% {
  # f=filenames[1]
  # ID <- gsub(".*-(.+)-.*", "\\1", f)
  ID <- gsub("-.*", "\\1", f)
  # print(f)
  # print(ID)
  raw.data <- data.frame(cbind(f, read.table(paste(getwd(),'/data_raw/plt_7mo/', f, sep=''), sep="\t")[1:28]))
  colnames(raw.data) <- list('ID', '#T2TGazeTime', 'RemoteTime', 'LineNum', 'GazeCounter', 'GazepointX (L)', 'GazepointY (L)', 'CamX (L)', 'CamY (L)', 'Distance (L)', 'Pupil (L)', 'Validity (L)', 'GazepointX (R)', 'GazepointY (R)', 'CamX (R)', 'CamY (R)', 'Distance (R)', 'Pupil (R)', 'Validity (R)', 'GazepointX (M)', 'GazepointY (M)', 'Pupil (M)', 'Validity (S)', 'FixationID', 'FixationX', 'FixationY', 'FixationConfidence', 'ev', 'correctside')
  
  ## Include events in the eye-T dataframe ##
  raw.data %>% mutate(ev.n=ifelse(ev == 'p' & lag(ev) == 'f', 1,
                                  ifelse(ev == 'f' & lag(ev) == 'p', 2, 0))) -> raw.data
  raw.data$Trial <- NA
  events.start <- which(raw.data$ev.n == 1)
  events.stop <- c(which(raw.data$ev.n == 2), nrow(raw.data)+1)
  for (event in 1:length(events.start)) {
    # event=48
    low=events.start[event]
    high=events.stop[event]-1
    raw.data$Trial[low:high] <- event
  }
  
  ## Add column with recording duration ##
  raw.data$duration <- c(diff(raw.data$`LineNum`), as.integer(mean(diff(raw.data$`LineNum`))))
  
  ## Subset trials ##
  raw.data$group <- '7mo_bodies'
  raw.data$file <- f
  raw.data$age <- get_age(f,'months')
  subset(raw.data, ev == 'p') %>%
    select('group', 'file', 'ID', 'age', 'Trial',
           'LineNum', 'RemoteTime', 'duration', 'ev.n',
           'FixationX', 'FixationY', 'GazepointX (M)', 'GazepointY (M)', 'Pupil (M)', 'FixationID',
           'GazepointX (L)', 'GazepointY (L)', 'Distance (L)', 'GazepointX (R)', 'GazepointY (R)', 'Distance (R)',
           'correctside')} -> bigDF.7mo.bodies
bigDF.7mo.bodies$ID <- as.factor(bigDF.7mo.bodies$ID)
levels(bigDF.7mo.bodies$ID) <- seq(1,length(unique(bigDF.7mo.bodies$ID)))
info_7mo_bodies <- readxl::read_xlsx(paste0(getwd(),'/DATA_RAW/plt_7mo/infos_bb.xlsx'))
unique(bigDF.7mo.bodies$file)
unique(info_7mo_bodies$file)
info_7mo_bodies$age_weeks <- difftime(info_7mo_bodies$test,info_7mo_bodies$birth,units="weeks")
info_7mo_bodies$age_days <- difftime(info_7mo_bodies$test,info_7mo_bodies$birth,units="days") # Warnings from this code 
table(info_7mo_bodies$inclusion)
table(info_7mo_bodies$gender[info_7mo_bodies$inclusion==1])
min(info_7mo_bodies$months[info_7mo_bodies$inclusion==1])
max(info_7mo_bodies$months[info_7mo_bodies$inclusion==1])
(days_7mo <- round(psych::describe(as.numeric(info_7mo_bodies$age_days[info_7mo_bodies$inclusion==1]))))
as.Date('30/11/1999','%d/%m/%Y') + days_7mo$mean # from the rational of =DATEDIF(DATE(2020,0,0),DATE(2020,0,L27),"m")&" mois, et "&DATEDIF(DATE(2020,0,0),DATE(2020,0,L27),"md")&" jour."
info_7mo_bodies%>%mutate(withStudy=ifelse(orderStudy=='withStudy',1,0))%>%select(file,withStudy,age_days,age_weeks)->info_7mo_bodies
bigDF.7mo.bodies%>%left_join(info_7mo_bodies,by=c('file'))->bigDF.7mo.bodies #  This line does not run
sum(is.na(bigDF.7mo.bodies$age_days))

## 10mo ##
filenames <- list.files(paste0(getwd(), '/data_raw/plt_10mo'), pattern='*.topd')
foreach (f=filenames, .combine=rbind.data.frame,.packages='dplyr') %dopar% {
  # for (f in filenames) {
  # f=filenames[1]
  # ID <- gsub(".*-(.+)-.*", "\\1", f)
  ID <- gsub("-.*", "\\1", f)
  # print(f)
  # print(ID)
  raw.data <- data.frame(cbind(f, read.table(paste(getwd(),'/data_raw/plt_10mo/', f, sep=''), sep="\t")[1:28]))
  colnames(raw.data) <- list('ID', '#T2TGazeTime', 'RemoteTime', 'LineNum', 'GazeCounter', 'GazepointX (L)', 'GazepointY (L)', 'CamX (L)', 'CamY (L)', 'Distance (L)', 'Pupil (L)', 'Validity (L)', 'GazepointX (R)', 'GazepointY (R)', 'CamX (R)', 'CamY (R)', 'Distance (R)', 'Pupil (R)', 'Validity (R)', 'GazepointX (M)', 'GazepointY (M)', 'Pupil (M)', 'Validity (S)', 'FixationID', 'FixationX', 'FixationY', 'FixationConfidence', 'ev', 'correctside')
  
  ## Include events in the eye-T dataframe ##
  raw.data %>% mutate(ev.n=ifelse(ev == 'p' & lag(ev) == 'f', 1,
                                  ifelse(ev == 'f' & lag(ev) == 'p', 2, 0))) -> raw.data
  raw.data$Trial <- NA
  events.start <- which(raw.data$ev.n == 1)
  events.stop <- c(which(raw.data$ev.n == 2), nrow(raw.data)+1)
  for (event in 1:length(events.start)) {
    # event=48
    low=events.start[event]
    high=events.stop[event]-1
    raw.data$Trial[low:high] <- event
  }
  
  ## Add column with recording duration ##
  raw.data$duration <- c(diff(raw.data$`LineNum`), as.integer(mean(diff(raw.data$`LineNum`))))
  
  ## Subset trials ##
  raw.data$group <- '10mo_bodies'
  raw.data$file <- f
  raw.data$age <- get_age(f,'months')
  subset(raw.data, ev == 'p') %>%
    select('group', 'file', 'ID', 'age', 'Trial',
           'LineNum', 'RemoteTime', 'duration', 'ev.n',
           'FixationX', 'FixationY', 'GazepointX (M)', 'GazepointY (M)', 'Pupil (M)', 'FixationID',
           'GazepointX (L)', 'GazepointY (L)', 'Distance (L)', 'GazepointX (R)', 'GazepointY (R)', 'Distance (R)',
           'correctside')} -> bigDF.10mo.bodies
bigDF.10mo.bodies$ID <- as.factor(bigDF.10mo.bodies$ID)
levels(bigDF.10mo.bodies$ID) <- seq(max(as.numeric(as.character((bigDF.7mo.bodies$ID))))+1,
                                    max(as.numeric(as.character((bigDF.7mo.bodies$ID))))+
                                      length(unique(bigDF.10mo.bodies$ID)))
info_10mo_bodies <- readxl::read_xlsx(paste0(getwd(),'/data_raw/plt_10mo/infos_bb.xlsx'))
unique(bigDF.10mo.bodies$file)
unique(info_10mo_bodies$file)
info_10mo_bodies$age_weeks <- difftime(info_10mo_bodies$test,info_10mo_bodies$birth,units="weeks")
info_10mo_bodies$age_days <- difftime(info_10mo_bodies$test,info_10mo_bodies$birth,units="days")
table(info_10mo_bodies$inclusion)
table(info_10mo_bodies$gender[info_10mo_bodies$inclusion==1])
min(info_10mo_bodies$months[info_10mo_bodies$inclusion==1])
max(info_10mo_bodies$months[info_10mo_bodies$inclusion==1])
(days_10mo <- round(psych::describe(as.numeric(info_10mo_bodies$age_days[info_10mo_bodies$inclusion==1]))))
as.Date('30/11/1999','%d/%m/%Y') + days_10mo$mean # from the rational of =DATEDIF(DATE(2020,0,0),DATE(2020,0,L27),"m")&" mois, et "&DATEDIF(DATE(2020,0,0),DATE(2020,0,L27),"md")&" jour."
info_10mo_bodies%>%mutate(withStudy=ifelse(orderStudy=='withStudy',1,0))%>%select(file,withStudy,age_days,age_weeks)->info_10mo_bodies
bigDF.10mo.bodies%>%left_join(info_10mo_bodies,by=c('file'))->bigDF.10mo.bodies
sum(is.na(bigDF.10mo.bodies$age_days))

## 15mo ##
filenames <- list.files(paste0(getwd(), '/data_raw/plt_15mo'), pattern='*.topd')
foreach (f=filenames, .combine=rbind.data.frame,.packages='dplyr') %dopar% {
  # f=filenames[1]
  # ID <- gsub(".*-(.+)-.*", "\\1", f)
  ID <- gsub("-.*", "\\1", f)
  print(f)
  print(ID)
  raw.data <- data.frame(cbind(f, read.table(paste(getwd(),'/data_raw/plt_15mo/', f, sep=''), sep="\t")[1:28]))
  colnames(raw.data) <- list('ID', '#T2TGazeTime', 'RemoteTime', 'LineNum', 'GazeCounter', 'GazepointX (L)', 'GazepointY (L)', 'CamX (L)', 'CamY (L)', 'Distance (L)', 'Pupil (L)', 'Validity (L)', 'GazepointX (R)', 'GazepointY (R)', 'CamX (R)', 'CamY (R)', 'Distance (R)', 'Pupil (R)', 'Validity (R)', 'GazepointX (M)', 'GazepointY (M)', 'Pupil (M)', 'Validity (S)', 'FixationID', 'FixationX', 'FixationY', 'FixationConfidence', 'ev', 'correctside')
  
  ## Include events in the eye-T dataframe ##
  raw.data %>% mutate(ev.n=ifelse(ev == 'p' & lag(ev) == 'f', 1,
                                  ifelse(ev == 'f' & lag(ev) == 'p', 2, 0))) -> raw.data
  raw.data$Trial <- NA
  events.start <- which(raw.data$ev.n == 1)
  events.stop <- c(which(raw.data$ev.n == 2), nrow(raw.data)+1)
  for (event in 1:length(events.start)) {
    # event=48
    low=events.start[event]
    high=events.stop[event]-1
    raw.data$Trial[low:high] <- event
  }
  
  ## Add column with recording duration ##
  raw.data$duration <- c(diff(raw.data$`LineNum`), as.integer(mean(diff(raw.data$`LineNum`))))
  
  ## Subset trials ##
  raw.data$group <- '15mo_bodies'
  raw.data$file <- f
  raw.data$age <- get_age(f,'months')
  subset(raw.data, ev == 'p') %>%
    select('group', 'file', 'ID', 'age', 'Trial',
           'LineNum', 'RemoteTime', 'duration', 'ev.n',
           'FixationX', 'FixationY', 'GazepointX (M)', 'GazepointY (M)', 'Pupil (M)', 'FixationID',
           'GazepointX (L)', 'GazepointY (L)', 'Distance (L)', 'GazepointX (R)', 'GazepointY (R)', 'Distance (R)',
           'correctside')} -> bigDF.15mo.bodies
bigDF.15mo.bodies$ID <- as.factor(bigDF.15mo.bodies$ID) 
levels(bigDF.15mo.bodies$ID) <- seq(max(as.numeric(as.character((bigDF.10mo.bodies$ID))))+1,
                                    max(as.numeric(as.character((bigDF.10mo.bodies$ID))))+
                                      length(unique(bigDF.15mo.bodies$ID))) # 19-2-1524 = 95 (reported as fussy)
info_15mo_bodies <- readxl::read_xlsx(paste0('data_raw/plt_15mo/infos_bb.xlsx'))
unique(bigDF.15mo.bodies$file)
unique(info_15mo_bodies$file)
info_15mo_bodies$age_weeks <- difftime(info_15mo_bodies$test,info_15mo_bodies$birth,units="weeks")
info_15mo_bodies$age_days <- difftime(info_15mo_bodies$test,info_15mo_bodies$birth,units="days")
table(info_15mo_bodies$inclusion)
table(info_15mo_bodies$gender[info_15mo_bodies$inclusion==1])
min(info_15mo_bodies$months[info_15mo_bodies$inclusion==1])
max(info_15mo_bodies$months[info_15mo_bodies$inclusion==1])
(days_15mo <- round(psych::describe(as.numeric(info_15mo_bodies$age_days[info_15mo_bodies$inclusion==1]))))
as.Date('30/11/1999','%d/%m/%Y') + days_15mo$mean # from the rational of =DATEDIF(DATE(2020,0,0),DATE(2020,0,L27),"m")&" mois, et "&DATEDIF(DATE(2020,0,0),DATE(2020,0,L27),"md")&" jour."
info_15mo_bodies%>%mutate(withStudy=ifelse(orderStudy=='withStudy',1,0))%>%select(file,withStudy,age_days,age_weeks)->info_15mo_bodies
bigDF.15mo.bodies%>%left_join(info_15mo_bodies,by=c('file'))->bigDF.15mo.bodies
sum(is.na(bigDF.15mo.bodies$age_days))

## 18mo ##
filenames <- list.files(paste0(getwd(), '/data_raw/plt_18mo'), pattern='*.topd')
foreach (f=filenames, .combine=rbind.data.frame,.packages='dplyr') %dopar% {
  # f=filenames[1]
  # ID <- gsub(".*-(.+)-.*", "\\1", f)
  ID <- gsub("-.*", "\\1", f)
  print(f)
  print(ID)
  raw.data <- data.frame(cbind(f, read.table(paste(getwd(),'/data_raw/plt_18mo/', f, sep=''), sep="\t")[1:28]))
  colnames(raw.data) <- list('ID', '#T2TGazeTime', 'RemoteTime', 'LineNum', 'GazeCounter', 'GazepointX (L)', 'GazepointY (L)', 'CamX (L)', 'CamY (L)', 'Distance (L)', 'Pupil (L)', 'Validity (L)', 'GazepointX (R)', 'GazepointY (R)', 'CamX (R)', 'CamY (R)', 'Distance (R)', 'Pupil (R)', 'Validity (R)', 'GazepointX (M)', 'GazepointY (M)', 'Pupil (M)', 'Validity (S)', 'FixationID', 'FixationX', 'FixationY', 'FixationConfidence', 'ev', 'correctside')
  
  ## Include events in the eye-T dataframe ##
  raw.data %>% mutate(ev.n=ifelse(ev == 'p' & lag(ev) == 'f', 1,
                                  ifelse(ev == 'f' & lag(ev) == 'p', 2, 0))) -> raw.data
  raw.data$Trial <- NA
  events.start <- which(raw.data$ev.n == 1)
  events.stop <- c(which(raw.data$ev.n == 2), nrow(raw.data)+1)
  for (event in 1:length(events.start)) {
    # event=48
    low=events.start[event]
    high=events.stop[event]-1
    raw.data$Trial[low:high] <- event
  }
  
  ## Add column with recording duration ##
  raw.data$duration <- c(diff(raw.data$`LineNum`), as.integer(mean(diff(raw.data$`LineNum`))))
  
  ## Subset trials ##
  raw.data$group <- '18mo_bodies'
  raw.data$file <- f
  raw.data$age <- get_age(f,'months')
  subset(raw.data, ev == 'p') %>%
    select('group', 'file', 'ID', 'age', 'Trial',
           'LineNum', 'RemoteTime', 'duration', 'ev.n',
           'FixationX', 'FixationY', 'GazepointX (M)', 'GazepointY (M)', 'Pupil (M)', 'FixationID',
           'GazepointX (L)', 'GazepointY (L)', 'Distance (L)', 'GazepointX (R)', 'GazepointY (R)', 'Distance (R)',
           'correctside')} -> bigDF.18mo.bodies
bigDF.18mo.bodies$ID <- as.factor(bigDF.18mo.bodies$ID)
levels(bigDF.18mo.bodies$ID) <- seq(max(as.numeric(as.character((bigDF.15mo.bodies$ID))))+1,
                                    max(as.numeric(as.character((bigDF.15mo.bodies$ID))))+
                                      length(unique(bigDF.18mo.bodies$ID)))
info_18mo_bodies <- readxl::read_xlsx(paste0('data_raw/plt_18mo/infos_bb.xlsx'))
unique(bigDF.18mo.bodies$file)
unique(info_18mo_bodies$file)
info_18mo_bodies$age_weeks <- difftime(info_18mo_bodies$test,info_18mo_bodies$birth,units="weeks")
info_18mo_bodies$age_days <- difftime(info_18mo_bodies$test,info_18mo_bodies$birth,units="days")
table(info_18mo_bodies$inclusion)
table(info_18mo_bodies$gender[info_18mo_bodies$inclusion==1])
min(info_18mo_bodies$months[info_18mo_bodies$inclusion==1])
max(info_18mo_bodies$months[info_18mo_bodies$inclusion==1])
(days_18mo <- round(psych::describe(as.numeric(info_18mo_bodies$age_days[info_18mo_bodies$inclusion==1]))))
as.Date('30/11/1999','%d/%m/%Y') + days_18mo$mean # from the rational of =DATEDIF(DATE(2020,0,0),DATE(2020,0,L27),"m")&" mois, et "&DATEDIF(DATE(2020,0,0),DATE(2020,0,L27),"md")&" jour."
info_18mo_bodies%>%mutate(withStudy=ifelse(orderStudy=='withStudy',1,0))%>%select(file,withStudy,age_days,age_weeks)->info_18mo_bodies
bigDF.18mo.bodies%>%left_join(info_18mo_bodies,by=c('file'))->bigDF.18mo.bodies
sum(is.na(info_18mo_bodies$age_days))

## 3yo ##
filenames <- list.files(paste0(getwd(), '/data_raw/plt_3yo'), pattern='*.topd')
foreach (f=filenames, .combine=rbind.data.frame,.packages='dplyr') %dopar% {
  # f=filenames[1]
  # ID <- gsub(".*-(.+)-.*", "\\1", f)
  ID <- gsub("-.*", "\\1", f)
  print(f)
  print(ID)
  raw.data <- data.frame(cbind(f, read.table(paste(getwd(),'/data_raw/plt_3yo/', f, sep=''), sep="\t")[1:28]))
  colnames(raw.data) <- list('ID', '#T2TGazeTime', 'RemoteTime', 'LineNum', 'GazeCounter', 'GazepointX (L)', 'GazepointY (L)', 'CamX (L)', 'CamY (L)', 'Distance (L)', 'Pupil (L)', 'Validity (L)', 'GazepointX (R)', 'GazepointY (R)', 'CamX (R)', 'CamY (R)', 'Distance (R)', 'Pupil (R)', 'Validity (R)', 'GazepointX (M)', 'GazepointY (M)', 'Pupil (M)', 'Validity (S)', 'FixationID', 'FixationX', 'FixationY', 'FixationConfidence', 'ev', 'correctside')
  
  ## Include events in the eye-T dataframe ##
  raw.data %>% mutate(ev.n=ifelse(ev == 'p' & lag(ev) == 'f', 1,
                                  ifelse(ev == 'f' & lag(ev) == 'p', 2, 0))) -> raw.data
  raw.data$Trial <- NA
  events.start <- which(raw.data$ev.n == 1)
  events.stop <- c(which(raw.data$ev.n == 2), nrow(raw.data)+1)
  for (event in 1:length(events.start)) {
    # event=48
    low=events.start[event]
    high=events.stop[event]-1
    raw.data$Trial[low:high] <- event
  }
  
  ## Add column with recording duration ##
  raw.data$duration <- c(diff(raw.data$`LineNum`), as.integer(mean(diff(raw.data$`LineNum`))))
  
  ## Subset trials ##
  raw.data$group <- '3yo_bodies'
  raw.data$file <- f
  raw.data$age <- get_age(f,'months')
  subset(raw.data, ev == 'p') %>%
    select('group', 'file', 'ID', 'age', 'Trial',
           'LineNum', 'RemoteTime', 'duration', 'ev.n',
           'FixationX', 'FixationY', 'GazepointX (M)', 'GazepointY (M)', 'Pupil (M)', 'FixationID',
           'GazepointX (L)', 'GazepointY (L)', 'Distance (L)', 'GazepointX (R)', 'GazepointY (R)', 'Distance (R)',
           'correctside')} -> bigDF.3yo.bodies
bigDF.3yo.bodies$ID <- as.factor(bigDF.3yo.bodies$ID)
levels(bigDF.3yo.bodies$ID) <- seq(max(as.numeric(as.character((bigDF.18mo.bodies$ID))))+1,
                                   max(as.numeric(as.character((bigDF.18mo.bodies$ID))))+
                                     length(unique(bigDF.3yo.bodies$ID)))
info_3yo_bodies <- readxl::read_xlsx(paste0(getwd(),'/data_raw/plt_3yo/infos_bb.xlsx'))
unique(bigDF.3yo.bodies$file)
unique(info_3yo_bodies$file)
table(info_3yo_bodies$inclusion)
table(info_3yo_bodies$gender[info_3yo_bodies$inclusion==1])
info_3yo_bodies$age_weeks <- round(difftime(info_3yo_bodies$test,info_3yo_bodies$birth,units="weeks"))
info_3yo_bodies$age_days <- round(difftime(info_3yo_bodies$test,info_3yo_bodies$birth,units="days"))
min(info_3yo_bodies$months[info_3yo_bodies$inclusion==1])
max(info_3yo_bodies$months[info_3yo_bodies$inclusion==1])
(days_3yo <- round(psych::describe(as.numeric(info_3yo_bodies$age_days[info_3yo_bodies$inclusion==1]))))
as.Date('30/11/1999','%d/%m/%Y') + days_3yo$mean # from the rational of =DATEDIF(DATE(2020,0,0),DATE(2020,0,L27),"m")&" mois, et "&DATEDIF(DATE(2020,0,0),DATE(2020,0,L27),"md")&" jour."
info_3yo_bodies%>%mutate(withStudy=ifelse(orderStudy=='withStudy',1,0))%>%select(file,withStudy,age_days,age_weeks)->info_3yo_bodies
bigDF.3yo.bodies%>%left_join(info_3yo_bodies,by=c('file'))->bigDF.3yo.bodies
sum(is.na(info_3yo_bodies$age_days))
bigDF.3yo.bodies%>%group_by(file,ID)%>%summarise(age=unique(age),age_days=unique(age_days))

## 5yo ##
filenames <- list.files(paste0(getwd(), '/data_raw/plt_5yo'), pattern='*.topd')
foreach (f=filenames, .combine=rbind.data.frame,.packages='dplyr') %dopar% {
  # f=filenames[1]
  # ID <- gsub(".*-(.+)-.*", "\\1", f)
  ID <- gsub("-.*", "\\1", f)
  print(f)
  print(ID)
  raw.data <- data.frame(cbind(f, read.table(paste(getwd(),'/data_raw/plt_5yo/', f, sep=''), sep="\t")[1:28]))
  colnames(raw.data) <- list('ID', '#T2TGazeTime', 'RemoteTime', 'LineNum', 'GazeCounter', 'GazepointX (L)', 'GazepointY (L)', 'CamX (L)', 'CamY (L)', 'Distance (L)', 'Pupil (L)', 'Validity (L)', 'GazepointX (R)', 'GazepointY (R)', 'CamX (R)', 'CamY (R)', 'Distance (R)', 'Pupil (R)', 'Validity (R)', 'GazepointX (M)', 'GazepointY (M)', 'Pupil (M)', 'Validity (S)', 'FixationID', 'FixationX', 'FixationY', 'FixationConfidence', 'ev', 'correctside')
  
  ## Include events in the eye-T dataframe ##
  raw.data %>% mutate(ev.n=ifelse(ev == 'p' & lag(ev) == 'f', 1,
                                  ifelse(ev == 'f' & lag(ev) == 'p', 2, 0))) -> raw.data
  raw.data$Trial <- NA
  events.start <- which(raw.data$ev.n == 1)
  events.stop <- c(which(raw.data$ev.n == 2), nrow(raw.data)+1)
  for (event in 1:length(events.start)) {
    # event=48
    low=events.start[event]
    high=events.stop[event]-1
    raw.data$Trial[low:high] <- event
  }
  
  ## Add column with recording duration ##
  raw.data$duration <- c(diff(raw.data$`LineNum`), as.integer(mean(diff(raw.data$`LineNum`))))
  
  ## Subset trials ##
  raw.data$group <- '5yo_bodies'
  raw.data$file <- f
  raw.data$age <- get_age(f,'months')
  subset(raw.data, ev == 'p') %>%
    select('group', 'file', 'ID', 'age', 'Trial',
           'LineNum', 'RemoteTime', 'duration', 'ev.n',
           'FixationX', 'FixationY', 'GazepointX (M)', 'GazepointY (M)', 'Pupil (M)', 'FixationID',
           'GazepointX (L)', 'GazepointY (L)', 'Distance (L)', 'GazepointX (R)', 'GazepointY (R)', 'Distance (R)',
           'correctside')} -> bigDF.5yo.bodies
bigDF.5yo.bodies$ID <- as.factor(bigDF.5yo.bodies$ID)
levels(bigDF.5yo.bodies$ID) <- seq(max(as.numeric(as.character((bigDF.3yo.bodies$ID))))+1,
                                   max(as.numeric(as.character((bigDF.3yo.bodies$ID))))+
                                     length(unique(bigDF.5yo.bodies$ID)))
info_5yo_bodies <- readxl::read_xlsx(paste0(getwd(),'/data_raw/plt_3yo/infos_bb.xlsx'))
unique(bigDF.5yo.bodies$file)
unique(info_5yo_bodies$file)
setdiff(unique(bigDF.5yo.bodies$file),unique(info_5yo_bodies$file))
table(info_5yo_bodies$inclusion)
table(info_5yo_bodies$gender[info_5yo_bodies$inclusion==1])
info_5yo_bodies$age_weeks <- round(difftime(info_5yo_bodies$test,info_5yo_bodies$birth,units="weeks"))
info_5yo_bodies$age_days <- round(difftime(info_5yo_bodies$test,info_5yo_bodies$birth,units="days"))
min(info_5yo_bodies$months[info_5yo_bodies$inclusion==1])
max(info_5yo_bodies$months[info_5yo_bodies$inclusion==1])
(days_5yo <- round(psych::describe(as.numeric(info_5yo_bodies$age_days[info_5yo_bodies$inclusion==1]))))
as.Date('30/11/1999','%d/%m/%Y') + days_5yo$mean # from the rational of =DATEDIF(DATE(2020,0,0),DATE(2020,0,L27),"m")&" mois, et "&DATEDIF(DATE(2020,0,0),DATE(2020,0,L27),"md")&" jour."
info_5yo_bodies%>%mutate(withStudy=ifelse(orderStudy=='withStudy',1,0))%>%select(file,withStudy,age_days,age_weeks)->info_5yo_bodies
bigDF.5yo.bodies%>%left_join(info_5yo_bodies,by=c('file'))->bigDF.5yo.bodies
sum(is.na(info_5yo_bodies$age_days))
bigDF.5yo.bodies%>%group_by(ID)%>%summarise(age=unique(age),age_days=unique(age_days))
unique(bigDF.5yo.bodies$ID[is.na(bigDF.5yo.bodies$age_days)])

## adults ##
filenames <- list.files(paste0(getwd(), '/data_raw/plt_adult'), pattern='*.topd')
foreach (f=filenames, .combine=rbind.data.frame,.packages='dplyr') %dopar% {
  # f=filenames[1]
  ID <- gsub(".*-(.+)-.*", "\\1", f)
  # ID <- gsub("-.*", "\\1", f)
  # print(f)
  # print(ID)
  raw.data <- data.frame(cbind(ID, read.table(paste0(getwd(), '/data_raw/plt_adult', '/', f), sep="\t"))[1:31])
  colnames(raw.data) <- 
    c('ID', '#T2TGazeTime', 'RemoteTime', 'LineNum', 'GazeCounter', 'GazepointX (L)', 'GazepointY (L)', 'CamX (L)', 'CamY (L)', 'Distance (L)', 'Pupil (L)', 'Validity (L)', 'GazepointX (R)', 'GazepointY (R)', 'CamX (R)', 'CamY (R)', 'Distance (R)', 'Pupil (R)', 'Validity (R)', 'GazepointX (M)', 'GazepointY (M)', 'Pupil (M)', 'Validity (S)', 'FixationID', 'FixationX', 'FixationY', 'FixationConfidence', 'ev', 'correctside', 'condition', 'ssresponse')
  
  ## Stimuli lists ##
  stim.list.part1 <- cbind('part1', read.table(paste0(getwd(),'/data_raw/plt_adult/stim_list/xp_part1/list', ID), sep='\t'))
  colnames(stim.list.part1) <- c('part','stim1', 'stim2', 'facingside', 'condition')
  stim.list.part2 <- cbind('part2', NA, read.table(paste0(getwd(),'/data_raw/plt_adult/stim_list/xp_part2/list', ID), sep='\t'))
  colnames(stim.list.part2) <- c('part','stim1', 'stim2', 'facingside', 'condition')
  stim.list <- rbind(stim.list.part1, stim.list.part2[1:16,])
  
  ## Include events and stimuli in the eye-T dataframe ##
  raw.data %>% mutate(ev.n=ifelse(ev == 'p' & lag(ev) == 'f', 1,
                                  ifelse(ev == 'f' & lag(ev) == 'p', 2, 0))) -> raw.data
  raw.data$stim1 <- NA
  raw.data$stim2 <- NA
  raw.data$Trial <- NA
  events.start <- which(raw.data$ev.n == 1)
  events.stop <- c(which(raw.data$ev.n == 2), nrow(raw.data)+1)
  for (event in 1:length(events.start)) {
    # event=48
    low=events.start[event]
    high=events.stop[event]-1
    raw.data$Trial[low:high] <- event
    raw.data$stim1[low:high] <- as.character(stim.list$stim1[event])
    raw.data$stim2[low:high] <- as.character(stim.list$stim2[event])
  }
  
  ## Add column with recording duration ##
  raw.data$duration <- c(diff(raw.data$`LineNum`), as.integer(mean(diff(raw.data$`LineNum`))))
  
  ## Reorder condition ##
  raw.data <- subset(raw.data, ev == 'p' & !is.na(stim1))
  raw.data$age <- 'adults'
  raw.data$group <- ifelse(raw.data$condition==1,'adults_up','adults_inv')
  
  ## Reorder dataframe ##
  raw.data$file <- f
  raw.data %>%
    select('group', 'file', 'ID', 'age', 'Trial',
           'LineNum', 'RemoteTime', 'duration', 'ev.n',
           'FixationX', 'FixationY', 'GazepointX (M)', 'GazepointY (M)', 'Pupil (M)', 'FixationID',
           'GazepointX (L)', 'GazepointY (L)', 'Distance (L)', 'GazepointX (R)', 'GazepointY (R)', 'Distance (R)',
           'correctside')} -> bigDF.adults
bigDF.adults$ID <- as.factor(bigDF.adults$ID)
levels(bigDF.adults$ID) <- seq(max(as.numeric(as.character((bigDF.5yo.bodies$ID))))+1,
                               max(as.numeric(as.character((bigDF.5yo.bodies$ID))))+
                                 length(unique(bigDF.adults$ID)))
bigDF.adults$withStudy <- 0
bigDF.adults$age_days <- NA
bigDF.adults$age_weeks <- NA
info_adults <- readxl::read_xlsx(paste0(getwd(),'/data_raw/plt_adult/infos_adults.xlsx'))
unique(bigDF.adults$file)
unique(info_adults$file)
setdiff(unique(bigDF.adults$file),unique(info_adults$file))
table(info_adults$inclusion)
table(info_adults$gender[info_adults$inclusion==1])
bigDF.adults$age_days <- NA
bigDF.adults$age_weeks <- NA
round(psych::describe(info_adults$year[info_adults$inclusion==1]),2)
#
#
#### DATA NO WAKU ####

## combine ##
bigDF <- rbind.data.frame(bigDF.7mo.bodies,bigDF.10mo.bodies,bigDF.15mo.bodies,bigDF.18mo.bodies,bigDF.3yo.bodies,bigDF.5yo.bodies,bigDF.adults)
# save(bigDF,file='preprocessed_data/bigDF_timecoursesv11.RData')
# load('preprocessed_data/bigDF_timecoursesv11.RData')

# Shortcut preprocessig #
bigDF_trial_summary <- aggregate(Trial~group+ID, FUN=function(x)length(unique(x)), data=bigDF)
bigDF_trial_summary%>%group_by(group)%>%tally


# Interpolate missing values # 
bigDF[bigDF == -10000] <- NA
bigDF%>%group_by(ID, Trial) %>%
  mutate(FixationY=zoo::na.approx(FixationY, maxgap=5, rule=2),
         FixationX=zoo::na.approx(FixationX, maxgap=5, rule=2),
         `Distance (L)`=zoo::na.approx(`Distance (L)`, maxgap=5, rule=2)) -> bigDF

## Set variables ##
bigDF <- add_aoi(bigDF, data.frame(x_min_col=c(0), y_min_col=c(325), x_max_col=c(784), y_max_col=c(875)),
                 aoi_name="left_side", x_col="FixationX", y_col="FixationY", x_min_col="x_min_col", x_max_col="x_max_col", y_min_col="y_min_col", y_max_col="y_max_col")
bigDF <- add_aoi(bigDF, data.frame(x_min_col=c(785), y_min_col=c(325), x_max_col=c(1135), y_max_col=c(875)),
                 aoi_name="center", x_col="FixationX", y_col="FixationY", x_min_col="x_min_col", x_max_col="x_max_col", y_min_col="y_min_col", y_max_col="y_max_col")
bigDF <- add_aoi(bigDF, data.frame(x_min_col=c(1136), y_min_col=c(325), x_max_col=c(1920), y_max_col=c(875)),
                 aoi_name="right_side", x_col="FixationX", y_col="FixationY", x_min_col="x_min_col", x_max_col="x_max_col", y_min_col="y_min_col", y_max_col="y_max_col")
bigDF <- subset(bigDF, correctside == "1" | correctside == "2")
bigDF$trackloss <- ifelse(bigDF$`FixationX` == -10000, 1, 0)

# Trial handling #
bigDF %>% group_by(group, ID, Trial) %>%
  mutate(ev.n=ifelse(row_number()==1,'Start_trial',
                     ifelse(row_number()==n(),'End_trial', 'Mid_trial'))) -> bigDF
bigDF %>% filter(age!='adults'&Trial<=16 | age=='adults'&Trial<=32) -> bigDF
aggregate(Trial~group+ID,FUN=function(x)length(unique(x)),data=bigDF)%>%group_by(group)%>%tally

## Format data for the package ##
ET.data <- make_eyetrackingr_data(
  data=bigDF,
  participant_column="ID",
  trackloss_column="trackloss",
  time_column="LineNum",
  trial_column="Trial",
  aoi_columns=c('left_side','center','right_side'),
  treat_non_aoi_looks_as_missing=F)

# # ET.data <- bigDF
ET.data <- add_aoi(ET.data, data.frame(x_min_col=c(0), y_min_col=c(325), x_max_col=c(784), y_max_col=c(875)),
                   aoi_name="left_side", x_col="FixationX", y_col="FixationY", x_min_col="x_min_col", x_max_col="x_max_col", y_min_col="y_min_col", y_max_col="y_max_col")
ET.data <- add_aoi(ET.data, data.frame(x_min_col=c(785), y_min_col=c(325), x_max_col=c(1135), y_max_col=c(875)),
                   aoi_name="center", x_col="FixationX", y_col="FixationY", x_min_col="x_min_col", x_max_col="x_max_col", y_min_col="y_min_col", y_max_col="y_max_col")
ET.data <- add_aoi(ET.data, data.frame(x_min_col=c(1136), y_min_col=c(325), x_max_col=c(1920), y_max_col=c(875)),
                   aoi_name="right_side", x_col="FixationX", y_col="FixationY", x_min_col="x_min_col", x_max_col="x_max_col", y_min_col="y_min_col", y_max_col="y_max_col")

# Organize factors #
ET.data$correctside <- as.factor(ET.data$correctside)
levels(ET.data$correctside)<-  c("facing_left", "facing_right")

# Collapse sides #
ET.data %>%
  mutate(Condition=ifelse(correctside == 'facing_left' & left_side == T, "facing",
                          ifelse(correctside == 'facing_right' & right_side == T, "facing",
                                 ifelse(correctside == 'facing_left' & right_side == T, "away",
                                        ifelse(correctside == 'facing_right' & left_side == T, "away",
                                               "elsewhere"))))) -> ET.data

# Window data #
ET.data <- subset_by_window(ET.data,
                            rezero=T, remove=F,
                            window_start_msg="Start_trial",
                            window_end_msg='End_trial',
                            msg_col='ev.n', quiet=FALSE)

ET.data %>%
  mutate(facing=ifelse(correctside == 'facing_left' & left_side == T, TRUE,
                       ifelse(correctside == 'facing_right' & right_side == T, TRUE, FALSE)),
         away=ifelse(correctside == 'facing_left' & right_side == T, TRUE,
                     ifelse(correctside == 'facing_right' & left_side == T, TRUE, FALSE)),
         images=ifelse(right_side == T | left_side == T, TRUE, FALSE),
         elsewhere=ifelse(right_side == F & left_side == F, TRUE, FALSE)) -> ET.data
#
#
#### CLEANING ####

# Shortcut preprocessig #
# save(ET.data,file='preprocessed_data/ET.data_timecoursesv11.RData')
# load('preprocessed_data/ET.data_timecoursesv11.RData')

# Relevant window only #
ET.data%>%group_by(group,ID,Trial)%>%select(RemoteTime)%>%mutate(Sample=1:n(),SampleTime=lead(RemoteTime)-RemoteTime)->sample_time
ET.data.clean.init <- subset(ET.data, `LineNum`>=0 & LineNum<=300)
ET.data.clean.init%>%filter(age!='adults'&LineNum<=300 | age=='adults'&LineNum<=150) -> ET.data.clean.init
aggregate(ID~group,FUN=function(x)length(unique(x)),data=ET.data.clean.init)

# Infants with too few trials #
(ET.data.clean.init%>%group_by(group,ID)%>%summarise(n=length(unique(Trial)))%>%arrange(n)->trial_summary)
ET_data_trial_summary <- aggregate(Trial~group+ID+Trial, FUN=function(x)length(unique(x)), data=ET.data.clean.init)

# Discard outlier trials & participants with old script #
filter.data <- function(df, lim){
  # df=droplevels(subset(ET.data.clean.init,group=='10mo_bodies'))
  df <- droplevels(df)

  # Filter trials - SD cut-off #
  original.trial.df <- droplevels(aggregate(Trial ~ ID, data=df, FUN=function(x) length(unique(x))))
  df %>%
    group_by(ID, Trial, images) %>%
    dplyr::filter(images == T) %>%
    dplyr::summarise(trial.dur=sum(duration)) -> df.agg.trial
  df.agg.trial$dur.scaled <- scale(df.agg.trial$trial.dur)
  df.agg.trial %>% mutate(outlier.trial=ifelse(dur.scaled <= -lim, 1, 0)) -> df.agg.trial
  if (sum(df.agg.trial$outlier.trial, na.rm=T) > 0) {
    outlier.trial <- subset(df.agg.trial, outlier.trial == 1)[,1:2]
    df <- droplevels(subset(df, ID:Trial %notin% droplevels(outlier.trial$ID:outlier.trial$Trial)))
  }
  outlier.trial.df <- aggregate(Trial ~ ID, data=outlier.trial, FUN=function(x) length(unique(x)))
  no.outlier.trial.df <- aggregate(Trial ~ ID, data=df, FUN=function(x) length(unique(x)))
  original.trial.df %>%
    dplyr::left_join(outlier.trial.df, by=c('ID')) %>%
    dplyr::left_join(no.outlier.trial.df, by=c('ID')) %>%
    replace(is.na(.), 0) %>% 
    arrange(as.numeric(ID)) %>%
    dplyr::rename(tested=Trial.x,rejected=Trial.y,included=Trial) -> trial.df
  trial.df%>%left_join(outlier.trial%>%group_by(ID)%>%dplyr::summarise(outlier_trials=paste(unique(Trial),collapse='_')),by='ID')->trial_df

  outlier_trial_desc <- round(psych::describe(df.agg.trial$trial.dur[df.agg.trial$outlier.trial==1]*(1/60)*1000))
  print(paste0('Excluded trial duration M=',outlier_trial_desc$mean,' SD=',outlier_trial_desc$sd,' ms'))
  no_outlier_trial_desc <- round(psych::describe(df.agg.trial$trial.dur[df.agg.trial$outlier.trial==0]*(1/60)*1000))
  print(paste0('Included trial duration M=',no_outlier_trial_desc$mean,' SD=',no_outlier_trial_desc$sd,' ms'))
  
  print(paste('Total original trials:', sum(trial.df$tested)))
  print(paste('Mean original trials:', mean(trial.df$tested)))
  print(paste('SD original trials:', sd(trial.df$tested)))
  
  print(paste('Total trial exluded:', sum(trial.df$rejected)))
  print(paste('Mean trial exluded:', mean(trial.df$rejected)))
  print(paste('SD trial exluded:', sd(trial.df$rejected)))
  
  print(paste('Total trial included:', sum(trial.df$included)))
  print(paste('Mean trial included:', mean(trial.df$included)))
  print(paste('SD trial included:', sd(trial.df$included)))

  # Filter participants by SD #
  df %>%
    group_by(ID, images) %>%
    dplyr::filter(images == T) %>% 
    dplyr::summarise(trial.dur=sum(duration)) -> df.agg.ID
  df.agg.ID$dur.scaled <- scale(df.agg.ID$trial.dur)
  # df.agg.ID %>% mutate(outlier.ID=ifelse(dur.scaled <= -2, 1, 0)) -> df.agg.ID # Old code
  df.agg.ID %>% mutate(outlier.ID=ifelse(dur.scaled <= -2, 0, 1)) -> df.agg.ID
  if (sum(df.agg.ID$outlier.ID) > 0) {
    outlier.ID <- subset(df.agg.ID, outlier.ID == 1)[,1:2]
    # print(outlier.ID)
    # df <- droplevels(subset(df, ID %notin% droplevels(outlier.ID$ID))) # Old code
    df <- droplevels(subset(df, ID %in% droplevels(outlier.ID$ID)))
  }
  if(any(original.trial.df$ID %notin% unique(df$ID))) {
    print('Excluded infants :')
    print(as.vector(original.trial.df$ID[unique(original.trial.df$ID) %notin% unique(df$ID)]))
  }
  return(df)
}
thres=1
ET.data.clean.init <- rbind.data.frame(
  filter.data(subset(ET.data.clean.init, group == '7mo_bodies'), thres),
  filter.data(subset(ET.data.clean.init, group == '10mo_bodies'), thres),
  filter.data(subset(ET.data.clean.init, group == '15mo_bodies'), thres),
  filter.data(subset(ET.data.clean.init, group == '18mo_bodies'), thres),
  filter.data(subset(ET.data.clean.init, group == '3yo_bodies'), thres),
  filter.data(subset(ET.data.clean.init, group == '5yo_bodies'), thres),
  subset(ET.data.clean.init, group == 'adults_up'),
  subset(ET.data.clean.init, group == 'adults_inv'))

# Trial summary #
unique(ET.data$file)[unique(ET.data$file)%notin%unique(ET.data.clean.init$file)]
select_group=c('7mo_bodies','10mo_bodies','15mo_bodies','18mo_bodies','3yo_bodies','5yo_bodies')
ET_data_clean_trial_summary <- aggregate(Trial~group+ID+Trial, FUN=function(x)length(unique(x)), data=ET.data.clean.init)
subset(ET_data_trial_summary,group%in%select_group)%>%left_join(subset(ET_data_clean_trial_summary,group%in%select_group),by=c('group','ID'))->group_rej_summary
group_rej_summary%>%mutate(a_tested=Trial.x,b_included=Trial.y)%>%select(-c(Trial.x,Trial.y))->group_rej_summary
group_rej_summary[is.na(group_rej_summary)] <- 0
group_rej_summary%>%group_by(group)%>%mutate(ID=1:n())->group_rej_summary
group_rej_summary%>%mutate(c_rejected=a_tested-b_included,`d_%rejected`=c_rejected/a_tested*100)->group_rej_summary
group_rej_summary$`d_%rejected`<-ifelse(group_rej_summary$`d_%rejected`==100,NA,group_rej_summary$`d_%rejected`)
tidyr::pivot_wider(data = group_rej_summary,
                   id_cols = ID,
                   names_from = group,
                   names_glue = "{group}_{.value}",
                   names_sort = T,
                   values_from = c("a_tested", "b_included","c_rejected","d_%rejected"))->group_rej_summary
group_rej_summary[,stringr::str_sort(names(group_rej_summary),numeric=T)]%>%select(ID,everything())->group_rej_summary
group_rej_summary<-cbind.data.frame(group_rej_summary[,c(1,10:ncol(group_rej_summary))],group_rej_summary[,c(2:9)])
(group_rej_summary_total<-rbind(group_rej_summary,
                                TOTAL=group_rej_summary%>%summarise(across(everything(),sum,na.rm=T)),
                                M=group_rej_summary%>%summarise(across(everything(),mean,na.rm=T)),
                                SD=group_rej_summary%>%summarise(across(everything(),sd,na.rm=T))))
group_rej_summary_total%>%mutate(across(where(is.numeric) & -contains('%'),round,0))->group_rej_summary_total
group_rej_summary_total%>%mutate(across(where(is.numeric) & contains('%'),round,2))->group_rej_summary_total
group_rej_summary_total$ID[(nrow(group_rej_summary_total)-2):nrow(group_rej_summary_total)]<-c('TOTAL','M','SD')
group_rej_summary_total[23,seq(5,ncol(group_rej_summary_total),4)]<-NA
flex_group_rej_summary<-flextable::flextable(group_rej_summary_total)
flex_group_rej_summary<-flextable::delete_part(flex_group_rej_summary,part="header")
flex_group_rej_summary<-flextable::add_header_row(flex_group_rej_summary,values=c("ID",rep(c("tested", "included","rejected","%rejected"),6)))
(flex_group_rej_summary<-flextable::theme_booktabs(flextable::add_header_row(flex_group_rej_summary,colwidths=c(1,4,4,4,4,4,4),values=c('',select_group))))
flextable::save_as_docx(flex_group_rej_summary,path='trial_summaries/group_rej_summary.docx')
# write.csv(group_rej_summary,file='trial_summaries/group_rej_summary.csv',row.names=F)

psych::describe(unlist(group_rej_summary_total%>%select(ends_with('%rejected')&starts_with(c('7mo','10mo','15mo','18mo')))%>%filter(rownames(group_rej_summary_total)%notin%c('TOTAL','M','SD'))))
psych::describe(unlist(group_rej_summary_total%>%select(ends_with('%rejected')&starts_with(c('3yo','5yo')))%>%filter(rownames(group_rej_summary_total)%notin%c('TOTAL','M','SD'))))
#
#
#### INFORMATIVE TIME WINDOW ####

# Shortcut preprocessig #
# save(ET.data.clean.init,file='preprocessed_data/ET.data.clean_timecoursesv11.RData')
# load('preprocessed_data/ET.data.clean_timecoursesv11.RData')

# Time course
tbin=1
age_group <- droplevels(subset(ET.data.clean.init,grepl('adu',group))) # Select adults
# age_group <- droplevels(subset(ET.data.clean.init,grepl('mo',group))) # Select infants
# age_group <- droplevels(subset(ET.data.clean.init,grepl('yo',group))) # Select children
age_group_seq <- make_time_sequence_data(
  data=age_group,predictor_columns=c('group','age'),aois=c('images','elsewhere'),summarize_by=c('ID'),time_bin_size=tbin)

ET_diff_ITW_group <- ET_diff_ITW(age_group_seq,agg_trial=T)
unique(ET_diff_ITW_group$group)
alpha=.01
threshold=qt(p=alpha/2,df=19,lower.tail=FALSE) # two-tailed
# # adults
# perm_test_rslts_TOI <- perm_test_ttest(droplevels(subset(ET_diff_ITW_group,grepl('adu',group))),threshold=threshold,n_samples=5,paired=F,'greater',chance=T,mu=0,gap=0,tbin=tbin)
# # infants
# perm_test_rslts_TOI <- perm_test_ttest(droplevels(subset(ET_diff_ITW_group,!grepl('adu',group)&!grepl('yo',group))),threshold=threshold,n_samples=5,paired=F,'greater',chance=T,mu=0,gap=0,tbin=tbin)
# perm_test_rslts_TOI <- perm_test_ttest(droplevels(subset(ET_diff_ITW_group,grepl('7',group))),threshold=threshold,n_samples=5,paired=F,'greater',chance=T,mu=0,gap=0,tbin=tbin)
# perm_test_rslts_TOI <- perm_test_ttest(droplevels(subset(ET_diff_ITW_group,grepl('10',group))),threshold=threshold,n_samples=5,paired=F,'greater',chance=T,mu=0,gap=0,tbin=tbin)
# perm_test_rslts_TOI <- perm_test_ttest(droplevels(subset(ET_diff_ITW_group,grepl('15',group))),threshold=threshold,n_samples=5,paired=F,'greater',chance=T,mu=0,gap=0,tbin=tbin)
# perm_test_rslts_TOI <- perm_test_ttest(droplevels(subset(ET_diff_ITW_group,grepl('18',group))),threshold=threshold,n_samples=5,paired=F,'greater',chance=T,mu=0,gap=0,tbin=tbin)
# # Children
# perm_test_rslts_TOI <- perm_test_ttest(droplevels(subset(ET_diff_ITW_group,grepl('yo',group))),threshold=threshold,n_samples=5,paired=F,'greater',chance=T,mu=0,gap=0,tbin=tbin)
# perm_test_rslts_TOI <- perm_test_ttest(droplevels(subset(ET_diff_ITW_group,grepl('3yo',group))),threshold=threshold,n_samples=5,paired=F,'greater',chance=T,mu=0,gap=0,tbin=tbin)
# perm_test_rslts_TOI <- perm_test_ttest(droplevels(subset(ET_diff_ITW_group,grepl('5yo',group))),threshold=threshold,n_samples=5,paired=F,'greater',chance=T,mu=0,gap=0,tbin=tbin)
#
#
#### COMPUTE TIME COURSE OF DIFFERENTIAL LOOKING ####


### DATA SELECTION ###
ET.data.clean <- ET.data.clean.init
levels(ET.data.clean$group <- as.factor(ET.data.clean$group))
ET.data.clean$age <- ET.data.clean$age_days

# Select adult data #
ET.data.clean <- droplevels(subset(ET.data.clean,grepl('adu',group)))
levels(ET.data.clean$group <- factor(ET.data.clean$group,levels(ET.data.clean$group)[c(2,1)],ordered=F))
ET.data.clean <- droplevels(subset(ET.data.clean,LineNum>21&LineNum<151)) # Adults ITW

# Select groups of infants #
# ET.data.clean<-droplevels(subset(ET.data.clean,grepl('mo',group)))
# levels(ET.data.clean$group <- factor(ET.data.clean$group,levels(ET.data.clean$group)[c(4,1,2,3)],ordered=T))
# ET.data.clean <- droplevels(subset(ET.data.clean,LineNum>33&LineNum<301)) # Inf whole trial ITW

# Select groups of children #
# ET.data.clean <- droplevels(subset(ET.data.clean,grepl('yo',group)))
# ET.data.clean$group <- as.factor(ET.data.clean$group)
# ET.data.clean <- droplevels(subset(ET.data.clean,LineNum>30&LineNum<301)) # Children ITW


### ANALYSIS PARAMETERS ###
tbin=1
n_samples=500
alpha=.10
threshold=qt(p=alpha/2,df=19,lower.tail=FALSE) # two-tailed
gap=0
ET.data.clean.prop.seq <- eyetrackingR::make_time_sequence_data(
  data=ET.data.clean,predictor_columns=c('group','age'),aois=c('away','facing'),summarize_by=c('ID'),time_bin_size=tbin)
#
#
#### ANALYSIS OF ADULT DIFFERENTIAL LOOKING ####
unique(ET.data.clean.prop.seq$group)
unique(ET.data.clean.prop.seq$AOI)
group1=as.character(unique(ET.data.clean.prop.seq$group)[1])
group2=as.character(unique(ET.data.clean.prop.seq$group)[2])
ET_diff_new(ET.data.clean.prop.seq,group1,group2,agg_trial=T,all=T,return_plot=F,plot_split=T) -> ET_diff_df


colnames(ET.data.clean.prop.seq)
#LMER MODEL
model4<-lmer(Prop ~ AOI + group + AOI*group + (1|ID),data=ET.data.clean.prop.seq)
summary(model4)
plot(model4)
install.packages('emmeans')
library(emmeans)
?emmeans
emmeans(model4,"group",pbkrtest.limit = 12384)


#creating a pivoted dataframe
df <- ET.data.clean.prop.seq%>%select(group,age,ID,AOI,Time,Prop)
df2<- df%>%tidyr::spread(AOI,Prop)
View(df3)
df2$IndexTrial <- seq.int(nrow(df2))
df2$IndexTrial <-as.factor(df2$IndexTrial)

df2<-df2%>%filter(Time>=28& Time<=57)

df3 <- df2 %>% filter(group=='adults_up')
binom_counts <- df3 %>%
  ungroup() %>%  # Remove grouping to summarize across all data
  summarise(
    facing_greater = sum(facing > away),
    facing_away_greater = sum(facing < away)
  )
# Extract counts
facing_greater <- binom_counts$facing_greater
facing_away_greater <- binom_counts$facing_away_greater
total_trials <- facing_greater + facing_away_greater

# Perform the binomial test
binom_test <- binom.test(facing_greater, total_trials, p = 0.5, alternative = "two.sided")
p<- 2.2e-16
options(scipen = 100, digits = 4)
p

# Compare upright vs. inverted with adults (group level) #
perm_test_rslts <- perm_test_ttest(ET_diff_df,threshold=threshold,n_samples=n_samples,paired=T,'two.sided',chance=F,mu=0,gap=gap,tbin=tbin)
# load(file=paste0(getwd(),'/RESULTS/PLT/HUMAN/perm_test_adults_diff_ttest.RData')) # Reported analysis

# Upright stimuli #
ET_diff_df.group1 <- droplevels(subset(ET_diff_df,AOI==group1))
perm_test_rslts <- perm_test_ttest(ET_diff_df.group1,threshold=threshold,n_samples=n_samples,paired=F,'two.sided',chance=T,mu=0,gap=gap,tbin=tbin)
# load(file=paste0(getwd(),'/RESULTS/PLT/HUMAN/perm_test_adults_up_ttest.RData')) # Reported analysis

# Inverted stimuli #
ET_diff_df.group2 <- droplevels(subset(ET_diff_df,AOI==group2))
perm_test_rslts <- perm_test_ttest(ET_diff_df.group2,threshold=threshold,n_samples=n_samples,paired=F,'two.sided',chance=T,mu=0,gap=gap,tbin=tbin)
# load(file=paste0(getwd(),'/RESULTS/PLT/HUMAN/perm_test_adults_inv_ttest.RData')) # Reported analysis

# # Main effect #
# ET_diff_df.main <- ET_diff_df
# ET_diff_df.main$AOI <- "main"
# perm_test_rslts <- perm_test_ttest(ET_diff_df.main,threshold=threshold,n_samples=n_samples,paired=F,'two.sided',chance=T,mu=0,gap=gap,tbin=tbin)
library(tidyverse)
# Analyse cluster #
cbind(round(perm_test_rslts$clusters[,2:3]*refresh_period),p=unlist(perm_test_rslts$clusters$p_value))
perm_test_rslts$clusters
ET_diff_df%>%filter(Time>=28& Time<=57)%>%rename(Difference=Prop)->disc_window
disc_window%>%group_by(group,ID)%>%summarise(Difference=mean(Difference))->adult_diff_df

# adult_anova_data <-adult_diff_df
# model5 <- lm(Difference ~ group,data=adult_anova_data)
# summary(model5)
# plot(model5)
# emmeans(model5,"group")
# 
# t.test(Difference~group,data=adult_anova_data)

adult_diff_df%>%group_by(group)%>%summarise(M=mean(Difference),SD=sd(Difference))%>%mutate_if(is.numeric,round,2)
adult_diff_df%>%group_by(group)%>%rstatix::t_test(Difference~0,var.equal=T,detailed=T)%>%select(group,statistic,df,p)
adult_diff_df%>%group_by(group)%>%rstatix::cohens_d(Difference~0,var.equal=T)%>%select(group,effsize)
# write.csv(file=paste0(getwd(),'/preprocessed_data/differential_looking_times/adult_diff_df_new.csv'),adult_diff_df,row.names=F)
adult_data1<- adult_diff_df%>%filter(group=="adults_up")
adult_data2 <- adult_diff_df%>%filter(group=="adults_inv")
t.test(adult_data1$Difference,adult_data2$Difference, paired=T)
library(lsr)
cohensD(adult_data1$Difference,adult_data2$Difference)



#
#### ANALYSIS OF INFANT DIFFERENTIAL LOOKING ####
unique(ET.data.clean.prop.seq$group)
ET_diff_new(ET.data.clean.prop.seq,NA,NA,agg_trial=T,all=T,return_plot=F,plot_split=T) -> ET_diff_df

## Time course of age group differences ##
perm_test_lm(ET_diff_df,to_test='group',to_shuffle='condition',tbin=tbin,
             alpha=alpha,thres=NA,n_samples=n_samples,gap=gap,coll_clust=F,
             random=F,bayes=F,robust_lm=F,categorical_VI=F) -> perm_test_rslts
load(file=paste0(getwd(),'/RESULTS/PLT/HUMAN/perm_test_inf_diff_lm_group_cond.RData')) # Reported analysis

## Analyse cluster ##
cbind(round(perm_test_rslts$clusters[,2:3]*refresh_period),p=round(unlist(perm_test_rslts$clusters$p_value),3))
perm_test_rslts$clusters
ET_diff_df%>%filter(Time>=157&Time<=170 | Time>=179&Time<=196 | Time>=207&Time<=224)%>%rename(Difference=Prop)->disc_window
levels(disc_window$group) <- c('7','10','15','18')
disc_window$year <- disc_window$group
levels(disc_window$year <- factor(ifelse(disc_window$year%in%c('7','10'),'1yo_bodies',as.character(disc_window$year)),ordered=T))
levels(disc_window$year <- factor(ifelse(disc_window$year%in%c('15','18'),'2yo_bodies',as.character(disc_window$year)),ordered=T))
disc_window%>%group_by(year,group,ID)%>%summarise(Difference=mean(Difference))->infant_diff_df

# Differences between age groups #
infant_diff_df%>%group_by(group)%>%summarise(M=mean(Difference),SD=sd(Difference))%>%mutate_if(is.numeric,round,2)
(infant_diff_df%>%ungroup%>%rstatix::t_test(Difference~group,var.equal=T,detailed=T,paired=F,alternative='less')%>%select(group1,group2,estimate,n1,n2,statistic,df,p)%>%mutate_if(is.numeric,round,3))
infant_diff_df%>%ungroup%>%rstatix::cohens_d(Difference~group,var.equal=T,paired=F)

# Differences between infants below/above 1 year #
infant_diff_df%>%group_by(year)%>%summarise(M=mean(Difference),SD=sd(Difference))%>%mutate_if(is.numeric,round,2)
(infant_diff_df%>%ungroup%>%rstatix::t_test(Difference~year,var.equal=T,detailed=T,paired=F,alternative='less')%>%select(group1,group2,estimate,statistic,df,p)%>%mutate_if(is.numeric,round,3))
infant_diff_df%>%ungroup%>%rstatix::cohens_d(Difference~year,var.equal=T,paired=F)

(infant_diff_df%>%group_by(year)%>%rstatix::t_test(Difference~0,var.equal=T,detailed=T)%>%select(year,estimate,n,statistic,df,p)%>%mutate_if(is.numeric,round,3))
infant_diff_df%>%group_by(year)%>%rstatix::cohens_d(Difference~0,var.equal=T)
#
#
#### ANALYSIS OF CHILDREN DIFFERENTIAL LOOKING ####
unique(ET.data.clean.prop.seq$group)
group1=as.character(unique(ET.data.clean.prop.seq$group)[1])
group2=as.character(unique(ET.data.clean.prop.seq$group)[2])
ET_diff_group1_group2(ET.data.clean.prop.seq,group1,group2) -> ET_diff_df

## Time course of age group differences ##

# Difference between age groups #
perm_test_rslts <- perm_test_ttest(ET_diff_df,threshold=threshold,n_samples=n_samples,paired=F,'two.sided',chance=F,mu=0,gap=gap,tbin=tbin)
load(file=paste0(getwd(),'/RESULTS/PLT/HUMAN/perm_test_child_diff_ttest.RData')) # Reported analysis

# 3 years #
ET_diff_df.group1 <- droplevels(subset(ET_diff_df,AOI==group1))
perm_test_rslts <- perm_test_ttest(ET_diff_df.group1,threshold=threshold,n_samples=n_samples,paired=F,'two.sided',chance=T,mu=0,gap=gap,tbin=tbin)
load(file=paste0(getwd(),'/RESULTS/PLT/HUMAN/perm_test_child3yo_ttest.RData')) # Reported analysis

# 5 years #
ET_diff_df.group2 <- droplevels(subset(ET_diff_df,AOI==group2))
perm_test_rslts <- perm_test_ttest(ET_diff_df.group2,threshold=threshold,n_samples=n_samples,paired=F,'two.sided',chance=T,mu=0,gap=gap,tbin=tbin)
load(file=paste0(getwd(),'/RESULTS/PLT/HUMAN/perm_test_child5yo_ttest.RData')) # Reported analysis

# Cluster of differences between age groups #
cbind(round(perm_test_rslts$clusters[,2:3]*refresh_period),p=round(unlist(perm_test_rslts$clusters$p_value),3))
perm_test_rslts$clusters
ET_diff_df%>%filter(Time>=262&Time<=299)%>%rename(Difference=Prop)->disc_window

disc_window%>%group_by(group,ID)%>%summarise(Difference=mean(Difference))->child_diff_df

child_diff_df%>%group_by(group)%>%summarise(M=mean(Difference),SD=sd(Difference))%>%mutate_if(is.numeric,round,2)
(child_diff_df%>%group_by(group)%>%rstatix::t_test(Difference~0,var.equal=T,detailed=T)%>%select(group,estimate,n,statistic,df,p)%>%mutate_if(is.numeric,round,3))
child_diff_df%>%group_by(group)%>%rstatix::cohens_d(Difference~0,var.equal=T)
