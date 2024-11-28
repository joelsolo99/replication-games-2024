#### SPACE ####
library('dplyr')
library('ggplot2')
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
`%notin%` <- Negate(`%in%`)
#
#
#### DATA LOADING ####

## Eye track data ##
filenames <- list.files(paste0(getwd(), '/data_raw/plt_adult'), pattern='*.topd')
foreach (f=filenames, .combine=rbind.data.frame) %do% {
  # f=filenames[1]
  ID <- gsub(".*-(.+)-.*", "\\1", f)
  raw.data <- data.frame(cbind(sub("\\.topd.*", "", f), read.table(paste(getwd(),'/data_raw/plt_adult/', f, sep=''), sep="\t")[1:30]))
  colnames(raw.data) <- list('ID', '#T2TGazeTime', 'RemoteTime', 'LineNum', 'GazeCounter', 'GazepointX (L)', 'GazepointY (L)',
                             'CamX (L)', 'CamY (L)', 'Distance (L)', 'Pupil (L)', 'Validity (L)', 'GazepointX (R)',
                             'GazepointY (R)', 'CamX (R)', 'CamY (R)', 'Distance (R)', 'Pupil (R)', 'Validity (R)',
                             'GazepointX (M)', 'GazepointY (M)', 'Pupil (M)', 'Validity (S)', 'FixationID', 'FixationX',
                             'FixationY', 'FixationConfidence', 'ev', 'facingside', 'condition', 'ssresponse')
  
  ## Stimuli lists ##
  stim.list.part1 <- cbind('part1', read.table(paste0(getwd(),'/data_raw/plt_adult/stim_list/xp_part1/list', ID), sep='\t'))
  colnames(stim.list.part1) <- c('part','stim1', 'stim2', 'facingside', 'condition')
  stim.list.part2 <- cbind('part2', NA, read.table(paste0(getwd(),'/data_raw/plt_adult/stim_list/xp_part2/list', ID), sep='\t'))
  colnames(stim.list.part2) <- c('part','stim1', 'stim2', 'facingside', 'condition')
  stim.list <- rbind(stim.list.part1, stim.list.part2[1:16,])
  
  ## Include events and stimuli in the eye-T dataframe ##
  raw.data %>% mutate(ev.n=ifelse(ev=='p' & lag(ev)=='f', 1,
                                    ifelse(ev=='f' & lag(ev)=='p', 2, 0))) -> raw.data
  raw.data$stim1 <- NA
  raw.data$stim2 <- NA
  raw.data$Trial <- NA
  events.start <- which(raw.data$ev.n==1)
  events.stop <- c(which(raw.data$ev.n==2), nrow(raw.data)+1)
  for (event in 1:length(events.start)) {
    # event=48
    low=events.start[event]
    high=events.stop[event]-1
    raw.data$Trial[low:high] <- event
    raw.data$stim1[low:high] <- as.character(stim.list$stim1[event])
    raw.data$stim2[low:high] <- as.character(stim.list$stim2[event])
  }
  
  ## Add column with recording duration ##
  raw.data$duration <- c(diff(raw.data$`#T2TGazeTime`), as.integer(mean(diff(raw.data$`#T2TGazeTime`))))

  ## Subset trials ##
  subset(raw.data, ev=='p') %>%
    select('ID', 'Trial', 'stim1', 'stim2',
           '#T2TGazeTime', 'RemoteTime', 'duration', 'ev.n',
           'GazepointX (M)', 'GazepointY (M)', 'Pupil (M)', 'Validity (S)',
           'FixationID', 'FixationX', 'FixationY',
           'facingside', 'condition', 'ssresponse') -> test.data
  } -> bigDF.init

## Response data ##
filenames <- list.files(paste(getwd(),'/data_raw/behavior_adult/memory', sep=''))
filenames <- filenames[!grepl('.csv',filenames)]
foreach (f=filenames, .combine=rbind.data.frame) %do% {
  # f=filenames[1]
  file <- paste(getwd(),'/data_raw/behavior_adult/memory/',f , sep ='')
  print(file)
  ID=f
  # ID <- gsub(".*-(.+)-.*", "\\1", f)
  
  # Discard header #
  data.start <- grep("Trial", readLines(file))-1
  data.end <- grep("Local", readLines(file))-(data.start+3)
  
  # Include recorded data in template with all trials #
  dfFile <- read.table(file, skip=data.start, nrows=data.end, header=T, sep="")
  template.df <- data.frame(ID=rep(ID, 16))
  template.df$ID <- ID
  template.df$Trial <- seq(36, 51)
  template.df <- merge(template.df, dfFile, by="Trial", all.x=TRUE) 
  template.df$keys <- gsub("[^A-Za-z0-9,;._-]", "", template.df$keys)
  template.df$keys[is.na(template.df$keys)] <- 0
  template.df$Trial <- seq(33, 48)
  as.data.frame(template.df)
  } -> resp_df
#
#
#### CODE MEMORY TASK ####
bigDF.part2 <- subset(bigDF.init, is.na(stim1))
# Select interesting columns
bigDF.part2%>%select(ID,Trial,stim2,condition,ssresponse)->bigDF.part2
# Restart time
bigDF.part2%>%group_by(ID, Trial)%>%mutate(Time=seq(1,n()))->bigDF.part2
# Find response sample
bigDF.part2%>%mutate(response_sample=ifelse(ssresponse!=lag(ssresponse),T,F))->bigDF.part2
# Select response sample and last row (in case times out)
bigDF.part2%>%filter(row_number()==n()|response_sample==T)->response_df
# Disentangle trials with responses from trials without
response_df%>%summarise(n())->trial_summary
response_df%>%filter(row_number()==1)->response_df
response_df%>%select(-response_sample)->response_df
# Response instructions (counterbalancing)
resp_side <- read.table(paste0(getwd(),'/DATA_RAW/behavior_adult/memory/part2_side.csv'), header=T, sep=',')
resp_side$old_key <- ifelse(resp_side$oui.o==1,'o','w')
response_df%>%left_join(select(resp_side,c('ID','old_key')), by='ID')->response_df
# Accuracy
old_stim <- unique(bigDF.init$stim1)
bigDF.init$ssresponse <- as.character(bigDF.init$ssresponse)
response_df %>%
  mutate(orientation=ifelse(grepl('f_', stim2)==T, "away", "facing"),
         condition=ifelse(stim2%in%old_stim, 'old', 'new'),
         accuracy=case_when(old_key=='o' & ssresponse=='o' & condition=='old'~1,
                            old_key=='o' & ssresponse=='o' & condition=='new'~0,
                            old_key=='o' & ssresponse=='w' & condition=='old'~0,
                            old_key=='o' & ssresponse=='w' & condition=='new'~1,
                            old_key=='w' & ssresponse=='o' & condition=='old'~0,
                            old_key=='w' & ssresponse=='o' & condition=='new'~1,
                            old_key=='w' & ssresponse=='w' & condition=='old'~1,
                            old_key=='w' & ssresponse=='w' & condition=='new'~0,
                            ssresponse==0~0))->coded_response_df
# Check psyscope response dataframe
resp_df%>%select(ID,Trial,Condition,keys,Time)->resp_df
resp_df%>%select(ID,Trial,keys)%>%
  left_join(select(response_df,ID,Trial,ssresponse,old_key),by=c('ID','Trial'))->compare_keys
compare_keys%>%mutate(same=ifelse(ssresponse==keys,T,F))->compare_keys
# Code accuracy Psyscope response df
resp_df%>%left_join(select(resp_side,c('ID','old_key')), by='ID')->resp_df
resp_df %>% filter(!is.na(Condition)) %>%
  mutate(orientation=ifelse(grepl('f_', Condition)==T, "away", "facing"),
         condition=ifelse(Condition%in%old_stim, 'old', 'new'),
         accuracy=case_when(old_key=='o' & keys=='o' & condition=='old'~1,
                            old_key=='o' & keys=='o' & condition=='new'~0,
                            old_key=='o' & keys=='w' & condition=='old'~0,
                            old_key=='o' & keys=='w' & condition=='new'~1,
                            old_key=='w' & keys=='o' & condition=='old'~0,
                            old_key=='w' & keys=='o' & condition=='new'~1,
                            old_key=='w' & keys=='w' & condition=='old'~1,
                            old_key=='w' & keys=='w' & condition=='new'~0,
                            keys==0~0))->coded_resp_df
#
#
#### ANALYSIS ####
response_df <- coded_resp_df

## Cleaning ##
response_df%>%group_by(ID)%>%summarise(accuracy=mean(accuracy,na.rm=T))->agg_response_df
agg_response_df%>%ungroup()%>%mutate(z_acc=scale(accuracy),outlier=ifelse(abs(z_acc)>2.5,T,F))->agg_response_df
barplot(agg_response_df$accuracy)
response_df%>%group_by(ID)%>%mutate(z_RT=scale(Time),outlier=ifelse(abs(z_RT)>2.5,T,F))->response_df

## Accuracy ##
response_df%>%group_by(ID,orientation)%>%summarise(accuracy=mean(accuracy,na.rm=T))->agg_response_df
(agg_response_df%>%group_by(orientation)%>%
    summarise(N=n(), M=mean(accuracy), sd=sd(accuracy),
              sem=goeveg::sem(accuracy))%>%rename(accuracy=M) -> desc_stats)
(t.test(accuracy~orientation, paired=T, var.equal=T,data=agg_response_df))
lsr::cohensD(accuracy~orientation, method='paired', data=agg_response_df)
ggplot(agg_response_df, aes(x=orientation, y=accuracy)) +
  geom_histogram(position='dodge', stat='summary', aes(fill=orientation, alpha=ID)) +
  geom_point(data=desc_stats, aes(stat='summary')) +
  geom_errorbar(data=desc_stats, aes(ymin=accuracy-sem, ymax=accuracy+sem)) +
  ggpubr::stat_compare_means(method = "t.test", paired=T) +
  scale_alpha_discrete(range=c(1, 1)) + theme_minimal(base_size=20) + guides(fill=F,alpha=F) + theme(axis.title.x=element_blank())

## RT ##
response_df%>%filter(accuracy==1)->response_df_RT
response_df_RT%>%group_by(ID,orientation)%>%summarise(RT=mean(Time))->agg_response_df
(agg_response_df%>%group_by(orientation)%>%
    summarise(N=n(), M=mean(RT), sd=sd(RT),
              sem=goeveg::sem(RT))%>%rename(RT=M) -> desc_stats)
(t.test(RT~orientation, paired=T, var.equal=T,data=agg_response_df))
lsr::cohensD(RT~orientation, method='paired', data=agg_response_df)
ggplot(agg_response_df, aes(x=orientation, y=RT)) +
  geom_histogram(position='dodge', stat='summary', aes(fill=orientation, alpha=ID)) +
  geom_point(data=desc_stats, aes(stat='summary')) +
  geom_errorbar(data=desc_stats, aes(ymin=RT-sem, ymax=RT+sem)) +
  ggpubr::stat_compare_means(method = "t.test", paired=T) +
  scale_alpha_discrete(range=c(1, 1)) + theme_minimal(base_size=20) + guides(fill=F,alpha=F) + theme(axis.title.x=element_blank())
#
#