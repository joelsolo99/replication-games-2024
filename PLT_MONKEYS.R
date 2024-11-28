#### SPACE ####
library('dplyr')
library('ggplot2')
library('foreach')
library('doParallel')
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

# Get datafile #
filenames <- list.files(paste0(getwd(), '/DATA_RAW/plt_monkeys/'), pattern='*.csv')
filenames <- filenames[!grepl('frame_coding.csv',filenames)]
foreach(file=filenames, .combine=rbind.data.frame) %do% {
  Subject <- sub("\\_.*", "", file)
  datafile <- cbind(Subject, read.csv(paste0(getwd(), '/DATA_RAW/plt_monkeys/', file), header=T))
} -> bigDF
bigDF%>%mutate_if(is.character,as.factor)->bigDF
bigDF$Trial <- as.factor(bigDF$Trial)

# Find back conditions #
condition_df <- read.csv(paste0(getwd(), '/DATA_RAW/plt_monkeys/frame_coding.csv'), header=T)
condition_df%>%mutate_if(is.character,as.factor)->condition_df
condition_df$Trial <- as.factor(condition_df$Trial)
condition_df <- droplevels(subset(condition_df, Subject:Trial %in% bigDF$Subject:bigDF$Trial))
select(condition_df, -c(Duration)) %>% 
  inner_join(bigDF, by=c('Subject', 'Trial', 'Direction')) -> bigDF.init
levels(bigDF.init$BodiesRelation)[1] <- "off-images"
#
#
#### DIFFERENCES IN LOOKING TIME ####

stringr::str_split(filenames,'_',simplify=T)[,1]->initial_sample
bigDF.init->frame_df
frame_df%>%mutate(Duration.Frame=End.Frame-Start.Frame)->frame_df
frame_df%>%group_by(Direction)%>%summarise(Duration.Frame=sum(Duration.Frame))->frame_df
frame_df%>%tidyr::pivot_wider(names_from=Direction,values_from=Duration.Frame)%>%mutate(TOTAL=rowSums(across(where(is.numeric))))->frame_df
(frame_df/frame_df$TOTAL)*100

## General looking ##
bigDF.init->dur_df
dur_df%>%mutate(Duration.Frame=End.Frame-Start.Frame,Duration=Duration.Frame*(1/30))->dur_df
dur_df%>%group_by(Group,Subject,Trial,BodiesRelation)%>%summarise(Duration=sum(Duration))->dur_df

## Differential looks on images ##
dur_df%>%mutate(images=ifelse(BodiesRelation!='off-images','on','off'))->images_df
images_df%>%group_by(Group,Subject,Trial,images)%>%summarise(Duration=sum(Duration))%>%tidyr::spread(images,Duration)->images_df
images_df[is.na(images_df)] <- 0
images_df%>%mutate(nolook=ifelse(on<.5,T,F))->images_df
(images_df%>%group_by(Group,Subject)%>%summarise(total=n(),nolook=sum(nolook),included=total-nolook,TOTAL_on=sum(on))%>%arrange(TOTAL_on)->trial_rej_df)
(trial_rej_df%>%mutate(`% rejected`=(nolook/total)*100)->trial_rej_df)
psych::describe(trial_rej_df[,4:6])
psych::describe(trial_rej_df$`% rejected`)
images_df%>%mutate(Trial_ID=paste0(Trial,Subject))%>%filter(nolook)->outlier_trials_df
(trial_rej_df%>%mutate(outlier=ifelse(included<2,T,F))%>%filter(outlier)->outlier_subjects_df)

## Differential looking times ##
droplevels(dur_df%>%filter(BodiesRelation%in%c('facing','facing_away')))->diff_df
diff_df%>%tidyr::spread(BodiesRelation,Duration)->diff_df
diff_df%>%mutate(Trial_Subject=paste0(Trial,Subject))->diff_df
droplevels(diff_df%>%filter(Trial_Subject%notin%outlier_trials_df$Trial_ID))->diff_df
droplevels(diff_df%>%filter(Subject%notin%outlier_subjects_df$Subject))->diff_df
diff_df[is.na(diff_df)] <- 0

# diff_df%>%group_by(Group,Subject)%>%tally%>%summarise(mean_N=mean(n),sd_N=sd(n))
# diff_df%>%group_by(Group,Subject)%>%tally%>%ungroup%>%rstatix::t_test(n~Group,paired=F,var.equal=T)
# diff_df%>%group_by(Group,Subject,Trial)%>%summarise(LT_images=sum(facing,facing_away))%>%group_by(Group,Subject)%>%summarise(LT_images=mean(LT_images))%>%group_by(Group)%>%summarise(mean_LT_images=mean(LT_images),sd_LT_images=sd(LT_images))
# diff_df%>%group_by(Group,Subject,Trial)%>%summarise(LT_images=sum(facing,facing_away))%>%group_by(Group,Subject)%>%summarise(LT_images=mean(LT_images))%>%ungroup%>%rstatix::t_test(LT_images~Group,paired=F,var.equal=T)

diff_df%>%mutate(Difference=(facing-facing_away)/(facing+facing_away))->diff_df
diff_df%>%group_by(Group,Subject)%>%summarise(Difference=mean(Difference))->diff_df
diff_df%>%ungroup%>%summarise(N=n(),M=mean(Difference),sd=sd(Difference),sem=goeveg::sem(Difference))%>%mutate_if(is.numeric,round,2)
initial_sample[initial_sample%in%unique(diff_df$Subject)]
initial_sample[initial_sample%notin%unique(diff_df$Subject)]
t.test(diff_df$Difference,mu=0)
lsr::cohensD(diff_df$Difference,mu=0)
binom.test(sum(diff_df$Difference>0),nrow(diff_df),p=0.5,alternative="two.sided")
diff_df <- diff_df[order(diff_df$Difference),]
diff_df$order <- seq(1:nrow(diff_df))
(part_diff_plot <- ggplot(data=diff_df,aes(x=order,y=Difference)) + ylab('Differential looking time') + 
    geom_text(aes(x=nrow(diff_df)/2,label='**',y=.4),size=10) +
    geom_bar(stat="identity") + theme_minimal(base_size=25) + 
    theme(axis.title.x=element_blank(),axis.text.x=element_blank())+ylim(c(-.6,.6)))

# Group difference #
diff_df%>%group_by(Group)%>%summarise(N=n(),M=mean(Difference),sd=sd(Difference),sem=goeveg::sem(Difference))%>%mutate_if(is.numeric,round,2)
t.test(Difference~Group,var.equal=T,diff_df)
diff_df%>%group_by(Group)%>%summarise(M_diff=mean(Difference),sd_diff=sd(Difference))%>%mutate_if(is.numeric,round,2)
diff_df%>%ungroup%>%rstatix::t_test(Difference~Group,var.equal=T)
diff_df%>%ungroup%>%rstatix::cohens_d(Difference~Group,var.equal=T)

