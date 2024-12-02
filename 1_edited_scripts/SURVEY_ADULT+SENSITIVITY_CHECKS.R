#### SPACE ####
library('tidyverse')
library('ggplot2')
library('foreach')
library('doParallel')
library('dplyr')
## Set working directory ##
thisFilePath<-rstudioapi::getSourceEditorContext()$path
thisDir <- dirname(thisFilePath)
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
#### DATA ####

# Stimulus types #
filenames_facing <- stringr::str_remove(list.files(paste0(thisDir,'/STIMULI/SURVEY/Facing'),pattern='*.png'),'.png')
filenames_away <- stringr::str_remove(list.files(paste0(thisDir,'/STIMULI/SURVEY/Facing_Away'),pattern='*.png'),'.png')
filenames_single <- stringr::str_remove(c(list.files(paste0(thisDir,'/STIMULI/SURVEY/Left'),pattern='*.png'),
                                          list.files(paste0(thisDir,'/STIMULI/SURVEY/Right'),pattern='*.png')),'.png')

# Load data #
filenames <- list.files(paste0(thisDir,'/DATA_RAW/behavior_adult/ratings/'),pattern='*.csv',recursive=T)
foreach (f=filenames,.combine=rbind.data.frame)%dopar%{
  infos <- read.csv(paste0(thisDir,'/DATA_RAW/behavior_adult/ratings/',f),nrows=2,sep=",",header=F,stringsAsFactors=F)
  colnames(infos)<-infos[1,]
  infos$group <- stringr::str_split(f,'/')[[1]][1]
  infos$file <- f
  infos[2,]}->info_df
foreach (f=filenames, .combine=rbind.data.frame,.packages='dplyr')%dopar%{
  # for (f in filenames) {
  # f=filenames[1]
  # print(f)
  infos <- read.csv(paste0(thisDir,'/DATA_RAW/behavior_adult/ratings/',f), nrows=2, sep=",", header=F, stringsAsFactors=F)
  dfFile <- read.csv(paste0(thisDir,'/DATA_RAW/behavior_adult/ratings/',f), skip=3, header=T, sep=",")
  dfFile$group <- stringr::str_split(f,'/')[[1]][1]
  dfFile$file <- f
  dfFile$ID <- infos$V2[2]
  dfFile$Trial <- seq(1,nrow(dfFile))
  dfFile$Trial <- paste0('Trial',dfFile$Trial)
  # Code conditions #
  dfFile%>%rename(dimension=trialText)->dfFile
  dfFile%>%mutate(configuration=case_when(stim1%in%filenames_facing~'facing',
                                          stim1%in%filenames_away~'away',
                                          stim1%in%filenames_single~'single'))->dfFile
  # Recompute accuracy for verification
  dfFile%>%select(group,file,ID,Trial,stim1,dimension,configuration,response,RT)} -> bigDF_init
bigDF_init%>%mutate_if(is.character,as.factor)->bigDF_init
levels(bigDF_init$dimension)
levels(bigDF_init$configuration <- factor(bigDF_init$configuration,levels(bigDF_init$configuration)[c(2,1,3)]))
bigDF_init%>%group_by(dimension,configuration)%>%summarise(trialN=n())

# Some participants did the survey more than once, we only analyze ratings collected during their first participation
sum(unique(info_df$id)%notin%unique(bigDF_init$ID))
aggregate(file~group,bigDF_init,function(x)length(unique(x)))
info_df$duplicated <- duplicated(info_df$id)
aggregate(file~group,info_df,function(x)length(unique(x)))
duplicate_subjects_info <- subset(info_df,id%in%info_df$id[info_df$duplicated])
length(unique(duplicate_subjects_info$id))
duplicate_subjects_info$time <- by(duplicate_subjects_info,seq_len(nrow(duplicate_subjects_info)),function(row)as.numeric(paste0(stringr::str_split(row$file,'_')[[1]][3],stringr::str_remove(stringr::str_split(row$file,'_')[[1]][4],'.csv'))))
duplicate_subjects_info%>%arrange(id,time)->duplicate_subjects_info
subset(duplicate_subjects_info,file%in%duplicate_subjects_info$file[duplicated(duplicate_subjects_info$id)])$time-subset(duplicate_subjects_info,file%notin%duplicate_subjects_info$file[duplicated(duplicate_subjects_info$id)])$time
duplicate_subjects_info <- subset(duplicate_subjects_info,file%in%duplicate_subjects_info$file[duplicate_subjects_info$duplicated])
aggregate(file~group,duplicate_subjects_info,function(x)length(unique(x)))
aggregate(file~group,bigDF_init,function(x)length(unique(x)))
aggregate(file~dimension+configuration,bigDF_init,function(x)length(unique(x)))
bigDF_init <- droplevels(subset(bigDF_init,file%notin%duplicate_subjects_info$file))
aggregate(file~group,bigDF_init,function(x)length(unique(x)))
bigDF_init%>%group_by(group,file,dimension,configuration)%>%tally
#
#
#### ANALYZE RATINGS ####

## Cleaning ##
bigDF_init%>%group_by(ID)%>%mutate(z_response=scale(response)[,1],z_RT=scale(RT)[,1])->bigDF_init
bigDF_init->bigDF_clean
aggregate(ID~group,bigDF_clean,function(x)length(unique(x)))
aggregate(Trial~group+ID+dimension+configuration,bigDF_clean,function(x)length(unique(x)))

# Analysis #
aggregate(ID~configuration+dimension,bigDF_clean,function(x)length(unique(x)))
aggregate(Trial~ID+dimension+configuration,bigDF_clean,function(x)length(unique(x)))
aggregate(response~ID+dimension+configuration,bigDF_clean,mean)->part_rating_df

# ez::ezANOVA(droplevels(subset(part_rating_df,dimension=='Meaning')),wid=ID,dv=response,within=.(configuration),type=3)$ANOVA
droplevels(subset(part_rating_df,dimension=='Meaning'))%>%group_by(dimension)%>%rstatix::t_test(response~configuration,paired=T)
droplevels(subset(part_rating_df,dimension=='Meaning'))%>%group_by(dimension)%>%rstatix::cohens_d(response~configuration,paired=T)

# ez::ezANOVA(droplevels(subset(part_rating_df,dimension=='Emotion')),wid=ID,dv=response,within=.(configuration),type=3)$ANOVA
droplevels(subset(part_rating_df,dimension=='Emotion'))%>%group_by(dimension)%>%rstatix::t_test(response~configuration,paired=T)
droplevels(subset(part_rating_df,dimension=='Emotion'))%>%group_by(dimension)%>%rstatix::cohens_d(response~configuration,paired=T)

# ez::ezANOVA(droplevels(subset(part_rating_df,dimension=='Intent')),wid=ID,dv=response,within=.(configuration),type=3)$ANOVA
droplevels(subset(part_rating_df,dimension=='Intent'))%>%group_by(dimension)%>%rstatix::t_test(response~configuration,paired=T)
droplevels(subset(part_rating_df,dimension=='Intent'))%>%group_by(dimension)%>%rstatix::cohens_d(response~configuration,paired=T)

# ez::ezANOVA(droplevels(subset(part_rating_df,dimension=='Motion')),wid=ID,dv=response,within=.(configuration),type=3)$ANOVA
droplevels(subset(part_rating_df,dimension=='Motion'))%>%group_by(dimension)%>%rstatix::t_test(response~configuration,paired=T)
droplevels(subset(part_rating_df,dimension=='Motion'))%>%group_by(dimension)%>%rstatix::cohens_d(response~configuration,paired=T)

#Graph of ratings by dimension: facing vs non-facing
part_rating_df%>%group_by(dimension)%>%rstatix::t_test(response~configuration,paired=T)
levels(part_rating_df$configuration) <- c('Facing','Non-facing','Single')
levels(part_rating_df$dimension <- factor(part_rating_df$dimension,levels(part_rating_df$dimension)[c(3,1,2,4)]))
part_rating_df%>%group_by(dimension)%>%rstatix::t_test(response~configuration,paired=T)->comp_df
comp_df%>%group_by(dimension)%>%mutate(y.position=c(10.8,11.6,10.8),label=ifelse(p<.05,'*',''))%>%ungroup%>%filter(p.adj.signif!='ns')->comp_df
line_color_three=c('#CC3366','#9DBFF2','#66cc33')
ggplot(part_rating_df)+geom_boxplot(aes(x=configuration,y=response-1,fill=configuration,alpha=.5))+
  stat_summary(aes(x=configuration,y=response-1),fun.x='mean',geom="point",shape=20,size=5) +
  facet_grid(.~dimension)+ylab('Rating')+theme_minimal(base_size=20)+
  ggpubr::stat_pvalue_manual(comp_df,label='label',y.position='y.position',tip.length=0,size=8) +
  scale_fill_manual(values=line_color_three) +
  scale_y_continuous(breaks=seq(0,10,2)) +
  theme(legend.title=element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank(),legend.spacing.y=unit(1,'cm'),panel.grid=element_blank())+
  guides(alpha=F,fill=guide_legend(byrow=T))

part_rating_df%>%filter(configuration!='Single')->part_rating_df_fn
part_rating_df_fn%>%group_by(dimension)%>%rstatix::t_test(response~configuration,paired=T)
part_rating_df_fn%>%group_by(dimension)%>%rstatix::t_test(response~configuration,paired=T)->comp_df
comp_df%>%mutate(y.position=12,label=ifelse(p<.05,'*',''))%>%filter(p<.05)->comp_df
ggplot(part_rating_df_fn)+geom_boxplot(aes(x=configuration,y=response,fill=configuration,alpha=.5))+
  facet_grid(.~dimension)+ylab('Rating')+theme_minimal(base_size=20)+
  ggpubr::stat_pvalue_manual(comp_df,label='label',y.position='y.position',tip.length=0,size=8) +
  theme(legend.title=element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank(),legend.spacing.y=unit(1,'cm'))+
  guides(alpha=F,fill=guide_legend(byrow=T))

##SENSITIVITY CHECKS##
#Check for normal distribution by performing Shapiro-Wilk test split by group
shapiro_by_group <- function(data, group_vars = c("dimension", "configuration"), value_var = "response") {
  data %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      p_value = shapiro.test(!!sym(value_var))$p.value,      
      statistic = shapiro.test(!!sym(value_var))$statistic,  
      .groups = 'drop'
    )
}
shapiro_results <- shapiro_by_group(
  data = part_rating_df,
  group_vars = c("dimension", "configuration"),
  value_var = "response"
)
print(shapiro_results)

#Run non-parametric tests: Wilcoxon signed-rank tests split by group

# Facing vs Non-facing
wilcoxon_by_group <- function(data, group_vars = c("dimension"), value_var = "response", paired_var = "configuration") {
  data %>%
    pivot_wider(names_from = !!sym(paired_var), values_from = !!sym(value_var)) %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      p_value = tryCatch({
        test_result <- wilcox.test(
          .data[["Facing"]], .data[["Non-facing"]],  
          paired = TRUE
        )
        test_result$p.value
      }, error = function(e) NA),
      statistic = tryCatch({
        test_result <- wilcox.test(
          .data[["Facing"]], .data[["Non-facing"]],  
          paired = TRUE
        )
        test_result$statistic
      }, error = function(e) NA),
      .groups = 'drop'  
    )
}
wilcoxon_results <- wilcoxon_by_group(
  data = part_rating_df_fn,        
  group_vars = c("dimension"),  
  value_var = "response",        
  paired_var = "configuration"  
)
print('Facing vs Non-facing')
print(wilcoxon_results)
print("---------------------")

part_rating_df%>%filter(configuration!='Non-facing')->part_rating_df_sf
# Facing vs Single
wilcoxon_by_group <- function(data, group_vars = c("dimension"), value_var = "response", paired_var = "configuration") {
  data %>%
    pivot_wider(names_from = !!sym(paired_var), values_from = !!sym(value_var)) %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      p_value = tryCatch({
        test_result <- wilcox.test(
          .data[["Facing"]], .data[["Single"]],  
          paired = TRUE
        )
        test_result$p.value
      }, error = function(e) NA),
      statistic = tryCatch({
        test_result <- wilcox.test(
          .data[["Facing"]], .data[["Single"]],  
          paired = TRUE
        )
        test_result$statistic
      }, error = function(e) NA),
      .groups = 'drop'  
    )
}
wilcoxon_results <- wilcoxon_by_group(
  data = part_rating_df_sf,        
  group_vars = c("dimension"),  
  value_var = "response",        
  paired_var = "configuration"  
)
print('Facing vs Single')
print(wilcoxon_results)
print("---------------------")

part_rating_df%>%filter(configuration!='Facing')->part_rating_df_sn
# Non-facing vs Single
wilcoxon_by_group <- function(data, group_vars = c("dimension"), value_var = "response", paired_var = "configuration") {
  data %>%
    pivot_wider(names_from = !!sym(paired_var), values_from = !!sym(value_var)) %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      p_value = tryCatch({
        test_result <- wilcox.test(
          .data[["Non-facing"]], .data[["Single"]],  
          paired = TRUE
        )
        test_result$p.value
      }, error = function(e) NA),
      statistic = tryCatch({
        test_result <- wilcox.test(
          .data[["Non-facing"]], .data[["Single"]],  
          paired = TRUE
        )
        test_result$statistic
      }, error = function(e) NA),
      .groups = 'drop'  
    )
}
wilcoxon_results <- wilcoxon_by_group(
  data = part_rating_df_sn,        
  group_vars = c("dimension"),  
  value_var = "response",        
  paired_var = "configuration"  
)
print('Non-facing vs Single')
print(wilcoxon_results)
print("---------------------")

#Recoding variable "response" for sensitivity checks
part_rating_df_recoded <- part_rating_df %>%
  mutate("response_recoded" = case_when(
        part_rating_df$response < 3 ~ 1,
        part_rating_df$response >= 3 & part_rating_df$response < 5 ~ 2,
        part_rating_df$response >= 5 & part_rating_df$response < 8 ~ 3,
        part_rating_df$response >= 8 & part_rating_df$response < 10 ~ 4,
        part_rating_df$response >= 10 ~ 5
  ))

#Re-running analyses and graph
# ez::ezANOVA(droplevels(subset(part_rating_df_recoded,dimension=='Meaning')),wid=ID,dv=response,within=.(configuration),type=3)$ANOVA
droplevels(subset(part_rating_df_recoded,dimension=='Meaning'))%>%group_by(dimension)%>%rstatix::t_test(response_recoded~configuration,paired=T)
droplevels(subset(part_rating_df_recoded,dimension=='Meaning'))%>%group_by(dimension)%>%rstatix::cohens_d(response_recoded~configuration,paired=T)

# ez::ezANOVA(droplevels(subset(part_rating_df_recoded,dimension=='Emotion')),wid=ID,dv=response,within=.(configuration),type=3)$ANOVA
droplevels(subset(part_rating_df_recoded,dimension=='Emotion'))%>%group_by(dimension)%>%rstatix::t_test(response_recoded~configuration,paired=T)
droplevels(subset(part_rating_df_recoded,dimension=='Emotion'))%>%group_by(dimension)%>%rstatix::cohens_d(response_recoded~configuration,paired=T)

# ez::ezANOVA(droplevels(subset(part_rating_df_recoded,dimension=='Intent')),wid=ID,dv=response,within=.(configuration),type=3)$ANOVA
droplevels(subset(part_rating_df_recoded,dimension=='Intent'))%>%group_by(dimension)%>%rstatix::t_test(response_recoded~configuration,paired=T)
droplevels(subset(part_rating_df_recoded,dimension=='Intent'))%>%group_by(dimension)%>%rstatix::cohens_d(response_recoded~configuration,paired=T)

# ez::ezANOVA(droplevels(subset(part_rating_df_recoded,dimension=='Motion')),wid=ID,dv=response,within=.(configuration),type=3)$ANOVA
droplevels(subset(part_rating_df_recoded,dimension=='Motion'))%>%group_by(dimension)%>%rstatix::t_test(response~configuration,paired=T)
droplevels(subset(part_rating_df_recoded,dimension=='Motion'))%>%group_by(dimension)%>%rstatix::cohens_d(response~configuration,paired=T)
