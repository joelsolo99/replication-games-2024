#### SPACE ####

# Load required libraries for data manipulation, visualisation, and parallel processing
library('dplyr')         # For data manipulation using pipes and verbs like filter, mutate, and summarise
library('ggplot2')       # For creating data visualisations
library('foreach')       # For performing iterations, particularly in parallel
library('doParallel')    # For enabling parallel computing functionality

## Set working directory ##
# Get the file path of the current script in RStudio
thisFilePath <- rstudioapi::getSourceEditorContext()$path
# Extract the directory of the current script file
thisDir <- dirname(thisFilePath)
# Set the working directory to the script's location
setwd(thisDir)

## Set parallel computation ##
# Set the number of cores available for multi-threading operations
options(mc.cores = parallel::detectCores())  # Automatically detect the number of cores
rstan::rstan_options(auto_write = TRUE)      # Save compiled Stan models to avoid recompilation
cores <- detectCores()                       # Detect the total number of cores on the machine
cl <- makeCluster(cores[1] - 1)              # Create a cluster using all but one core (to avoid overloading)
registerDoParallel(cl)                       # Register the parallel backend for 'foreach'

## Miscellaneous options ##
# Set contrasts for factors to use Helmert and polynomial contrasts
options(contrasts = c('contr.helmert', 'contr.poly'))
# Define a custom operator for "not in"
`%notin%` <- Negate(`%in%`)

#
#
#### DATA LOADING ####

# Get data file #
# List all CSV files in the 'DATA_RAW/plt_monkeys/' directory
filenames <- list.files(paste0(getwd(), '/DATA_RAW/plt_monkeys/'), pattern = '*.csv')
# Exclude the specific file 'frame_coding.csv' from the list
filenames <- filenames[!grepl('frame_coding.csv', filenames)]

# Load all data files into a single data frame using 'foreach'
foreach(file = filenames, .combine = rbind.data.frame) %do% {
  Subject <- sub("\\_.*", "", file)  # Extract the subject identifier from the file name
  datafile <- cbind(Subject, read.csv(paste0(getwd(), '/DATA_RAW/plt_monkeys/', file), header = TRUE))
} -> bigDF

# Convert all character columns in the dataset to factors for analysis
bigDF %>% mutate_if(is.character, as.factor) -> bigDF
# Ensure the 'Trial' column is treated as a factor
bigDF$Trial <- as.factor(bigDF$Trial)

# Find back conditions #
# Load the 'frame_coding.csv' file, which presumably contains experimental conditions
condition_df <- read.csv(paste0(getwd(), '/DATA_RAW/plt_monkeys/frame_coding.csv'), header = TRUE)
# Convert character columns in this file to factors
condition_df %>% mutate_if(is.character, as.factor) -> condition_df
# Ensure the 'Trial' column is treated as a factor
condition_df$Trial <- as.factor(condition_df$Trial)
# Drop any levels in 'condition_df' that don't match the trials in 'bigDF'
condition_df <- droplevels(subset(condition_df, Subject:Trial %in% bigDF$Subject:bigDF$Trial))
# Remove the 'Duration' column and merge condition data into 'bigDF'
select(condition_df, -c(Duration)) %>%
  inner_join(bigDF, by = c('Subject', 'Trial', 'Direction')) -> bigDF.init
# Rename the first level of 'BodiesRelation' to "off-images" for clarity
levels(bigDF.init$BodiesRelation)[1] <- "off-images"

#
#
#### DIFFERENCES IN LOOKING TIME ####

# Extract subject IDs from filenames
stringr::str_split(filenames, '_', simplify = TRUE)[, 1] -> initial_sample
bigDF.init -> frame_df

# Calculate frame durations for each observation
frame_df %>% mutate(Duration.Frame = End.Frame - Start.Frame) -> frame_df
# Summarise total frame duration by direction
frame_df %>% group_by(Direction) %>% summarise(Duration.Frame = sum(Duration.Frame)) -> frame_df
# Convert data into a wide format and calculate percentages for each direction
frame_df %>%
  tidyr::pivot_wider(names_from = Direction, values_from = Duration.Frame) %>%
  mutate(TOTAL = rowSums(across(where(is.numeric)))) -> frame_df
(frame_df / frame_df$TOTAL) * 100

## General looking ##
# Calculate overall looking times
bigDF.init -> dur_df
dur_df %>% mutate(Duration.Frame = End.Frame - Start.Frame, Duration = Duration.Frame * (1 / 30)) -> dur_df
dur_df %>%
  group_by(Group, Subject, Trial, BodiesRelation) %>%
  summarise(Duration = sum(Duration)) -> dur_df

## Differential looks on images ##
# Add a column indicating whether the look was 'on' or 'off' images
dur_df %>% mutate(images = ifelse(BodiesRelation != 'off-images', 'on', 'off')) -> images_df
images_df %>%
  group_by(Group, Subject, Trial, images) %>%
  summarise(Duration = sum(Duration)) %>%
  tidyr::spread(images, Duration) -> images_df
# Replace NA values with 0
images_df[is.na(images_df)] <- 0
# Flag trials where participants made no looks on images
images_df %>% mutate(nolook = ifelse(on < 0.5, TRUE, FALSE)) -> images_df

# Summarise the proportion of trials rejected due to no looking
images_df %>%
  group_by(Group, Subject) %>%
  summarise(total = n(), nolook = sum(nolook), included = total - nolook, TOTAL_on = sum(on)) %>%
  arrange(TOTAL_on) -> trial_rej_df
trial_rej_df %>% mutate(`% rejected` = (nolook / total) * 100) -> trial_rej_df
# Get descriptive statistics for trials rejected and included
psych::describe(trial_rej_df[, 4:6])
psych::describe(trial_rej_df$`% rejected`)

# Identify outlier trials and subjects
images_df %>% mutate(Trial_ID = paste0(Trial, Subject)) %>% filter(nolook) -> outlier_trials_df
trial_rej_df %>% mutate(outlier = ifelse(included < 2, TRUE, FALSE)) %>% filter(outlier) -> outlier_subjects_df

## Differential looking times ##
# Filter data to include only specific levels of 'BodiesRelation'
droplevels(dur_df %>% filter(BodiesRelation %in% c('facing', 'facing_away'))) -> diff_df


# Run a paired t-test
paired_t_test <- t.test(wide_df$facing, wide_df$facing_away, paired = TRUE)



# Reshape data for comparisons and remove outliers
diff_df %>% tidyr::spread(BodiesRelation, Duration) -> diff_df
diff_df %>% mutate(Trial_Subject = paste0(Trial, Subject)) -> diff_df
droplevels(diff_df %>% filter(Trial_Subject %notin% outlier_trials_df$Trial_ID)) -> diff_df
droplevels(diff_df %>% filter(Subject %notin% outlier_subjects_df$Subject)) -> diff_df
diff_df[is.na(diff_df)] <- 0



# Perform the binomial test: this is what they actually did
binom_test <- binom.test(13, 15, p = 0.5, alternative = "two.sided")

# This is what we think they should have done

# Ungroup the data and count the number of trials
binom_counts <- diff_df %>%
  ungroup() %>%  # Remove grouping to summarise across all data
  summarise(
    facing_greater = sum(facing > facing_away),
    facing_away_greater = sum(facing < facing_away)
  )

# Extract counts
facing_greater <- binom_counts$facing_greater
facing_away_greater <- binom_counts$facing_away_greater
total_trials <- facing_greater + facing_away_greater

binom_test <- binom.test(facing_greater, total_trials, p = 0.5, alternative = "two.sided")

# Calculate difference in looking times and summarise
diff_df %>% mutate(Difference = (facing - facing_away) / (facing + facing_away)) -> diff_df



diff_df %>%
  group_by(Group, Subject) %>%
  summarise(Difference = mean(Difference)) -> diff_df






diff_df %>%
  ungroup %>%
  summarise(N = n(), M = mean(Difference), sd = sd(Difference), sem = goeveg::sem(Difference)) %>%
  mutate_if(is.numeric, round, 2)

