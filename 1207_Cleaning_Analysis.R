library(ngram)
library(ggplot2)
library(gridExtra)

data_raw <- read.csv("~/Desktop/Senior_Fall/Gov_1010/1206_data_raw.csv", stringsAsFactors = FALSE)

data_raw <- data_raw[-c(1, 2), ]

names(data_raw) <- c("progress", "duration_seconds", "finished", "date", "id", "dist_channel", "language", 
                     "consent", "activity_types", "time_service", "num_service", "intensity_service", "time_social",
                     "num_social", "intensity_social", "time_preprof", "num_preprof", "intensity_preprof", 
                     "time_art_music", "num_arts_music", "intensity_arts_music", "time_politics", "num_politics",
                     "intensity_politics", "time_research", "num_research", "intensity_research", "time_job",
                     "num_job", "intensity_job", "time_paf", "num_paf", "intensity_paf", "time_tutor", "num_tutor",
                     "intensity_tutor", "time_var_sports", "num_var_sports", "intensity_var_sports", 
                     "time_club_sports", "num_club_sports", "intensity_club_sports", "time_im_sports", "num_im_sports",
                     "intensitvy_im_sports", "stream_platforms", "stream_platforms_fill_in", "hours_streaming_week",
                     "time_streaming_relative_class", "streaming_with_others", "unwind_agree", "take_break_agree", 
                     "background_noise_agree", "social_discomfort_agree", "social_activity_agree", "fall_asleep_agree",
                     "ethnicity", "ethnicity_other", "gender_id", "gender_id_other", "class", "class_other", "fresh_neighborhood",
                     "fresh_neighborhood_other", "upper_neighborhood", "upper_neighborhood_other", "blocking_group",
                     "concentration", "joint_primary", "joint_secondary", "concentration_open")

# View(data_raw)
data <- data.frame(data_raw)

# make needed cols numerica
data$num_arts_music <- as.numeric(data$num_arts_music)
data$num_club_sports <- as.numeric(data$num_club_sports)
data$num_im_sports <- as.numeric(data$num_im_sports)
data$num_job <- as.numeric(data$num_job)
data$num_paf <- as.numeric(data$num_paf)
data$num_politics <- as.numeric(data$num_politics)
data$num_preprof <- as.numeric(data$num_preprof)
data$num_research <- as.numeric(data$num_research)
data$num_service <- as.numeric(data$num_service)
data$num_social <- as.numeric(data$num_social)
data$num_tutor <- as.numeric(data$num_tutor)
data$num_var_sports <- as.numeric(data$num_var_sports)

# code hours of streaming as continuous, ignoring 20+ and coding as just 20
data$hours_streaming_week <- gsub("\\+", "", data$hours_streaming_week)
data$hours_streaming_week <- as.numeric(data$hours_streaming_week)

data[is.na(data)] <- 0

num_groups <- list(rep(0, nrow(data)))[[1]]
num_activites <- list(rep(0, nrow(data)))[[1]]
for (i in 1:nrow(data)){
  num_groups[i] <- wordcount(data$activity_types[i], sep = ",")[1]
  num_activites[i] <- sum(data$num_arts_music[i], data$num_club_sports[i], data$num_im_sports[i], data$num_job[i], data$num_paf[i],
                          data$num_politics[i], data$num_preprof[i], data$num_research[i], data$num_service[i], data$num_social[i],
                          data$num_tutor[i], data$num_var_sports[i])
}


data$num_groups = num_groups
data$num_activites = num_activites

data$concentration <- as.factor(data$concentration)
data$upper_neighborhood <- as.factor(data$upper_neighborhood)
data$class <- as.factor(data$class)
data$ethnicity <- as.factor(data$ethnicity)
data$gender_id <- as.factor(data$gender_id)

# create new factor column for upperclassmen/freshman
data$upper_freshmen <- data$class
data$upper_freshmen
levels(data$upper_freshmen) <- list(upperclassman=c("2019", "2020", "2021"), freshman="2022", blank="")


# View(data)

# histogram of ours streamed by gender, not including those who left gender_id blank
hours.stream.hist <- ggplot(subset(data, !(gender_id == "")) , aes(x = hours_streaming_week, fill = gender_id)) + 
                      geom_histogram(alpha = 0.65, binwidth = 2, position = "identity") + facet_wrap(gender_id ~ ., nrow=2) + 
                      scale_fill_brewer(palette = "Set1")
                      # scale_fill_manual(breaks = c("Female", "Male", "Nonbinary", "Prefer not to answer"), 
                      #                    values=c("red", "blue", "green", "black"))

class.stream.hist <- ggplot(subset(data, !(class == "")), aes(x = hours_streaming_week, y = ..density.., fill = class)) + 
                      geom_histogram(alpha = 0.65, binwidth = 2, position = "identity") + facet_wrap(class ~ ., nrow=2)+ 
                      scale_fill_brewer(palette = "Set2")
hours.stream.hist
class.stream.hist

# only included the data with demographics that included people who answered with their gender identity
# this eliminates those who did not answer with their class year
basic.lm <- lm(hours_streaming_week ~ upper_freshmen + gender_id + num_activites + concentration, 
               data = subset(data, !(gender_id =="")))
summary(basic.lm)

upper.under.lm <- lm(hours_streaming_week ~ upper_freshmen, data = subset(data, !(upper_freshmen =="blank")))
summary(upper.under.lm)

num.actvities.lm <- lm(hours_streaming_week ~ num_activites, data = data)
summary(num.actvities.lm)















