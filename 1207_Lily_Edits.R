library(ngram)
library(ggplot2)
library(gridExtra)

#f = file.choose()
f = "1206_data_raw.csv"
data_raw <- read.csv(f, stringsAsFactors = FALSE)

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

View(data_raw)
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


View(data)

# histogram of ours streamed by gender, not including those who left gender_id blank
hours.stream.hist <- ggplot(subset(data, !(gender_id == "")) , aes(x = hours_streaming_week, fill = gender_id)) + 
                      geom_histogram(alpha = 0.65, binwidth = 2, position = "identity") + facet_wrap(gender_id ~ ., nrow=2) + 
                      scale_fill_brewer(palette = "Set1")
                      #scale_fill_manual(breaks = c("Female", "Male", "Nonbinary", "Prefer not to answer"), 
                      #                   values=c("red", "blue", "green", "black"))

class.stream.hist <- ggplot(subset(data, !(class == "")), aes(x = hours_streaming_week, y = ..density.., fill = class)) + 
                      geom_histogram(alpha = 0.65, binwidth = 2, position = "identity") + facet_wrap(class ~ ., nrow=2)+ 
                      scale_fill_brewer(palette = "Set2")
hours.stream.hist
class.stream.hist

# Create new factor variable for concentration category
data$conc_cat = data$concentration
levels(data$conc_cat) <- list(humanitites=c("African and African American Studies", "Anthropology", "Classics", "Comparative Literature",
                                            "East Asian Studies", "English", "Folklore and Mythology", "Germanic Languages and Literatures", "History",
                                            "History and Literature", "History and Science", "History of Art and Architecture","Linguistics", "Music", 
                                            "Near Eastern Languages and Civilizations", "Philosophy", "Religion, Comparative Study of", "Romance Languages and Literatures",
                                            "Slavic Languages and Literatures", "Social Studies", "South Asian Studies", "Theater, Dance, and Media", "Visual and Environmental Studies",
                                            "Studies of Women, Gender, and Sexuality"),
                              stem=c("Applied Mathematics", "Astrophysics", "Biomedical Engineering", "Chemical and Physical Biology", "Chemistry",
                                     "Chemistry and Physics", "Computer Science", "Electrical Engineering", "Engineering Sciences", "Earth and Planetary Sciences",
                                     "Environmental Science and Engineering","Human Development and Regenerative Biology", "Human Evolutionary Biology", 
                                     "Integrative Biology","Mathematics", "Mechanical Engineering", "Molecular and Cellular Biology", "Neurobiology",
                                     "Physics","Statistics"),
                              blank="",
                              undeclared = "Undecided",
                              other = "Special Concentrations",
                              socsci = c("Environmental Science and Public Policy", "Economics", "Government","Psychology", "Sociology"),
                              joint = "Joint/Non-conventional concentrators")



# only included the data with demographics that included people who answered with their gender identity
# this eliminates those who did not answer with their class year
basic.lm <- lm(hours_streaming_week ~ upper_freshmen + gender_id + num_activites + conc_cat, 
               data = subset(data, !(gender_id =="")))
summary(basic.lm)

upper.under.lm <- lm(hours_streaming_week ~ upper_freshmen, data = subset(data, !(upper_freshmen =="blank")))
summary(upper.under.lm)

num.actvities.lm <- lm(hours_streaming_week ~ num_activites, data = data)
summary(num.actvities.lm)

conccat.lm = lm(hours_streaming_week ~ conc_cat, data = data)
summary(conccat.lm)

#################################LILY ADDITIONS#################################
## filter to only those who finished survey
data <- data %>%
  filter(
    progress == "100"
  )

# create variable for time spent in extracurriculars

data <- data %>% rowwise() %>%
  mutate(
    time_service = as.numeric(time_service),
    time_social = as.numeric(time_social),
    time_preprof = as.numeric(time_preprof),
    time_art_music = as.numeric(time_art_music),
    time_politics = as.numeric(time_politics),
    time_research = as.numeric(time_research),
    time_job = as.numeric(time_job),
    time_paf = as.numeric(time_paf),
    time_tutor = as.numeric(time_tutor),
    time_var_sports = as.numeric(time_var_sports),
    time_club_sports = as.numeric(time_club_sports),
    time_im_sports = as.numeric(time_im_sports),
    total_time_extracurr = sum(time_service,time_social,time_preprof,
                               time_art_music, time_politics,
                               time_research,time_job,time_paf,time_tutor,
                               time_var_sports,time_club_sports,time_im_sports, 
                               na.rm = TRUE)
  )

# get some basic demographics of respondents 
summary(data$gender_id)
summary(data$class)
summary(data$upper_neighborhood)
summary(data$conc_cat)

# histogram of number of groups 
summary(data$num_groups)
ggplot(data, aes(num_groups))+
  geom_histogram(binwidth=1, color="darkblue", fill="seagreen3")+
  theme_calc()+scale_colour_calc()+labs(x="Number of Groups", y = "Count",
                                        title="Histogram of Number of Groups")+
  scale_x_continuous(breaks=seq(0,8,1))



# histogram of number of activites 
summary(data$num_activites)
ggplot(data, aes(num_activites))+
  geom_histogram(binwidth=1, color="darkblue", fill="seagreen3")+
  theme_calc()+scale_colour_calc()+labs(x="Number of Activities", y = "Count",
                                        title="Histogram of Number of Activities")+
  scale_x_continuous(breaks=seq(0,11,1))

# histogram hours spent per week
summary(data$total_time_extracurr)
ggplot(data, aes(total_time_extracurr))+
  geom_histogram(binwidth=5, color="darkblue", fill="seagreen3")+
  theme_calc()+scale_colour_calc()+labs(x="Hours", y = "Count",
                                        title="Histogram of Number Hours Extracurriculars")+
  scale_x_continuous(breaks=seq(0,40,5))

# histogram of hours streaming per week
summary(data$hours_streaming_week)
ggplot(data, aes(hours_streaming_week))+
  geom_histogram(binwidth=2, color="darkblue", fill="seagreen3")+
  theme_calc()+scale_colour_calc()+labs(x="Hours", y = "Count",
                                        title="Histogram of Number Hours Streaming")+
  scale_x_continuous(breaks=seq(0,20,2))

# histogram of relative hours streaming
summary(data$time_streaming_relative_class)
data$time_streaming_relative_class <- factor(data$time_streaming_relative_class,
                                            levels = c("Less than half the time spent in class",
                                                       "Between half the time and the full time spent in class",
                                                       "About the same amount of time as the time spent in class",
                                                       "Between the full time spent and class and double the time spent in class",
                                                       "More than double the time spent in class"))

summary(data$streaming_with_others)

# check whether students' relative and absolute estimates match up 
summary(data$hours_streaming_week[data$time_streaming_relative_class=="Less than half the time spent in class"])
summary(data$hours_streaming_week[data$time_streaming_relative_class=="Between half the time and the full time spent in class"])
summary(data$hours_streaming_week[data$time_streaming_relative_class=="About the same amount of time as the time spent in class"])

# hypothesis testing 
# mean streaming, freshmen
mean(data$hours_streaming_week[data$upper_freshmen=="freshman"])
# mean streaming, upper
mean(data$hours_streaming_week[data$upper_freshmen=="upperclassman"])
# t.test 
t.test(data$hours_streaming_week[data$upper_freshmen=="freshman"],
       data$hours_streaming_week[data$upper_freshmen=="upperclassman"],
       var.equal = TRUE)
# also do a linear regression
model.year <- lm(hours_streaming_week~upper_freshmen+gender_id+conc_cat+num_activites,
                 data=data)
summary(model.year)
# out of curiosity, hours extracurriculars for freshmen
mean(data$total_time_extracurr[data$upper_freshmen=="freshman"])
mean(data$total_time_extracurr[data$upper_freshmen=="upperclassman"])
t.test(data$total_time_extracurr[data$upper_freshmen=="freshman"],
       data$total_time_extracurr[data$upper_freshmen=="upperclassman"], 
       var.equal = TRUE)
# lm to control for othe rthings 
model.year2 <- lm(total_time_extracurr~upper_freshmen+gender_id+conc_cat, data=data)
summary(model.year2)

# now look at extracurricular involvement and streaming 
model.streaming <- lm(hours_streaming_week~total_time_extracurr+gender_id+conc_cat+
                        upper_freshmen, data)
summary(model.streaming)
# plot extracurriculars and streaming by class
ggplot(data,
       aes(x=hours_streaming_week, y=total_time_extracurr, color=gender_id)) +
  geom_point() +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE)+
  labs(x = "Hours Streaming", y = "Hours Extracurriculars")+
  guides(color=guide_legend(title="Gender"))

# lms of just hours of extracurriculars and streaming 
summary(lm(hours_streaming_week~total_time_extracurr, data=data))
summary(lm(hours_streaming_week~num_groups, data=data))
summary(lm(hours_streaming_week~num_activites, data=data))

summary(lm(hours_streaming_week~gender_id, data=data))
summary(lm(hours_streaming_week~conc_cat, data=data))
summary(lm(hours_streaming_week~ethnicity, data=data))

data$streaming_with_others <- as.factor(data$streaming_with_others)
summary(lm(hours_streaming_week~streaming_with_others, data=data))

# house neighborhoods?
summary(lm(data$hours_streaming_week~data$upper_neighborhood),
        data=subset(data, upper_freshmen=="upperclassman"))

# more lms 
# lm of just hours of extracurriculars and streaming 
summary(lm(total_time_extracurr~gender_id, data=data))
summary(lm(total_time_extracurr~conc_cat, data=data))
summary(lm(total_time_extracurr~ethnicity, data=data))

