
library(stringr)
library(ggplot2)
library(dplyr)
library(car)
library(reshape2)
library(tidyr)

##check for the presence of certain words in the data
#first prepare the data for it
data$text = tolower(data$text)
data$text = str_replace_all(data$text, "ã©", "é")
data$text = str_replace_all(data$text, "ã¡", "á")
data$text = str_replace_all(data$text, "ã³", "ó")
data$text = str_replace_all(data$text, "ãº", "ú")
data$text = str_replace_all(data$text, "ã±", "ñ")
data$text = str_replace_all(data$text, "ã", "í")

 
data$negro_count = str_count(data$text, paste(c("negro ", "negrín ", "negrin ", "negros ", "negrínes ", "negrines ", "negras ", "negra "), collapse = "|"))
data$anon_count = str_count(data$text, paste(c("anon", "anons ", "anoncito ", "anoncitos ", "anona ", "anonas ", "anoncitas ", "anoncitas ", "anoncita "), collapse = "|"))
data$kek_count = str_count(data$text, paste(c("kek ", "kkk "), collapse = "|"))
data$question_count = str_count(data$text, "`?`")
data$corona_count = str_count(data$text, paste(c("corona", "coronoso ", "coronosos ", "coronosa ", "coronosas ", "covid", "covid-19", "covid ", "covid 19"), collapse = "|"))


data$anon_logi = FALSE
data$anon_logi[data$anon_count != 0] = TRUE

data$negro_logi = FALSE
data$negro_logi[data$negro_count != 0] = TRUE

data$kek_logi = FALSE
data$kek_logi[data$kek_count != 0] = TRUE

data$question_logi = FALSE
data$question_logi[data$question_count != 0] = TRUE

data$corona_logi = FALSE
data$corona_logi[data$corona_count != 0] = TRUE

#text proportion in thread
thread_nchar = data %>% group_by(subgraph) %>%
  summarise(thread_nchar = sum(nchar(str_trim(text, "both"))))

data = left_join(data, thread_nchar, by = "subgraph")

data$text_length = nchar(str_trim(data$text, "both"))

data$text_proportion = data$text_length / data$thread_nchar

#anon
anon = table(data$anon_logi, data$neighbors)
hasnt_anon = anon[c(TRUE,FALSE)]
has_anon = anon[c(FALSE,TRUE)]
anon_prop = has_anon/hasnt_anon
neighbors = c(1:length(anon_prop))
anon_data = as.data.frame(cbind(anon_prop, neighbors))

#data exclucion
anon_data = anon_data[anon_data$anon_prop != 0 & anon_data$anon_prop != Inf,]

ggplot(anon_data, aes(x = anon_prop, y = neighbors))+
  geom_point() +
  stat_smooth(method = "lm") +
  scale_x_continuous(name = "Probability of Concept 'Anon'") +
  scale_y_continuous(name = "Degree") +
  theme_minimal()

anon_model = lm(data = anon_data, formula = neighbors ~ anon_prop)
summary(anon_model)#doesn't yet account for text length
qqPlot(anon_model$residuals)
shapiro.test(anon_model$residuals)

#proportion of negro 
#negro 
negro = table(data$negro_logi, data$neighbors)
hasnt_negro = negro[c(TRUE,FALSE)]
has_negro = negro[c(FALSE,TRUE)]
negro_prop = has_negro/hasnt_negro
neighbors = c(1:length(negro_prop))
negro_data = as.data.frame(cbind(negro_prop, neighbors))

#data exclusion
#exclude uncalculable values
negro_data = negro_data[negro_data$negro_prop != 0 & negro_data$negro_prop != Inf,]
#exclude the outlier
#negro_data = negro_data[negro_data$negro_prop != 2,]
hist(log(negro_data$negro_prop))
qqPlot(log(negro_data$negro_prop))
shapiro.test(log(negro_data$negro_prop))

ggplot(negro_data, aes(x = negro_prop, y = neighbors)) +
  geom_point() +
  stat_smooth(method = "lm") +
  scale_x_continuous(name = "Probability of Concept 'Negro'") +
  scale_y_continuous(name = "Degree") +
  theme_minimal()

negro_model = lm(data = negro_data, formula = neighbors ~ negro_prop)
summary(negro_model) #we still have to account for text length
qqPlot(negro_model$residuals)
shapiro.test(negro_model$residuals)


#works as well for kek
kek = table(data$kek_logi, data$neighbors)
hasnt_kek = kek[c(TRUE,FALSE)]
has_kek = kek[c(FALSE,TRUE)]
kek_prop = has_kek/hasnt_kek
neighbors = c(1:length(kek_prop))
kek_data = as.data.frame(cbind(kek_prop, neighbors))

#data exclusion
kek_data = kek_data[kek_data$kek_prop != 0 & kek_data$kek_prop != Inf,]

#normal distribution
hist(kek_data$kek_prop)
qqPlot(log(kek_data$kek_prop))
shapiro.test(log(kek_data$kek_prop))

ggplot(kek_data, aes(x = kek_prop, y = neighbors)) +
  geom_point() +
  stat_smooth(method = "lm")+
  scale_x_continuous(name = "Probability of Concept 'kek'") +
  scale_y_continuous(name = "Degree") +
  theme_minimal()

kek_model = lm(data = kek_data, formula = neighbors ~ kek_prop)
summary(kek_model)
qqPlot(kek_model$residuals)
shapiro.test(kek_model$residuals)

#corona
cor = as.data.frame(dcast(data,time+neighbors~corona_logi,value.var = "corona_logi", fun.aggregate = length))
cor$corona_prop = cor$`TRUE` / cor$`FALSE`
cor = cor[cor$corona_prop != 0 & cor$corona_prop != Inf,]

hist(cor$corona_prop)
shapiro.test(cor$corona_prop)

ggplot(cor, aes(x = corona_prop, y = neighbors, col = time)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x + I(log(x))) +
  scale_x_continuous(name = "Probability of topic 'Corona'") +
  scale_y_continuous(name = "Degree") +
  theme_minimal()

corona_model = lm(data = cor, formula = neighbors ~ corona_prop + time + I(log(corona_prop)))
summary(corona_model)
qqPlot(corona_model$residuals)
shapiro.test(corona_model$residuals)

sum(data$anon_logi)
sum(data$kek_logi)
sum(data$negro_logi)
sum(data$corona_logi)
