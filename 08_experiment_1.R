#import modules
library(car)
library(ggplot2)
library(lme4)
library(rgl)
library(caret)

#orig_data = thread_data
#thread_data = orig_data[orig_data$model_coherence > median(orig_data$model_coherence),]
#we use this to test whether or not it makes a difference to only include highly coherent models
#cohernece choice
noto = c(1:8)
cohe = c(0.303977,0.693671,0.667608,0.609873,0.588151,0.710454,0.612601,0.66332)
df = as.data.frame(cbind(noto,cohe))

ggplot(df, aes(noto, cohe)) +
  geom_point() +
  geom_line(color = "Blue", size = 0.8) +
  theme_minimal() +
  scale_x_continuous(name = "Nº of Topics modelled") +
  scale_y_continuous(name = "Coherence of generated model")  +
  geom_point(aes(x = 2, y = 0.693671), col = "red", fill = "red", size = 3, shape = 17) +
  geom_point(aes(x = 6, y = 0.710454), col = "red", fill = "red", size = 2.5, shape = 15) 
  
#coherence
ggplot(thread_data, aes(x = model_coherence)) +
  geom_histogram(binwidth = 0.05) +
  theme_minimal() +
  scale_x_continuous(name = "Model Coherence") +
  scale_y_continuous(name = "Count")

mean(thread_data$model_coherence)
shapiro.test(thread_data$model_coherence)
qqPlot(thread_data$model_coherence)

ggplot(thread_data, aes(x = model_topic_numbers)) +
  geom_histogram(binwidth = 1) +
  theme_minimal() +
  scale_x_continuous(name = "Nº of topic models") +
  scale_y_continuous(name = "Count")


#integer graph_centrality
ggplot(thread_data, aes(x = mean_mean_similarity)) +
  geom_histogram() +
  theme_minimal() +
  scale_x_continuous(name = "Mean of Means Similarities") +
  scale_y_continuous(name = "Count")

ggplot(thread_data, aes(x = model_topic_numbers, y = mean_mean_similarity)) +
  geom_point(alpha = .3) +
  stat_smooth(method = "lm", formula = y ~ x +I (log(x))) +
  scale_x_continuous(name = "Nº of Topics modelled") +
  scale_y_continuous(name = "Mean of Means Similarities") +
  theme_minimal()

topics_vs_mean_similarity_model = lm(mean_mean_similarity ~ model_topic_numbers + I(log(model_topic_numbers)), data = thread_data)
summary(topics_vs_mean_similarity_model)
shapiro.test(topics_vs_mean_similarity_model$residuals)
#good way to model the confounding variable in the other models
#the model using the mean_mean_similarity after power transformation is the one that best allows us to model the influence of topic number on similarity
#we compared to the variables mean_median and geom_mean both normalized and non-normal AND to quadratic models of all the aforementioned as well
#logarithmic models with normalized means are the best way of doing this, but since it is best to approach this with untransformed data, we will use untransformed data
#we can thus use topic numbers as a proxy to similarity.


topic_convergence_model = glm(formula = model_topic_numbers ~ thread_length * diameter, data = thread_data, family = "poisson")
summary(topic_convergence_model)#inclusion of graphtype didn't enhance the performance of the model
Anova(topic_convergence_model)

plot3d(glm(family = "poisson", formula = model_topic_numbers ~ thread_length * diameter, data = thread_data),xlab = "",ylab="", zlab= "") 
rgl.bbox(color = "grey50",          # grey60 surface and black text
         emission = "grey50",       # emission color is grey50
         xlen = 0, ylen = 0, zlen = 0)  # Don't add tick marks
rgl.material(color = "black")
axes3d(edges = c("x--", "y+-", "z--"),
       ntick = 6,                       # Attempt 6 tick marks on each side
       cex = .75)                       # Smaller font

# Add axis labels. 'line' specifies how far to set the label from the axis.
mtext3d("No of Nodes",       edge = "x--", line = 2)
mtext3d("Diameter", edge = "y+-", line = 3)
mtext3d("No of Topics",          edge = "z--", line = 3)


