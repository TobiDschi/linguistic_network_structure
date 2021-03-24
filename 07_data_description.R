#import necessary packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(car)
library(stringr)
library(corrplot)
library(MASS)
library(rsq)

#define function to convert factors w/ levls 1/2 to booleans (0/1)
make_bool = function(column){
  column = as.character(column)
  column[column == "False"] = "FALSE"
  column[column == "True"] = "TRUE"
  column = as.logical(column)
}

#import data from hard drive
file = "" #import final_dataset.csv
data = read.csv(file, header = TRUE)
data = data[,2:33]

#we need to cut down the strings from the community URL, because they contain unnecessary information about the page
#we revocered the messages from, we only want the first 29 characters, which tell us the community we published in
data$id = as.character(data$id)
data$text = as.character(data$text)
data$retrieved = as.character.POSIXt(data$retrieved) #change to use as time series
data$central = make_bool(data$central)
data$incomplete = make_bool(data$incomplete)
data$center_most_central = make_bool(data$center_most_central)
data$center_most_neighbors = make_bool(data$center_most_neighbors)
data$p.regular = make_bool(data$p.regular)
data$prep_text = as.character(data$prep_text)
data$community = strtrim(data$community, 28)
data$community = as.factor(data$community)
data$subgraph = as.character(data$subgraph)
data$time = "January 2021"
data$time[substr(data$retrieved,39,39) == 4] = "April 2020"
data$time = as.factor(data$time)
data = data[,-c(11,22,23,25)]
data$community = as.character(data$community)
data$community[data$community == "https://www.hispachan.org/ar"] = "Argentina"
data$community[data$community == "https://www.hispachan.org/bo"] = "Bolivia"
data$community[data$community == "https://www.hispachan.org/cc"] = "Centroamérica y Caribe"
data$community[data$community == "https://www.hispachan.org/cl"] = "Chile"
data$community[data$community == "https://www.hispachan.org/co"] = "Colombia"
data$community[data$community == "https://www.hispachan.org/es"] = "España"
data$community[data$community == "https://www.hispachan.org/mx"] = "México"
data$community[data$community == "https://www.hispachan.org/pe"] = "Perú"
data$community[data$community == "https://www.hispachan.org/uy"] = "Uruguay"
data$community[data$community == "https://www.hispachan.org/ve"] = "Venezuela"
data$community[data$community == "https://www.hispachan.org/hu"] = "Humanidades"
data$community[data$community == "https://www.hispachan.org/k/"] = "Economía"
data$community[data$community == "https://www.hispachan.org/m/"] = "Meta"
data$community[data$community == "https://www.hispachan.org/po"] = "Política"
data$community[data$community == "https://www.hispachan.org/q/"] = "LGTBQ+"
data$community[data$community == "https://www.hispachan.org/t/"] = "Tecnología"
data$community_type = "Region"
data$community_type[data$community %in% c("Humanidades", "Economía", "Meta", "Política", "LGTBQ+","Tecnología")] = "Topic"
data$community = as.factor(data$community)
data$community_type = as.factor(data$community_type)

#calculate length of threads
data$thread_length = 1
lengths = aggregate(data$thread_length, by = list(data$subgraph), FUN = sum)
data = data[,1:31]
colnames(lengths) = c("subgraph", "thread_length")
data = left_join(data, lengths, by = "subgraph")
rm(lengths)

#make a dataframe for measures that concern the whole graph
thread_data = data[!duplicated(data$subgraph),]
thread_data = thread_data[,- c(1,3,6,7,8,9,10)]
mean_mean_similarity = data %>% group_by(subgraph) %>% summarise(mean_mean_similarity = mean(mean_similarity))
thread_data = left_join(thread_data, mean_mean_similarity, by = "subgraph")
rm(mean_mean_similarity)
mean_median_similarity = data %>% group_by(subgraph) %>% summarise(mean_median_similarity = mean(median_similarity))
thread_data = left_join(thread_data, mean_median_similarity, by = "subgraph")
mean_geom_mean_similarity = data %>% group_by(subgraph) %>% summarise(mean_geom_mean_similarity = mean(geom_mean_similarity))
thread_data = left_join(thread_data, mean_geom_mean_similarity, by = "subgraph")
rm(mean_geom_mean_similarity, mean_median_similarity)
thread_data$thread_length = thread_data$thread_length.y
thread_data = thread_data[,-c(24,25)]

#data description thread length & diameter
summary(thread_data$thread_length)
summary(thread_data$thread_length[thread_data$time == "April 2020"])
summary(thread_data$thread_length[thread_data$time == "January 2021"])
sd(thread_data$thread_length)

ggplot(thread_data, aes(x = thread_length, fill = time)) +
  geom_density(alpha = .3, show.legend = FALSE) +
  theme_minimal() +
  scale_x_continuous(name = "Thread length") +
  scale_y_continuous(name = "Density") +
  facet_grid(time~.) 

ggplot(thread_data, aes(x = diameter, fill = time)) +
  geom_density(alpha = .3, show.legend = FALSE) +
  theme_minimal() +
  scale_x_continuous(name = "Diameter") +
  scale_y_continuous(name = "Density")

median(thread_data$diameter)
mean(thread_data$diameter)
summary(thread_data$diameter)

ggplot(thread_data[thread_data$graphtype == "decentralized",], aes(x = thread_length, y = diameter)) +
  geom_jitter(alpha = .5) +
  geom_smooth(method = "lm", formula = y ~ x +I(log(x))) +
  theme_minimal() +
  scale_x_continuous(name = "Thread Lenght") +
  scale_y_continuous(name = "Diameter")

diameter_by_length_log = glm(formula = diameter  ~ thread_length + I(log(thread_length)), family = "poisson", data = thread_data[thread_data$graphtype == "decentralized",])
summary(diameter_by_length_log)
Anova(diameter_by_length_log)
rsq(diameter_by_length_log)

#communities
table(data$community[data$time == "April 2020"])
summary(c(1436, 1031, 1972, 2207, 1976, 3057,2820,2352,1377,5425))
mean(c(1436, 1031, 1972, 2207, 1976, 3057,2820,2352,1377,5425)) - sd(c(1436, 1031, 1972, 2207, 1976, 3057,2820,2352,1377,5425))
mean(c(1436, 1031, 1972, 2207, 1976, 3057,2820,2352,1377,5425)) + sd(c(1436, 1031, 1972, 2207, 1976, 3057,2820,2352,1377,5425))
shapiro.test(c(1436, 1031, 1972, 2207, 1976, 3057,2820,2352,1377,5425))
qqPlot(c(1436, 1031, 1972, 2207, 1976, 3057,2820,2352,1377,5425))

table(data$community[data$time == "January 2021"])
summary(c(1347,2216,1873,1908,794,2756,1661,1391,1192,2880,3010,2944,2220,1279,3269))
mean(c(1347,2216,1873,1908,794,2756,1661,1391,1192,2880,3010,2944,2220,1279,3269)) - sd(c(1347,2216,1873,1908,794,2756,1661,1391,1192,2880,3010,2944,2220,1279,3269))
mean(c(1347,2216,1873,1908,794,2756,1661,1391,1192,2880,3010,2944,2220,1279,3269)) + sd(c(1347,2216,1873,1908,794,2756,1661,1391,1192,2880,3010,2944,2220,1279,3269))
shapiro.test(c(1347,2216,1873,1908,794,2756,1661,1391,1192,2880,3010,2944,2220,1279,3269))
qqPlot(c(1347,2216,1873,1908,794,2756,1661,1391,1192,2880,3010,2944,2220,1279,3269))

table(thread_data$community)  


#network topologies
table(thread_data$graphtype[thread_data$time == "April 2020"])
table(thread_data$graphtype[thread_data$time == "January 2021"])
table(thread_data$graphtype)

graphtype_by_community = table(thread_data$community, thread_data$graphtype)
chisq = chisq.test(graphtype_by_community)
chisq
corrplot(chisq$residuals, is.cor = FALSE)

#topologies and thread_length
ggplot(thread_data, aes(x = thread_length, fill = graphtype)) +
  geom_density(alpha = 0.5, show.legend = FALSE) +
  facet_grid(graphtype ~., scales = "free") +
  coord_cartesian(xlim = c(0, 50))+
  theme_minimal() +
  scale_x_continuous(name = "Thread Length") +
  scale_y_continuous(name = "Density")

mean(thread_data$thread_length[thread_data$graphtype == "centralized"])
mean(thread_data$thread_length[thread_data$graphtype == "decentralized"])
mean(thread_data$thread_length[thread_data$graphtype == "linear network"]) 
mean(thread_data$thread_length[thread_data$graphtype == "none"])
median(thread_data$thread_length[thread_data$graphtype == "centralized"])
median(thread_data$thread_length[thread_data$graphtype == "decentralized"])
median(thread_data$thread_length[thread_data$graphtype == "linear network"])
median(thread_data$thread_length[thread_data$graphtype == "none"])

kruskal.test(thread_data$thread_length ~ thread_data$graphtype)


#degree distribution of nodes
table(data$neighbors)
rank = c(1:length(unique(data$neighbors)))
count = sort(unique(data$neighbors), decreasing = TRUE)
expected_count = c(max(unique(data$neighbors)) / rank)
rank_data = as.data.frame(cbind(rank, count, expected_count))
rank_data = rank_data %>% gather(key = "label", value = "count", 2:3)

ggplot(rank_data, aes(x = rank, y = count, col = label)) +
  geom_line(size = 1, show.legend = FALSE) +
  scale_x_continuous(name = "Rank of Nodes by Degree") +
  scale_y_continuous(name = "Degree") +
  theme_minimal()

wilcox.test(rank_data$count ~ rank_data$label)
# there is a significant difference between the rank and the count but at least it gets into the vecinity of the phenomenon
degree = sort(unique(data$neighbors))
Count = as.vector(table(data$neighbors))
`Expected Count` = c(max(Count) / degree)
node_data = as.data.frame(cbind(degree,Count,`Expected Count`))
node_data = node_data %>% gather(key = "Distributions", value = "count",2:3 )

wilcox.test(node_data$count ~ node_data$Distributions)

ggplot(node_data, aes(x = degree, y = count, col = Distributions)) +
  geom_line(size = 1, show.legend = TRUE) +
  coord_cartesian(xlim = c(0,20)) +
  theme_minimal() +
  scale_x_continuous(name = "Degree")+
  scale_y_continuous(name = "Number of Nodes")

summary(data$neighbors)
quantile(data$neighbors, 0.999)

#then look at the centrality measures
ggplot(thread_data, aes(x = integer_graph_centrality)) +
  geom_histogram(binwidth = 0.1) +
  facet_wrap(~graphtype) +
  theme_minimal() +
  scale_x_continuous(name = "Graph Centrality") +
  scale_y_continuous(name = "Count")

mean(thread_data[thread_data$graphtype == "decentralized",]$integer_graph_centrality)
median(thread_data[thread_data$graphtype == "decentralized",]$integer_graph_centrality)
sd(thread_data[thread_data$graphtype == "decentralized",]$integer_graph_centrality)


ggplot(thread_data[thread_data$graphtype == "decentralized",], aes(x = integer_graph_centrality, fill = center_most_central)) +
  geom_density(alpha = .3, show.legend = FALSE) +
  theme_minimal() +
  scale_x_continuous(name = "Graph Centrality") +
  scale_y_continuous(name = "Density")

wilcox.test(thread_data[thread_data$graphtype == "decentralized",]$integer_graph_centrality ~ thread_data[thread_data$graphtype == "decentralized",]$center_most_central)

ggplot(thread_data[thread_data$graphtype == "decentralized" & thread_data$center_most_central == TRUE,], aes(x = diameter, y = integer_graph_centrality))+ 
  geom_point(alpha = .5) +
  stat_smooth(method = "lm", formula = y ~ x + I(log(x))) +
  theme_minimal() +
  scale_x_continuous(name = "Diameter") +
  scale_y_continuous(name = "Graph Centrality")

central_by_diameter = lm(formula = integer_graph_centrality ~ diameter + I(log(diameter)), data = thread_data[thread_data$graphtype == "decentralized" & thread_data$center_most_central == TRUE,]) 
summary(central_by_diameter)

shapiro.test(central_by_diameter$residuals)
qqPlot(central_by_diameter$residuals, envelope = .99)




