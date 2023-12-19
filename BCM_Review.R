#Maya Powell
#BCM Review
#Initial code and analysis July 30 2023
#Updated plots August 31 2023
#Final version of plots and upload to github: 

#setup
setwd("~/Documents/McCoy Lab/Review Paper")
library(dplyr)
library(ggplot2); packageVersion("ggplot2")
library(RColorBrewer)
library(ggpubr); packageVersion("ggpubr")
library(viridis); packageVersion("viridis")

#loading data in
rev <- read.csv("Review_Paper_All_Papers_Final.csv", header = TRUE)
#for some reason it didn't add in the NAs so here's me doing that
rev <- read.csv("Review_Paper_All_Papers_Final.csv", header=TRUE,  strip.white = TRUE, sep=",",
         na.strings= c("999", "NA", " ", ""))  

#look at only relevant papers
rev.p <- rev %>% filter(primary=="1")
rev.r <- rev %>% filter(review=="1")
rev.pr <- rev %>% filter(primary=="1"|review=="1")

#summaries
type.counts <- rev %>%
  dplyr::select(primary,review,not_relevant,no_access,no_DOI) %>%
  summarize_all(~ sum(!is.na(.)))

sapply(rev,class)
rev <- rev %>% mutate_if(is.integer,as.numeric)

#plots of papers over the years - messing with bin sizes here a bit
p.time.plot <- rev.p %>% ggplot(aes(x=year)) +
  theme_classic(base_size = 25)+
  geom_histogram(bins = 43)
p.time.plot

r.time.plot <- rev.r %>% ggplot(aes(x=year)) +
  theme_classic(base_size = 25)+
  geom_histogram(bins = 43)
r.time.plot

#all relevant pubs plot
pr.time.plot <- rev.pr %>% ggplot(aes(x=year)) +
  theme_classic(base_size = 25)+
  geom_histogram(bins = 44, fill="lightblue4")+
  xlab("Year")+
  ylab("Publications")
pr.time.plot
ggsave(plot=pr.time.plot,file="publications_relevant_time.pdf")

#All pubs related to this topic
#Put this in paper!! - shows how they are growing over time
all.time.plot <- rev %>% ggplot(aes(x=year)) +
  theme_classic(base_size = 25)+
  geom_histogram(bins = 59, fill="lightblue4")+
  theme(axis.text.x = element_text(color = "black"), axis.text.y = element_text(color="black"))+
  xlab("Year")+
  ylab("Publications")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))
all.time.plot
ggsave(plot=all.time.plot,file="all_publications_time.png")

#environmental count data prep
envt.counts <- rev.pr %>%
  dplyr::select(marine,hypersaline,brackish,freshwater,acid,lab,terrestrial,geologic,cold,hot) %>%
  summarize_all(~ sum(!is.na(.)))
envt.counts <- t(envt.counts)
envt.counts <- as.data.frame(envt.counts)
colnames(envt.counts)[1]="counts"
envt.counts <- envt.counts %>% mutate(relative_counts = counts/sum(counts))
envt.counts$environment_type <- row.names(envt.counts)
envt.counts$mat = "mat"

#environmental bar plot
envt.bar <- ggplot(envt.counts, aes(y=factor(environment_type, level = c('lab', 'terrestrial', 'geologic', 'acid', 'cold', 'hot', 'freshwater', 'brackish', 'marine', 'hypersaline')), x=counts,fill=environment_type))+
  geom_col(show.legend = FALSE)+
  theme_classic(base_size = 25)+
  theme(axis.text.y = element_text(hjust = 0.95, vjust = 0.4,color = "black"), axis.text.x = element_text(color="black"))+
  #scale_fill_brewer(palette = "RdYlBu")+
  xlab("Publications")+
  ylab("Environment Type")
envt.bar <- envt.bar + 
  scale_fill_viridis(option = "C", discrete = TRUE, limits = c('lab', 'terrestrial', 'geologic', 'acid', 'cold', 'hot', 'freshwater', 'brackish', 'marine', 'hypersaline')) +
  scale_y_discrete(label = c('Lab', 'Terrestrial', 'Geologic', 'Acid', 'Cold', 'Hot', 'Freshwater', 'Brackish', 'Marine', 'Hypersaline'))
envt.bar
#ggsave(plot = envt.bar, filename = "environment_bar.pdf")

#testing to see if I want to do stacked bar plots instead
# envt.stack <- ggplot(envt.counts,aes(x=mat,y=relative_counts,fill=environment_type))+
#   geom_col(show.legend = TRUE)+
#   theme_classic(base_size = 25)+
#   theme(axis.text.x = element_blank(), axis.title.x = element_blank())+
#   scale_fill_brewer(palette = "RdYlBu")
# envt.stack
# ggsave(plot = envt.stack, filename = "environment_stack.pdf")
#palettes that I like: RdYlBu, BrBG, Paired, BuPu, PiYG

#nutrient count data prep
nut.counts <- rev.pr %>%
  dplyr::select(nitrogen,photosynthesis,heterotrophy,oxic,anoxic,methane,iron,metal,sulfur,phosphorus,lithification,toxins,cooperation) %>%
  summarize_all(~ sum(!is.na(.)))
nut.counts <- t(nut.counts)
nut.counts <- as.data.frame(nut.counts)
colnames(nut.counts)[1]="counts"
nut.counts <- nut.counts %>% mutate(relative_counts = counts/sum(counts))
nut.counts$environment_type <- row.names(nut.counts)
nut.counts$mat = "mat"

#nutrient bar graph
nut.bar <- ggplot(nut.counts,aes(y=factor(environment_type, level = c('cooperation', 'anoxic', 'oxic', 'toxins', 'lithification', 'metal', 'iron', 'sulfur', 'methane', 'phosphorus', 'nitrogen', 'heterotrophy', 'photosynthesis')),x=counts,fill=environment_type))+
  geom_col(show.legend = FALSE)+
  theme_classic(base_size = 25)+
  theme(axis.text.y = element_text(hjust = 0.95, vjust = 0.4,color = "black"), axis.text.x = element_text(color="black"))+
  xlab("Publications")+
  ylab("Nutrient Cycling")
nut.bar <- nut.bar + 
  scale_fill_viridis(option = "C", discrete = TRUE, limits = c('cooperation', 'anoxic', 'oxic', 'toxins', 'lithification', 'metal', 'iron', 'sulfur', 'methane', 'phosphorus', 'nitrogen', 'heterotrophy', 'photosynthesis')) +
  scale_y_discrete(label = c("Cooperative Processes", "Anoxic Processes", "Oxic Processes", "Toxin Production", "Lithification", "Other Metal Cycling", "Iron Cycling", "Sulfur Cycling", "Methane Cycling", "Phosphorus Cycling", "Nitrogen Cycling", "Heterotrophy", "Photosynthesis"))
nut.bar
#ggsave(plot = nut.bar, filename = "nutrients_bar.pdf")

#misc count data prep
misc.counts <- rev.pr %>%
  dplyr::select(climate_change,anthropogenic,health,boundary_layers,disturbance_grazing,virus,trophic,singular,not_mats) %>%
  summarize_all(~ sum(!is.na(.)))
misc.counts <- t(misc.counts)
misc.counts <- as.data.frame(misc.counts)
colnames(misc.counts)[1]="counts"
misc.counts <- misc.counts %>% mutate(relative_counts = counts/sum(counts))
misc.counts$misc_type <- row.names(misc.counts)
misc.counts$mat = "mat"

#misc bar plot
misc.bar <- ggplot(misc.counts,aes(y=factor(misc_type, level = c('not_mats', 'singular', 'boundary_layers', 'health', 'anthropogenic', 'climate_change', 'disturbance_grazing', 'virus', 'trophic')),x=counts,fill=misc_type))+
  geom_col(show.legend = FALSE)+
  theme_classic(base_size = 25)+
  theme(axis.text.y = element_text(hjust = 0.95, vjust = 0.4,color = "black"), axis.text.x = element_text(color="black"))+
  #scale_fill_brewer(palette="BuPu")+
  xlab("Publications")+
  ylab("Additional Factors")
misc.bar <- misc.bar + scale_fill_viridis(option = "C",discrete = TRUE, limits = c('not_mats', 'singular', 'boundary_layers', 'health', 'anthropogenic', 'climate_change', 'disturbance_grazing', 'virus', 'trophic'))+
  scale_y_discrete(label = c("Not BCMs", "Singular Microbes", "Boundary Layers", "Health Impacts", "Anthropogenic Impacts", "Climate Change", "Disturbance & Grazing", "Viruses", "Trophic Interactions"))
misc.bar
#ggsave(plot = misc.bar, filename = "misc_bar.pdf")

#saving graphs in 4 panel plot
gg.panel.all <- ggarrange(all.time.plot,envt.bar,nut.bar,misc.bar,nrow=2,ncol=2,labels=c("A","B","C","D"),font.label = list(size = 30))
gg.panel.all
ggsave(plot = gg.panel.all, filename = "4.panel.plot.all.png", w=13, h=10)
