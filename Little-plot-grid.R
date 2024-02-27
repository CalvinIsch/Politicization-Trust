library(ggplot2); library(dplyr); library(ggpubr); library(ggrepel); library(tidyr)

df <- read.csv('Study 4 R Data.csv')

# Calculate the mean values for each variable so we can order our figure nicely
trust <- c()
skept <- c()
slant <- c()
polit <- c()
all_groups <- c()
for (n in names(df %>% select(starts_with("Slant_")))){
  all_groups <- c(all_groups,substring(n,unlist(gregexpr('_', n))[1]+1))
}
for (g in all_groups){
  trust <- c(trust,mean(df[,paste("Trust_",g,sep='')],na.rm=TRUE))
  skept <- c(skept,mean(df[,paste('Skept_',g,sep='')],na.rm=TRUE))
  slant <- c(slant,mean(df[,paste("Slant_",g,sep='')],na.rm=TRUE))
  polit <- c(polit,mean(df[,paste("Polit_",g,sep='')],na.rm=TRUE))
}
df_means <- data.frame(all_groups,trust,slant,polit,skept)

# Add associated nice labels for the plot
df_means <- df_means[order(df_means$slant),]
df_means$nice_names <- c("Gender Studies", "Ethnic Studies", "Environmental Sci.", "Social Work", "Art", "Sociology",
                         "Music", "Psychology","Earth Science", "Foreign Language", "Education", "Literature", "Philosophy",
                         "Public Health", "Anthropology","Biology","Communication","Nursing","Physics","Political Science",
                         "History","Chemistry", "Computer Science", "IT", "Math", "Criminal Justice", "English", "Econ",
                         "Business", "Religious Studies")
df_means <- rename(df_means, Slant = slant)


# First, a function to plot the relationship between trust/skepticism and politicization for each discipline
# along with several variables to include/exclude different components of the graph
plot_congruence <- function(data, group,include_y,xlab_use,include_legend, include_x,trust_or_skept, is_top){
  
  # Select the group of interest
  group_prefix <- paste0("_", group)
  df_reg <- data %>%
    select(ends_with(group_prefix), Ideology) %>%
    drop_na() %>%
    # Create centered slant and ideology, congruence metric from if these match
    mutate(Slant_2 = get(paste0('Slant',group_prefix)) - 50,
           Ideo_2 = Ideology - 4,
           Matched_Bias = Slant_2 * Ideo_2,
           Congruence = case_when(
             is.na(Matched_Bias) ~ NA_character_,
             Matched_Bias < 0 ~ "Incongruent",
             Matched_Bias > 0 ~ "Congruent",
             TRUE ~ 'Neutral'
           )
    ) %>% drop_na()
  # Turn congruence into a factor with levels to match out colors
  df_reg$Congruence <- factor(df_reg$Congruence, levels = c('Incongruent','Neutral','Congruent')) 
  
  plot_return <- ggplot(df_reg, aes(x = get(paste0("Polit",group_prefix)), y = get(paste0(ifelse(trust_or_skept, "Trust", "Skept"),group_prefix)), color = Congruence)) + 
    geom_smooth(method = glm,se=F) + 
    scale_color_manual(values = c('#FF5733','#d4af37',"#007200")) + 
    ylab(ifelse(include_y, ifelse(trust_or_skept, "Trust", "Skepticism"), ""))  + ggtitle(xlab_use) + xlab(ifelse(include_x, "Politicization", "")) + 
    ylim(0.5, 6.2) + xlim(0.5,6.2)+ labs(color = NULL)+
    theme(
      axis.text = element_text(size = 28), # 25
      plot.margin = unit(c(ifelse(is_top,1,-0.05),ifelse(include_legend,1,-0.01),ifelse(include_x,1,-0.01),ifelse(include_y,1,-0.01)), 'lines'),
      plot.title = element_text(size = 30, hjust=0.5), #27
      axis.title.x = element_text(size = 28),
      axis.title.y = element_text(size = 28),
      legend.key=element_rect(fill="white"),
      legend.text=element_text(size=28),
      legend.position = "none",#ifelse(include_legend,'right',"none"),
      panel.background = element_rect(fill = 'white'),
      plot.background = element_rect(fill = 'white'),
      axis.text.x = element_text(size = ifelse(include_x, 20, 0)),
      axis.text.y = element_text(size = ifelse(include_y, 20, 0)),
      panel.border = element_rect(colour = "black", fill = NA, size = 1)
    )
  
  # Report the coefficients -- necessary for figure 4B (not for the main plot)
  d_c <- df_reg %>% filter(df_reg$Congruence == 'Congruent')
  m <- lm(get(paste0(ifelse(trust_or_skept, "Trust", "Skept"),group_prefix)) ~ get(paste0("Polit",group_prefix)), d_c)
  c <- as.numeric(m$coefficients[2])
  
  d_i <- df_reg %>% filter(df_reg$Congruence == 'Incongruent')
  m <- lm(get(paste0(ifelse(trust_or_skept, "Trust", "Skept"),group_prefix)) ~ get(paste0("Polit",group_prefix)), d_i)
  i <-  as.numeric(m$coefficients[2])
  
  d_n <- df_reg %>% filter(df_reg$Congruence == 'Neutral')
  m <- lm(get(paste0(ifelse(trust_or_skept, "Trust", "Skept"),group_prefix)) ~ get(paste0("Polit",group_prefix)), d_n)
  n <- as.numeric(m$coefficients[2])
  
  return(list(plot_return,c,i,n))
}

# Get a list of all groups and the labels we want to use on their plots
all_groups <- df_means[order(df_means$Slant),]$all_groups
all_group_labels <- df_means[order(df_means$Slant),]$nice_names

# Now create the plot for each org, keeping the various plot attributes depending
# on the position in the grid.
d <- list(); d_2 <- list(); labels_here <- c()
coefs_con_t <- c(); coefs_inc_t <- c(); coefs_neu_t <- c()
coefs_con_s <- c(); coefs_inc_s <- c(); coefs_neu_s <- c()
for (org in 1:length(all_groups)){
  put_legend = F
  put_x = F
  put_y = F
  tp = F
  if (org / 6 >= 4.01) {
    put_x = T
  }
  if (org %% 6 == 0) {
    put_legend = T
  }
  if (org < 7) {
    tp = T
  }
  if (org %% 6 == 1){ 
    put_y = T
  }
  # Create trust plots
  plot <- plot_congruence(df, all_groups[org],put_y,all_group_labels[org],put_legend,put_x, T,tp)
  d[[org]] <- plot[[1]]
  coefs_con_t <- c(coefs_con_t,plot[[2]])
  coefs_inc_t <- c(coefs_inc_t,plot[[3]])
  coefs_neu_t <- c(coefs_neu_t,plot[[4]])
  
  # Create Skepticism plots
  plot_2 <- plot_congruence(df, all_groups[org],put_y,all_group_labels[org],put_legend,put_x, F,tp)
  d_2[[org]] <- plot_2[[1]]
  coefs_con_s <- c(coefs_con_s,plot_2[[2]])
  coefs_inc_s <- c(coefs_inc_s,plot_2[[3]])
  coefs_neu_s <- c(coefs_neu_s,plot_2[[4]])
  labels_here <- c(labels_here,"")
}

# Arrange Panel A - plotting skepticism
A <- ggarrange(plotlist=d_2, labels = labels_here,ncol = 6, nrow = 5, widths=c(0.8,0.78,0.78,0.78,0.78,0.78)) + bgcolor("White") + theme(plot.margin = margin(.9,.9,.9,.9, "in"), panel.border = element_rect(color = "black",fill = NA,size = 1.25))
ggsave("figures/Plot-w-30.png",A,width = 27,height = 22.5,units = "in",dpi = 300)
