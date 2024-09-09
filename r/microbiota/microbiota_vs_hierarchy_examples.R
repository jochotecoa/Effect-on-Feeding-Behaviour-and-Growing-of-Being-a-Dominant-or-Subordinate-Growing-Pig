library(tidyverse)
library(dplyr)
library(lmerTest)

Genus_GG2_long_CCS <- read.delim2("~/gutbrain/data/Genus_GG2_long_CCS.txt") %>% 
  t() %>% 
  as.data.frame()

samples = Genus_GG2_long_CCS %>% rownames()
Genus_GG2_long_CCS$Genus = samples

colname = Genus_GG2_long_CCS[1, ]
colnames(Genus_GG2_long_CCS) = colname
Genus_GG2_long_CCS = Genus_GG2_long_CCS[-1, ]

Genus_GG2_long_CCS = Genus_GG2_long_CCS %>% 
  # select(Genus, g__Acetitomaculum, g__Anaerobutyricum, g__Coprococcus, 
  #        g__Treponema, g__Floccifex) %>% 
  mutate(across(contains(c("g__")), as.numeric)) %>% 
  mutate(Genus = gsub("X", "", Genus))

metadata_mod_long <- read.delim("~/gutbrain/data/metadata_mod_long.tsv")

Genus_meta = Genus_GG2_long_CCS %>% 
  merge.data.frame(y = metadata_mod_long, by.x = "Genus", by.y = "SampleID") 

Genus_meta$Samplingday <- gsub("07_2022", "2022-07-22", Genus_meta$Samplingday)
Genus_meta$Samplingday <- gsub("09_2022", "2022-09-05", Genus_meta$Samplingday)
Genus_meta$Samplingday <- gsub("10_2022", "2022-10-05", Genus_meta$Samplingday)
Genus_meta$Samplingday = Genus_meta$Samplingday %>% as.Date()

Genus_meta$Subject.ID_Samplingday = paste(Genus_meta$Subject.ID, 
                                          Genus_meta$Samplingday)

# Phase 1 -----------------------------------------------------------------


hierarchy_data_1 = readRDS('output/hierarchy_ranking_groups/dominance_phase_1_by_pen.rds') %>% 
  mutate(treatment = ifelse(Pen %% 2, 'Control', 'Stress'),
         Pen = as.factor(Pen)) %>% 
  filter(grepl("F840|M917", ID))

# hierarchy_data_1$ID[hierarchy_data_1$ID=='M904'] = 'M686'


genus_hierarchy = merge.data.frame(x = Genus_meta, y = hierarchy_data_1, 
                              by.x = "Subject.ID", 
                              by.y = "ID") %>% 
  filter(Samplingday == "2022-07-22")
colnames(genus_hierarchy) = make.names(colnames(genus_hierarchy))

genus_hierarchy_rel_phase1 = genus_hierarchy %>% select(contains("g__")) 
genus_hierarchy_rel_phase1 = genus_hierarchy_rel_phase1/rowSums(genus_hierarchy_rel_phase1)
rownames(genus_hierarchy_rel_phase1) = genus_hierarchy$Subject.ID
colnames(genus_hierarchy_rel_phase1) = colnames(genus_hierarchy_rel_phase1) %>% 
  gsub("g__", "", .)
genus_hierarchy_rel_F840 = genus_hierarchy_rel_phase1['F840',]
genus_hierarchy_rel_phases = genus_hierarchy_rel_F840
genus = order(genus_hierarchy_rel_F840 %>% unlist, decreasing = T) 
genus_hierarchy_rel_F840 = genus_hierarchy_rel_F840[, genus]
genus_hierarchy_rel_F840 = genus_hierarchy_rel_F840[, 1:15]

genus_hierarchy_rel_M917 = genus_hierarchy_rel_phase1['M917',]
genus = order(genus_hierarchy_rel_M917 %>% unlist, decreasing = T) 
genus_hierarchy_rel_M917 = genus_hierarchy_rel_M917[, genus]
genus_hierarchy_rel_M917 = genus_hierarchy_rel_M917[, 1:15]

top_genus = data.frame()
top_genus = rbind(top_genus, colnames(genus_hierarchy_rel_F840))
top_genus = rbind(top_genus, colnames(genus_hierarchy_rel_M917))
# bacteria = which(order(abs(as.numeric(genus_hierarchy_rel_phase1[1,] - genus_hierarchy_rel_phase1[2,])), decreasing = T) <= 10)
# 
# genus_hierarchy_rel_phase1 = genus_hierarchy_rel_phase1[, bacteria]

library(ggplot2)

# Assuming your dataset has a structure like this:
# Row 1: Observation 1
# Row 2: Observation 2
# Columns represent categories

# Let's assume your dataset is named genus_hierarchy_rel_phase1

custom_colors <-viridisLite::viridis(75) 
custom_colors = data.frame(custom_colors, genus = sort(colnames(genus_hierarchy_rel_phase1)))

# Convert the transposed data to a data frame
df <- as.data.frame(genus_hierarchy_rel_F840)


# Add a column for row names (observation names)
df$Observation <- row.names(df)

# Melt the data frame for plotting
library(reshape2)
df_melted <- melt(df, id.vars = "Observation")

cstm_clrs = custom_colors[custom_colors$genus %in% df_melted$variable, ]
cstm_clrs = cstm_clrs[order(match(cstm_clrs$genus, df_melted$variable)), ]
# Create a stacked barplot
ggplot(df_melted, aes(x = Observation, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  labs(x = "Observation", y = "Value", fill = "Category", title = "2022-07-22") +
  scale_fill_manual(values = cstm_clrs$custom_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) # Rotate x-axis labels for better readability


# Convert the transposed data to a data frame
df <- as.data.frame(genus_hierarchy_rel_M917)


# Add a column for row names (observation names)
df$Observation <- row.names(df)

# Melt the data frame for plotting
library(reshape2)
df_melted <- melt(df, id.vars = "Observation")

cstm_clrs = custom_colors[custom_colors$genus %in% df_melted$variable, ]
cstm_clrs = cstm_clrs[order(match(cstm_clrs$genus, df_melted$variable)), ]

# Create a stacked barplot
ggplot(df_melted, aes(x = Observation, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  labs(x = "Observation", y = "Value", fill = "Category", title = "2022-07-22") +
  scale_fill_manual(values = cstm_clrs$custom_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) # Rotate x-axis labels for better readability





# Phase 3_2 ---------------------------------------------------------------

hierarchy_data_3 = readRDS('output/hierarchy_ranking_groups/dominance_phase_3_by_pen.rds') %>% 
  mutate(treatment = ifelse(Pen %% 2, 'Control', 'Stress'),
         Pen = as.factor(Pen)) %>% 
  filter(grepl("F840|M917", ID))

# hierarchy_data_1$ID[hierarchy_data_1$ID=='M904'] = 'M686'


genus_hierarchy = merge.data.frame(x = Genus_meta, y = hierarchy_data_3, 
                                   by.x = "Subject.ID", 
                                   by.y = "ID") %>% 
  filter(Samplingday == "2022-09-05")
colnames(genus_hierarchy) = make.names(colnames(genus_hierarchy))

genus_hierarchy_rel_phase3_2 = genus_hierarchy %>% select(contains("g__")) 
genus_hierarchy_rel_phase3_2 = genus_hierarchy_rel_phase3_2/rowSums(genus_hierarchy_rel_phase3_2)
rownames(genus_hierarchy_rel_phase3_2) = genus_hierarchy$Subject.ID
colnames(genus_hierarchy_rel_phase3_2) = colnames(genus_hierarchy_rel_phase3_2) %>% 
  gsub("g__", "", .)
genus_hierarchy_rel_F840 = genus_hierarchy_rel_phase3_2['F840',]
genus = order(genus_hierarchy_rel_F840 %>% unlist, decreasing = T) 
genus_hierarchy_rel_F840 = genus_hierarchy_rel_F840[, genus]
genus_hierarchy_rel_F840 = genus_hierarchy_rel_F840[, 1:15]

genus_hierarchy_rel_M917 = genus_hierarchy_rel_phase3_2['M917',]
genus = order(genus_hierarchy_rel_M917 %>% unlist, decreasing = T) 
genus_hierarchy_rel_M917 = genus_hierarchy_rel_M917[, genus]
genus_hierarchy_rel_M917 = genus_hierarchy_rel_M917[, 1:15]


# bacteria = which(order(abs(as.numeric(genus_hierarchy_rel_phase3_2[1,] - genus_hierarchy_rel_phase3_2[2,])), decreasing = T) <= 10)
# 
# genus_hierarchy_rel_phase3_2 = genus_hierarchy_rel_phase3_2[, bacteria]

library(ggplot2)

# Assuming your dataset has a structure like this:
# Row 1: Observation 1
# Row 2: Observation 2
# Columns represent categories

# Let's assume your dataset is named genus_hierarchy_rel_phase3_2

custom_colors <-viridisLite::viridis(75) 
custom_colors = data.frame(custom_colors, genus = sort(colnames(genus_hierarchy_rel_phase3_2)))

# Convert the transposed data to a data frame
df <- as.data.frame(genus_hierarchy_rel_F840)


# Add a column for row names (observation names)
df$Observation <- row.names(df)

# Melt the data frame for plotting
library(reshape2)
df_melted <- melt(df, id.vars = "Observation")

cstm_clrs = custom_colors[custom_colors$genus %in% df_melted$variable, ]
cstm_clrs = cstm_clrs[order(match(cstm_clrs$genus, df_melted$variable)), ]
# Create a stacked barplot
ggplot(df_melted, aes(x = Observation, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  labs(x = "Observation", y = "Value", , title = "2022-09-05") +
  scale_fill_manual(values = cstm_clrs$custom_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) # Rotate x-axis labels for better readability


# Convert the transposed data to a data frame
df <- as.data.frame(genus_hierarchy_rel_M917)


# Add a column for row names (observation names)
df$Observation <- row.names(df)

# Melt the data frame for plotting
library(reshape2)
df_melted <- melt(df, id.vars = "Observation")

cstm_clrs = custom_colors[custom_colors$genus %in% df_melted$variable, ]
cstm_clrs = cstm_clrs[order(match(cstm_clrs$genus, df_melted$variable)), ]

# Create a stacked barplot
ggplot(df_melted, aes(x = Observation, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  labs(x = "Observation", y = "Value", , title = "2022-09-05") +
  scale_fill_manual(values = cstm_clrs$custom_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) # Rotate x-axis labels for better readability

top_genus = rbind(top_genus, colnames(genus_hierarchy_rel_F840))
top_genus = rbind(top_genus, colnames(genus_hierarchy_rel_M917))



# Phase 3_3 ---------------------------------------------------------------


hierarchy_data_3 = readRDS('output/hierarchy_ranking_groups/dominance_phase_3_by_pen.rds') %>% 
  mutate(treatment = ifelse(Pen %% 2, 'Control', 'Stress'),
         Pen = as.factor(Pen)) %>% 
  filter(grepl("F840|M917", ID))

# hierarchy_data_1$ID[hierarchy_data_1$ID=='M904'] = 'M686'


genus_hierarchy = merge.data.frame(x = Genus_meta, y = hierarchy_data_3, 
                                   by.x = "Subject.ID", 
                                   by.y = "ID") %>% 
  filter(Samplingday == "2022-10-05")
colnames(genus_hierarchy) = make.names(colnames(genus_hierarchy))

genus_hierarchy_rel_phase3_3 = genus_hierarchy %>% select(contains("g__")) 
genus_hierarchy_rel_phase3_3 = genus_hierarchy_rel_phase3_3/rowSums(genus_hierarchy_rel_phase3_3)
rownames(genus_hierarchy_rel_phase3_3) = genus_hierarchy$Subject.ID
colnames(genus_hierarchy_rel_phase3_3) = colnames(genus_hierarchy_rel_phase3_3) %>% 
  gsub("g__", "", .)
genus_hierarchy_rel_F840 = genus_hierarchy_rel_phase3_3['F840',]
genus = order(genus_hierarchy_rel_F840 %>% unlist, decreasing = T) 
genus_hierarchy_rel_F840 = genus_hierarchy_rel_F840[, genus]
genus_hierarchy_rel_F840 = genus_hierarchy_rel_F840[, 1:15]

genus_hierarchy_rel_M917 = genus_hierarchy_rel_phase3_3['M917',]
genus = order(genus_hierarchy_rel_M917 %>% unlist, decreasing = T) 
genus_hierarchy_rel_M917 = genus_hierarchy_rel_M917[, genus]
genus_hierarchy_rel_M917 = genus_hierarchy_rel_M917[, 1:15]


# bacteria = which(order(abs(as.numeric(genus_hierarchy_rel_phase3_3[1,] - genus_hierarchy_rel_phase3_3[2,])), decreasing = T) <= 10)
# 
# genus_hierarchy_rel_phase3_3 = genus_hierarchy_rel_phase3_3[, bacteria]

library(ggplot2)

# Assuming your dataset has a structure like this:
# Row 1: Observation 1
# Row 2: Observation 2
# Columns represent categories

# Let's assume your dataset is named genus_hierarchy_rel_phase3_3

custom_colors <-viridisLite::viridis(75) 
custom_colors = data.frame(custom_colors, genus = sort(colnames(genus_hierarchy_rel_phase3_3)))

# Convert the transposed data to a data frame
df <- as.data.frame(genus_hierarchy_rel_F840)


# Add a column for row names (observation names)
df$Observation <- row.names(df)

# Melt the data frame for plotting
library(reshape2)
df_melted <- melt(df, id.vars = "Observation")

cstm_clrs = custom_colors[custom_colors$genus %in% df_melted$variable, ]
cstm_clrs = cstm_clrs[order(match(cstm_clrs$genus, df_melted$variable)), ]
# Create a stacked barplot
ggplot(df_melted, aes(x = Observation, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  labs(x = "Observation", y = "Value", , title = "2022-10-05") +
  scale_fill_manual(values = cstm_clrs$custom_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) # Rotate x-axis labels for better readability


# Convert the transposed data to a data frame
df <- as.data.frame(genus_hierarchy_rel_M917)


# Add a column for row names (observation names)
df$Observation <- row.names(df)

# Melt the data frame for plotting
library(reshape2)
df_melted <- melt(df, id.vars = "Observation")

cstm_clrs = custom_colors[custom_colors$genus %in% df_melted$variable, ]
cstm_clrs = cstm_clrs[order(match(cstm_clrs$genus, df_melted$variable)), ]

# Create a stacked barplot
ggplot(df_melted, aes(x = Observation, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  labs(x = "Observation", y = "Value", , title = "2022-10-05") +
  scale_fill_manual(values = cstm_clrs$custom_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) # Rotate x-axis labels for better readability

top_genus = rbind(top_genus, colnames(genus_hierarchy_rel_F840))
top_genus = rbind(top_genus, colnames(genus_hierarchy_rel_M917))


# UpSet Plot --------------------------------------------------------------


# Define the two vectors
vector1 <- c('F840', 'M917')
vector2 <- c('Phase 1', 'Phase 2', 'Phase 3')

# Create all combinations using expand.grid
combinations <- expand.grid(vector1, vector2)

# Rename the columns if needed
colnames(combinations) <- c("Element1", "Element2")

# Paste the combinations row-wise into a single string
rownames(top_genus) <- apply(combinations, 1, paste, collapse = " ")


all_genus = top_genus %>% unlist %>% unique
top_genus_upset = data.frame()
for (row in rownames(top_genus)) {
  top_genus_log = all_genus %in% top_genus[row, ]
  top_genus_upset = rbind(top_genus_upset, as.numeric(top_genus_log))
  
}
rownames(top_genus_upset) = rownames(top_genus)
colnames(top_genus_upset) = all_genus

library(UpSetR)

upset(as.data.frame(t(top_genus_upset)), order.by = "freq", nsets = 6)

genus_hierarchy_rel_phases = rbind.data.frame(
  genus_hierarchy_rel_phase1,
  genus_hierarchy_rel_phase3_2,
  genus_hierarchy_rel_phase3_3
)

# Corrplot ----------------------------------------------------------------


cor(t(genus_hierarchy_rel_phases)) %>% corrplot::corrplot()

png(width = 1080*4, height = 720*4, res = 150)
heatmap(as.matrix(genus_hierarchy_rel_phases))
dev.off()
