# Data analysis for the RSG-Brazil Educational Committee Survey 2023

### Load Libraries ############################################################################

required_packages <- c(
  "svglite", "readr", "rnaturalearth", "devtools", "ggplot2", "patchwork", "dplyr",
  "sf", "ggthemes", "rnaturalearthdata", "cowplot", "stringr", "tidyr")

install_and_load <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package)
  }
  library(package, character.only = TRUE)
}

invisible(sapply(required_packages, install_and_load))

if (!requireNamespace("rnaturalearthhires", quietly = TRUE)) {
  devtools::install_github("ropensci/rnaturalearthhires")
}
library(rnaturalearthhires)


### Set working directory ############################################################################
setwd("F:/PROGRAMACAO/artigoRSG")

### Directory creation 
dir.create("plots", showWarnings = FALSE)
dir.create("plots/figure1", recursive = TRUE, showWarnings = FALSE)
dir.create("plots/figure2", recursive = TRUE, showWarnings = FALSE)
dir.create("plots/figure3", recursive = TRUE, showWarnings = FALSE)
dir.create("plots/figure4", recursive = TRUE, showWarnings = FALSE)
dir.create("plots/figure5", recursive = TRUE, showWarnings = FALSE)
dir.create("plots/figure6", recursive = TRUE, showWarnings = FALSE)
dir.create("plots/figure7", recursive = TRUE, showWarnings = FALSE)

#### Set colorblind friendly palette ############################################################################
color_palette <- c("#0072B2", "#E69F00", "#009E73")
color_palette_six <- c("#0072B2", "#E69F00", "#009E73", "#F0E442", "#D55E00", "#CC79A7")

### Custom theme ############################################################################
custom_theme <- theme_minimal(base_size = 15) + 
  theme(
    axis.line = element_line(linetype = "solid"),
    axis.ticks = element_line(colour = "black"),
    axis.title = element_text(size = 12, face = "bold", vjust = 0),
    axis.text = element_text(colour = "black", vjust = 0),
    plot.title = element_text(face = "bold"),
    panel.background = element_rect(fill = "white"),
    legend.key = element_rect(fill = NA),
    legend.background = element_rect(fill = NA),
    legend.position = "right"
  )

### Data cleaning ############################################################################
resp_2023 = read.csv(file = "demanda_april.csv")
colnames(resp_2023) = c("timestamp", "acceptance", "color", "gender",
                        "state", "academic_level", "profile", "profile_user", 
                        "profile_scientist", "others", "profile_developer",
                        "quant_bio", "quant_stat", "quant_comp", "quant_ethics", 
                        "quant_usability", "quant_communication", "quant_development", 
                        "topics", "knowledge_level")

# Remove data before the forms update
if (nrow(resp_2023) >= 8) {
  resp_2023 = resp_2023[8:nrow(resp_2023),]
}

# Remove data share deny answers
resp_2023 = resp_2023[!grepl("No", resp_2023$acceptance), ]

### Figure 1: Geo Heatmap ############################################################################

# Get Brazil geometry
brasil_shape <- ne_states(country = "brazil", returnclass = "sf")
resp_2023 <- resp_2023[resp_2023$state != "Resido fora do Brasil", ]

# Data organization
citacoes <- resp_2023 %>%
  group_by(State = state) %>%
  summarise(citacoes = n()) %>%
  mutate(State = gsub(" \\(.*\\)", "", State)) %>%
  mutate(State = gsub("Amazônas", "Amazonas", State))

# States to regions
regioes <- c("Norte", "Norte", "Norte", "Nordeste", "Nordeste", "Centro-Oeste", 
             "Sudeste", "Centro-Oeste", "Nordeste", "Centro-Oeste", "Centro-Oeste", 
             "Sudeste", "Norte", "Nordeste", "Sul", "Nordeste", 
             "Nordeste", "Sudeste", "Nordeste", "Sul", 
             "Norte", "Norte", "Sul", "Sudeste", "Nordeste", 
             "Norte")

estados_regioes <- data.frame(
  state = c("Acre", "Amapá", "Amazonas", "Bahia", "Ceará", "Distrito Federal", 
            "Espírito Santo", "Goiás", "Maranhão", "Mato Grosso", "Mato Grosso do Sul", 
            "Minas Gerais", "Pará", "Paraíba", "Paraná", "Pernambuco", 
            "Piauí", "Rio de Janeiro", "Rio Grande do Norte", "Rio Grande do Sul", 
            "Rondônia", "Roraima", "Santa Catarina", "São Paulo", "Sergipe", 
            "Tocantins"),
  regiao = regioes
)

# Data to sf
brasil_shape <- brasil_shape %>%
  left_join(citacoes, by = c("name" = "State")) %>%
  # Corrigir geometrias inválidas
  mutate(geometry = st_make_valid(geometry))

# Calculate answer per region
citacoes_por_regiao <- brasil_shape %>%
  left_join(estados_regioes, by = c("name" = "state")) %>%
  group_by(regiao) %>%
  summarise(total_citacoes = sum(citacoes, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(regiao))

# Bar chart
bar_chart <- ggplot(citacoes_por_regiao, aes(x = regiao, y = total_citacoes, fill = regiao)) +
  geom_col(fill = "#67000d", width = 0.7) +
  coord_flip() +
  geom_hline(yintercept = 0) +
  geom_text(aes(label = total_citacoes), hjust = -0.2, size = 5) +
  guides(color = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  labs(title = "Figure 1 - Barchart of regions with participants that\nanswered the survey and the number of answers found in each region.", 
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(size = 14, hjust = 0.5),
    axis.text.y = element_text(size = 12, hjust = 1),
    axis.text.x = element_blank(),
    plot.margin = margin(1, 2, 1, 1, "cm"),
    panel.background = element_rect(fill = NA),
    legend.position = "none"
  )
print(bar_chart)
print("Saving bar chart...")
ggsave("plots/figure1/bar_chart.png", plot = bar_chart, width = 8, height = 10, units = "in", dpi = 1000)
ggsave("plots/figure1/bar_chart.tiff", plot = bar_chart, width = 8, height = 10, units = "in", dpi = 1000)
ggsave("plots/figure1/bar_chart.pdf", plot = bar_chart, width = 8, height = 10, units = "in", dpi = 1000)
ggsave("plots/figure1/bar_chart.svg", plot = bar_chart, width = 8, height = 10, units = "in", dpi = 1000)
print("Bar chart saved!")

# Load neighboring countries
world <- ne_countries(scale = "medium", returnclass = "sf")

# Heatmap with thick outer border and thin internal borders
map_plot <- ggplot() +
  geom_sf(data = world, fill = "grey80", color = "black", size = 0.9) +
  geom_sf(data = brasil_shape, aes(fill = citacoes), lwd = 0.5, color = "black") + # Increased line width
  scale_fill_continuous(
    low = "#fff5f0", 
    high = "#67000d", 
    na.value = "grey90", 
    name = NULL,
    guide = guide_colorbar(direction = "horizontal", barwidth = 10, barheight = 0.5)
  ) +
  xlim(c(-76, -32)) +
  ylim(c(-36, 7)) +
  labs(title = "Figure 1 - States with participants that answered the\nsurvey and the number of answers found in each state.") + 
  theme_map() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, margin = margin(b = 20), face = "plain"),
    legend.position = "bottom"
  )

print(map_plot)
print("Saving map plot...")
ggsave("plots/figure1/Figure_1_map.png", plot = map_plot, width = 8, height = 10, units = "in", dpi = 1000)
ggsave("plots/figure1/Figure_1_map.tiff", plot = map_plot, width = 8, height = 10, units = "in", dpi = 1000)
ggsave("plots/figure1/Figure_1_map.pdf", plot = map_plot, width = 8, height = 10, units = "in", dpi = 1000)
ggsave("plots/figure1/Figure_1_map.svg", plot = map_plot, width = 8, height = 10, units = "in", dpi = 1000)
print("Figure 1 saved!")

### Complementary Figure 1: Geographic Distribution by Academic Level ############################################################################

# Count data
academic_counts <- resp_2023 %>%
  mutate(state = gsub(" \\(.*\\)", "", state)) %>%  # Remove state abbreviations in parentheses
  mutate(state = gsub("Amazônas", "Amazonas", state)) %>%  # Fix state name
  group_by(state, academic_level) %>%
  summarise(count = n(), .groups = "drop")

# Join with brasil_shape
brasil_academic <- brasil_shape %>%
  left_join(academic_counts, by = c("name" = "state"))

# Create the plot
geo_academic <- ggplot() +
  geom_sf(data = world, fill = "grey80", color = "black", size = 0.5) +
  geom_sf(data = brasil_academic[!is.na(brasil_academic$academic_level), ], aes(fill = count)) +
  facet_wrap(~academic_level) +
  scale_fill_gradient(
    low = "#fff5f0",
    high = "#67000d",
    na.value = "grey90",
    name = "Count"
  ) +
  coord_sf(xlim = c(-76, -32), ylim = c(-36, 7)) +
  labs(
    title = "Geographic Distribution by Academic Level"
  ) +
  theme_map() +
  theme(
    strip.text = element_text(size = 12, face = "plain"),
    strip.background = element_rect(fill = "white", color = "black"),
    legend.position = "right",
    plot.title = element_text(size = 14, hjust = 0.5, face = "plain"),
    plot.caption = element_text(size = 10, hjust = 1)
  )

print(geo_academic)
print("Saving geo academic plot...")
ggsave("plots/figure1/Suplementary_Figure_1_geo_dist_academic.png", plot = geo_academic, width = 15, height = 10, units = "in", dpi = 1000)
ggsave("plots/figure1/Suplementary_Figure_1_geo_dist_academic.pdf", plot = geo_academic, width = 15, height = 10, units = "in", dpi = 1000)
ggsave("plots/figure1/Suplementary_Figure_1_geo_dist_academic.svg", plot = geo_academic, width = 15, height = 10, units = "in", dpi = 1000)
ggsave("plots/figure1/Suplementary_Figure_1_geo_dist_academic.tiff", plot = geo_academic, width = 15, height = 10, units = "in", dpi = 1000)
print("Figure 1 saved!")

### Figure 2: Gender x profile ############################################################################

create_pie <- function(data, profile_name) {
  color_scales <- list(
    "user" = c("#9F00E6", "#9c435b", "#009E73"),
    "scientist" = c("#9F00E6", "#009E73", "#9c435b"),
    "developer" = c("#9F00E6", "#009E73", "#9c435b")
  )
  
  # Capitalize first letter
  title_name <- str_to_title(profile_name)
  
  ggplot(data %>% filter(profile == profile_name), 
         aes(x = "", y = count, fill = gender)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    geom_text(aes(label = count),
              position = position_stack(vjust = 0.5),
              size = 7,
              fontface = "plain") +
    scale_fill_manual(values = color_scales[[tolower(profile_name)]]) +
    labs(title = title_name,
         fill = "Gender") +
    theme_void() +
    theme(
      plot.title = element_text(size = 16, hjust = 0.5, face = "plain"),
      legend.title = element_text(size = 14, face = "plain"),
      legend.text = element_text(size = 12)
    )
}

gender_data <- resp_2023 %>%
  mutate(
    profile = tolower(profile),
    gender = case_when(
      tolower(gender) %in% c("man / male (transgender)", "non-binary", "no binary") ~ "Non-binary",
      tolower(gender) == "binary" ~ NA_character_,
      tolower(gender) == "man / male (cisgender)" ~ "Man",
      tolower(gender) == "woman / female (cisgender)" ~ "Woman",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(gender)) %>%
  group_by(profile, gender) %>%
  summarise(count = n(), .groups = 'drop')

p1 <- create_pie(gender_data, "developer")
print(p1)
p2 <- create_pie(gender_data, "scientist")
print(p2)
p3 <- create_pie(gender_data, "user")
print(p3)

combined_pies <- (p1 | p2 | p3) + 
  plot_annotation(title = "Figure 2 - Gender distribution by profile", theme = theme(plot.title = element_text(size = 18, hjust = 0.5, face = "plain")))

print(combined_pies)
print("Saving combined pies plot...")
ggsave("plots/figure2/Figure_2_gender_pies.png", combined_pies, width = 15, height = 5, dpi = 1000)
ggsave("plots/figure2/Figure_2_gender_pies.svg", combined_pies, width = 15, height = 5, dpi = 1000)
ggsave("plots/figure2/Figure_2_gender_pies.pdf", combined_pies, width = 15, height = 5, dpi = 1000)
ggsave("plots/figure2/Figure_2_gender_pies.tiff", combined_pies, width = 15, height = 5, dpi = 1000)
print("Figure 2 saved!")

### Figure 3: Academic level x profile - barplot ############################################################################

level_profile <- ggplot(resp_2023) +
  geom_bar(mapping = aes(x = academic_level, fill = profile), 
           show.legend = TRUE, 
           width = 0.9) +
  theme_minimal() +
  xlab("Academic level") +
  ylab("Count") +
  labs(title = "Figure 3 - The academic careers found in each of the profiles researched\nin this study are user, science, and developers in bioinformatics.", 
       fill = "Profile") +
  scale_fill_manual(values = color_palette) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme(
    axis.text.x = element_text(size = 14, color = "black", hjust = 0.5),
    axis.text.y = element_text(size = 14, color = "black"),
    axis.title = element_text(size = 16, color = "black"),
    plot.title = element_text(size = 16, hjust = 0.5, color = "black"),
    legend.title = element_text(size = 14, color = "black"),
    legend.text = element_text(size = 14, color = "black"),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    plot.margin = margin(1, 1, 1, 1, "cm")
  )

print(level_profile)
print("Saving level profile plot...")
ggsave("plots/figure3/Figure_3_level_profile.png", plot = level_profile, width = 18, height = 10, units = "in", dpi = 1000)
ggsave("plots/figure3/Figure_3_level_profile.tiff", plot = level_profile, width = 18, height = 10, units = "in", dpi = 1000)
ggsave("plots/figure3/Figure_3_level_profile.svg", plot = level_profile, width = 18, height = 10, units = "in", dpi = 1000)
ggsave("plots/figure3/Figure_3_level_profile.pdf", plot = level_profile, width = 18, height = 10, units = "in", dpi = 1000)
print("Figure 3 saved!")

### Figure 4: Confidence scale of each competence ############################################################################

color_palette <- c("#0072B2", "#E69F00", "#009E73", "#F0E442", "#D55E00", "#CC79A7", "#56B4E9")

plot_names <- paste0("T", 1:7, c(
    "_knowledge_biology",
    "_statistics_data_science",
    "_computing_programming",
    "_ethics_society",
    "_uses_bioinformatics",
    "_communication_bioinformatics",
    "_continuous_development"
))

custom_theme <- theme_minimal() + 
  theme(
    text = element_text(size = 16, color = "black"),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 16),
    plot.title = element_text(size = 20, hjust = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
    panel.border = element_rect(color = "black", fill = NA),
    strip.text = element_text(face = "bold", angle = 270, size = 16),
    strip.background = element_blank(),
    panel.spacing = unit(2, "lines")
  )

create_plot <- function(data, x, fill_color, title) {
  ggplot(data) +
    geom_bar(aes(x = factor(.data[[x]])), 
             fill = fill_color, 
             color = "black") +
    labs(x = "CF-Score", 
         y = "Number of Participants", 
         title = title) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    facet_wrap(~profile, ncol = 1, scales = "free_y", 
               strip.position = "right") +
    custom_theme
}

plots <- Map(
    create_plot,
    x = paste0("quant_", c("bio", "stat", "comp", "ethics", "usability", "communication", "development")),
    fill_color = color_palette,
    title = paste("T", 1:7, c(
        " - Knowledge in Biology",
        " - Statistics and Data Science",
        " - Computing and Programming",
        " - Ethics and Society",
        " - Uses of Bioinformatics",
        " - Communication in Bioinformatics",
        " - Continuous Development"
    )),
    MoreArgs = list(data = resp_2023)
)

combined_plot <- wrap_plots(plots, ncol = 2) +
  plot_layout(guides = "collect") & 
  theme(plot.margin = margin(t = 0.5, r = 0.2, b = 0.5, l = 0.2, unit = "cm"))

# Create directory and save plots
dir.create("plots/figure4", recursive = TRUE, showWarnings = FALSE)

print(combined_plot)
print("Saving combined plot...")
ggsave("plots/figure4/Figure_4_confidence_scale.png", plot = combined_plot, width = 16, height = ceiling(length(plots)/2) * 8, dpi = 1000)
ggsave("plots/figure4/Figure_4_confidence_scale.svg", plot = combined_plot, width = 16, height = ceiling(length(plots)/2) * 8, dpi = 1000)
ggsave("plots/figure4/Figure_4_confidence_scale.pdf", plot = combined_plot, width = 16, height = ceiling(length(plots)/2) * 8, dpi = 1000)
ggsave("plots/figure4/Figure_4_confidence_scale.tiff", plot = combined_plot, width = 16, height = ceiling(length(plots)/2) * 8, dpi = 1000)
print("Figure 4 saved!")

### Figure 5: Career diversity ############################################################################

# Career diversity within the bioinformatics Brazilian community and its distribution in the profiles (developer, scientist, and user).

n_user <- length(unique(resp_2023[resp_2023$profile == "User", "profile_user"]))
n_scientist <- length(unique(resp_2023[resp_2023$profile == "Scientist", "profile_scientist"]))
n_dev <- length(unique(resp_2023[resp_2023$profile == "Developer", "profile_developer"]))

wrap_text <- function(x) {
  words <- unlist(strsplit(x, " "))
  if(length(words) > 3) {
    n_chunks <- ceiling(length(words) / 3)
    chunks <- vector("character", n_chunks)
    
    for(i in 1:n_chunks) {
      start_idx <- (i-1)*3 + 1
      end_idx <- min(i*3, length(words))
      chunks[i] <- paste(words[start_idx:end_idx], collapse=" ")
    }
    paste(chunks, collapse="\n")
  } else {
    x
  }
}

custom_theme_box <- theme_minimal() + 
  theme(
    text = element_text(size = 16, color = "black"),
    axis.title.x = element_text(size = 16, face = "plain"),
    axis.text.y = element_text(size = 16, hjust = 1),
    axis.text.x = element_text(size = 14),
    plot.title = element_text(size = 16, hjust = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
    panel.border = element_rect(color = "black", fill = NA),
    legend.position = "none",
    plot.margin = margin(t = 0.5, r = 0.2, b = 0.5, l = 0.5, unit = "cm")
  )

USER_CAREER <- ggplot(resp_2023[resp_2023$profile == "User",]) +
  geom_bar(aes(y = factor(profile_user, levels = unique(profile_user)), fill = profile_user), width = 0.6) +
  geom_text(stat = 'count', aes(y = profile_user, label = after_stat(count)), hjust = -0.2, size = 4) +
  scale_fill_manual(values = color_palette) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  scale_y_discrete(labels = function(x) sapply(x, wrap_text)) +
  custom_theme_box +
  labs(y = "", x = "Count", title = "Users")

SCIENTIST_CAREER <- ggplot(resp_2023[resp_2023$profile == "Scientist",]) +
  geom_bar(aes(y = factor(profile_scientist, levels = unique(profile_scientist)), fill = profile_scientist), width = 0.6) +
  geom_text(stat = 'count', aes(y = profile_scientist, label = after_stat(count)), hjust = -0.2, size = 4) +
  scale_fill_manual(values = color_palette) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  scale_y_discrete(labels = function(x) sapply(x, wrap_text)) +
  custom_theme_box +
  labs(y = "", x = "Count", title = "Scientists")

DEV_CAREER <- ggplot(resp_2023[resp_2023$profile == "Developer",]) +
  geom_bar(aes(y = factor(profile_developer, levels = unique(profile_developer)), fill = profile_developer), width = 0.6) +
  geom_text(stat = 'count', aes(y = profile_developer, label = after_stat(count)), hjust = -0.2, size = 4) +
  scale_fill_manual(values = color_palette) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  scale_y_discrete(labels = function(x) sapply(x, wrap_text)) +
  custom_theme_box +
  labs(y = "", x = "Count", title = "Developers")

FIGURE_5 <- plot_grid(USER_CAREER, SCIENTIST_CAREER, DEV_CAREER,
                     nrow = 3, ncol = 1,
                     labels = c("A", "B", "C"),
                     align = "hv",
                     label_size = 12,
                     rel_heights = c(n_user, n_scientist, n_dev))

print(FIGURE_5)
print("Saving career diversity plot...")
ggsave("plots/figure5/Figure_5_career_diversity.png", plot = FIGURE_5, width = 12, height = (n_user + n_scientist + n_dev) * 2, dpi = 1000)
ggsave("plots/figure5/Figure_5_career_diversity.svg", plot = FIGURE_5, width = 12, height = (n_user + n_scientist + n_dev) * 2, dpi = 1000)
ggsave("plots/figure5/Figure_5_career_diversity.pdf", plot = FIGURE_5, width = 12, height = (n_user + n_scientist + n_dev) * 2, dpi = 1000)
ggsave("plots/figure5/Figure_5_career_diversity.tiff", plot = FIGURE_5, width = 12, height = (n_user + n_scientist + n_dev) * 2, dpi = 1000)
print("Figure 5 saved!")

### Figure 6: Demands needed ############################################################################

# Track Demands per Profile
resp_2023$topics <- as.character(resp_2023$topics)

# Data transformation
resp_2023.topics <- resp_2023 %>%
  select(profile, topics) %>%
  mutate(
    `Data Science and AI` = str_count(topics, "Artificial Intelligence and Data Science in Bioinformatics"),
    `Database and Software Dev.` = str_count(topics, "Database and Software Development"),
    `System Biology` = str_count(topics, "Metabolomics and Systems Biology"),
    `Structural Biology` = str_count(topics, "Structural Biology and Modeling"),
    `Cancer Studies` = str_count(topics, "Molecular Basis of Cancer"),
    `Personalized Medicine` = str_count(topics, "Personalized Medicine"),
    `Communication` = str_count(topics, "Didactics and Communication in Bioinformatics"),
    `Genomics` = str_count(topics, "DNA and Genomics"),
    `Evolution` = str_count(topics, "Phylogeny and Evolution"),
    `Metagenomics` = str_count(topics, "Metagenomics and Microbiome"),
    `Transcriptomics` = str_count(topics, "RNA and Transcriptomics"),
    `Epigenomics` = str_count(topics, "Epigenomics"),
    `Proteomics` = str_count(topics, "Proteins and Proteomics"),
    `Mentorship` = str_count(topics, "Career Mentoring in Bioinformatics")
  )

# Formating table 
resp_2023.g.t <- data.frame(matrix(ncol = 1, nrow = 42))
colnames(resp_2023.g.t) <- "topics"
topics_2023 <- colnames(resp_2023.topics)[4:17]
resp_2023.g.t$topics <- rep(topics_2023, 3)
resp_2023.g.t$profile <- c(rep("User", 14),							
                           rep("Scientist", 14),							
                           rep("Developer", 14))							

resp_2023.g.t$counts <- c(rep(0, 42))							

# Populating table according profile and topics							
resp_2023.g.t$counts <- 0  
for (profile in c("User", "Scientist", "Developer")) {
  for (i in 1:14) {
    index <- (i + (14 * (which(c("User", "Scientist", "Developer") == profile) - 1)))
    resp_2023.g.t[index, 3] <- sum(resp_2023.topics[resp_2023.topics$profile == profile, i + 2])
  }
}

g.temas2 <- ggplot(data = resp_2023.g.t[!is.na(resp_2023.g.t$topics), ]) +
  geom_col(mapping = aes(x = reorder(topics, -counts), y = counts, fill = profile)) + 
  coord_flip() +
  labs(title = "Figure 6 - Demands in Training", fill = "Profile") + 
  theme_bw() + 
  labs(x = "Tracks", y = "Count") +
  scale_fill_manual(values = color_palette_six) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    axis.text.x = element_text(size = 5, hjust = 0, vjust = 0.5),
    plot.title = element_text(size = 10, hjust = 0.5),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.2, unit = "cm")
  )

print(g.temas2)
print("Saving demands needs profile plot...")
ggsave("plots/figure6/Figure_6_Demands_needs_profile_pilleup.png", plot = g.temas2, width = 15, height = 5, dpi = 1000)
ggsave("plots/figure6/Figure_6_Demands_needs_profile_pilleup.svg", plot = g.temas2, width = 15, height = 5, dpi = 1000)
ggsave("plots/figure6/Figure_6_Demands_needs_profile_pilleup.pdf", plot = g.temas2, width = 15, height = 5, dpi = 1000)
ggsave("plots/figure6/Figure_6_Demands_needs_profile_pilleup.tiff", plot = g.temas2, width = 15, height = 5, dpi = 1000)
print("Figure 6 saved!")


### Figure 7: Demands course complexity ############################################################################

training_data <- resp_2023 %>%
  separate_rows(knowledge_level, sep = ", ") %>%
  group_by(profile, knowledge_level) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(profile) %>%
  mutate(percentage = count/sum(count) * 100)

training_data$knowledge_level <- factor(training_data$knowledge_level, 
                                      levels = c("Advanced", "Intermediate", "Basic"))

training_plot <- ggplot(training_data, 
       aes(x = profile, y = percentage, fill = knowledge_level)) +
  geom_bar(stat = "identity", 
           position = "stack",
           width = 0.7) +
  scale_fill_manual(
    values = c("Basic" = "#FFD6A5",
              "Intermediate" = "#FB6A4A",
              "Advanced" = "#67000D"),
    name = "Training Level"
  ) +
  scale_y_continuous(expand = c(0,0)) +
  labs(
    title = "Figure 7. Distribution of Required Training Levels by Profile",
    x = "Profile",
    y = "Percentage (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    axis.text = element_text(size = 14, color = "black"),
    axis.title = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
    plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.2, unit = "cm"),
    legend.position = "right"
  ) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_stack(vjust = 0.5),
            size = 4)
            
print(training_plot)
print("Saving training levels plot...")
ggsave("plots/figure7/Figure_7_training_levels.png", plot = training_plot, width = 10, height = 8, units = "in", dpi = 1000)
ggsave("plots/figure7/Figure_7_training_levels.svg", plot = training_plot, width = 10, height = 8, units = "in", dpi = 1000)
ggsave("plots/figure7/Figure_7_training_levels.pdf", plot = training_plot, width = 10, height = 8, units = "in", dpi = 1000)
ggsave("plots/figure7/Figure_7_training_levels.tiff", plot = training_plot, width = 10, height = 8, units = "in", dpi = 1000)
print("Figure 7 saved!")