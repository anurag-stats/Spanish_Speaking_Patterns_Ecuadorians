#' ---
#' output:
#'   html_document:
#'     df_print: paged
#'   always_allow_html: true
#'   pdf_document: default
#' ---
#' 
#' 
#' 
#' 
## ---------------------------------------------------------------------------------------------------------------
library(readxl)
df <- read_excel("data_returnees.xlsx")
head(df)


#' 
#' Data Quality Checks:
#' 
## ---------------------------------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(tidyverse)

#' 
## ---------------------------------------------------------------------------------------------------------------
df %>% distinct(speaker, age, gender, stay_spain, return_ecuador, edu_level) %>%
    arrange(speaker)


#' 
#' 
#' We observe speakers with multiple ages, stays in Spain, return to Ecuador and edu_level. Is this accurate?
#' 
#' S07 should be S06
#' 
#' Stay in Spain - years
#' Return to Ecuador - years - how long they have been Ecuador currently? (originally recorded months, then divided by 12)
#' 
#' 
#' 
#' One of the sentence types has value 112, which I think is a data entry error and should be 12.
## ---------------------------------------------------------------------------------------------------------------
df <- df %>%
  mutate(sentence_type = if_else(sentence_type == 112, 12, sentence_type))

df %>% count(sentence_type) %>% arrange(sentence_type)


#' 
## ---------------------------------------------------------------------------------------------------------------
df <- df %>%
  mutate(
    response_lab = factor(
      dv,
      levels = c(1, 2, 3, 4, 5),
      labels = c("Ecuadorian", "Spanish", "Shared", "Different", "Other")
    )
  )

#' 
#' New label encodings:
#' Ecuadorian + Spanish - unique patterns
#' Shared + Different - mixed patterns
#' others
#' 
## ---------------------------------------------------------------------------------------------------------------
df <- df %>%
  mutate(new_response_lab = if_else(dv <= 2, "Unique",
                      if_else(dv <= 4 & dv>2, "Mixed", "Other")))

#' 
#' 
#' Overall Response Distribution:
## ---------------------------------------------------------------------------------------------------------------
tot_n <- nrow(df)
response_dist <- df %>% count(response_lab) %>% 
                    mutate(prop = n/sum(n))

response_dist_plot <- response_dist %>%
  arrange(prop) %>%
  mutate(
    response_lab = factor(response_lab, levels = response_lab),
    label = paste0(n, " (", scales::percent(prop, accuracy = 0.1), ")")
  )

ggplot(response_dist_plot, aes(x = prop, y = response_lab)) +
  geom_segment(aes(x = 0, xend = prop, y = response_lab, yend = response_lab),
               linewidth = 0.8, color = "grey75") +
  geom_point(size = 4, color = "#2C7FB8") +
  geom_text(aes(label = label), hjust = -0.1, size = 4) +
  scale_x_continuous(
    labels = scales::label_percent(),
    limits = c(0, max(response_dist_plot$prop) * 1.15)
  ) +
  labs(
    title = "Overall response distribution",
    x = "Proportion of observations",
    y = "Response category"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

#' 
#' Response distribution by speaker:
## ---------------------------------------------------------------------------------------------------------------
response_dist_by_speaker <- df %>% count(speaker, response_lab) %>% 
                            group_by(speaker) %>%
                            mutate(within_speaker_prop = n/sum(n)) %>% 
                            ungroup() %>% arrange(speaker)


#' 
#' New Speaker Distribution:
## ---------------------------------------------------------------------------------------------------------------
df %>% count(speaker, new_response_lab) %>% 
                            group_by(speaker) %>%
                            mutate(within_speaker_prop = n/sum(n)) %>% 
                            ungroup() %>% arrange(speaker)

#' 
## ---------------------------------------------------------------------------------------------------------------
plot_dat <- response_dist_by_speaker %>%
  mutate(
    label = paste0(n, "\n", scales::percent(within_speaker_prop, accuracy = 0.1))
  )

ggplot(plot_dat, aes(x = response_lab, y = speaker, fill = within_speaker_prop)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = label), size = 3.2) +
  scale_fill_gradient(
    low = "#F7FBFF",
    high = "#08519C",
    labels = scales::label_percent()
  ) +
  labs(
    title = "Response distribution by speaker",
    x = "Response category",
    y = "Speaker",
    fill = "Within-speaker proportion"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


#' 
#' Response Distribution by Sentence Type:
## ---------------------------------------------------------------------------------------------------------------
response_dist_by_sentence <- df %>% count(sentence_type, response_lab) %>% 
                            group_by(sentence_type) %>%
                            mutate(within_sentence_prop = n/sum(n)) %>% 
                            ungroup() %>% arrange(sentence_type)
response_dist_by_sentence

#' 
## ---------------------------------------------------------------------------------------------------------------
plot_dat <- response_dist_by_sentence %>%
  mutate(
    sentence_type_num = as.numeric(as.character(sentence_type)),
    sentence_block = case_when(
      sentence_type_num >= 1  & sentence_type_num <= 4  ~ "Sentence types 1–4",
      sentence_type_num >= 5  & sentence_type_num <= 8  ~ "Sentence types 5–8",
      sentence_type_num >= 9  & sentence_type_num <= 12 ~ "Sentence types 9–12",
      sentence_type_num > 12                           ~ "Sentence types >12"
    ),
    sentence_block = factor(
      sentence_block,
      levels = c(
        "Sentence types 1–4",
        "Sentence types 5–8",
        "Sentence types 9–12",
        "Sentence types >12"
      )
    ),
    sentence_type = factor(sentence_type_num, levels = sort(unique(sentence_type_num)))
  )

plot_sentence_block <- function(data, block_name) {
  
  plot_sub <- data %>%
    filter(sentence_block == block_name)
  
  p <- ggplot(plot_sub, aes(x = within_sentence_prop, y = sentence_type)) +
    geom_segment(
      aes(x = 0, xend = within_sentence_prop, y = sentence_type, yend = sentence_type),
      color = "grey80",
      linewidth = 0.6
    ) +
    geom_point(
      aes(size = n, color = within_sentence_prop),
      alpha = 0.9
    ) +
    facet_wrap(~ response_lab, ncol = 1) +
    scale_x_continuous(labels = scales::label_percent()) +
    scale_color_gradient(low = "#9ECAE1", high = "#08519C") +
    labs(
      title = paste("Response distribution by sentence type:", block_name),
      x = "Within-sentence proportion",
      y = "Sentence type",
      size = "Count",
      color = "Proportion"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  print(p)
  invisible(p)
}
p1 <- plot_sentence_block(plot_dat, "Sentence types 1–4")
p2 <- plot_sentence_block(plot_dat, "Sentence types 5–8")
p3 <- plot_sentence_block(plot_dat, "Sentence types 9–12")
p4 <- plot_sentence_block(plot_dat, "Sentence types >12")


#' 
#' Speaker x Sentence Type Distribution:
## ---------------------------------------------------------------------------------------------------------------
speaker_sentence_dist <- df %>% count(speaker, sentence_type) %>% 
                        group_by(speaker) %>%
                        mutate(within_speaker_sentence_prop = n/sum(n)) %>% 
                        ungroup() %>% arrange(speaker)
speaker_sentence_dist

## ---------------------------------------------------------------------------------------------------------------
plot_dat <- speaker_sentence_dist %>%
  mutate(
    sentence_type = factor(sentence_type, levels = sort(unique(sentence_type)))
  )

scale_color_viridis_c(
  option = "plasma",
  labels = scales::label_percent()
)

ggplot(plot_dat, aes(x = sentence_type, y = speaker)) +
  geom_point(
    aes(size = n, color = within_speaker_sentence_prop),
    alpha = 0.9
  ) +
  scale_size(range = c(2, 8)) +
  scale_color_viridis_c(
    option = "plasma",
    labels = scales::label_percent()
  ) +
  labs(
    title = "Sentence type distribution by speaker",
    x = "Sentence type",
    y = "Speaker",
    size = "Count",
    color = "Within-speaker proportion"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



#' 
#' 
#' 
#' Speaker x Sentence x Response Distribution (distribution of response given speaker and sentence type, for a particular speaker when you sum up particular sentence types for each response category it should add upto 100% or 1):
## ---------------------------------------------------------------------------------------------------------------
speaker_sentence_response_long <- df %>%
  count(speaker, sentence_type, response_lab, .drop = FALSE) %>%
  group_by(speaker, sentence_type) %>%
  mutate(
    total = sum(n),
    prop = ifelse(total > 0, n / total, NA_real_) #Avoiding division by 0
  ) %>%
  ungroup() %>%
  arrange(speaker, sentence_type, response_lab)


#' 
#' New response:
## ---------------------------------------------------------------------------------------------------------------
df %>%
  count(speaker, sentence_type, new_response_lab, .drop = FALSE) %>%
  group_by(speaker, sentence_type) %>%
  mutate(
    total = sum(n),
    prop = ifelse(total > 0, n / total, NA_real_) #Avoiding division by 0
  ) %>%
  ungroup() %>%
  arrange(speaker, sentence_type, new_response_lab)


#' 
#' 
## ---------------------------------------------------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(plotly)

plot_speaker_response_profile <- function(data, speaker_id, alpha = 0.05, bonferroni = FALSE) {
  
  plot_dat <- data %>%
    filter(speaker == speaker_id, n > 0)
  
  n_panels <- plot_dat %>%
    distinct(response_lab) %>%
    nrow()
  
  alpha_used <- if (bonferroni) alpha / n_panels else alpha
  z <- qnorm(1 - alpha_used / 2)
  
  plot_dat <- plot_dat %>%
    mutate(
      phat = prop,
      denom = 1 + (z^2 / total),
      center = (phat + (z^2 / (2 * total))) / denom,
      half_width = (z / denom) * sqrt((phat * (1 - phat) / total) + (z^2 / (4 * total^2))),
      lower = pmax(0, center - half_width),
      upper = pmin(1, center + half_width),
      hover_txt = paste0(
        "Speaker: ", speaker,
        "<br>Sentence type: ", sentence_type,
        "<br>Response: ", response_lab,
        "<br>Count: ", n,
        "<br>Total in cell: ", total,
        "<br>Proportion: ", scales::percent(prop, accuracy = 0.1),
        "<br>Lower CI: ", scales::percent(lower, accuracy = 0.1),
        "<br>Upper CI: ", scales::percent(upper, accuracy = 0.1)
      )
    )
  
  p <- ggplot(
    plot_dat,
    aes(x = sentence_type, y = prop, group = 1, text = hover_txt)
  ) +
    geom_line(color = "grey40", linewidth = 0.7) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.12, color = "grey50") +
    geom_point(aes(color = n), size = 3) +
    facet_wrap(~ response_lab) +
    scale_y_continuous(labels = scales::label_percent(), limits = c(0, 1)) +
    scale_color_gradient(low = "skyblue", high = "darkred") +
    labs(
      title = paste("Response profile for speaker", speaker_id),
      subtitle = if (bonferroni) {
        paste0(round((1 - alpha) * 100), "% Bonferroni-adjusted Wilson intervals")
      } else {
        paste0(round((1 - alpha) * 100), "% Wilson intervals")
      },
      x = "Sentence type",
      y = "Proportion",
      color = "Count"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank()
    )
  
  ggplotly(p, tooltip = "text")
}




#' 
#' Wilson's interval for Confidence Interval for proportions for small samples:
#' https://www.ucl.ac.uk/arts-humanities/sites/arts_humanities/files/confidence-intervals.pdf
#' 
#' 
#' Speaker 1:
## ---------------------------------------------------------------------------------------------------------------
speaker_sentence_response_long %>%
  filter(speaker == "S01") %>%
  count(sentence_type)


#' 
#' %age of response category given speaker and sentence type
## ---------------------------------------------------------------------------------------------------------------
plot_speaker_response_profile(speaker_sentence_response_long, "S01")


#' 
#' 
#' 
#' 
#' S02:
## ---------------------------------------------------------------------------------------------------------------
plot_speaker_response_profile(speaker_sentence_response_long, "S02")


#' 
#' 
#' S03:
## ---------------------------------------------------------------------------------------------------------------
plot_speaker_response_profile(speaker_sentence_response_long, "S03")


#' 
#' 
#' S04:
## ---------------------------------------------------------------------------------------------------------------
plot_speaker_response_profile(speaker_sentence_response_long, "S04")


#' 
#' 
#' 
#' S05:
## ---------------------------------------------------------------------------------------------------------------
plot_speaker_response_profile(speaker_sentence_response_long, "S05")


#' 
#' 
#' S06:
## ---------------------------------------------------------------------------------------------------------------
plot_speaker_response_profile(speaker_sentence_response_long, "S06")


#' 
#' 
#' There is a S07, weren't there 6 speakers?
#' There is a NA for S06?
#' The subject data is not consistent, a single subject has multiple values for Age, Stay in Spain, edu_level, was the study repeated as in were measurements were taken again? However, this is unlikely since ages differ by a large number of years, making it unlikely and this is probably a data entry issue.
#' There was a data point with sentence type 112, this was replaced by 12.
#' 
#' 
#' 
#' 
