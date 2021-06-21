# Script to make figures that appear in manuscript

library(tidyverse)

e2t_data <- readr::read_csv("figs/2021-04-09_e2t-data.csv")

## Create a column called block_id
e2t_tib <- e2t_data %>% 
  dplyr::mutate(block_id = rep(1:7, 75))

## Reorder columns
e2t_tib <- e2t_tib %>%
  dplyr::select(1:4, block_id, everything())

# Make factors
e2t_tib <- e2t_tib %>% 
  dplyr::mutate(
    id = forcats::as_factor(id),
    group = forcats::as_factor(group),
    phase = forcats::as_factor(phase),
    block_id = forcats::as_factor(block_id)
  )

# Collapse across participants to group level summary
# Will be used to make line graph figure option
e2t_group <- e2t_tib %>% 
  dplyr::group_by(group, phase, block_id) %>% 
  dplyr::summarize(
    n = n(),
    mean_re = mean(score, na.rm = TRUE),
    sd_re = sd(score, na.rm = TRUE),
    se_re = sd(score, na.rm = TRUE)/sqrt(n),
    ci_low = ggplot2::mean_cl_normal(score)$ymin,
    ci_upp = ggplot2::mean_cl_normal(score)$ymax
  )

# Create figure for e2t_group tibble
fig1 <- ggplot2::ggplot(e2t_group, aes(x = block_id, 
                                       y = mean_re,
                                       group = interaction(group, phase),
                                       shape = group
                                       )) +
  geom_line(aes(linetype = group, group = interaction(group, phase)), 
            position = position_dodge(0.2)) +
  geom_pointrange(aes(ymin = e2t_group$ci_low,
                      ymax = e2t_group$ci_upp
                      ),
                  position = position_dodge(0.2)) +
  scale_x_discrete(name = NULL,
                   labels = c("1" = "Pre-test",
                              "2" = "Block 1",
                              "3" = "Block 2",
                              "4" = "Block 3",
                              "5" = "Block 4",
                              "6" = "Block 5",
                              "7" = "Retention")) +
  scale_y_continuous(name = "Radial Error (cm)",
                     limits = c(0, 90),
                     breaks = seq(0, 90, 10)) +
  scale_shape_discrete(labels = c("Teach Group", "Test Group")) +
  scale_linetype_discrete(labels = c("Teach Group", "Test Group")) +
  theme_classic()

fig1 + theme(
  axis.title.y = element_text(size = 14, face = "bold"),
  axis.text = element_text(size = 12, face = "bold"),
  legend.position = "top",
  legend.title = element_blank(),
  legend.text = element_text(size = 14, face = "bold")
)



# Create tibble just for pre-test and retention
# Will be used to make a dot plot with group mean and
# individual scores in the background
e2t_pre_ret <- dplyr::filter(e2t_data, phase != 2)


# e2t_pre_ret_group <- e2t_pre_ret %>% 
#   dplyr::group_by(group, phase) %>% 
#   dplyr::summarize(
#     n = n(),
#     mean_re = mean(score, na.rm = TRUE),
#     sd_re = sd(score, na.rm = TRUE),
#     se_re = sd_re/sqrt(n),
#     ci_re = 1.96 * se_re
#   )
