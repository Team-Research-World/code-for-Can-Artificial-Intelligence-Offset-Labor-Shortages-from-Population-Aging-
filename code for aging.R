
##########corr
df <- data.frame(
  ai_in_total = c(0.008354872, 0.026049892, 0.014777745, 0.010666011, 0.029973771,
                  0.040148686, 0.013275808, 0.017944751, 0.008416732, 0.009105159),
  age55_in_total = c(0.025792346, 0.007514123, 0.014064478, 0.029497314, 0.041623498,
                     0.017337088, 0.023361967, 0.019435061, 0.029386563, 0.065299085)
)

cor(df$ai_in_total, df$age55_in_total, method = "pearson")


######
df<-read_excel("C:/Users/Huang/Desktop/aging/5e.xlsx")

library(DescTools)
df$age55_in_total_w <- Winsorize(df$age55_in_total, probs = c(0.05, 0.95), na.rm = TRUE)
df$age60_in_total_w <- Winsorize(df$age60_in_total, probs = c(0.05, 0.95), na.rm = TRUE)
df$age65_in_total_w <- Winsorize(df$age65_in_total, probs = c(0.05, 0.95), na.rm = TRUE)
df$ai_in_total_w <- Winsorize(df$ai_in_total, probs = c(0.05, 0.95), na.rm = TRUE)





#################
df <- df[order(df$country, df$naics2, df$year), ]
df$ai_in_total_w_lag1 <- ave(df$ai_in_total_w, df$country, df$naics2, 
                             FUN = function(x) c(NA, head(x, -1)))

library(fixest)
fe1 <- feols(age55_in_total_w ~ ai_in_total_w | year, data = df)
summary(fe1)

fe2 <- feols(age55_in_total_w ~ ai_in_total_w | naics2 + year, data = df)
summary(fe2)

fe3 <- feols(age55_in_total_w ~ ai_in_total_w | naics2 + country + year, data = df)
summary(fe3)

fe4 <- feols(age55_in_total_w ~ ai_in_total_w_lag1 | naics2 + country + year, data = df)
summary(fe4)

library(texreg)
htmlreg(
  list(fe1, fe2, fe3, fe4),
  file = "C:/Users/Huang/Desktop/aging/5e.doc",
  caption = "Estimation of Models",
  caption.above = TRUE,
  custom.model.names = c(
    "age55_in_total_w",
    "age55_in_total_w",
    "age55_in_total_w",
    "age55_in_total_w"
  ),
  digits = 4,
  stars = c(0.1, 0.05, 0.01)
)






library(fixest)
fe1 <- feols(age60_in_total_w ~ ai_in_total_w | year, data = df)
summary(fe1)

fe2 <- feols(age60_in_total_w ~ ai_in_total_w | naics2 + year, data = df)
summary(fe2)

fe3 <- feols(age60_in_total_w ~ ai_in_total_w | naics2 + country + year, data = df)
summary(fe3)

fe4 <- feols(age60_in_total_w ~ ai_in_total_w_lag1 | naics2 + country + year, data = df)
summary(fe4)

library(texreg)
htmlreg(
  list(fe1, fe2, fe3, fe4),
  file = "C:/Users/Huang/Desktop/aging/5e_60.doc",
  caption = "Estimation of Models",
  caption.above = TRUE,
  custom.model.names = c(
    "age60_in_total_w",
    "age60_in_total_w",
    "age60_in_total_w",
    "age60_in_total_w"
  ),
  digits = 4,
  stars = c(0.1, 0.05, 0.01)
)









library(fixest)
fe1 <- feols(age65_in_total_w ~ ai_in_total_w | year, data = df)
summary(fe1)

fe2 <- feols(age65_in_total_w ~ ai_in_total_w | naics2 + year, data = df)
summary(fe2)

fe3 <- feols(age65_in_total_w ~ ai_in_total_w | naics2 + country + year, data = df)
summary(fe3)

fe4 <- feols(age65_in_total_w ~ ai_in_total_w_lag1 | naics2 + country + year, data = df)
summary(fe4)

library(texreg)
htmlreg(
  list(fe1, fe2, fe3, fe4),
  file = "C:/Users/Huang/Desktop/aging/5e_65.doc",
  caption = "Estimation of Models",
  caption.above = TRUE,
  custom.model.names = c(
    "age65_in_total_w",
    "age65_in_total_w",
    "age65_in_total_w",
    "age65_in_total_w"
  ),
  digits = 4,
  stars = c(0.1, 0.05, 0.01)
)









#####
fe3 <- feols(age55_in_total_w ~ i(country, ai_in_total_w) | naics2 + year, data = df)
summary(fe3)

library(texreg)
htmlreg(
  list(fe3),
  file = "C:/Users/Huang/Desktop/aging/5d.doc",
  caption = "Estimation of Models",
  caption.above = TRUE,
  custom.model.names = c(
    "age55_in_total_w"
  ),
  digits = 4,
  stars = c(0.1, 0.05, 0.01)
)

fe4 <- feols(age55_in_total_w ~ i(naics2, ai_in_total_w) | country + year, data = df)
summary(fe4)



library(texreg)
htmlreg(
  list(fe4),
  file = "C:/Users/Huang/Desktop/aging/5e.doc",
  caption = "Estimation of Models",
  caption.above = TRUE,
  custom.model.names = c(
    "age55_in_total_w"
  ),
  digits = 4,
  stars = c(0.1, 0.05, 0.01)
)


library(broom)
library(ggplot2)

coefs <- broom::tidy(fe3, conf.int = TRUE)

coefs_interaction <- coefs %>%
  filter(grepl("ai_in_total", term))

coefs_interaction <- coefs_interaction %>%
  mutate(stars = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01  ~ "**",
    p.value < 0.05  ~ "*",
    p.value < 0.1   ~ ".",
    TRUE            ~ ""
  ))

label_names <- c("Australia : AIP",
                 "China : AIP",
                 "France : AIP",
                 "ltaly : AIP",
                 "Japan : AIP", 
                 "South Korea : AIP",
                 "Spain : AIP",
                 "Sweden : AIP",
                 "United Kingdom : AIP",
                 "United States : AIP"
                 )  
coefs_interaction$label <- label_names


p_5d<-ggplot(coefs_interaction, aes(x = reorder(label, estimate), y = estimate)) +
  geom_point(color = "#5aae61", size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "#5aae61") +
  geom_text(aes(label = stars), 
            nudge_x = 0.1, nudge_y = 0.25, size = 4, color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  coord_flip() +
  labs(x = NULL, y = "Estimate", title = "d") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0, face = "bold"),
    axis.line = element_line(color = "black", size = 0.5),
    axis.ticks = element_line(color = "black", size = 0.3),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )







coefs <- broom::tidy(fe4, conf.int = TRUE)

coefs_interaction <- coefs %>%
  filter(grepl("ai_in_total", term))

coefs_interaction <- coefs_interaction %>%
  mutate(stars = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01  ~ "**",
    p.value < 0.05  ~ "*",
    p.value < 0.1   ~ ".",
    TRUE            ~ ""
  ))

label_names <- c("11 : AIP",
                 "21 : AIP",
                 "22 : AIP",
                 "23 : AIP",
                 "31-33 : AIP", 
                 "42 : AIP",
                 "44-45 : AIP",
                 "48-49 : AIP",
                 "51 : AIP",
                 "52 : AIP",
                 "53 : AIP",
                 "54 : AIP",
                 "55 : AIP",
                 "56 : AIP",
                 "61 : AIP", 
                 "62 : AIP",
                 "71 : AIP",
                 "72 : AIP",
                 "81 : AIP",
                 "92 : AIP"
)  
coefs_interaction$label <- label_names


p_5e<-ggplot(coefs_interaction, aes(x = reorder(label, estimate), y = estimate)) +
  geom_point(color = "#5aae61", size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "#5aae61") +
  geom_text(aes(label = stars), 
            nudge_x = 0.2, nudge_y = 0.55, size = 4, color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  coord_flip() +
  labs(x = NULL, y = "Estimate", title = "e") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0, face = "bold"),
    axis.line = element_line(color = "black", size = 0.5),
    axis.ticks = element_line(color = "black", size = 0.3),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )




###
df<-read_excel("C:/Users/Huang/Desktop/5a.xlsx")
library(ggplot2)
library(ggrepel)
library(ggsci)
library(scales)  

p_5a<-ggplot(df, aes(x = ai_in_total, y = age55_in_total, color = country)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_text_repel(aes(label = country), size = 3, fontface = "bold", 
                  show.legend = FALSE, max.overlaps = 20) +
  scale_color_npg() +
  scale_x_continuous(labels = label_percent(accuracy = 1)) + 
  scale_y_continuous(labels = label_percent(accuracy = 1)) + 
  labs(x = "AIP", y = "ELF55", title = "a") +
  theme_minimal(base_size = 10) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", hjust = 0, size = 14),
    legend.position = "none",
    axis.line = element_line(color = "black", size = 0.3),
    axis.ticks = element_line(color = "black", size = 0.3),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey85", size = 0.2)
  )






###
df<-read_excel("C:/Users/Huang/Desktop/5b.xlsx")


ggplot(df, aes(x = ai_in_total, y = age55_in_total)) +
  geom_point(color = "forestgreen", alpha = 0.6, size = 3) +
  geom_text_repel(aes(label = naics2), size = 3, fontface = "bold", 
                  color = "forestgreen", max.overlaps = 30) +
  scale_x_continuous(labels = label_percent(accuracy = 1)) + 
  scale_y_continuous(labels = label_percent(accuracy = 1)) + 
  labs(x = "AIP", y = "ELF55", title = "b") +
  theme_minimal(base_size = 10) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", hjust = 0, size = 14),
    legend.position = "none",
    axis.line = element_line(color = "black", size = 0.3),
    axis.ticks = element_line(color = "black", size = 0.3),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey85", size = 0.2)
  )





###
df<-read_excel("C:/Users/Huang/Desktop/5b.xlsx")


p_5b<-ggplot(df, aes(x = ai_in_total, y = age55_in_total)) +
  geom_point(color = "forestgreen", alpha = 0.6, size = 3) +
  geom_text_repel(aes(label = naics2), size = 3, fontface = "bold", 
                  color = "black", max.overlaps = 30) +
  scale_x_continuous(labels = label_percent(accuracy = 1)) + 
  scale_y_continuous(labels = label_percent(accuracy = 1)) + 
  labs(x = "AIP", y = "ELF55", title = "b") +
  theme_minimal(base_size = 10) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", hjust = 0, size = 14),
    legend.position = "none",
    axis.line = element_line(color = "black", size = 0.3),
    axis.ticks = element_line(color = "black", size = 0.3),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey85", size = 0.2)
  )









###
df<-read_excel("C:/Users/Huang/Desktop/5c.xlsx")
library(ggplot2)
library(ggrepel)
library(scales)
library(dplyr)

df_label <- df %>%
  filter(age55_in_total > 0.11 | ai_in_total >= 0.02)

p_5c<-ggplot(df, aes(x = ai_in_total, y = age55_in_total)) +
  geom_point(color = "#FF6F61", alpha = 0.6, size = 3) +
  geom_text_repel(data = df_label, aes(label = role_k150), 
                  color = "black", size = 3, fontface = "bold", max.overlaps = 20) +
  scale_x_continuous(labels = label_percent(accuracy = 1)) +
  scale_y_continuous(labels = label_percent(accuracy = 1)) +
  labs(x = "AIP", y = "ELF55", title = "c") +
  theme_minimal(base_size = 10) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", hjust = 0, size = 14),
    legend.position = "none",
    axis.line = element_line(color = "black", size = 0.3),
    axis.ticks = element_line(color = "black", size = 0.3),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey85", size = 0.2)
  )


###


library(gridExtra)
library(grid)

row1 <- arrangeGrob(p_5a, p_5b, p_5c, ncol = 3,
                    widths = unit.c(unit(10, "cm"), unit(10, "cm"), unit(10, "cm")))
row2 <- arrangeGrob(p_5d, p_5e, ncol = 2,
                    widths = unit.c(unit(12, "cm"), unit(12, "cm")))


plots_grid <- arrangeGrob(
  row1, row2,
  ncol = 1,
  heights = unit(rep(8, 2), "cm")
)

grid.newpage()
grid.draw(plots_grid)

ggsave("C:/Users/Huang/Desktop/aging/fig5.png", plots_grid, 
       width = 30, height = 16, units = "cm", dpi = 600)













