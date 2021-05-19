#######################################
####### analysis of netlogo abm #######
#######################################

pacman::p_load(tidyverse, RColorBrewer, ggthemes, patchwork)

## LOADING THE UGLY NETLOGO DATA ##
topic_base <- read.csv("~/Documents/UNI/sem 4/social and cultural dynamics/exam/soccult_abm/base_topic.csv", sep=";")
topic_non <- read.csv("~/Documents/UNI/sem 4/social and cultural dynamics/exam/soccult_abm/non_topic.csv", sep=";")
topic_social <- read.csv("~/Documents/UNI/sem 4/social and cultural dynamics/exam/soccult_abm/social_topic.csv", sep=";")

bias_base <- read.csv("~/Documents/UNI/sem 4/social and cultural dynamics/exam/soccult_abm/base_bias.csv", sep=";")
bias_non <- read.csv("~/Documents/UNI/sem 4/social and cultural dynamics/exam/soccult_abm/non_bias.csv", sep=";")
bias_social <- read.csv("~/Documents/UNI/sem 4/social and cultural dynamics/exam/soccult_abm/social_bias.csv", sep=";")

### trying wide to long ###

#topics function
wide_to_long <- function(df, cond) {
p <- df %>% 
  select(ends_with("_p")) %>% 
  rename(x = x_p,
         y = y_p,
         color = color_p)
bt <- df %>% 
  select(ends_with("bt"))%>% 
  rename(x = x_bt,
         y = y_bt,
         color = color_bt)
jp <- df %>% 
  select(ends_with("jp")) %>% 
  rename(x = x_jp,
         y = y_jp,
         color = color_jp)
b <- df %>% 
  select(ends_with("_b")) %>% 
  rename(x = x_b,
         y = y_b,
         color = color_b)
eb <- df %>% 
  select(ends_with("eb"))%>% 
  rename(x = x_eb,
         y = y_eb,
         color = color_eb)
fn <- df %>% 
  select(ends_with("fn"))%>% 
  rename(x = x_fn,
         y = y_fn,
         color = color_fn)

long <- rbind(p, jp, eb, b, bt, fn) %>% 
  mutate(condition = cond)

return(long)
 }


topic_base_long <- wide_to_long(topic_base, "baseline")

topic_non_long <- wide_to_long(topic_non, "non-social")

topic_social_long <- wide_to_long(topic_social, "social")

#biases function
wide_2_long <- function(df, cond) {
  b1 <- df %>% 
    select(ends_with("_1")) %>% 
    rename(x = x_1,
           y = y_1,
           color = color_1)
  b2 <- df %>% 
    select(ends_with("_2"))%>% 
    rename(x = x_2,
           y = y_2,
           color = color_2)
  b3 <- df %>% 
    select(ends_with("_3")) %>% 
    rename(x = x_3,
           y = y_3,
           color = color_3)
  b4 <- df %>% 
    select(ends_with("_4")) %>% 
    rename(x = x_4,
           y = y_4,
           color = color_4)
  b5 <- df %>% 
    select(ends_with("_5"))%>% 
    rename(x = x_5,
           y = y_5,
           color = color_5)
  
  long <- rbind(b1, b2, b3, b4, b5) %>% 
    mutate(condition = cond)
  
  return(long)
}

bias_base_long <- wide_2_long(bias_base, "baseline")

bias_non_long <- wide_2_long(bias_non, "non-social")

bias_social_long <- wide_2_long(bias_social, "social")



## data cleaning
#combining the conditions for topics
df <- rbind(topic_base_long, topic_non_long, topic_social_long) %>% 
  mutate(color = as.factor(color),
         condition = as.factor(condition))
str(df)

#combining the conditions for biases 
df_b <- rbind(bias_base_long, bias_non_long, bias_social_long) %>% 
  mutate(color = as.factor(color),
         condition = as.factor(condition))
str(df_b)

#cleaning up in my environment
rm(list = ls(pattern = "topic|bias"))

#making levels meaningful
levels(df$color) <- c("Fake News", 
                      "Politiken", 
                      "Ekstra Bladet",
                      "Berlingske",
                      "BT", 
                      "Jyllands-Posten")

levels(df_b$color) <- c("Bias 1", "Bias 2", "Bias 3", "Bias 4", "Bias 5")

write_csv(df, "compmodel_topic_data.csv")
write_csv(df_b, "compmodel_bias_data.csv")

#### PLOTS ####

#topic prevalence in each condition - AREA PLOT
p1 <- ggplot(df, aes(x, y, fill = color)) + 
  geom_area(alpha = 0.9, color = "white", size = 0.2, stat = "smooth", span = 0.12) + 
  facet_wrap(~condition) +
  scale_fill_manual( name = "News Media",
                      values = c("black","red","orange", "yellow", "blue", "pink")) +
  labs(title = "Topic Prevalence by Condition over Time", x = "Ticks", y = "Percent") +
  theme_minimal() +
  theme(strip.text = element_text(size=9,  color = "black"),
        strip.background = element_rect(fill="light grey", color = "white"))

p2 <-  ggplot(df, aes(x, y, color = color)) + 
  geom_line(size = 0.5, stat = "smooth", span = 0.1) + 
  facet_wrap(~condition) +
  scale_color_manual( name = "News Media",
                     values = c("black","red","orange", "yellow", "blue", "pink")) +
  labs(title = "Topic Prevalence by Condition over Time", x = "Ticks", y = "Percent") + 
  theme_minimal() +
  theme(strip.text = element_text(size=9,  color = "black"),
        strip.background = element_rect(fill="light grey", color = "white"))
p1+p2

p1
p2

#the prevalence of fake news in each bias across conditions - AREA PLOT
ggplot(df_b, aes(x, y, fill = color)) + 
  geom_area(alpha = 0.9, color = "white", size = 0.2, stat = "smooth", span = 0.15) + 
  facet_wrap(~condition) +
  scale_fill_colorblind(name = "Social Identity")+
  labs(title = "Fake News Prevalence in each Bias by Condition", x = "Ticks", y = "Percent") +
  ylim(0, 15) +
  theme_minimal() +
  theme(strip.text = element_text(size=9,  color = "black"),
        strip.background = element_rect(fill="light grey", color = "white"))

# #the prevalence of fake news in each bias across conditions - GRID LINE PLOT
# ggplot(df_b, aes(x, y, color = color)) +
#   geom_line(stat = "smooth", span = 0.05) +
#   facet_grid(condition~color) +
#   labs(title = "Fake News Prevalence in each Bias by Condition", x = "Ticks", y = "Percent") +
#   scale_color_colorblind( ) +
#   #theme_minimal() +
#   theme(axis.text.x=element_blank(), legend.position = "none")

#density plots 
df %>%
  filter(color == "Fake News") %>%
  ggplot(., aes(x, y))+
  geom_line() +
  facet_wrap(~condition)+
  labs(title = "Fake News Prevalence by Condition", x = "Percent") +
  theme_minimal()
# 
# df %>% 
#   filter(color != "Fake News") %>% 
#   ggplot(., aes(y, color = color))+
#   geom_density() +
#   facet_wrap(~condition)+
#   labs(title = "Other News Media Prevalence by Condition", x = "Percent") +
#   scale_color_manual( name = "News Media",
#                      values = c("red","orange", "yellow", "blue", "pink")) +
#   theme_minimal()

# Topic prevalence by condition - DENSITY PLOT
df %>% 
  ggplot(., aes(y, color = color))+
  geom_density() +
  facet_wrap(~condition)+
  labs(title = "Topic Prevalence by Condition", x = "Percent") +
  scale_color_manual( name = "News Media",
                     values = c("black","red","orange", "yellow", "blue", "pink")) +
  theme_minimal()+
  theme(strip.text = element_text(size=9,  color = "black"),
        strip.background = element_rect(fill="light grey", color = "white"))




### NOW FOR THE POPULATION I GUESS ###

##make a function ??

read_nm <- function(filename, cond) {
  read_csv(filename, col_names = F) %>% 
    mutate(condition = cond,
           X1 = paste0(X1, sep = "_", X2 )) %>% 
    rename(turtle_id = X1,
           color = X2,
           nr_readers = X3)
}

read_ppl <- function(filename, cond) {
  read_csv(filename, col_names = F) %>% 
    mutate(condition = cond,
           X1 = paste0(X1, sep = "_", X3 )) %>% 
    rename(turtle_id = X1,
           color = X2,
           bias = X3,
           my_news = X4, 
           nr_source = X5, 
           nr_friends = X6,
           fake_news_reader = X7)
}

base_nm <- read_nm("baseline/base_nm.csv", "baseline")
non_nm <- read_nm("non/non_nm.csv", "non")
social_nm <- read_nm("social/social_nm.csv", "social")


base_ppl <- read_ppl("baseline/base_ppl.csv", "baseline")
non_ppl <- read_ppl("non/non_ppl.csv", "non")
social_ppl <- read_ppl("social/social_ppl.csv", "social")


ppl <- rbind(base_ppl, non_ppl, social_ppl) %>% 
  mutate(color = as.factor(color),
         bias = as.factor(bias),
         condition = as.factor(condition))
nm <- rbind(base_nm, non_nm, social_nm) %>% 
  mutate(color = as.factor(color),
         condition = as.factor(condition))

str(nm)

# cleany clean
rm(list = ls(pattern = "social|non|base"))

#meaningful levels
levels(ppl$color) <- c("Fake News", 
                      "Politiken", 
                      "Ekstra Bladet",
                      "Berlingske",
                      "BT", 
                      "Jyllands-Posten")
levels(nm$color) <- c("Fake News", 
                       "Politiken", 
                       "Ekstra Bladet",
                       "Berlingske",
                       "BT", 
                       "Jyllands-Posten")

write_csv(ppl, "compmodel_ppl_data.csv")
write_csv(nm, "compmodel_nm_data.csv")

### stuffs and things ###

# number of friends 

ppl %>% 
  filter(condition != "baseline") %>% 
  ggplot(., aes(nr_friends, color = condition))+
  geom_density() + 
  scale_color_colorblind() +
  theme_minimal()

ppls <- ppl %>% group_by(condition) 

sd(ppl$nr_friends)
sd(ppl$nr_source)

aggregate(nr_friends~condition, ppl, mean)
aggregate(nr_friends~condition, ppl, sd)
aggregate(nr_friends~condition, ppl, min)
aggregate(nr_friends~condition, ppl, max)

aggregate(nr_source~condition, ppl, max)
aggregate(nr_source~condition, ppl, min)
aggregate(nr_source~condition, ppl, mean)
aggregate(nr_source~condition, ppl, sd)


ppl_bias <- ppl %>% 
  #group_by(condition) %>%  
  count(bias)

mean(ppl_bias$n)

?count

ppl_friends <- ppl %>% 
  filter(condition != "baseline") 

mean(ppl_friends$nr_friends)


aggregate(condition ~ bias, data = ppl, mean)

ppl %>% filter(condition == "baseline" & my_news > 0.9)
ppl %>% filter(condition == "baseline" & my_news < 0.3)

df_fn <- df %>% filter(color == "Fake News" & condition == "baseline") 
max(df_fn$y)


df_bt <- df %>% filter(color == "BT" & condition == "non-social") 
max(df_bt$y)
min(df_bt$y)


df_p <- df %>% filter(color == "Politiken" & condition == "baseline") 
max(df_p$y)
min(df_p$y)
sd(df_p$y)

df_fn123 <- df %>% filter(color == "Fake News" & y < mean(df_fn$y) & condition == "social")

mean(df_fn$y)


ppl %>% count(color)


df_sad <- df %>% 
  filter(color == "Fake News" & condition == "social")
mean(df_sad$y)
sd(df_sad$y)



df_b_soc <- df_b %>% 
  filter(condition == "social")


