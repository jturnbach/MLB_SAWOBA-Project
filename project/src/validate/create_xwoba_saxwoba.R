# Front Matter ----------

# Load libraries
library(tidyverse)
library(tidymodels)
library(kknn)
library(mgcv)
library(scales)

# Load in final models (without spray)
final_knn_without_model <- readRDS("project/volume/models/knn_gam_without_spray_model_knn.rds")
final_gam_without_model <- readRDS("project/volume/models/knn_gam_without_spray_model_gam.rds")

# Load in final models (with spray)
final_knn_with_model <- readRDS("project/volume/models/knn_gam_with_spray_model_knn.rds")
final_gam_with_model <- readRDS("project/volume/models/knn_gam_with_spray_model_gam.rds")

# Read in raw data
batter2020 <- read.csv("project/volume/data/raw/sc_2020.csv")
batter2021 <- read.csv("project/volume/data/raw/sc_2021.csv")
batter2022 <- read.csv("project/volume/data/raw/sc_2022.csv")
batter2023 <- read.csv("project/volume/data/raw/sc_2023.csv")

sprintspeed20 <- read.csv("project/volume/data/raw/sprint_speed_2020_new.csv")
sprintspeed21 <- read.csv("project/volume/data/raw/sprint_speed_2021_new.csv")
sprintspeed22 <- read.csv("project/volume/data/raw/sprint_speed_2022_new.csv")
sprintspeed23 <- read.csv("project/volume/data/raw/sprint_speed_2023_new.csv")

woba20 <- read.csv("project/volume/data/raw/woba_2020.csv")
woba21 <- read.csv("project/volume/data/raw/woba_2021.csv")
woba22 <- read.csv("project/volume/data/raw/woba_2022.csv")
woba23 <- read.csv("project/volume/data/raw/woba_2023.csv")
woba_master <- rbind(woba20, woba21, woba22, woba23)
woba_master <- woba_master %>% rename(player_name = last_name..first_name)

# Combine pitch by pitch data with sprint speed data
total2020 <- left_join(batter2020, sprintspeed20, by = c("batter" = "player_id"))
total2021 <- left_join(batter2021, sprintspeed21, by = c("batter" = "player_id"))
total2022 <- left_join(batter2022, sprintspeed22, by = c("batter" = "player_id"))
total2023 <- left_join(batter2023, sprintspeed23, by = c("batter" = "player_id"))
sc_master <- rbind(total2020, total2021, total2022, total2023)
sc_master <- sc_master %>% rename(player_id = batter)

# Create a dataframe of MLB names and IDs
mlb_ids <- woba_master %>%
  distinct(player_id, player_name)

# Remove large data frames
rm(list = c("batter2020", "batter2021", "batter2022", "batter2023", "total2020", "total2021", "total2022", "total2023"))

# Separate batted balls
bbe_master <- sc_master %>% filter(description %in% c("hit_into_play"))

bbe_master <- bbe_master %>%
  mutate(bip_id = row_number(),
         bb_class = case_when(
    (launch_speed * 1.5 - launch_angle >= 117) & (launch_speed + launch_angle >= 124) & (launch_speed >= 98) & (launch_angle > 4 & launch_angle < 50) ~ "barrel",
    (launch_speed * 1.5 - launch_angle >= 111) & (launch_speed + launch_angle >= 119) & (launch_speed >= 95) & (launch_angle > 0 & launch_angle < 52) ~ "solid_contact",
    (launch_speed <= 59) ~ "poorly_weak",
    (launch_speed * 2 - launch_angle >= 87) & (launch_angle <= 41) & (launch_speed * 2 + launch_angle <= 175) & (launch_speed + launch_angle * 1.3 >= 89) & (launch_speed > 59 & launch_speed < 72) ~ "flare_burner",
    (launch_speed + launch_angle * 1.3 <= 112) & (launch_speed + launch_angle * 1.55 >= 92) & (launch_speed > 72 & launch_speed < 86) ~ "flare_burner",
    (launch_angle <= 20) & (launch_speed + launch_angle * 2.4 >= 98) & (launch_speed > 86 & launch_speed < 95) ~ "flare_burner",
    (launch_speed - launch_angle >= 76) & (launch_speed + launch_angle * 2.4 >= 98) & (launch_speed >= 95) & (launch_angle <= 30) ~ "flare_burner",
    (launch_speed + launch_angle * 2 >= 116) ~ "poorly_under",
    (launch_speed + launch_angle * 2 <= 116) ~ "poorly_topped",
    TRUE ~ NA_character_),
        spray_angle = round((atan((hc_x-125.42)/(198.27-hc_y))*180/pi*.75),1)) %>%
  mutate(bip_id = row_number()) %>%
  filter(!(is.na(sprint_speed) |is.na(launch_speed) | is.na(launch_angle) | is.na(spray_angle)))



# Calculate raw totals for each player/year ----------
raw_totals <- sc_master %>%
  group_by(player_id, game_year) %>%
  summarise(bb = sum(events == "walk"),
            hbp = sum(events == "hit_by_pitch"),
            ab = sum(events %in% c("double", "double_play", "field_error", "field_out", "fielders_choice", "fielders_choice_out", "force_out",
                                   "grounded_into_double_play", "home_run", "other_out", "single", "strikeout", "strikeout_double_play", "triple", "triple_play")),
            sf = sum(events %in% c("sac_fly", "sac_fly_double_play")),
            est_woba_denom = bb + hbp + ab + sf,
            woba_denom = sum(woba_denom, na.rm = TRUE))



# Predict xwOBAcon and sa-xwOBAcon for every batted ball event ----------
bbe_fb_ld <- bbe_master %>% filter(!(bb_class %in% c("poorly_weak", "poorly_topped")))
bbe_weak_gb <- bbe_master %>% filter(bb_class %in% c("poorly_weak", "poorly_topped"))

# xwOBAcon
xwobacon_fb_ld <- predict(final_knn_without_model, bbe_fb_ld)
xwobacon_fb_ld <- as.array(xwobacon_fb_ld$.pred)
xwobacon_fb_ld <- cbind(bbe_fb_ld, pred_woba_value = xwobacon_fb_ld)

xwobacon_weak_gb <- predict(final_gam_without_model, bbe_weak_gb)
xwobacon_weak_gb <- cbind(bbe_weak_gb, pred_woba_value = xwobacon_weak_gb)

xwobacon_preds <- rbind(xwobacon_fb_ld, xwobacon_weak_gb)

# sa-xwOBAcon
sa_xwobacon_fb_ld <- predict(final_knn_with_model, bbe_fb_ld)
sa_xwobacon_fb_ld <- as.array(sa_xwobacon_fb_ld$.pred)
sa_xwobacon_fb_ld <- cbind(bbe_fb_ld, pred_woba_value_spray = sa_xwobacon_fb_ld)

sa_xwobacon_weak_gb <- predict(final_gam_with_model, bbe_weak_gb)
sa_xwobacon_weak_gb <- cbind(bbe_weak_gb, pred_woba_value_spray = sa_xwobacon_weak_gb)

sa_xwobacon_preds <- rbind(sa_xwobacon_fb_ld, sa_xwobacon_weak_gb)

# Sum xwOBAcon for every player/year ----------
xwobacon_totals <- xwobacon_preds %>%
  group_by(player_id, game_year) %>%
  summarise(bbe = n(),
            xwobacon = sum(pred_woba_value, na.rm = TRUE))

sa_xwobacon_totals <- sa_xwobacon_preds %>%
  group_by(player_id, game_year) %>%
  summarise(sa_xwobacon = sum(pred_woba_value_spray, na.rm = TRUE))


# Calculate xwOBA ----------
final_xwoba <- raw_totals %>%
  left_join(xwobacon_totals, by = c("player_id", "game_year")) %>%
  left_join(sa_xwobacon_totals, by = c("player_id", "game_year")) %>%
  mutate(xwoba = round((xwobacon + 0.7 * bb + 0.7 * hbp) / (woba_denom), 3),
         sa_xwoba = round((sa_xwobacon + 0.7 * bb + 0.7 * hbp) / (woba_denom), 3))

final_xwoba <- final_xwoba %>%
  left_join(mlb_ids, by = "player_id")


# Compare results
compare <- final_xwoba %>%
  left_join(woba_master, by = c("player_id" = "player_id", "player_name" = "player_name", "game_year" = "year")) %>%
  select(player_name, player_id, game_year, woba_denom, bbe, woba, est_woba, xwoba, sa_xwoba, xwobacon, sa_xwobacon)


compare <- compare %>% drop_na()

correlation <- cor(compare$xwoba, compare$est_woba)

plot <- ggplot(compare, aes(x = xwoba, y = est_woba)) + 
  geom_point() +
  scale_x_continuous(breaks = pretty_breaks(n = 10), labels = scales::number_format(accuracy = 0.001)) +
  scale_y_continuous(breaks = pretty_breaks(n = 10), labels = scales::number_format(accuracy = 0.001)) +
  labs(title = expression(bold("Relationship Between Our xwOBA and MLB xwOBA")), 
       x = "Our xwOBA", y = "MLB xwOBA") +
  geom_text(aes(label = paste("Correlation:", round(correlation, 2))), x = max(compare$xwoba), y = min(compare$est_woba), hjust = 1, vjust = 0) +
  theme(panel.grid = element_blank())

print(plot)

# Save final metrics for comparison measure
write.csv(compare, "project/volume/data/processed/xwoba_saxwoba.csv", row.names = FALSE)
