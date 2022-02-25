library(plyr)
library(tidyverse)
library(openxlsx)
library(smatr)
library(patchwork)
library(scales)
library(tmap)
library(sf)
library(lme4)


## GHS Data obtained from https://ghsl.jrc.ec.europa.eu/datasets.php
data_location <- "datasets/GHS_urban_centers/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.csv"
urban <- read.csv(data_location)

## CONSTANTS
AREA_FILTER <- 100

##########################
#### Appendix Table 1
##########################

## This function condenses the results from both SMA and LM into a single table,
## Input are the results from dlply of both lm and sma and requires that
## the dlply variables are CTR_MN_ISO, CTR_MN_NM, DEV_CMI, GRGN_L2.
##
## EXAMPLE: countries_rma <- dlply(urban, .(CTR_MN_ISO, CTR_MN_NM, DEV_CMI), sma,
## formula = log(AREA) ~ log(P15))
## countries_lm <- dlply(urban, .(CTR_MN_ISO, CTR_MN_NM, DEV_CMI, GRGN_L1), lm,
##                      formula = log(AREA) ~ log(P15))
## WriteJoinedTable(countries_rma, countries_lm)
##
WriteJoinedTable <- function(countries_rma, countries_lm) {
  df <- data.frame(matrix(ncol = 15, nrow = 0))
  headers <- c("Region",
               "Country (n)",
               "Development Class",
               "Ind",
               "OLS Exponent [95% CI]",
               "OLS Elevation [95% CI]",
               "RMA Exponent [95% CI]",
               "RMA Elevation [95% CI]",
               "95% CI lower",
               "95% CI upper",
               "P-value",
               "R2")
  colnames(df) <- headers

  # append results from each list entry into a dataframe
  for (i in 1:length(countries_lm)) {
    rma_summ <- countries_rma[[i]]
    rma_r2 <- round(unlist(rma_summ$r2), digits = 2)
    ind <- rma_summ$variables[2]
    rma_Exponent <- round(unlist(rma_summ$coef)[2], digits = 2)
    rma_Exponent_CI <- unlist(rma_summ$slopetest)
    rma_Exponent_str <- sprintf("%.2f  [%.2f, %.2f]", rma_Exponent,
                                rma_Exponent_CI[6], rma_Exponent_CI[7])
    rma_Elevation <- round(unlist(rma_summ$coef)[1], digits = 2)

    rma_int_CI <- unlist(rma_summ$elevtest)
    rma_int_str <- sprintf("%.2f  [%.2f, %.2f]", rma_Elevation,
                           rma_int_CI[4], rma_int_CI[5])
    rma_country <- unlist(strsplit(names(countries_rma)[i], "[.]"))
    rma_p_val <- round(unlist(rma_summ$pval), 3)

    summ <- countries_lm[[i]]
    ols_country <- unlist(strsplit(names(countries_lm)[i], "[.]"))
    ols_Elevation <- round(summ$coefficients[1], digits = 2)
    ols_Exponent <- round(summ$coefficients[2], digits = 2)
    CI.95 <- confint(countries_lm[[i]], level = 0.95)
    ols_Exponent_str <- sprintf("%.2f  [%.2f, %.2f]", ols_Exponent,
                                CI.95[2, 1], CI.95[2, 2])
    ols_int_str <- sprintf("%.2f  [%.2f, %.2f]", ols_Elevation,
                           CI.95[1, 1], CI.95[1, 2])
    x <- data.frame(ols_country[4],
                    sprintf("%s (%d)",rma_country[2], unlist(rma_summ$n)),
                    rma_country[3],
                    ind,
                    ols_Exponent_str,
                    ols_int_str,
                    rma_Exponent_str,
                    rma_int_str,
                    rma_Exponent_CI[6],
                    rma_Exponent_CI[7],
                    rma_p_val,
                    rma_r2)
    colnames(x) <- headers
    df <- rbind(df, x)
  }
  return(df)
}

countries_rma <- dlply(urban, .(CTR_MN_ISO, CTR_MN_NM, DEV_CMI), sma,
                       formula = log(AREA) ~ log(P15))
countries_lm <- dlply(urban, .(CTR_MN_ISO, CTR_MN_NM, DEV_CMI, GRGN_L1), lm,
                      formula = log(AREA) ~ log(P15))
joined_area_df <- WriteJoinedTable(countries_rma, countries_lm)

countries_rma <- urban %>%
  subset(subset = H00_AREA >= AREA_FILTER) %>%
  group_by(CTR_MN_ISO) %>%
  filter(n() >= 5) %>%
  dlply(.(CTR_MN_ISO, CTR_MN_NM, DEV_CMI), sma, formula = log(H00_AREA) ~ log(P00))
countries_lm <- urban %>%
  subset(subset = H00_AREA >= AREA_FILTER) %>%
  group_by(CTR_MN_ISO) %>%
  filter(n() >= 5) %>%
  dlply(.(CTR_MN_ISO, CTR_MN_NM, DEV_CMI, GRGN_L1), lm,
        formula = log(H00_AREA) ~ log(P00))
joined_area_df <- WriteJoinedTable(countries_rma, countries_lm) %>% rbind(joined_area_df)

countries_rma <- urban %>%
  subset(subset = H75_AREA >= AREA_FILTER) %>%
  group_by(CTR_MN_ISO) %>%
  filter(n() >= 5) %>%
  dlply(.(CTR_MN_ISO, CTR_MN_NM, DEV_CMI), sma, formula = log(H75_AREA) ~ log(P75))
countries_lm <- urban %>%
  subset(subset = H75_AREA >= AREA_FILTER) %>%
  group_by(CTR_MN_ISO) %>%
  filter(n() >= 5) %>%
  dlply(.(CTR_MN_ISO, CTR_MN_NM, DEV_CMI, GRGN_L1), lm,
        formula = log(H75_AREA) ~ log(P75))
joined_area_df <- WriteJoinedTable(countries_rma, countries_lm) %>% rbind(joined_area_df)

countries_rma <- urban %>%
  subset(subset = H90_AREA >= AREA_FILTER) %>%
  group_by(CTR_MN_ISO) %>%
  filter(n() >= 5) %>%
  dlply(.(CTR_MN_ISO, CTR_MN_NM, DEV_CMI), sma,
        formula =  log(H90_AREA) ~ log(P90))
countries_lm <- urban %>%
  subset(subset = H90_AREA >= AREA_FILTER) %>%
  group_by(CTR_MN_ISO) %>%
  filter(n() >= 5) %>%
  dlply(.(CTR_MN_ISO, CTR_MN_NM, DEV_CMI, GRGN_L1), lm,
        formula = log(H90_AREA) ~ log(P90))
joined_area_df <- WriteJoinedTable(countries_rma, countries_lm) %>% rbind(joined_area_df)

joined_area_df$Ind <- replace(joined_area_df$Ind, joined_area_df$Ind == "log(P15)", 2015)
joined_area_df$Ind <- replace(joined_area_df$Ind, joined_area_df$Ind == "log(P00)", 2000)
joined_area_df$Ind <- replace(joined_area_df$Ind, joined_area_df$Ind == "log(P75)", 1975)
joined_area_df$Ind <- replace(joined_area_df$Ind, joined_area_df$Ind == "log(P90)", 1990)
colnames(joined_area_df)[4] = "Year"

#write.xlsx(joined_area_df, "time_df.xlsx")





##########################
#### Appendix Table 2
##########################

#### Create tables from analyses results
# This function writes the results from SMA, performed by dlply on each country
# in the dataset, into a table. Input is the resulting list from dlply and
# requires that the dlply variables are CTR_MN_ISO, CTR_MN_NM, and DEV_CMI.
#
# EXAMPLE:
# countries_sma <- dlply(urban2, .(CTR_MN_ISO, CTR_MN_NM, DEV_CMI), sma,
#                      formula = log_GDPCAP15 ~ log10(density15))
# df <- WriteRmaTable(countries_sma)
WriteRmaTable <- function(countries_rma) {
  rma_df <- data.frame(matrix(ncol = 15, nrow = 0))
  headers <- c("Code",
               "Country",
               "Region",
               "Development Class",
               "Ind",
               "Dep",
               "n",
               "P-value",
               "R-squared",
               "Elevation",
               "Exponent",
               "95% CI lower",
               "95% CI upper",
               "min_x",
               "max_x")
  colnames(rma_df) <- headers

  # append results from each list entry into a dataframe
  for (i in 1:length(countries_rma)) {
    summ <- countries_rma[[i]]
    r2 <- round(unlist(summ$r2), digits = 2)
    dep <- summ$variables[1]
    ind <- summ$variables[2]
    Elevation <- round(unlist(summ$coef)[1], digits = 2)
    Exponent <- round(unlist(summ$coef)[2], digits = 2)
    vals <- unlist(summ$slopetest)
    country <- unlist(strsplit(names(countries_rma)[i], "[.]"))
    p_val <- round(unlist(summ$pval), 3)
    min_x <- min(summ$data[2], na.rm = T)
    max_x <- max(summ$data[2], na.rm = T)
    x <-data.frame(country[1],
                   country[2],
                   country[4],
                   country[3],
                   ind,
                   dep,
                   unlist(summ$n),
                   p_val,
                   r2,
                   Elevation,
                   Exponent,
                   vals[6],
                   vals[7],
                   min_x,
                   max_x)
    colnames(x) <- headers
    rma_df <- rbind(rma_df, x)
  }
  return(rma_df)
}



## Store RMA results for each year in a table
area_pop_df_15 <- urban %>%
  subset(AREA >= AREA_FILTER) %>%
  group_by(CTR_MN_ISO) %>%
  filter(n() >= 5) %>%
  dlply(.(CTR_MN_ISO, CTR_MN_NM, DEV_CMI, GRGN_L1), sma,
        formula = log10(AREA) ~ log10(P15)) %>%
  WriteRmaTable()

area_pop_df_00 <- urban %>%
  subset(subset = H00_AREA >= AREA_FILTER) %>%
  group_by(CTR_MN_ISO) %>%
  filter(n() >= 5) %>%
  dlply(.(CTR_MN_ISO, CTR_MN_NM, DEV_CMI, GRGN_L1), sma,
        formula = log10(H00_AREA) ~ log10(P00)) %>%
  WriteRmaTable()

area_pop_df_90 <- urban %>%
  subset(subset = H90_AREA >= AREA_FILTER) %>%
  group_by(CTR_MN_ISO) %>%
  filter(n() >= 5) %>%
  dlply(.(CTR_MN_ISO, CTR_MN_NM, DEV_CMI, GRGN_L1), sma,
        formula = log10(H90_AREA) ~ log10(P90)) %>%
  WriteRmaTable()

area_pop_df_75 <- urban %>%
  subset(subset = H75_AREA >= AREA_FILTER) %>%
  group_by(CTR_MN_ISO) %>%
  filter(n() >= 5) %>%
  dlply(.(CTR_MN_ISO, CTR_MN_NM, DEV_CMI, GRGN_L1),
        sma,
        formula = log10(H75_AREA) ~ log10(P75)) %>%
  WriteRmaTable()

area_pop_df <- rbind(area_pop_df_15, area_pop_df_00, area_pop_df_90, area_pop_df_75)
area_pop_df$Year <- area_pop_df$Ind
area_pop_df$Year <- replace(area_pop_df$Year, area_pop_df$Year == "log10(P15)", 2015)
area_pop_df$Year <- replace(area_pop_df$Year, area_pop_df$Year == "log10(P00)", 2000)
area_pop_df$Year <- replace(area_pop_df$Year, area_pop_df$Year == "log10(P90)", 1990)
area_pop_df$Year <- replace(area_pop_df$Year, area_pop_df$Year == "log10(P75)", 1975)
area_pop_df$Year <- as.numeric(as.character(area_pop_df$Year))

ggplot(area_pop_df, aes(x = Year, y = Exponent)) +
  facet_wrap(~ Country, ncol = 4) +
  geom_line() +
  geom_point(show.legend = F, size = 0.7) +
  geom_linerange(data = area_pop_df,
                 aes(ymin = `95% CI lower`, ymax = `95% CI upper`), size = 1,
                 alpha = 0.6, show.legend = F) +
  geom_abline(slope = 0, intercept = 1, linetype = "dashed", alpha = 0.7) +
  geom_abline(slope = 0, intercept = 2 / 3, linetype = "dashed", color = "red") +
  scale_y_continuous(breaks = c(0.4, 2 / 3, 1, 1.6),
                     labels = c(0.4, expression("2/3"), 1, 1.6)) +
  coord_cartesian(ylim = c(0.4, 1.8)) +
  theme_bw() +
  theme(legend.direction = "horizontal",
        legend.position = c(0.70, 0.82),
        legend.text = element_text(size = 7),
        legend.spacing.y = unit(2, "pt"),
        legend.key.size = unit(5, "pt")) +
  guides(color = guide_legend(byrow = TRUE)) +
  ylab(expression(paste("Scaling Exponent ", alpha))) +
  scale_x_continuous(breaks = c(1975, 1990, 2000, 2015))

#ggsave("countries_plot.pdf",width = 10, height = 25, device = 'pdf',dpi = 500)







##########################
#### Fig 1: Map
##########################


# Filter out cities less than AREA_FILTER and countries with less than 5 cities
# remaining for 2015
urban <- subset(urban, urban$AREA >= AREA_FILTER)
urban <- subset(urban, with(urban, CTR_MN_ISO %in% names(which(
  table(CTR_MN_ISO) >= 5))))

data("World")
World <- merge(World, area_pop_df_15,by.x = "iso_a3", by.y = "Code", all.x = T)
urban$density15 <- (urban$P15 / urban$AREA)

pop_area_map <- ggplot() +
  geom_sf(World, mapping = aes(fill = Exponent),lwd = 0.3,color = "black") +
  geom_point(arrange(urban, density15),mapping = aes(x = GCPNT_LON,
      y = GCPNT_LAT,size = density15),stroke = 0.0,shape = 16) +
  coord_sf(crs = 8857,default_crs = sf::st_crs(4326),label_axes = "----") +
  theme_classic()  +
  scale_radius(limits = c(0, 30000), range = c(0.5, 3)) +
  labs(size = "Density", fill = "Exponent") +
  theme(axis.title = element_blank())

# Synthesize data for drawing lines in Exponent plot
slope_seq = seq(min(area_pop_df_15$Exponent),
                max(area_pop_df_15$Exponent), by = 0.002)
x_y <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(x_y) <- c("x", "Exponent")
for (i in 1:length(slope_seq)) {
  x <- 1 / (1 + slope_seq[i] ^ 2) ^ (1 / 2)
  r <- data.frame(x, slope_seq[i])
  colnames(r) <- c("x", "Exponent")
  x_y <- rbind(x_y, r)
}

# Create label for Bettencourt's model
model_lbl <- expression(paste(alpha, " = 2/3"))

# Create reference plot to show color scale gradient
allom_p <- ggplot(x_y) +
  geom_segment(aes(x = 0, y = 0, xend = x, yend = Exponent * x, color = Exponent),
               show.legend = F) +
  geom_abline(slope = 2 / 3) +
  geom_abline(slope = 1, linetype = "dashed") +
  geom_text(mapping = aes(1, 2 / 3), size = 3, label = model_lbl, vjust = -0.5) +
  geom_text(mapping = aes(0.7, 0.7), size = 3, label = "1:1", vjust = -1) +
  ylab("log Area") + xlab("log Population") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  coord_fixed(clip = "off") +
  theme_classic() +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.position = "none"
  )


pop_area_map + inset_element(allom_p, 0, 0.1, 0.25, 0.55) &
  scale_fill_gradientn(
    colors = c("#181C43FF", "#3787BAFF", "#F1ECEBFF", "#BF593BFF", "#3C0912FF"),
    limits = c(0.5, 1.5),
    breaks = c(0.5,  1, 1.5),
    values = rescale(c(0.5, 0.75,  1, 1.25, 1.5))
  ) &
  scale_color_gradientn(
    colors = c("#181C43FF", "#3787BAFF", "#F1ECEBFF", "#BF593BFF", "#3C0912FF"),
    limits = c(0.5, 1.5),
    breaks = c(0.5, 1, 1.5),
    values = rescale(c(0.5, 0.75,  1, 1.25, 1.5))
  ) &
  theme(
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.title = element_text(size = 10, vjust = 0.7)
  )

#ggsave ("map.png",units = "in", width = 8,height = 6,device = "png",dpi = 500)






##########################
#### Fig 2: Scatter plots
##########################

region_scatter <- ggplot(urban, aes(log10(P15), log10(AREA), color = GRGN_L1)) +
  geom_point(stroke = 0.4, shape = 3, size = 0.7) +
  xlab("Population") + ylab("Area") +
  labs(color = "Region") +
  guides(color = guide_legend(keywidth = 0.1, keyheight = 0.1,
                              default.unit = "inch")) +
  theme_bw() +
  theme(legend.position = c(0.24, 0.82),
        legend.text = element_text(size = 5),
        legend.background = element_rect(fill = "white", color = "black",
                                         size = 0.1),
        legend.title = element_blank(),
        legend.spacing.y = unit(2, "pt"),
        legend.key.size = unit(5, "pt"),
        legend.margin=margin(t=-0.05,l=0.02,b=0.02,r=0.02, unit='cm'))

dev_scatter <- ggplot(urban, aes(log10(P15), log10(AREA), color = DEV_CMI)) +
  geom_point(stroke = 0.4, shape = 3, size = 0.7) +
  xlab("Population") + ylab("") +
  theme_classic() + labs(color = "Development Class") +
  guides(color = guide_legend(keywidth = 0.1, keyheight = 0.1,
                              default.unit = "inch")) +
  theme_bw() +
  theme(legend.position = c(0.08, 0.90),
        legend.background = element_rect(fill = "white", color = "black",
                                         size = 0.1),
        legend.title = element_blank(),
        legend.spacing.y = unit(2, "pt"),
        legend.key.size = unit(5, "pt"),
        legend.margin=margin(t=-0.05,l=0.02,b=0.02,r=0.02, unit='cm'),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

region_scatter + dev_scatter &
  scale_x_log10(labels = math_format(10^.x)) &
  scale_y_log10(breaks = c(2, 3),
                labels = math_format(10^.x)) & annotation_logticks(sides = "lb")

#ggsave ("scatter.pdf",units = "in",width = 6.5, height = 2.5, device = "pdf",
#        dpi = 500)







##########################
#### Fig 3: Line graphs
##########################

region_plot <-
  ggplot(area_pop_df, aes(x = Year, y = Exponent, color = Region, group = Country)) +
  geom_abline() +
  geom_line(show.legend = F, alpha = 0.7) +
  geom_point(size = 0.7) +
  theme_bw() +
  theme(legend.position = c(0.7, 0.86),
        legend.text = element_text(size = 5),
        legend.background = element_rect(fill = "white", color = "black", size = 0.1),
        legend.title = element_blank(),
        legend.spacing.y = unit(2, "pt"),
        legend.key.size = unit(5, "pt"),
        legend.margin = margin(t = -0.05,l = 0.02, b = 0.02,r = 0.02, unit = 'cm')) +
  scale_y_continuous(breaks = c(2/3, 0.8, 1, 1.2),
                     labels = c(expression("2/3"), 0.8, 1, 1.2)) +
  guides(color = guide_legend(byrow = TRUE)) +
  ylab(expression(paste("Scaling Exponent ", alpha)))


# Order development for plotting aesthetics
area_pop_df$`Development Class` <- factor(area_pop_df$`Development Class`,
                                          levels = c("LDCL", "LDC", "MDR"))

dev_plot <-ggplot(area_pop_df, aes(x = Year, y = Exponent, group = Country,
                          color = `Development Class`, linetype = `Development Class`)) +
  geom_line(show.legend = T, alpha = 0.7) +
  geom_point(size = 0.7, show.legend = F) +
  theme_bw() +
  theme(legend.position = c(0.94, 0.94),
        legend.text = element_text(size = 5),
        legend.background = element_rect(fill = "white", color = "black", size = 0.1),
        legend.title = element_blank(),
        legend.spacing.y = unit(2, "pt"),
        legend.key.size = unit(5, "pt"),
        legend.margin=margin(t=-0.05,l=0.02,b=0.02,r=0.02, unit='cm'),
        axis.text.y.left = element_blank()) + ylab("") +
  guides(color = guide_legend(byrow = TRUE))  +
  scale_linetype_manual(values = c("LDCL" = "solid", "LDC" = "solid", "MDR" = "dashed"))

region_plot + dev_plot &
  geom_abline(slope = 0, intercept = 1, linetype = "dashed", alpha = 0.7) &
  geom_abline(slope = 0, intercept = 2 / 3, linetype = "dashed", color = "red") &
  scale_x_continuous(breaks = c(1975, 1990, 2000, 2015))

#ggsave("time_plot.pdf",width = 7, height = 3, device = 'pdf',dpi = 600)

sub1 <- area_pop_df[!(area_pop_df$Country %in% c("China", "Pakistan", "India", "France",
                                               "Russia", "Mexico", "Spain", "Brazil",
                                               "Argentina", "Nigeria", "South Africa") |
                      area_pop_df$Region %in% c("Oceania", "Northern America")),]
sub2 <- area_pop_df[area_pop_df$Country %in% c("China", "Pakistan", "India", "France",
                                               "Russia", "Mexico", "Spain", "Brazil",
                                               "Argentina", "Nigeria", "South Africa") |
                      area_pop_df$Region %in% c("Oceania", "Northern America"),]

dodge <- position_dodge(width = 3)
cols = c( "South Africa" = "sienna1", "Nigeria" = "sienna4",  "China" =  "gold",
           "Pakistan" = "gold3", "India" = "gold4", "Russia" = "palegreen",
          "France" = "palegreen3", "Spain" = "olivedrab4", "Argentina" = "turquoise",
          "Brazil" = "mediumspringgreen", "Mexico" = "turquoise4",
          "Canada" = "dodgerblue", "United States" = "steelblue4", "Australia" = "violet")
region_plot2 <- ggplot(sub1, aes(x = Year, y = Exponent, group = Country)) +
  facet_wrap(~ Region, ncol = 2) +
  geom_linerange(aes(ymin = `95% CI lower`, ymax = `95% CI upper`), position = dodge,
                 size = 1, alpha = 0.5, show.legend = F, color = "grey70") +
  geom_line(show.legend = F, position = dodge, color = "grey70") +
  geom_point(show.legend = F, size = 0.7, position = dodge, color = "grey70") +
  geom_line(data = sub2, aes(x = Year, y = Exponent, color = Country),
            position = dodge) +
  geom_point(data = sub2, aes(x = Year, y = Exponent, color = Country),
             show.legend = F, size = 0.7, position = dodge) +
  geom_linerange(data = sub2,
                 aes(ymin = `95% CI lower`, ymax = `95% CI upper`, color = Country),
                 position = dodge,size = 1, alpha = 0.6, show.legend = F) +
  geom_abline(slope = 0, intercept = 1, linetype = "dashed", alpha = 0.7) +
  geom_abline(slope = 0, intercept = 2 / 3, linetype = "dashed", color = "red") +
  scale_y_continuous(breaks = c(0.4, 2 / 3, 1, 1.6),
                     labels = c(0.4, expression("2/3"), 1, 1.6)) +
  coord_cartesian(ylim = c(0.4, 1.8)) +
  theme_bw() +
  theme(legend.direction = "horizontal",
        legend.position = c(0.70, 0.82),
        legend.text = element_text(size = 7),
        legend.spacing.y = unit(2, "pt"),
        legend.key.size = unit(5, "pt")) +
  guides(color = guide_legend(byrow = TRUE)) +
  ylab(expression(paste("Scaling Exponent ", alpha))) +
  scale_x_continuous(breaks = c(1975, 1990, 2000, 2015)) +
  scale_color_manual(values = cols)

region_plot2 / guide_area() +
  plot_layout(guides = "collect", heights = c(0.95, 0.05))

#ggsave("region_plot.pdf",width = 6.5, height = 6, device = 'pdf',dpi = 500)







##########################
#### Fig 4: Show distribution of exponents and elevation
##########################

# Change order of Development class for plotting aesthetics
urban$DEV_CMI <-
  factor(urban$DEV_CMI, levels = c("LDCL", "LDC", "MDR"))

# 2015
p1 <- ggplot(area_pop_df_15, aes(color = `Development Class`)) +
  geom_segment(aes(x = min_x,y = Elevation + (Exponent * min_x), xend = max_x,
                   yend = Elevation + (Exponent * max_x)), show.legend = F) +
  geom_abline(slope = 1, intercept = -4, linetype = "dashed") +
  ggtitle("Year: 2015") +
  ylab(expression("log Area ("~ km ^ 2 ~")")) + xlab("") +
  scale_x_continuous(expand = c(0, 0), limits = c(4.69, 8)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-0.5, 5))
p2 <- ggplot(area_pop_df_15, aes(`Development Class`, Exponent,
                                 fill = `Development Class`)) +
  geom_violin(mapping = aes(color = `Development Class`), alpha = 0.6) +
  geom_boxplot(width = 0.1, show.legend = F) +
  geom_jitter(shape = 1, width = 0.2, size = 0.5, alpha = 0.6,show.legend = F) +
  geom_abline(slope = 0, intercept = 1, linetype = "dashed") +
  geom_abline(slope = 0, intercept = 2 / 3,linetype = "dashed", color = "red") +
  scale_x_discrete(limits = c("LDCL", "LDC", "MDR"),
                   labels = c("Least", "Less", "More")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.5), breaks = c(0, 2 / 3, 1),
                     labels = c(0, expression("2/3"), 1)) +
  xlab("")
p3 <- ggplot(area_pop_df_15, aes(`Development Class`,
                                 Elevation,fill = `Development Class`)) +
  geom_violin(mapping = aes(color = `Development Class`),alpha = 0.6) +
  geom_boxplot(width = 0.1, show.legend = F) +
  geom_jitter(shape = 1,width = 0.2,size = 0.5,alpha = 0.6,show.legend = F) +
  scale_x_discrete(limits = c("LDCL", "LDC", "MDR"),
                   labels = c("Least", "Less", "More")) + xlab("") +
  scale_y_continuous(expand = c(0, 0), limits = c(-7.5, 0))
p123 <- p1 + p2 + p3 + plot_layout(tag_level = "new")  &
  theme(legend.position = "top", legend.direction = "horizontal")

# 2000
p4 <- ggplot(area_pop_df_00, aes(color = `Development Class`)) +
  geom_segment(aes(x = min_x, y = Elevation + (Exponent * min_x), xend = max_x,
                   yend = Elevation + (Exponent * max_x)), show.legend = F) +
  geom_abline(slope = 1, intercept = -4, linetype = "dashed") +
  ggtitle("Year: 2000") +
  ylab(expression("log Area (" ~ km ^ 2 ~ ")")) + xlab("") +
  scale_x_continuous(expand = c(0, 0), limits = c(4.69, 8)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-0.5, 5))
p5 <- ggplot(area_pop_df_00,aes(`Development Class`, Exponent,
                                fill = `Development Class`))  +
  geom_violin(mapping = aes(color = `Development Class`),alpha = 0.6) +
  geom_boxplot(width = 0.1, show.legend = F) +
  geom_abline(slope = 0,intercept = 1,linetype = "dashed") +
  geom_abline(slope = 0,intercept = 2 / 3,linetype = "dashed",color = "red") +
  geom_jitter(shape = 1,width = 0.2,size = 0.5,alpha = 0.6,show.legend = F) +
  scale_x_discrete(limits = c("LDCL", "LDC", "MDR"),
                   labels = c("Least", "Less", "More")) + xlab("") +
  scale_y_continuous(expand = c(0, 0),limits = c(0, 1.5),breaks = c(0, 2 / 3, 1),
                     labels = c(0, expression("2/3"), 1))
p6 <- ggplot(area_pop_df_00,aes(`Development Class`, Elevation,
                                fill = `Development Class`)) +
  geom_violin(mapping = aes(color = `Development Class`), alpha = 0.6) +
  geom_boxplot(width = 0.1, show.legend = F) +
  geom_jitter(shape = 1, width = 0.2, size = 0.5, alpha = 0.6, show.legend = F) +
  scale_x_discrete(limits = c("LDCL", "LDC", "MDR"),
                   labels = c("Least", "Less", "More")) + xlab("") +
  scale_y_continuous(expand = c(0, 0), limits = c(-7.5, 0))
p456 <- p4 + p5 + p6 + plot_layout(tag_level = "new")  &
  theme(legend.position = "top", legend.direction = "horizontal")

# 1990
p7 <- ggplot(area_pop_df_90, aes(color = `Development Class`)) +
  geom_segment(aes(x = min_x,y = Elevation + (Exponent * min_x), xend = max_x,
                   yend = Elevation + (Exponent * max_x)),show.legend = F) +
  geom_abline(slope = 1,intercept = -4,linetype = "dashed") +
  ggtitle("Year: 1990") +
  ylab(expression("log Area (" ~ km ^ 2 ~ ")")) + xlab("") +
  scale_x_continuous(expand = c(0, 0), limits = c(4.69, 8)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-0.5, 5))
p8 <- ggplot(area_pop_df_90,aes(`Development Class`, Exponent,
                                fill = `Development Class`)) +
  geom_violin(mapping = aes(color = `Development Class`),alpha = 0.6) +
  geom_boxplot(width = 0.1, show.legend = F) +
  geom_jitter(shape = 1,width = 0.2,size = 0.5,alpha = 0.6,show.legend = F) +
  geom_abline(slope = 0,intercept = 1,linetype = "dashed") +
  geom_abline(slope = 0,intercept = 2 / 3,linetype = "dashed",color = "red") +
  scale_x_discrete(limits = c("LDCL", "LDC", "MDR"),
                   labels = c("Least", "Less", "More")) + xlab("") +
  scale_y_continuous(expand = c(0, 0),limits = c(0, 1.5),breaks = c(0, 2 / 3, 1),
                     labels = c(0, expression("2/3"), 1))
p9 <- ggplot(area_pop_df_90, aes(`Development Class`, Elevation, fill = `Development Class`)) +
  geom_violin(mapping = aes(color = `Development Class`),alpha = 0.6) +
  geom_boxplot(width = 0.1, show.legend = F) +
  geom_jitter(shape = 1,width = 0.2,size = 0.5,alpha = 0.6,show.legend = F) +
  scale_x_discrete(limits = c("LDCL", "LDC", "MDR"),
                   labels = c("Least", "Less", "More")) + xlab("") +
  scale_y_continuous(expand = c(0, 0), limits = c(-7.5, 0))
p789 <- p7 + p8 + p9 + plot_layout(tag_level = "new") &
  theme(legend.position = "top", legend.direction = "horizontal")

# 1975
p10 <- ggplot(area_pop_df_75, aes(color = `Development Class`)) +
  geom_segment(aes(x = min_x,y = Elevation + (Exponent * min_x),xend = max_x,
                   yend = Elevation + (Exponent * max_x)),show.legend = F) +
  geom_abline(slope = 1,intercept = -4,linetype = "dashed") +
  ggtitle("Year: 1975") +
  ylab(expression("log Area (" ~ km ^ 2 ~ ")")) + xlab("log Population") +
  scale_x_continuous(expand = c(0, 0), limits = c(4.69, 8)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-0.5, 5))
p11 <- ggplot(area_pop_df_75, aes(`Development Class`, Exponent,
                                  fill = `Development Class`)) +
  geom_violin(mapping = aes(color = `Development Class`), alpha = 0.6) +
  geom_boxplot(width = 0.1, show.legend = F) +
  geom_abline(slope = 0,intercept = 1,linetype = "dashed") +
  geom_abline(slope = 0,intercept = 2 / 3,linetype = "dashed",color = "red") +
  geom_jitter(shape = 1,width = 0.2,size = 0.5,alpha = 0.6,show.legend = F) +
  scale_x_discrete(limits = c("LDCL", "LDC", "MDR"),
                   labels = c("Least", "Less", "More")) +
  xlab("Development Class") +
  scale_y_continuous(expand = c(0, 0),limits = c(0, 1.5),breaks = c(0, 2 / 3, 1),
                     labels = c(0, expression("2/3"), 1))
p12 <- ggplot(area_pop_df_75,aes(`Development Class`, Elevation,
                  fill = `Development Class`)) +
  geom_violin(mapping = aes(color = `Development Class`),alpha = 0.6) +
  geom_boxplot(width = 0.1, show.legend = F) +
  geom_jitter(shape = 1,width = 0.2,size = 0.5,alpha = 0.6,show.legend = F) +
  scale_x_discrete(limits = c("LDCL", "LDC", "MDR"),
                   labels = c("Least", "Less", "More")) +
  scale_y_continuous(expand = c(0, 0), limits = c(-7.5, 0))
p10_11_12 <- p10 + p11 + p12 +
  plot_layout(tag_level = "new") &
  theme(legend.position = "top", legend.direction = "horizontal") +
  xlab("log Population")

patch <- guide_area() / p123 / p456 / p789 / p10_11_12 +
  plot_layout(guides = "collect", height = c(0.2, 1, 1, 1, 1)) &
  scale_color_discrete(limits = c("LDCL", "LDC", "MDR"),
                       labels = c("Least", "Less", "More")) &
  theme_bw() &
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.key.size = unit(0.7, "line"),
        plot.tag.position = c(0, 1),
        plot.tag = element_text(size = 10, hjust = 0, vjust = 0,
                                margin = margin(b = 3)),
        axis.title = element_text(size = 10),
        plot.margin = margin(t = 5, l = 5),
        plot.title = element_text(size = 10, face = "bold"),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA)) &
  scale_fill_discrete(limits = c("LDCL", "LDC", "MDR"),
                      labels = c("Least", "Less", "More")) &
  coord_cartesian(clip = "off")

#ggsave("patch", width = 7, height = 9.5, device='png', dpi = 500)
### uncomment to save to figure locally





summary(aov(log10(AREA) ~ log10(P15) * DEV_CMI, data = urban))


