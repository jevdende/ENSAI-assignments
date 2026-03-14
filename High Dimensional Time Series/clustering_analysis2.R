############################################################
# Cluster profile analysis for ONE chosen clustering
# - Loads airbox_stl_deseasonalized.rds
# - Uses ONE membership vector 'memb' (length = ncol(X_adj))
# - Rebuilds df_long from scratch (prevents stale workspace issues)
# Outputs:
# 1) Cluster sizes + summary stats
# 2) Mean time series per cluster (±1 SD band)
# 3) Mean 24h profile per cluster
# 4) Mean ACF per cluster (±1 SD band across sensors)
# 5) Taiwan map colored by cluster
############################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(lubridate)
  library(maps)
})

# -----------------------------
# 0) Load data
# -----------------------------
obj <- readRDS("airbox_stl_deseasonalized.rds")
X_adj <- obj$X_adj
dt <- obj$datetime
sensor_table <- obj$sensor_table

stopifnot(is.matrix(X_adj), length(dt) == nrow(X_adj))

obj <- readRDS("airbox_clusters.rds")

km1_sil = obj$km1_sil
km1_ch = obj$km1_ch
km2_sil = obj$km2_sil
km2_ch = obj$km2_ch
hc3 = obj$hc3
hc4 = obj$hc4
hc5 = obj$hc5
hc6 = obj$hc6
hc7 = obj$hc7
# -----------------------------
# 1) Choose ONE clustering membership vector
# -----------------------------

memb <- hc5$memb

# 1) K-means on ACF coefficients
method_1_name <- "K-means on ACF features (Silhouette)"
# method_1_name <- "K-means on ACF features (CH)"

# 2) K-means on time-series vectors (Euclidean)
method_2_name <- "K-means on time series — Euclidean (Silhouette)"
# method_2_name <- "K-means on time series — Euclidean, CH"

# 3) HC on ACF FEATURE distance (Euclidean on ACF coeffs)
method_3_name <- "HC on ACF features (Silhouette)"

# 4) HC on GCC distance
method_4_name <- "HC on GCC dissimilarity (Silhouette)"

# 5) HC on CC distance
method_5_name <- "HC on CC dissimilarity (Silhouette)"

# 6) HC on AR distance (AR.PIC)
method_6_name <- "HC on AR.PIC distance (Silhouette)"

# 7) HC on ACF DISSIMILARITY (TSclust)
method_7_name <- "HC on ACF dissimilarity (Silhouette)"

cluster_method_name <- method_5_name

stopifnot(length(memb) == ncol(X_adj))


clusters <- factor(memb)
Kc <- nlevels(clusters)
cat("Number of clusters:", Kc, "\n")

# -----------------------------
# 2) Sensor IDs and metadata joined with clusters
# -----------------------------
cn <- colnames(X_adj)
sensor_id <- suppressWarnings(as.integer(sub("^S", "", cn)))
if (anyNA(sensor_id)) stop("Could not parse sensor_id from colnames(X_adj). Expect names like 'S123'.")

sensor_meta <- sensor_table %>%
  left_join(data.frame(sensor_id = sensor_id, cluster = clusters), by = "sensor_id") %>%
  filter(!is.na(cluster))

cat("\nCluster sizes (all sensors):\n")
print(table(clusters))

cat("\nCluster sizes (only sensors with coords):\n")
print(table(sensor_meta$cluster))

# -----------------------------
# 3) Build long data for all time/profile plots
# -----------------------------
df_long <- as.data.frame(X_adj)
df_long$datetime <- dt

df_long <- df_long %>%
  pivot_longer(cols = all_of(cn), names_to = "sensor_name", values_to = "value") %>%
  mutate(
    cluster = clusters[match(sensor_name, cn)],
    hour = lubridate::hour(datetime)
  )

# sanity check: ensure all clusters appear in df_long
cat("\nClusters present in df_long:\n")
print(table(df_long$cluster, useNA = "ifany"))

# -----------------------------
# 4) Summary statistics per sensor + cluster summaries
# -----------------------------
h_acf <- 50

sensor_stats <- data.frame(
  sensor_name = cn,
  sensor_id   = sensor_id,
  cluster     = clusters,
  mean        = apply(X_adj, 2, mean),
  sd          = apply(X_adj, 2, sd),
  iqr         = apply(X_adj, 2, IQR)
)

acf_lags <- t(sapply(seq_len(ncol(X_adj)), function(j) {
  as.numeric(stats::acf(X_adj[, j], lag.max = h_acf, plot = FALSE)$acf[2:(h_acf + 1)])
}))
colnames(acf_lags) <- paste0("acf", 1:h_acf)

sensor_stats <- cbind(sensor_stats, acf_lags)

cluster_summary <- sensor_stats %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    mean_mean = mean(mean),
    mean_sd   = mean(sd),
    mean_iqr  = mean(iqr),
    .groups = "drop"
  )

cat("\nCluster summaries:\n")
print(cluster_summary)

# -----------------------------
# 5) Mean time series per cluster (THIS is the 'first plot')
# -----------------------------
ts_cluster <- df_long %>%
  group_by(cluster, datetime) %>%
  summarise(mean = mean(value), sd = sd(value), .groups = "drop")

p_ts <- ggplot(ts_cluster, aes(x = datetime, y = mean, color = cluster)) +
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, fill = cluster),
              alpha = 0.15, color = NA, show.legend = FALSE) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Cluster mean time series",
    subtitle = "Band: ±1 SD across sensors within cluster",
    x = NULL, y = "z-score", color = "Cluster"
  ) +
  theme_minimal()

print(p_ts)

# -----------------------------
# 6) Mean 24h profile per cluster
# -----------------------------
prof24 <- df_long %>%
  group_by(cluster, hour) %>%
  summarise(mean = mean(value), sd = sd(value), .groups = "drop")

p_24h <- ggplot(prof24, aes(x = hour, y = mean, color = cluster)) +
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, fill = cluster),
              alpha = 0.15, color = NA, show.legend = FALSE) +
  geom_line(linewidth = 0.9) +
  scale_x_continuous(breaks = seq(0, 23, by = 3)) +
  labs(
    title = "Mean 24h profile by cluster",
    x = "Hour of day", y = "z-score", color = "Cluster"
  ) +
  theme_minimal()

print(p_24h)

# -----------------------------
# 7) Mean ACF per cluster
# -----------------------------
acf_by_sensor <- data.frame(sensor_name = cn, cluster = clusters) %>%
  bind_cols(as.data.frame(acf_lags)) %>%
  pivot_longer(cols = starts_with("acf"), names_to = "lag", values_to = "acf") %>%
  mutate(lag = as.integer(sub("^acf", "", lag)))

acf_cluster <- acf_by_sensor %>%
  group_by(cluster, lag) %>%
  summarise(mean = mean(acf), sd = sd(acf), .groups = "drop")

p_acf <- ggplot(acf_cluster, aes(x = lag, y = mean, color = cluster, group = cluster)) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, fill = cluster),
              alpha = 0.15, color = NA, show.legend = FALSE) +
  geom_line(linewidth = 0.9) +
  scale_x_continuous(breaks = seq(1, h_acf, by = 5)) +
  labs(
    title = "Mean ACF by cluster",
    subtitle = paste0("ACF on time series; lags 1..", h_acf),
    x = "Lag", y = "ACF", color = "Cluster"
  ) +
  theme_minimal()

print(p_acf)

# -----------------------------
# 8) Taiwan map colored by cluster
# -----------------------------
# -----------------------------
# Clustering methods (KEEP ORDER FIXED)
# -----------------------------
methods <- list(
  list(
    memb = km1_sil$km$cluster,
    name = "K-means on ACF features (Silhouette)",
    file = "km_acf_sil.png"
  ),
  list(
    memb = km1_ch$km$cluster,
    name = "K-means on ACF features (CH)",
    file = "km_acf_ch.png"
  ),
  list(
    memb = km2_sil$km$cluster,
    name = "K-means Euclidean (Silhouette)",
    file = "km_ts_eucl_sil.png"
  ),
  list(
    memb = km2_ch$km$cluster,
    name = "K-means Euclidean (CH)",
    file = "km_ts_eucl_ch.png"
  ),
  list(
    memb = hc3$memb,
    name = "HC on ACF features (Silhouette)",
    file = "hc_acf_features.png"
  ),
  list(
    memb = hc4$memb,
    name = "HC on GCC dissimilarity (Silhouette)",
    file = "hc_gcc.png"
  ),
  list(
    memb = hc5$memb,
    name = "HC on CC dissimilarity (Silhouette)",
    file = "hc_cc.png"
  ),
  list(
    memb = hc6$memb,
    name = "HC on AR distance (Silhouette)",
    file = "hc_arpic.png"
  ),
  list(
    memb = hc7$memb,
    name = "HC on determinant (Silhouette)",
    file = "hc_acf_dissim.png"
  )
)

# -----------------------------
# Taiwan maps for ALL clustering methods
# -----------------------------
world_map  <- map_data("world")
taiwan_map <- world_map %>% filter(region == "Taiwan")
out_dir <- "plots_clustering"
for (m in methods) {
  
  memb <- m$memb
  stopifnot(length(memb) == ncol(X_adj))
  
  clusters <- factor(memb)
  
  sensor_meta <- sensor_table %>%
    left_join(
      data.frame(
        sensor_id = as.integer(sub("^S", "", colnames(X_adj))),
        cluster = clusters
      ),
      by = "sensor_id"
    ) %>%
    filter(!is.na(cluster))
  
  p_map <- ggplot() +
    geom_polygon(
      data = taiwan_map,
      aes(long, lat, group = group),
      fill = "grey95", color = "grey40", linewidth = 0.3
    ) +
    geom_point(
      data = sensor_meta,
      aes(x = longitude, y = latitude, color = cluster),
      size = 1.8, alpha = 0.85
    ) +
    coord_quickmap(xlim = c(119.5, 122), ylim = c(21.5, 25.5)) +
    labs(
      title = paste0(m$name),
      x = "Longitude",
      y = "Latitude",
      color = "Cluster"
    ) +
    theme_minimal()
  
  ggsave(
    filename = file.path(out_dir, m$file),
    plot = p_map,
    width = 4,
    height = 6,
    dpi = 300
  )
}

