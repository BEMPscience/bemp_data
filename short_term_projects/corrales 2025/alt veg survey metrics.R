
# Site×Transect community matrix
comm <- veg_clean %>%
  group_by(Site, Transect, Species) %>%
  summarise(cover = sum(difference, na.rm = TRUE), .groups = "drop") %>%
  filter(cover > 0) %>%
  mutate(line = paste(Site, Transect, sep = "_")) %>%
  select(line, Species, cover) %>%
  pivot_wider(names_from = Species, values_from = cover, values_fill = 0)

mat <- as.matrix(dplyr::select(comm, -line))
rownames(mat) <- comm$line

# Hill numbers & diversity profiles
library(vegan)
hill <- function(x, q) if (q == 1) exp(diversity(x, "shannon")) else (sum((x/sum(x))^q))^(1/(1-q))
hill_profiles <- apply(mat, 1, function(x) sapply(c(0, 0.5, 1, 2, 3), hill, x = x))
t(hill_profiles)  # rows = lines, cols = q

ra <- as.data.frame(mat) |>
  tibble::rownames_to_column("line") |>
  tidyr::pivot_longer(-line, names_to="sp", values_to="cover") |>
  group_by(line) |>
  mutate(rank = rank(-cover, ties.method="first"),
         p = cover/sum(cover)) |>
  filter(p > 0)

ggplot(ra, aes(rank, p, group=line)) + geom_line(alpha=.2) + scale_y_log10()

#
# mean_patches_per_sp = on average, how many separate intercept segments each species forms along that line.
# Higher ⇒ more fragmented / interwoven community (lots of starts/stops per species).
# Lower ⇒ species occur in fewer, more continuous chunks.

# mean_seg_len (in meters) = the average length of a contiguous segment (per species, then averaged across species).
# Larger ⇒ coarser, clumpier structure (long runs of the same species).
# Smaller ⇒ fine-grained mosaic (short, broken segments).

# Patchiness per line (Site × Transect)
patch_stats <- veg_clean %>%
  mutate(seg_len = End - Start) %>%
  # If you have multiple surveys and want *per-date* stats, add Date below:
  # group_by(Site, Transect, Date, Species)
  group_by(Site, Transect, Species) %>%
  summarise(
    patches = n(),                  # number of segments for this species on this line
    total   = sum(seg_len),         # total covered length for this species on this line
    .groups = "drop_last"
  ) %>%
  mutate(mean_seg_len_sp = total / patches) %>%   # species-level mean segment length
  group_by(Site, Transect) %>%
  summarise(
    mean_patches_per_sp     = mean(patches),
    mean_seg_len_unweighted = mean(mean_seg_len_sp),
    mean_seg_len_weighted   = weighted.mean(mean_seg_len_sp, w = total),  # emphasizes dominants
    total_patches           = sum(patches),
    total_cover             = sum(total),
    .groups = "drop"
  ) %>%
  # line length for standardization
  left_join(
    veg_clean %>%
      group_by(Site, Transect) %>%
      summarise(line_length = max(End, na.rm = TRUE) - min(Start, na.rm = TRUE),
                .groups = "drop"),
    by = c("Site","Transect")
  ) %>%
  mutate(
    patches_per_10m = (total_patches / line_length) * 10,
    seg_len_rel     = mean_seg_len_weighted / line_length
  )

patch_stats %>% ggplot(., aes(x=Site, y=mean_patches_per_sp)) + geom_boxplot()

write_csv(patch_stats, )

# Segment length (weighted) by site
ggplot(patch_stats,
       aes(x = reorder(Site, mean_seg_len_weighted, FUN = median),
           y = mean_seg_len_weighted)) +
  geom_boxplot() +
  coord_flip() +
  labs(x = "Site", y = "Mean segment length (m, weighted)",
       title = "Patch size by site")

# Fragmentation (patches per 10 m) by site
ggplot(patch_stats,
       aes(x = reorder(Site, patches_per_10m, FUN = median),
           y = patches_per_10m)) +
  geom_boxplot() +
  coord_flip() +
  labs(x = "Site", y = "Patches per 10 m",
       title = "Fragmentation by site")

#
# Upper-left (many patches, short segments) = highly fragmented mosaic.
# Lower-right (few patches, long segments) = clumped/banded stands.
patch_stats %>% ggplot(., aes(mean_patches_per_sp, y=mean_seg_len_weighted)) +
  geom_point()


