library(tidyverse)
library(sf)
library(patchwork)
library(mapview)
library(mgcv)
library(gratia)
library(gt)


#### old layers -------------------
fp <- "~/T/DFW-Team WDFW Watershed Synthesis - data_common/rc/Confinement-Confinement_for_Salt_Creek-Frontal_Strait_of_Juan_De_Fuca/outputs/confinement.gpkg"
lyr <- st_layers(fp) |> as_tibble()
z <- map(lyr$name, ~ st_read(fp, layer = .x)) |>
  set_names(lyr$name)

ggplot() +
  # geom_sf(data = z$confining_margins)
  geom_sf(data = z$confinement_ratio)

mapview::mapview(z$confinement_ratio, zcol = "constriction_ratio")

fp <- "~/T/DFW-Team WDFW Watershed Synthesis - data_common/rc/Confinement-Confinement_for_Salt_Creek-Frontal_Strait_of_Juan_De_Fuca/intermediates/confinement_intermediates.gpkg"
lyr <- st_layers(fp) |> as_tibble()
z2 <- st_read(fp, layer = "confinement_zones")
mapview::mapview(z2, zcol = "level_path")

fp <- "~/T/DFW-Team WDFW Watershed Synthesis - data_common/rc/Confinement-Confinement_for_Salt_Creek-Frontal_Strait_of_Juan_De_Fuca/inputs/inputs.gpkg"
lyr <- st_layers(fp) |> as_tibble()
z2 <- map(lyr$name, ~ st_read(fp, layer = .x)) |>
  set_names(lyr$name)

ggplot(z2$channel_area) + geom_sf(aes(fill = "bankfull_m"))
mapview::mapview(z2$channel_area, zcol = "bankfull_m")
mapview::mapview(z2$confining_polygon)


fp <- "~/T/DFW-Team WDFW Watershed Synthesis - data_common/rc/VBET-Valley_Bottom_for_Salt_Creek-Frontal_Strait_of_Juan_De_Fuca/inputs/vbet_inputs.gpkg"
fp <- "~/T/DFW-Team WDFW Watershed Synthesis - data_common/rc/VBET-Valley_Bottom_for_Salt_Creek-Frontal_Strait_of_Juan_De_Fuca/outputs/vbet.gpkg"
(lyr <- st_layers(fp) |> as_tibble())
z <- map(lyr$name, ~ st_read(fp, layer = .x)) |>
  set_names(lyr$name)

ggplot() +
  geom_sf(data = z$vbet_full) +
  geom_sf(data = z$vbet_igos, aes(color = active_channel_proportion)) +
  theme_void()
mapview::mapview(z$vbet_full)


#### metric engine + existing imw objects -----------------

load(
  "~/T/DFW-Team WDFW Watershed Synthesis - IMW analyses/imw_hab_251203.RData"
)

sf_h12_cat <- readRDS(
  "~/T/DFW-Team WDFW Watershed Synthesis - General/fifo/sf_nhdphr_h12_imw_nhdp_cat.rds"
)

## straits "salt creek frontal"
# https://data.riverscapes.net/p/1b5436f2-a04b-4f0d-ba04-fbfdb636919e/
## lc "germany frontal"
# https://data.riverscapes.net/p/0deaeb5d-c6cc-47bb-b5dc-37bd9d754932/

fp <- "~/Downloads/riverscapes_metrics_lc.gpkg"
(lyr <- st_layers(fp) |> as_tibble())
z <- map(lyr$name, ~ st_read(fp, layer = .x)) |>
  set_names(lyr$name)
# 15K polys with bunch of attribs
z$vw_dgo_geomorph_metrics
z$metrics
z$measurements
z$vw_dgo_hydro

# can prob create combo dgo object for efficiency
rme_geo <- st_transform(z$vw_dgo_geomorph_metrics, st_crs(sf_h12_cat))[
  sf_h12_cat,
]
rme_hyd <- st_transform(z$vw_dgo_hydro_metrics, st_crs(sf_h12_cat))[
  sf_h12_cat,
]
rme_met <- st_transform(z$vw_dgo_metrics, st_crs(sf_h12_cat))[
  sf_h12_cat,
]

#### 3 attrib fig emailed fig 6/30/26 --------------------
list(
  a = ggplot(rme_geo) +
    geom_sf(aes(fill = prim_channel_gradient, color = prim_channel_gradient)) +
    scale_fill_viridis_b(
      option = "inferno",
      breaks = quantile(
        rme_geo$prim_channel_gradient,
        p = seq(0, 1, by = 0.25),
        na.rm = T
      ),
      labels = round(
        quantile(
          rme_geo$prim_channel_gradient,
          p = seq(0, 1, by = 0.25),
          na.rm = T
        ),
        2
      ),
      aesthetics = c("fill", "color")
    ) +
    theme_void(),
  b = ggplot(rme_geo) +
    geom_sf(aes(fill = channel_width, color = channel_width)) +
    scale_fill_viridis_b(
      option = "mako",
      direction = -1,
      breaks = quantile(
        rme_geo$channel_width,
        p = seq(0, 1, by = 0.25),
        na.rm = T
      ),
      labels = round(
        quantile(rme_geo$channel_width, p = seq(0, 1, by = 0.25), na.rm = T),
        2
      ),
      aesthetics = c("fill", "color")
    ) +
    theme_void(),
  c = ggplot(rme_geo) +
    geom_sf(aes(fill = active_channel_ratio, color = active_channel_ratio)) +
    scale_fill_viridis_b(
      option = "turbo",
      direction = -1,
      breaks = quantile(
        rme_geo$active_channel_ratio,
        p = seq(0, 1, by = 0.25),
        na.rm = T
      ),
      labels = round(
        quantile(
          rme_geo$active_channel_ratio,
          p = seq(0, 1, by = 0.25),
          na.rm = T
        ),
        2
      ),
      aesthetics = c("fill", "color")
    ) +
    theme_void()
) |>
  wrap_plots(nrow = 1)

#### basic gam test -----------------------

# single basin, sd

b_site <- sf_site_meta |>
  filter(str_detect(strm, "Mill"))
b_cat <- sf_h12_cat |>
  filter(str_detect(huc12_nm, "Mill"))
# b_rme_geo <- rme_geo[b_cat, ] # mapview(b_rme)
# b_rme_hyd <- rme_hyd[b_cat, ] # mapview(b_rme)
b_rme <- rme_met[b_cat, ] # mapview(b_rme)

b <- st_join(
  b_site |>
    select(
      cmplx_strm,
      site,
      starts_with("year"),
      ends_with("mean"),
      ends_with("sd"),
      -contains("per100"),
      -contains("vol")
    ),
  # b_rme_geo,
  b_rme,
  join = st_intersects,
  largest = T
) |>
  # st_join(
  #   b_rme_hyd |> select(qlow, q2, splow, sphigh),
  #   join = st_intersects,
  #   largest = T
  # ) |>
  mutate(pct_pools_cv = pct_pools_sd / pct_pools_mean)


# sites MIL004 and MIL025 do not intersect a dgo polygon
# could 'nearest' the join
glimpse(b)
summary(as_tibble(b))
as_tibble(b) |> filter(is.na(prim_channel_gradient))
mapview(b_rme) + mapview(b_site)
mapview(b, zcol = "planform_sinuosity")

# if adding flow covariate to time-summarized response, use time-summarized predictor
# different response hab measures warrant different families
# but going to CV allows comparison across different scales?
# mgcv has betar, but Gamma() prob the way to go?

as_tibble(b) |>
  # select(site, ends_with("sd")) |>
  select(site, ends_with("cv")) |>
  pivot_longer(-site) |>
  ggplot() +
  geom_density(aes(value)) +
  facet_wrap(~name, scales = "free")

as_tibble(b) |>
  drop_na(prim_channel_gradient) |>
  select(
    pct_pools_sd,
    pct_pools_mean,
    pct_pools_cv,
    # prim_channel_gradient:rel_flow_length,
    # planform_sinuosity,
    # channel_area:confinement_ratio
    qlow:sphigh
  ) |>
  # summary()
  GGally::ggpairs()

# keeping in mind that strength of linear correlation does not necessarily imply lack of nonlinear reln
# CV neg correlated with mean but not SD, more pools on avg were less variable thru time
as_tibble(b) |>
  drop_na(prim_channel_gradient) |>
  select(
    pct_pools_sd,
    pct_pools_cv,
    prim_channel_gradient:rel_flow_length,
    planform_sinuosity,
    channel_area:confinement_ratio,
    qlow:sphigh
  ) |>
  corrr::correlate() |>
  corrr::focus(c(pct_pools_sd, pct_pools_cv)) |>
  arrange(pct_pools_sd)

# # quick guassian first looks
# # helpful to see relative lack of fit and signif from linear non-Gamma
# gam(
#   #pct_pools_sd ~ prim_channel_gradient + hect_vb_per_km + low_lying_ratio + q2 + sphigh,
#   pct_pools_cv ~ prim_channel_gradient + hect_vb_per_km + low_lying_ratio + q2 + sphigh,
#   data = b
# ) |>
#   summary()
# # versus, e.g.,
# gam(
#   pct_pools_sd ~ s(prim_channel_gradient) +
#     s(hect_vb_per_km) +
#     s(low_lying_ratio),
#   data = b,
#   family = Gamma()
# ) |>
#   summary()

# lots more needed re: response family and scaling of predictors
y <- "pct_pools_cv"
x <- c(
  "prim_channel_gradient",
  "hect_vb_per_km",
  "low_lying_ratio",
  "q2",
  "sphigh"
)

glimpse(z$metrics)
z$metrics |>
  select(field_name, description) |>
  #filter(str_detect(field_name, "gradient|vb_per|ratio|q2|sphigh"))
  filter(field_name %in% x) |>
  gt(caption = "RC RME candidate covariates")

# default Gamma() link is 'inverse', maybe want log? [YES - fixed negative/very positive preds, and better aic/r2]
bmod <- tibble(
  form = unlist(
    #lapply(seq_along(v), function(i) combn(v, i, simplify = FALSE)),
    map(1:3, \(i) combn(x, i, simplify = F)),
    recursive = FALSE
  ) |>
    map(~ paste(y, "~ s(", paste(.x, collapse = ") + s("), ")"))
) |>
  mutate(
    # works, but gives negative pred on response scale:   fit = map(form, ~ gam(as.formula(.x), data = b, family = Gamma())),
    fit = map(
      form,
      ~ gam(as.formula(.x), data = b, family = Gamma(link = "log"))
    ),
    devexpl = map_dbl(fit, ~ summary(.x)[["dev.expl"]]),
    glance = map(fit, ~ broom::glance(.x))
  ) |>
  unnest(form) |>
  unnest(glance) |>
  arrange(desc(devexpl), AIC)

bmod |>
  select(form, devexpl, AIC, adj.r.squared) |>
  mutate(form = str_remove(form, y)) |>
  gt(
    caption = md(
      paste(
        "Model ranks for GAMs of y =",
        y,
        "<br>Mill Creek sites relative to RC RME covariates<br>gamma response family"
      )
    )
  ) |>
  fmt_percent("devexpl", decimals = 1) |>
  fmt_number("AIC", decimals = 1) |>
  fmt_number("adj.r.squared", decimals = 2)

summary(bmod$fit[[1]])
gam.check(bmod$fit[[1]])
appraise(bmod$fit[[1]])
# nice, keep in mind on the link scale, adding type = "response" seems no effect
draw(bmod$fit[[1]], residuals = T)
# interesting, appears to be some wacky stuff going on with some covars
# may need to pre-scale covars
draw(
  bmod$fit[[1]],
  constant = model_constant(bmod$fit[[1]]), # Adds the model intercept
  fun = inv_link(bmod$fit[[1]]),
  ncol = 1
)

# [when using Gamma(link = 'inverse') default] how are negative and ~5000K CV values being generated from full set of DGOs?
# fixed with log link
bmod$fit[[1]] |>
  #  predict(type = "response")
  predict(type = "response", newdata = as_tibble(b_rme)) |>
  summary()
# pairs plots help show/suggest no simple linear univariate reln
# which is nice except makes it hard to account for where/why getting negatives and extremes
# [BUT switch to log link helps make clearer that model sees higher var at steeper/narrower/higher power]
b |>
  as_tibble() |>
  drop_na(prim_channel_gradient) |>
  mutate(pred_m1 = predict(bmod$fit[[1]], type = "response")) |>
  arrange(desc(pred_m1)) |>
  select(all_of(x), all_of(y), pred_m1) |>
  #print(n = Inf)
  GGally::ggpairs()

dgopred <- b_rme |>
  mutate(
    pred_m1 = predict(
      bmod$fit[[1]],
      newdata = as_tibble(b_rme),
      type = "response"
    )
  ) |>
  drop_na(pred_m1)

# # [from default/wrong inverse link] positive extremes appear to be 'headwater artifacts'? negative are more widely distributed
# list(
#   dgopred |>
#     select(all_of(x), pred_m1) |>
#     # as_tibble() |> arrange(desc(pred_m1))
#     # as_tibble() |> arrange(pred_m1)
#     filter(pred_m1 < 0) |>
#     ggplot() +
#     geom_sf(aes(fill = pred_m1)) +
#     scale_color_viridis_b(
#       option = "cividis",
#       direction = -1,
#       aesthetics = c("color", "fill")
#     ) +
#     theme_void(),
#   dgopred |>
#     select(all_of(x), pred_m1) |>
#     filter(pred_m1 > 5) |>
#     ggplot() +
#     geom_sf(aes(fill = pred_m1)) +
#     scale_color_viridis_b(
#       option = "cividis",
#       direction = 1,
#       aesthetics = c("color", "fill")
#     ) +
#     theme_void()
# ) |>
#   wrap_plots(nrow = 1)

# mapped predict()
dgopred |>
  ggplot() +
  geom_sf(aes(fill = pred_m1, color = pred_m1)) +
  scale_color_viridis_b(
    option = "plasma",
    aesthetics = c("color", "fill"),
    breaks = quantile(dgopred$pred_m1, seq(0, 1, by = 0.25)),
    labels = round(quantile(dgopred$pred_m1, seq(0, 1, by = 0.25), 2))
  ) +
  theme_void() +
  labs(
    title = "Predicted CV %pools",
    subtitle = paste(
      str_squish(paste0(format(bmod$fit[[1]]$formula), collapse = "")),
      paste(
        paste0(
          "Dev. expl.: ",
          scales::percent(summary(bmod$fit[[1]])[["dev.expl"]], 2)
        ),
        paste0("R^2: ", scales::percent(summary(bmod$fit[[1]])[["r.sq"]], 2)),
        sep = "; "
      ),
      sep = "\n"
    )
  )

# # mapped residuals

# so the "fit$residuals" slot appears to hold the "working" residuals
# which are not what to use
bind_cols(
  # b = predict(bmod$fit[[1]], type = "response"),
  # c = predict(
  #   bmod$fit[[1]],
  #   newdata = drop_na(b, prim_channel_gradient),
  #   type = "response"
  # ),
  est = bmod$fit[[1]]$fitted.values,
  obs = b |> drop_na(prim_channel_gradient) |> pull(pct_pools_cv),
  e = bmod$fit[[1]]$residuals,
  f = residuals(bmod$fit[[1]]),
  g = residuals(bmod$fit[[1]], type = "deviance"),
  h = residuals(bmod$fit[[1]], type = "response"),
  i = residuals(bmod$fit[[1]], type = "pearson"),
  j = residuals(bmod$fit[[1]], type = "working")
) |>
  mutate(
    estl = log(est),
    obsl = log(obs)
  ) |>
  print(n = 12)

set_theme(theme_minimal())

list(
  appraise(bmod$fit[[1]], method = "simulate"),
  b |>
    drop_na(prim_channel_gradient) |>
    mutate(
      # on response scale
      m1_fit = bmod$fit[[1]]$fitted.values,
      # on log link scale!
      m1_res = residuals(bmod$fit[[1]])
    ) |>
    select(all_of(x), all_of(y), contains("m1")) |>
    #mapview::mapview(zcol = "m1_res")
    ggplot() +
    geom_sf(data = b_rme, fill = "grey80", color = "grey80") +
    geom_sf(aes(color = m1_res), size = 2) +
    geom_sf(shape = 4, size = 0.8) +
    scale_color_gradient2(low = "cyan", high = "brown", midpoint = 0) +
    theme_void() +
    labs(
      title = "Deviance residuals\n(looking for spatial clustering)",
      subtitle = "(+): CV_obs > CV_est (under-est.)\n(-): CV_obs < CV_est (over-est.)"
    )
) |>
  wrap_plots(nrow = 1)
