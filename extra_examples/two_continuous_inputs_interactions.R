### behavior of two continuous inputs, examine additive and interaction
### relationships

library(tidyverse)

### first consider additive terms between two continuous inputs
### (no interaction)

### create a grid of beta coefficients and input values, use more values
### for `x1` than `x2`. the intercept, `beta_0`, is zero for simplicity

beta_input_grid_x1 <- expand.grid(x1 = seq(-3, 3, length.out = 101),
                                  x2 = seq(-2, 2, length.out = 9),
                                  beta_0 = 0,
                                  beta_1 = seq(-2, 2, length.out = 5),
                                  beta_2 = seq(-2, 2, length.out = 5),
                                  KEEP.OUT.ATTRS = FALSE, 
                                  stringsAsFactors = FALSE) %>% 
  as.data.frame() %>% tibble::as_tibble()

### calculate the mean trend (linear predictor) and visualize the
### behavior of the mean trend vs `x1` for different values of `x2`
### use the facets to specifcy the values of slopes
### 
### the figure below shows the trend wrt `x1` is shifted vertically
### based on the value of `x2`

beta_input_grid_x1 %>% 
  mutate(mu = beta_0 + beta_1 * x1 + beta_2 * x2) %>% 
  filter(beta_1 %in% c(-1, 0, 1), beta_2 %in% c(-1, 0, 1)) %>% 
  mutate(beta_2 = forcats::fct_rev(as.factor(beta_2))) %>% 
  ggplot(mapping = aes(x = x1, y = mu)) +
  geom_line(mapping = aes(color = as.factor(x2)),
            size = 1.15) +
  facet_grid(beta_2 ~ beta_1, labeller = "label_both") +
  scale_color_viridis_d("x2") +
  theme_bw() +
  theme(legend.position = "top") +
  guides(color = guide_legend(nrow = 1))

### if we make a grid with many points in `x2` and a few points of `x1`
### we will see similar behavior in that the trend wrt `x2` is shifted
### vertically based on the value of `x1`

beta_input_grid_x2 <- expand.grid(x1 = seq(-2, 2, length.out = 9),
                                  x2 = seq(-3, 3, length.out = 101),
                                  beta_0 = 0,
                                  beta_1 = seq(-2, 2, length.out = 5),
                                  beta_2 = seq(-2, 2, length.out = 5),
                                  KEEP.OUT.ATTRS = FALSE, 
                                  stringsAsFactors = FALSE) %>% 
  as.data.frame() %>% tibble::as_tibble()

beta_input_grid_x2 %>% 
  mutate(mu = beta_0 + beta_1 * x1 + beta_2 * x2) %>% 
  filter(beta_1 %in% c(-1, 0, 1), beta_2 %in% c(-1, 0, 1)) %>% 
  mutate(beta_2 = forcats::fct_rev(as.factor(beta_2))) %>% 
  ggplot(mapping = aes(x = x2, y = mu)) +
  geom_line(mapping = aes(color = as.factor(x1)),
            size = 1.15) +
  facet_grid(beta_2 ~ beta_1, labeller = "label_both") +
  scale_color_viridis_d("x1", option = "plasma") +
  theme_bw() +
  theme(legend.position = "top") +
  guides(color = guide_legend(nrow = 1))

### we can also look at SURFACE plot of the mean trend wrt both
### `x1` and `x2`

beta_input_grid_surface <- expand.grid(x1 = seq(-3, 3, length.out = 101),
                                       x2 = seq(-3, 3, length.out = 101),
                                       beta_0 = 0,
                                       beta_1 = seq(-2, 2, length.out = 5),
                                       beta_2 = seq(-2, 2, length.out = 5),
                                       KEEP.OUT.ATTRS = FALSE, 
                                       stringsAsFactors = FALSE) %>% 
  as.data.frame() %>% tibble::as_tibble()

beta_input_grid_surface %>% 
  mutate(mu = beta_0 + beta_1 * x1 + beta_2 * x2) %>% 
  filter(beta_1 %in% c(-1, 0, 1), beta_2 %in% c(-1, 0, 1)) %>% 
  mutate(beta_2 = forcats::fct_rev(as.factor(beta_2))) %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_raster(mapping = aes(fill = mu)) +
  coord_equal() +
  facet_grid(beta_2 ~ beta_1, labeller = "label_both") +
  scale_fill_viridis_c(option = "magma") +
  theme_bw() +
  theme(legend.position = "top")

### a recent addition to the ggplot2 package is to allow automatic
### binning of the viridis colors, can be useful to focus on groups
### of filled regions on a surface rather than a continuous scale

### will not work if you have an old version of ggplot2
beta_input_grid_surface %>% 
  mutate(mu = beta_0 + beta_1 * x1 + beta_2 * x2) %>% 
  filter(beta_1 %in% c(-1, 0, 1), beta_2 %in% c(-1, 0, 1)) %>% 
  mutate(beta_2 = forcats::fct_rev(as.factor(beta_2))) %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_raster(mapping = aes(fill = mu)) +
  coord_equal() +
  facet_grid(beta_2 ~ beta_1, labeller = "label_both") +
  scale_fill_viridis_b(option = "magma") +
  theme_bw() +
  theme(legend.position = "top")

### lets now consider what happens if we have an INTERACTION between
### the two continuous inputs. 

### the interaction parameter will be denoted as `beta_3` and the slope 
### of the `x1 * x2` predictor or feature

### make a grid of points to focus on the trends with respect to `x1`
### at a few distinct values of `x2`

beta_input_grid_interact_x1 <- expand.grid(x1 = seq(-3, 3, length.out = 101),
                                           x2 = seq(-2, 2, length.out = 9),
                                           beta_0 = 0,
                                           beta_1 = seq(-2, 2, length.out = 5),
                                           beta_2 = seq(-2, 2, length.out = 5),
                                           beta_3 = seq(-2, 2, length.out = 5),
                                           KEEP.OUT.ATTRS = FALSE, 
                                           stringsAsFactors = FALSE) %>% 
  as.data.frame() %>% tibble::as_tibble()

### let's first consider what happens when the slopes all equal 1

beta_input_grid_interact_x1 %>% 
  mutate(mu = beta_0 + beta_1 * x1 + beta_2 * x2 + beta_3 * x1 * x2) %>% 
  filter(beta_1 %in% c(1), beta_2 %in% c(1), beta_3 %in% c(1)) %>% 
  mutate(beta_2 = forcats::fct_rev(as.factor(beta_2))) %>% 
  ggplot(mapping = aes(x = x1, y = mu)) +
  geom_line(mapping = aes(color = as.factor(x2),
                          group = interaction(beta_1, beta_2, beta_3, x2)),
            size = 1.15) +
  facet_grid(beta_2 ~ beta_1, labeller = "label_both") +
  scale_color_viridis_d("x2") +
  theme_bw() +
  theme(legend.position = "top") +
  guides(color = guide_legend(nrow = 1))

### the trend with respect to `x1` now DEPENDS on the value of `x2` !!!!

### let's look at two values of `beta_3`: +1 and -1
### `beta_3` will be shown on the horizontal facet WITH `beta_1`

beta_input_grid_interact_x1 %>% 
  mutate(mu = beta_0 + beta_1 * x1 + beta_2 * x2 + beta_3 * x1 * x2) %>% 
  filter(beta_1 %in% c(1), beta_2 %in% c(1), beta_3 %in% c(-1, 1)) %>% 
  mutate(beta_2 = forcats::fct_rev(as.factor(beta_2))) %>% 
  ggplot(mapping = aes(x = x1, y = mu)) +
  geom_line(mapping = aes(color = as.factor(x2),
                          group = interaction(beta_1, beta_2, beta_3, x2)),
            size = 1.15) +
  facet_grid(beta_2 ~ beta_1 + beta_3, labeller = "label_both") +
  scale_color_viridis_d("x2") +
  theme_bw() +
  theme(legend.position = "top") +
  guides(color = guide_legend(nrow = 1))

### look at multiple values of the main effect coefficents, `beta_1` and `beta_2`
### for FIXED value of the interaction term, `beta_3`
beta_input_grid_interact_x1 %>% 
  mutate(mu = beta_0 + beta_1 * x1 + beta_2 * x2 + beta_3 * x1 * x2) %>% 
  filter(beta_1 %in% c(-1, 0, 1), beta_2 %in% c(-1, 0, 1), beta_3 %in% c(1)) %>% 
  mutate(beta_2 = forcats::fct_rev(as.factor(beta_2))) %>% 
  ggplot(mapping = aes(x = x1, y = mu)) +
  geom_line(mapping = aes(color = as.factor(x2),
                          group = interaction(beta_1, beta_2, beta_3, x2)),
            size = 1.15) +
  facet_grid(beta_2 ~ beta_1 + beta_3, labeller = "label_both") +
  scale_color_viridis_d("x2") +
  theme_bw() +
  theme(legend.position = "top") +
  guides(color = guide_legend(nrow = 1))

### use 2 values of `beta_3`: -1 and + 1
beta_input_grid_interact_x1 %>% 
  mutate(mu = beta_0 + beta_1 * x1 + beta_2 * x2 + beta_3 * x1 * x2) %>% 
  filter(beta_1 %in% c(-1, 0, 1), beta_2 %in% c(-1, 0, 1), beta_3 %in% c(-1, 1)) %>% 
  mutate(beta_2 = forcats::fct_rev(as.factor(beta_2))) %>% 
  ggplot(mapping = aes(x = x1, y = mu)) +
  geom_line(mapping = aes(color = as.factor(x2),
                          group = interaction(beta_1, beta_2, beta_3, x2)),
            size = 1.15) +
  facet_grid(beta_2 ~ beta_1 + beta_3, labeller = "label_both") +
  scale_color_viridis_d("x2") +
  theme_bw() +
  theme(legend.position = "top") +
  guides(color = guide_legend(nrow = 1))

### if we focus on the trend with respect to `x2` we will see similar behavior
### the trend now depends on the value of `x1` !!!

beta_input_grid_interact_x2 <- expand.grid(x1 = seq(-2, 2, length.out = 9),
                                           x2 = seq(-3, 3, length.out = 101),
                                           beta_0 = 0,
                                           beta_1 = seq(-2, 2, length.out = 5),
                                           beta_2 = seq(-2, 2, length.out = 5),
                                           beta_3 = seq(-2, 2, length.out = 5),
                                           KEEP.OUT.ATTRS = FALSE, 
                                           stringsAsFactors = FALSE) %>% 
  as.data.frame() %>% tibble::as_tibble()

### all slopes equal to 1

beta_input_grid_interact_x2 %>% 
  mutate(mu = beta_0 + beta_1 * x1 + beta_2 * x2 + beta_3 * x1 * x2) %>% 
  filter(beta_1 %in% c(1), beta_2 %in% c(1), beta_3 %in% c(1)) %>% 
  mutate(beta_2 = forcats::fct_rev(as.factor(beta_2))) %>% 
  ggplot(mapping = aes(x = x2, y = mu)) +
  geom_line(mapping = aes(color = as.factor(x1),
                          group = interaction(beta_1, beta_2, beta_3, x1)),
            size = 1.15) +
  facet_grid(beta_2 ~ beta_1, labeller = "label_both") +
  scale_color_viridis_d("x1", option = 'plasma') +
  theme_bw() +
  theme(legend.position = "top") +
  guides(color = guide_legend(nrow = 1))

### two values of `beta_3`: -1 and +1
beta_input_grid_interact_x2 %>% 
  mutate(mu = beta_0 + beta_1 * x1 + beta_2 * x2 + beta_3 * x1 * x2) %>% 
  filter(beta_1 %in% c(1), beta_2 %in% c(1), beta_3 %in% c(-1, 1)) %>% 
  mutate(beta_2 = forcats::fct_rev(as.factor(beta_2))) %>% 
  ggplot(mapping = aes(x = x2, y = mu)) +
  geom_line(mapping = aes(color = as.factor(x1),
                          group = interaction(beta_1, beta_2, beta_3, x1)),
            size = 1.15) +
  facet_grid(beta_2 + beta_3 ~ beta_1, labeller = "label_both") +
  scale_color_viridis_d("x1", option = 'plasma') +
  theme_bw() +
  theme(legend.position = "top") +
  guides(color = guide_legend(nrow = 1))

### fixed value of `beta_3` and multiple values of the main effect terms
beta_input_grid_interact_x2 %>% 
  mutate(mu = beta_0 + beta_1 * x1 + beta_2 * x2 + beta_3 * x1 * x2) %>% 
  filter(beta_1 %in% c(-1, 0, 1), beta_2 %in% c(-1, 0, 1), beta_3 %in% c(1)) %>% 
  mutate(beta_2 = forcats::fct_rev(as.factor(beta_2))) %>% 
  ggplot(mapping = aes(x = x2, y = mu)) +
  geom_line(mapping = aes(color = as.factor(x1),
                          group = interaction(beta_1, beta_2, beta_3, x1)),
            size = 1.15) +
  facet_grid(beta_2 + beta_3 ~ beta_1, labeller = "label_both") +
  scale_color_viridis_d("x1", option = 'plasma') +
  theme_bw() +
  theme(legend.position = "top") +
  guides(color = guide_legend(nrow = 1))

### use 2 values for `beta_3`: -1 and +1
beta_input_grid_interact_x2 %>% 
  mutate(mu = beta_0 + beta_1 * x1 + beta_2 * x2 + beta_3 * x1 * x2) %>% 
  filter(beta_1 %in% c(-1, 0, 1), beta_2 %in% c(-1, 0, 1), beta_3 %in% c(-1, 1)) %>% 
  mutate(beta_2 = forcats::fct_rev(as.factor(beta_2))) %>% 
  ggplot(mapping = aes(x = x2, y = mu)) +
  geom_line(mapping = aes(color = as.factor(x1),
                          group = interaction(beta_1, beta_2, beta_3, x1)),
            size = 1.15) +
  facet_grid(beta_2 + beta_3 ~ beta_1, labeller = "label_both") +
  scale_color_viridis_d("x1", option = 'plasma') +
  theme_bw() +
  theme(legend.position = "top") +
  guides(color = guide_legend(nrow = 1))

### we can likewise study the surface of the mean trend with respect to
### both `x1` and `x2` 
beta_input_grid_interact_surface <- expand.grid(x1 = seq(-3, 3, length.out = 101),
                                                x2 = seq(-3, 3, length.out = 101),
                                                beta_0 = 0,
                                                beta_1 = seq(-2, 2, length.out = 5),
                                                beta_2 = seq(-2, 2, length.out = 5),
                                                beta_3 = seq(-2, 2, length.out = 5),
                                                KEEP.OUT.ATTRS = FALSE, 
                                                stringsAsFactors = FALSE) %>% 
  as.data.frame() %>% tibble::as_tibble()

### look at the surface with all coefficients set equal to +1 (except the intercept
### which has been 0 throughout this script)

beta_input_grid_interact_surface %>% 
  mutate(mu = beta_0 + beta_1 * x1 + beta_2 * x2 + beta_3 * x1 * x2) %>% 
  filter(beta_1 %in% c(1), beta_2 %in% c(1), beta_3 %in% c(1)) %>% 
  mutate(beta_2 = forcats::fct_rev(as.factor(beta_2))) %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_raster(mapping = aes(fill = mu)) +
  coord_equal() +
  facet_grid(beta_2 ~ beta_1 + beta_3, labeller = "label_both") +
  scale_fill_viridis_c(option = 'magma') +
  theme_bw() +
  theme(legend.position = "top")

### and sometimes using a binned or dsicrete scale helps make it easier
### to see the behavior
beta_input_grid_interact_surface %>% 
  mutate(mu = beta_0 + beta_1 * x1 + beta_2 * x2 + beta_3 * x1 * x2) %>% 
  filter(beta_1 %in% c(1), beta_2 %in% c(1), beta_3 %in% c(1)) %>% 
  mutate(beta_2 = forcats::fct_rev(as.factor(beta_2))) %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_raster(mapping = aes(fill = mu)) +
  coord_equal() +
  facet_grid(beta_2 ~ beta_1 + beta_3, labeller = "label_both") +
  scale_fill_viridis_b(option = 'magma') +
  theme_bw() +
  theme(legend.position = "top")

### look at multiple values of the interaction term
beta_input_grid_interact_surface %>% 
  mutate(mu = beta_0 + beta_1 * x1 + beta_2 * x2 + beta_3 * x1 * x2) %>% 
  filter(beta_1 %in% c(1), beta_2 %in% c(1), beta_3 %in% c(-1, 0, 1)) %>% 
  mutate(beta_2 = forcats::fct_rev(as.factor(beta_2))) %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_raster(mapping = aes(fill = mu)) +
  coord_equal() +
  facet_grid(beta_2 ~ beta_1 + beta_3, labeller = "label_both") +
  scale_fill_viridis_b(option = 'magma') +
  theme_bw() +
  theme(legend.position = "top")

### fixed INTERACTION TERM with multiple values of the MAIN EFFECTS
### first, `beta_3` = -1
beta_input_grid_interact_surface %>% 
  mutate(mu = beta_0 + beta_1 * x1 + beta_2 * x2 + beta_3 * x1 * x2) %>% 
  filter(beta_1 %in% c(-1, 0, 1), beta_2 %in% c(-1, 0, 1), beta_3 %in% c(-1)) %>% 
  mutate(beta_2 = forcats::fct_rev(as.factor(beta_2))) %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_raster(mapping = aes(fill = mu)) +
  coord_equal() +
  facet_grid(beta_2 ~ beta_1 + beta_3, labeller = "label_both") +
  scale_fill_viridis_b(option = 'magma') +
  theme_bw() +
  theme(legend.position = "top")

### next `beta_3` = 0 -- so NO INTERACTION!!! equivalent to additive relationships!
beta_input_grid_interact_surface %>% 
  mutate(mu = beta_0 + beta_1 * x1 + beta_2 * x2 + beta_3 * x1 * x2) %>% 
  filter(beta_1 %in% c(-1, 0, 1), beta_2 %in% c(-1, 0, 1), beta_3 %in% c(0)) %>% 
  mutate(beta_2 = forcats::fct_rev(as.factor(beta_2))) %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_raster(mapping = aes(fill = mu)) +
  coord_equal() +
  facet_grid(beta_2 ~ beta_1 + beta_3, labeller = "label_both") +
  scale_fill_viridis_b(option = 'magma') +
  theme_bw() +
  theme(legend.position = "top")

### and now `beta_3` = 1
beta_input_grid_interact_surface %>% 
  mutate(mu = beta_0 + beta_1 * x1 + beta_2 * x2 + beta_3 * x1 * x2) %>% 
  filter(beta_1 %in% c(-1, 0, 1), beta_2 %in% c(-1, 0, 1), beta_3 %in% c(1)) %>% 
  mutate(beta_2 = forcats::fct_rev(as.factor(beta_2))) %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_raster(mapping = aes(fill = mu)) +
  coord_equal() +
  facet_grid(beta_2 ~ beta_1 + beta_3, labeller = "label_both") +
  scale_fill_viridis_b(option = 'magma') +
  theme_bw() +
  theme(legend.position = "top")

### show all `beta_3` values at once
beta_input_grid_interact_surface %>% 
  mutate(mu = beta_0 + beta_1 * x1 + beta_2 * x2 + beta_3 * x1 * x2) %>% 
  filter(beta_1 %in% c(-1, 0, 1), beta_2 %in% c(-1, 0, 1), beta_3 %in% c(-1, 0, 1)) %>% 
  mutate(beta_2 = forcats::fct_rev(as.factor(beta_2))) %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_raster(mapping = aes(fill = mu)) +
  coord_equal() +
  facet_grid(beta_2 ~ beta_1 + beta_3, labeller = "label_both") +
  scale_fill_viridis_b(option = 'magma') +
  theme_bw() +
  theme(legend.position = "top")
