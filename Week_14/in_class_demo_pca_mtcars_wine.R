### in class demo PCA - week 14

library(dplyr)
library(ggplot2)

mtcars %>% glimpse()

mtcars %>% count(vs)

mtcars %>% count(am)

mtcars %>% count(gear)

### select just the subset of columns that are not binary
mtcars_var <- mtcars %>% select(c(1:7), 10, 11)

mtcars_var %>% glimpse()

### perform PCA -- prcomp()
mtcars_pca <- prcomp(mtcars_var, center = TRUE, scale. = TRUE)

### look at the structure
mtcars_pca %>% str()

mtcars_pca$center

mtcars_pca$scale

mtcars_pca$x %>% class()

mtcars_pca$x %>% colnames()

### loadings matrix
mtcars_pca$rotation

sum(mtcars_pca$rotation[, 1]^2)

### calculate the contributions of each variable to PC1
mtcars_pca$rotation[, 1]^2 / sum(mtcars_pca$rotation[, 1]^2)

### plot the PC1 and PC2
mtcars_pca$x %>% as.data.frame() %>% 
  head()


mtcars_pca$x %>% as.data.frame() %>% 
  ggplot(mapping = aes(x = PC1, y = PC2)) +
  geom_point() +
  theme_bw()

library(factoextra)

summary(mtcars_pca)

factoextra::get_eigenvalue(mtcars_pca)

factoextra::fviz_screeplot(mtcars_pca)

### look at the correlation between the PCs and the original variables
### consider the first two PCs
factoextra::fviz_pca_var(mtcars_pca)

### consider the contribution of the original variables to the PCs
### as a barchart...with this approach we do not consider the SIGN
factoextra::fviz_contrib(mtcars_pca, choice = "var", axes = 1)

factoextra::fviz_contrib(mtcars_pca, choice = "var", axes = 2)

100*(1 / 9)

### biplot -- look at the PC directions and the observations
factoextra::fviz_pca_biplot(mtcars_pca)

mtcars_var %>% head()

### use the WINE DATASET to see a more realistic example

### wine data set
wine_url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"

wine <- readr::read_csv(wine_url,
                        col_names = c("Cultivar", "Alcohol", "Malic_acid",
                                      "Ash", "Alcalinity_of_ash",
                                      "Magnesium", "Total_phenols",
                                      "Flavanoids", "Nonflavanoid_phenols",
                                      "Proanthocyanin", "Color_intensity",
                                      "Hue", "OD280_OD315_of_diluted_wines",
                                      "Proline"))

### wine data set remove the culture variable
wine_var <- wine %>% select(-Cultivar)

### perform PCA
wine_pca <- prcomp(wine_var, center = TRUE, scale. = TRUE)

factoextra::get_eigenvalue(wine_pca)

factoextra::fviz_screeplot(wine_pca)

factoextra::fviz_pca_var(wine_pca)

factoextra::fviz_pca_var(wine_pca, axes = c(2, 3))

factoextra::fviz_contrib(wine_pca, choice = "var", axes = 1)

factoextra::fviz_contrib(wine_pca, choice = "var", axes = 2)

factoextra::fviz_contrib(wine_pca, choice = "var", axes = 3)

### calculate the recontruction of the original variables given
### a subset of the PC scores

my_reconstruct <- function(nComp, pca_obj)
{
  # extract the scores
  z_mat <- pca_obj$x[, 1:nComp, drop = FALSE]
  
  # extract the correspond loadings matrix
  phi_mat <- pca_obj$rotation[, 1:nComp, drop = FALSE]
  
  # reconstruct the centered and scaled variables
  xhat <- z_mat %*% t(phi_mat)
  
  # rescale the reconstructed variables 
  xhat <- scale(xhat, center = FALSE, scale = 1/pca_obj$scale)
  
  # add back the means
  scale(xhat, center = -pca_obj$center, scale = FALSE)
}

### reconstruct the smaller mtcars data set
### mtcars reconstruction with 1 PC
my_reconstruct(1, mtcars_pca) %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("row_name") %>% 
  tidyr::gather(key = "input_name", value = "recon_value", -row_name) %>% 
  left_join(mtcars_var %>% 
              tibble::rownames_to_column("row_name") %>% 
              tidyr::gather(key = "input_name", 
                            value = "true_value",
                            -row_name),
            by = c("input_name", "row_name")) %>% 
  ggplot(mapping = aes(x = true_value, y = recon_value)) +
  geom_point(color = "black") +
  geom_abline(slope = 1, intercept = 0,
              color = "red", linetype = "dashed") +
  facet_wrap(~input_name, scales = "free") +
  theme_bw()

### reconstruct with the first 2 pcs
my_reconstruct(2, mtcars_pca) %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("row_name") %>% 
  tidyr::gather(key = "input_name", value = "recon_value", -row_name) %>% 
  left_join(mtcars_var %>% 
              tibble::rownames_to_column("row_name") %>% 
              tidyr::gather(key = "input_name", 
                            value = "true_value",
                            -row_name),
            by = c("input_name", "row_name")) %>% 
  ggplot(mapping = aes(x = true_value, y = recon_value)) +
  geom_point(color = "black") +
  geom_abline(slope = 1, intercept = 0,
              color = "red", linetype = "dashed") +
  facet_wrap(~input_name, scales = "free") +
  theme_bw()

### use 3 PCs
my_reconstruct(3, mtcars_pca) %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("row_name") %>% 
  tidyr::gather(key = "input_name", value = "recon_value", -row_name) %>% 
  left_join(mtcars_var %>% 
              tibble::rownames_to_column("row_name") %>% 
              tidyr::gather(key = "input_name", 
                            value = "true_value",
                            -row_name),
            by = c("input_name", "row_name")) %>% 
  ggplot(mapping = aes(x = true_value, y = recon_value)) +
  geom_point(color = "black") +
  geom_abline(slope = 1, intercept = 0,
              color = "red", linetype = "dashed") +
  facet_wrap(~input_name, scales = "free") +
  theme_bw()

### use all PCs...perfect reconstruction
my_reconstruct(9, mtcars_pca) %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("row_name") %>% 
  tidyr::gather(key = "input_name", value = "recon_value", -row_name) %>% 
  left_join(mtcars_var %>% 
              tibble::rownames_to_column("row_name") %>% 
              tidyr::gather(key = "input_name", 
                            value = "true_value",
                            -row_name),
            by = c("input_name", "row_name")) %>% 
  ggplot(mapping = aes(x = true_value, y = recon_value)) +
  geom_point(color = "black") +
  geom_abline(slope = 1, intercept = 0,
              color = "red", linetype = "dashed") +
  facet_wrap(~input_name, scales = "free") +
  theme_bw()

### reconstuct with the wine data set using just the first PC
my_reconstruct(1, wine_pca) %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("row_name") %>% 
  tidyr::gather(key = "input_name", value = "recon_value", -row_name) %>% 
  left_join(wine_var %>% 
              tibble::rownames_to_column("row_name") %>% 
              tidyr::gather(key = "input_name", 
                            value = "true_value",
                            -row_name),
            by = c("input_name", "row_name")) %>% 
  ggplot(mapping = aes(x = true_value, y = recon_value)) +
  geom_point(color = "black") +
  geom_abline(slope = 1, intercept = 0,
              color = "red", linetype = "dashed") +
  facet_wrap(~input_name, scales = "free") +
  theme_bw()

### use more PCs
my_reconstruct(3, wine_pca) %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("row_name") %>% 
  tidyr::gather(key = "input_name", value = "recon_value", -row_name) %>% 
  left_join(wine_var %>% 
              tibble::rownames_to_column("row_name") %>% 
              tidyr::gather(key = "input_name", 
                            value = "true_value",
                            -row_name),
            by = c("input_name", "row_name")) %>% 
  ggplot(mapping = aes(x = true_value, y = recon_value)) +
  geom_point(color = "black") +
  geom_abline(slope = 1, intercept = 0,
              color = "red", linetype = "dashed") +
  facet_wrap(~input_name, scales = "free") +
  theme_bw()

### use 5 PCs
my_reconstruct(5, wine_pca) %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("row_name") %>% 
  tidyr::gather(key = "input_name", value = "recon_value", -row_name) %>% 
  left_join(wine_var %>% 
              tibble::rownames_to_column("row_name") %>% 
              tidyr::gather(key = "input_name", 
                            value = "true_value",
                            -row_name),
            by = c("input_name", "row_name")) %>% 
  ggplot(mapping = aes(x = true_value, y = recon_value)) +
  geom_point(color = "black") +
  geom_abline(slope = 1, intercept = 0,
              color = "red", linetype = "dashed") +
  facet_wrap(~input_name, scales = "free") +
  theme_bw()

### perfect reconstruction
my_reconstruct(13, wine_pca) %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("row_name") %>% 
  tidyr::gather(key = "input_name", value = "recon_value", -row_name) %>% 
  left_join(wine_var %>% 
              tibble::rownames_to_column("row_name") %>% 
              tidyr::gather(key = "input_name", 
                            value = "true_value",
                            -row_name),
            by = c("input_name", "row_name")) %>% 
  ggplot(mapping = aes(x = true_value, y = recon_value)) +
  geom_point(color = "black") +
  geom_abline(slope = 1, intercept = 0,
              color = "red", linetype = "dashed") +
  facet_wrap(~input_name, scales = "free") +
  theme_bw()

### PCA is sensitive to the SCALE of the variables
wine_pca_uns <- prcomp(wine_var, center = TRUE, scale. = FALSE)

factoextra::get_eigenvalue(wine_pca_uns)

factoextra::fviz_pca_var(wine_pca_uns)

wine_var %>% summary()
