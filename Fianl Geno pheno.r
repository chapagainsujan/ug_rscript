library(dplyr)
library(pheatmap)
library(viridis)

# Prepare data
trait_data <- Rice_yield[, 4:ncol(Rice_yield)]
traits <- colnames(trait_data)
geno_vec <- Rice_yield$Landraces
rep_vec  <- Rice_yield$REP

# Function for correlation matrices
get_corr_matrices <- function(data, Landraces, REP) {
  n_traits <- ncol(data)
  geno_cor <- matrix(NA, n_traits, n_traits)
  pheno_cor <- matrix(NA, n_traits, n_traits)
  colnames(geno_cor) <- rownames(geno_cor) <- colnames(data)
  colnames(pheno_cor) <- rownames(pheno_cor) <- colnames(data)
  
  for (i in 1:n_traits) {
    for (j in 1:n_traits) {
      trait1 <- data[[i]]
      trait2 <- data[[j]]
      
      model1 <- lm(trait1 ~ Landraces + REP)
      model2 <- lm(trait2 ~ Landraces + REP)
      
      g_effect1 <- coef(model1)[2:length(unique(Landraces)) + 1]
      g_effect2 <- coef(model2)[2:length(unique(Landraces)) + 1]
      
      geno_cor[i, j] <- cor(g_effect1, g_effect2, use = "complete.obs")
      pheno_cor[i, j] <- cor(trait1, trait2, use = "complete.obs")
    }
  }
  
  return(list(geno = geno_cor, pheno = pheno_cor))
}

# Compute correlation matrices
corr_matrices <- get_corr_matrices(trait_data, geno_vec, rep_vec)
geno_matrix <- corr_matrices$geno
pheno_matrix <- corr_matrices$pheno

# Set font
windowsFonts("Times New Roman" = windowsFont("Times New Roman"))

# Save Genotypic Correlation Matrix as TIFF
tiff("Genotypic_Correlation_Matrix.tiff", width = 2000, height = 2000, res = 300, compression = "lzw", family = "Times New Roman")
pheatmap(geno_matrix,
         main = "",
         col = hcl.colors(50),
         cluster_rows = TRUE,
         cluster_cols = TRUE,
         display_numbers = FALSE,
         fontsize = 12,
         fontsize_row = 10,
         fontsize_col = 10)
dev.off()

# Save Phenotypic Correlation Matrix as TIFF
tiff("Phenotypic_Correlation_Matrix.tiff", width = 2000, height = 2000, res = 300, compression = "lzw", family = "Times New Roman")
pheatmap(pheno_matrix,
         main = "",
         col = hcl.colors(50),
         cluster_rows = TRUE,
         cluster_cols = TRUE,
         display_numbers = FALSE,
         fontsize = 12,
         fontsize_row = 10,
         fontsize_col = 10)
dev.off()
