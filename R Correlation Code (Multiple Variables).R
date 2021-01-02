#Code to show the first parts of the 'mtcars' dataset
head(mtcars)

#Code to show load the 'ggpubr' package
library(ggpubr)

#Code to create a new object from the 'mtcars' dataset, to label the 'cyl' into factor levels (i.e. 1,2,3) and to display the structure of the data
my_data <- mtcars
my_data$cyl <- factor(my_data$cyl)
str(my_data)

#Code to create a scatterplot using 'ggpubr' package
ggscatter(my_data, x = "wt", y = "mpg",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Weight (1000 lbs)", ylab = "Miles/ (US) gallon")

#Code to run Shapiro-Wilk normality test for mpg (p-values greater than 0.05 imply that the distribution of the data are not significantly different from normal distribution)
shapiro.test(my_data$mpg)

#Code to run Shapiro-Wilk normality test for wt (p-values greater than 0.05 imply that the distribution of the data are not significantly different from normal distribution)
shapiro.test(my_data$wt)

#Code to create Q-Q plots to visually check for normality for 'mpg' and 'wt' variables
ggqqplot(my_data$mpg, ylab = "MPG")
ggqqplot(my_data$wt, ylab = "WT")

#If data does not follow a normal distribution then use a non-parametric correlation, including Spearman and Kendall rank-based tests.

#Code to run a Pearson correlation test
res <- cor.test(my_data$wt, my_data$mpg, method = "pearson")
res

#Code to show the structure of the previous 'cor.test' function and to extract the p-value and estimate
str(res)
res$p.value
res$estimate

#Code to run a Kendall rank correlation test (if data are not from a bivariate normal distribution ('tau' is the correlation coefficient))
res2 <- cor.test(my_data$mpg, my_data$wt, method = "kendall")
res2

#Code to run a Spearman correlation test (if data are not from a bivariate normal distribution ('rho' is the correlation coefficient))
res3 <- cor.test(my_data$wt, my_data$mpg, method = "spearman")
res3

#Code to load 'dplyr' package, create a 'cor_1' variable and create a correlation matrix (although this contains correlation coefficients only)
library(dplyr)

my_data <- select(mtcars, mpg, disp, hp, drat, wt, qsec)
head(my_data)
cor_1 <- round(cor(my_data), 2)
cor_1

#Code to load the 'Hmisc' package and create a correlation matrix containing p-values (and correlation coefficients)
library("Hmisc")

cor_2 <- rcorr(as.matrix(my_data))
cor_2

#Code to see the structure of the previous correlation matrix and to extract the p-values and correlation coeffecients
str(cor_2)
cor_2$P
cor_2$r

#Code to format a correlation matrix into a table with 4 columns containing row names, column names, correlation coefficients and p-values
flat_cor_mat <- function(cor_r, cor_p){
  library(tidyr)
  library(tibble)
  cor_r <- rownames_to_column(as.data.frame(cor_r), var = "row")
  cor_r <- gather(cor_r, column, cor, -1)
  cor_p <- rownames_to_column(as.data.frame(cor_p), var = "row")
  cor_p <- gather(cor_p, column, p, -1)
  cor_p_matrix <- left_join(cor_r, cor_p, by = c("row", "column"))
  cor_p_matrix
}

cor_3 <- rcorr(as.matrix(mtcars[, 1:7]))

my_cor_matrix <- flat_cor_mat(cor_3$r, cor_3$P)
head(my_cor_matrix)

#Code to symbolically encode a given numeric or logical vector or array (replacing correlation coefficients with symbols according to the level of correlation)
cor_4 <- cor(mtcars[1:6])
symnum(cor_4, abbr.colnames = FALSE)

#Code to create a correlogram by loading the 'corrplot' package, then creating a new obejct called 'M', then giving the correlation matrix and finally producing the correlogram
library(corrplot)
M <- cor(mtcars)
head(round(M,2))
corrplot(M, method = "circle")

#Code to create the same correlogram as above, but using elipses
corrplot(M, method = "ellipse")

#Code to create the same correlogram as above, but using pie charts
corrplot(M, method = "pie")

#Code to create the same correlogram as above, but using colours
corrplot(M, method = "color")

#Code to create the same correlogram as above, but using correlation coefficients
corrplot(M, method = "number")

#Code to display only the upper or lower triangulars of the correlogram
corrplot(M, type = "upper")
corrplot(M, type = "lower")

#Code to reorder to correlogram in order of correlation coefficients
corrplot(M, order = "hclust")

#Code to exploit the symetry of the correlogram
corrplot(M, type = "upper", order = "hclust")

#Code to change the colour of the correlogram to lightgreen and color of the circles to darkorange and steel blue
corrplot(M, type = "upper", order = "hclust", col = c("darkorange", "steelblue"),
         bg = "lightgreen")

#Code to change the correlogram using the RColorBrewer Package
library(RColorBrewer)
corrplot(M, type = "upper", order = "hclust",
         col = brewer.pal(n = 9, name = "PuOr"), bg = "darkgreen")

#Code to use the tl.col argument for defining the text label color and tl.srt for text label string rotation
corrplot(M, type = "upper", order = "hclust", tl.col = "darkblue", tl.srt = 45)

#Code to combine the correlogram with significance tests (marking insignificant coefficients in accordance with specified alpha level)
cor_5 <- rcorr(as.matrix(mtcars))
M <- cor_5$r
p_mat <- cor_5$P
corrplot(M, type = "upper", order = "hclust", 
         p.mat = p_mat, sig.level = 0.01)

#Code to leave blank on no significant coefficient
corrplot(M, type = "upper", order = "hclust", 
         p.mat = p_mat, sig.level = 0.05, insig = "blank")

#Code to fine tune customisation of the correlogram
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M, method = "color", col = col(200),  
         type = "upper", order = "hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "darkblue", tl.srt = 45, #Text label color and rotation
         # Combine with significance level
         p.mat = p_mat, sig.level = 0.01,  
         # hide correlation coefficient on the principal diagonal
         diag = FALSE 
)

#Code to create a heatmap of correlations of 'M' object
col <- colorRampPalette(c("darkblue", "white", "darkorange"))(20)
M <- cor(mtcars[1:7])
heatmap(x = M, col = col, symm = TRUE)