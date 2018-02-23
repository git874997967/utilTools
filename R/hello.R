# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {
  print("Hello, world!")
}

library(corrplot)
library(RColorBrewer)
train = data.frame(
  Id = c(1:10),
  Name = "ABC DEF",
  Age = (2:11),
  Gender = "Male",
  stringsAsFactors = F
)
train[c(2, 4, 7), c(2, 3)] = NA
train[c(2, 4, 7, 9), 4] = 'Female'

# show the miss value info in the data frame
missValue = function(train) {
  for (i in 1:ncol(train)) {
    if (nrow(train[is.na(train[, i]), ]) != 0) {
      print(paste(
        nrow(train[is.na(train[, i]), ]),
        "values missing in",
        colnames(train[i]),
        ". Col type is",
        typeof(train[, i]),
        sep =
          " "
      ))
    }
  }
}
missValue(train)

###trans the character into Factor
charFactor = function(train) {
  charRow = lapply(train, typeof) == 'character'
  train[, charRow] = lapply(train[, charRow], as.factor)
  str(train)
}
charFactor(train)
### get the cor index of all numberic col
getCor = function(test) {
  numSet = c('integer', 'numeric', 'double')
  numVars = sapply(test, class) %in% numSet
  test1 = test[, numVars]
  cor(test1)
  ifelse(is.null(missValue(test1)),
         corPlot(test1),
         print("find NA values please check again"))

}
####draw the corr plot
corPlot = function(x) {
  corr_mat = cor(x)
  corrplot(
    corr_mat,
    #title='the cor matrix among numeric vars',
    method = "color",
    outline = T,
    addgrid.col = "darkgray",
    order = "hclust",
    # addrect = 4,
    rect.col = "black",
    rect.lwd = 5,
    cl.pos = "b",
    tl.col = "indianred4",
    tl.cex = 0.5,
    cl.cex = 0.5,
    addCoef.col = "white",
    number.digits = 2,
    number.cex = 0.75,
    col = colorRampPalette(c("darkred", "white", "midnightblue"))(100)
  )
}
