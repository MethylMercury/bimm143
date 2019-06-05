# ---
# title: class05
# author: Sanghun Lee
# date: Thu Apr 18, 2019
# output: html_document
# ---


#Classwork in R, April 16

setwd("/R/class05/bimm_143_05_rstats")

#2A Line plot
weight <- read.table("weight_chart.txt",
                     header = T, #Header = F pushes the Headers into the data, and recognizes them as a value rather than a text denotation.
                     sep = "",
                     quote = "",
)

plot(weight$Age, weight$Weight,
     type = "o",
     pch = 15,
     cex = 1.5,
     lwd = 2,
     ylim = c(2,10),
     xlab = "Age (months)",
     ylab = "Weight (kgs)",
     main = "Baby Weight",
)

#2B Barplot exercise

feat <- read.table("feature_counts.txt", sep = "\t", quote = "", header = T)

barplot(feat$Count, 
        horiz = T, 
        names.arg = feat$Feature,
        main = "Gene Features", 
        las = 1,
        mar = c(4,20,2,3)
)

#3A Colored barplot

mnf <- read.table("male_female_counts.txt", sep = "\t", header = T)

barplot(mnf$Count,
        horiz = F,
        names.arg = mnf$Sample,
        ylab = "Counts",
        main = "Guys vs. Gals",
        las = 2, # labels are perpendicular to the barplot.
        col = c("blue2", "red2")
        #col = rainbow(nrow(mnf))
)

#3B Coloring by value

genes <- read.delim("up_down_expression.txt")

#number of rows in this data set is 5196. nrow(genes)

palette(c("red", "gray", "green3")) # down = red, unchanging = gray, up = green3.

plot(genes$Condition1,
     genes$Condition2,
     xlab = "Condition 1",
     ylab = "Condition 2",
     col = genes$State)

# Colors were chosen by the order of the data's label names. 
# So since table(genes$State) yielded down, unchanging and up, 
# this order will be used when colors are ran through genes$State.
# palette() yields black, red, green3, blue, cyan... so on.

# 3C Dynamic Use of Color

meth <- read.delim("expression_methylation.txt")
# View(meth)
# nrow(meth)
# There are 9241 genes in this dataset.
dcols <- densCols(meth$gene.meth, meth$expression, colramp = colorRampPalette("blue3", "green3", "red3", "yellow3"))
                  
plot(meth$gene.meth,
     meth$expression,
     xlab = "Methylated genes",
     ylab = "Expressed genes",
     col = dcols,
     pch = 20,
     main = "Expression and Methylation graph"
     )
