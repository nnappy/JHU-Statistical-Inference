library(datasets)
library(reshape2)

# Ten guinea pigs per supplement and dose level were observed in this data set. 
# This applies an id number to the data corresponding to the guinea pig being 
# observed and transforms the data from long format to wide format. "dose_one" 
# is a dose of 0.5mg,"dose_two" of 1.0mg and "dose_three" of 2.0mg

id <- 1:10
teeth <- cbind(id, ToothGrowth)
teeth <- dcast(teeth, id + supp ~ dose, value.var = "len")
teeth <- teeth[order(teeth$supp),]
names(teeth)[3:5] <- c("dose_one", "dose_two", "dose_three")

# Now that the data has been organized by dose and supplement
# separate data frames are created to faciliate the analysis
oj <- teeth[1:10,3:5]
vc <- teeth[11:20,3:5]

# Create a matrix for the purpose of creating a simple grouped bar chart 
# exploring the relationship between average tooth length in the subjects
# by delivery method and dose
counts <- rbind(colMeans(oj), colMeans(vc))
rownames(counts) <- c("OJ", "VC")
colnames(counts) <- c("0.5", "1.0", "2.0")

# Create a bar chart
barplot(counts, main = "Mean Tooth Length by Delivery Method/Dose", 
    xlab = "Dose Level in mg", ylab = "Tooth Length", 
    col = c("royalblue4", "greenyellow"), legend = c("OJ", "VC"), 
    beside = TRUE, ylim = c(0,35))

# provide a brief summary of the data by broken out by delivery method
summary(oj)
summary(vc)

# Perform t test measuring the difference in average tooth length
# between delivery methods by dosage

t.test(oj$dose_one, vc$dose_one, paired = FALSE)
t.test(oj$dose_two, vc$dose_two, paired = FALSE)
t.test(oj$dose_three, vc$dose_three, paired = FALSE)