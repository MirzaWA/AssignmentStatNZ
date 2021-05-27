
# Import datasets

ocp<- read.table(file = "C:/Users/mirza.ahmed_synergia/Documents/MirzaPer/Occupations.txt", header = TRUE, sep = "\t") 
View(ocp)

dupli <- duplicated(ocp[ ,c("ID")])
dup <-sum(1*duplicated(ocp[ ,c("ID")]))

inc<- read.csv(file = "C:/Users/mirza.ahmed_synergia/Documents/MirzaPer/Income.txt",    
               header = TRUE, sep = "", stringsAsFactors = T)
View(inc)
str(inc)

## Note # Different seperator used in both the datasets

## Data Cleaning


which(is.na(ocp), arr.ind=TRUE)
# No NA found in Occupations dataset.

na <-which(is.na(inc), arr.ind=TRUE)
na <- as.data.frame(na)
View(na)
na <- na[1:7,1  ]
na111<- as.list(na)

View(na111)

names(na)
colnames(na)<- "RowNumber"

nalist <- as.list(na)    #convert into list

# dataframe with NA values na_df

na_df <- inc[c(14,18,42,54,66,83,102), 1]
na_df <- as.data.frame(na_df)
colnames(na_df)<- "Income"
## We need a dataframe with the rows just before the NA values.


pre_na_df <- inc[(c(14,18,42,54,66,83,102)-1), ]
View(pre_na_df)

## Rename the columns of pre_na_df
names(pre_na_df)[1:7] <- c("Transaction_Date", "ID","First_Name","Last_Name", "Middle_Name",
                        "Birth_Date","Sex")


## Drop column Middle_Name from pre_na_df

pre_na_df$Last_Name = paste(pre_na_df$Middle_Name, pre_na_df$Last_Name, sep=" ")

pre_na_df <- pre_na_df[ ,-5]

## Bring the income column and cbind with pre_na_df
View(pre_na_df)
df <- cbind(pre_na_df, na_df)
View(df)

## rbind df with inc to get complete information.
## drop the columns with NA

c1 <- (c(14,18,42,54,66,83,102)-1)
c2 <- c(14,18,42,54,66,83,102)

c3 <- append(c1, c2)# combine c1,c2 and now remove those rows from inc dataframe.

incMod <- inc[-c3,]
View(incMod)



FinalInc<- rbind(incMod, df)

View(FinalInc)


###########################################
## Q1, Remove invalid occupations ########
#########################################

unique(ocp$Occupation) # not all jobs are unique nor all are valid. 

## 2 invalid jobs found. "Married to Jim" & "I look after my mother Sarah who is unwell"

which(ocp$Occupation == "Married to Jim", arr.ind = T) # Locate which row has the invalid jobs.
# Row 1 is an invalid job

which(ocp$Occupation == "I look after my mother Sarah who is unwell", arr.ind = T) # Locate which row has the invalid jobs.
# Row 2 is an invalid job

# Drop row 1, 2 from ocp
c4 <- c(1,2)

ocp <- ocp[-c4, ]
s <-unique(ocp$Occupation)
View(s)

## replace all "software Software Engineer" by "Software Engineer" to get unique jobs.s

ocp<- lapply(ocp, function(x) {
                  gsub("Software Software Engineer", "Software Engineer", x)
         })

ocp<- lapply(ocp, function(x) {
  gsub("Materials Engineerr", "Materials Engineer", x)
})

unique(ocp$Occupation)
## 13 unique and valid occupations found.

ocp <- as.data.frame(ocp)

View(ocp)


################################################################
## Q2. Merge occupation and income and bring income column in.##
###############################################################

ocp_Finalinc <- merge(x=ocp, y=FinalInc, by= "ID", all.x =  TRUE )
View(ocp_Finalinc)

names(ocp_Finalinc)

ocp_Finalinc <- ocp_Finalinc[ ,c(1,3,4,5,11,6,7,13)]
View(ocp_Finalinc)

names(ocp_Finalinc)[1:8] <- c("ID","First_Name","Last_Name","BirthDay.X","BirthDay.Y",
                              "Sex", "Occupation","Income")


####################################
## Q3. Remove all NA incomes #######
####################################
library(dplyr)
ocp_Finalinc_noNA <- ocp_Finalinc %>% 
                     filter(!is.na(Income)) %>% 
                     select(ID,First_Name,Last_Name,BirthDay.X,BirthDay.Y,Sex,Occupation,Income) %>% 
                     unique()

View(ocp_Finalinc_noNA)

#########################################################################################################
## Q4. Gather data so "Income", "Sex", and "Occupation" are values in a new column called "Variable". ##
########################################################################################################

# Before we convert the data into long format need to check if there is any duplicate ID.

dup <-sum(1*duplicated(ocp_Finalinc_noNA [ ,c("ID")]))

dup ## There are duplicate values.

dupli <- ocp_Finalinc_noNA$ID

dupli[duplicated(dupli)] ## Extract duplicate elements  ID

## Duplicate entries for IDs 81 82 83 84 85 86 87 88 89 90
## While we check the D.O.B of these individuals they are different
## It appears to me that they are different person. 

apply(ocp_inc, 2, function(x) any(is.na(x))) ## Now no NA in df

str(ocp_inc)

library(reshape2)

Q4 <- melt(ocp_inc,
      id.vars=c("ID", "Sex"),
      measure.vars=c("Sex", "Occupation", "Income" ),
      variable.name="Variable",
      value.name="Value")


View(Q4)

class(Q4) # data.frame

#Get the names into the data frame
names<- ocp_Finalinc_noNA[ , c(1:3)]

Q4 <- merge(x= Q4, y= names, by= "ID", all.x =  TRUE )

Q4 <- Q4[ , c(1,5,6,3,4)]

View(Q4)



###################################################################################################
## Q5.Create a function that creates a column called fullname that combines first and last names.##
###################################################################################################


Q5 <- ocp_Finalinc_noNA

NameFunction <- function (Q5){
  Q5$FUll_Name <-paste(Q5$First_Name," ",Q5$Last_Name)
  return(u)
}

Q5 <- Q5[ , c(1,2,3,9,4,5,6,7,8)] 

View(Q5)


#########################################################################################
## Q6. Plot overall distribution of income. Indicate the median on a line on the plot. ##
#########################################################################################

## density


ocp_Finalinc_noNA$Income<-as.integer(ocp_Finalinc_noNA$Income)

par(mfrow = c(1, 1))

hist(ocp_Finalinc_noNA$Income, probability = TRUE, ylab = "", col = "grey",
     axes = FALSE, main = "")

axis(1)

# Density
plot(density(ocp_Finalinc_noNA$Income), col = "red", lwd = 2)

# Add boxplot
par(new = TRUE)
boxplot(ocp_Finalinc_noNA$Income, horizontal = TRUE, axes = FALSE,
        lwd = 2, col = rgb(0, 1, 1, alpha = 0.15))



#######################################################################################
## Q7. Plot median income by occupation and compare with the overall median income. ##
######################################################################################
Q7 <- ocp_Finalinc_noNA[ ,c(7,8)]

library(dplyr)

medians <- reorder(Q7$Occupation, Q7$Income, median)

boxplot(Q7$Income ~ medians, las = 2, xlab = "", ylab = "")

View(Q7)
unique(Q7$Occupation)


View(unique(Q7$Occupation))





Q7$Income <- as.numeric(Q7$Income)

Q7 %>% 
  filter(Occupation != "Fitter and Turner"|Occupation !="Biomedical Engineer"|Occupation !="Binder and Finisher") %>% 
  select(Occupation, Income)

medians <- reorder(Q7$Occupation, Q7$Income, median)

boxplot(Q7$Income ~ medians, las = 2, xlab = "", ylab = "",las=1)

View(Q7)
unique(Q7$Occupation)







# Boxplot by group
ggplot(data = Q7, aes(x=reorder(Occupation,Income, FUN = median), y=Income)) +
  #stat_boxplot(geom = "errorbar", 
              # width = 0.2) +
  geom_boxplot(fill = "#4271AE", colour = "red", # Colors
               alpha = 0.9, outlier.colour = "red") +
  scale_y_continuous(name = "Income") +  # Continuous variable label
  scale_x_discrete(name = "Occupation") +      # Group label
  ggtitle("Income by occupation") + 
  theme(axis.line = element_line(colour = "black",
                                 size = 0.25))# +coord_flip()





































