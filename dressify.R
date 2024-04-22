choose.files()
data = read.csv("C:\\Users\\DELL\\OneDrive\\Desktop\\Desktop\\College\\DATA SCIENCE\\PROJECTS\\Dressify challenge\\Data\\RDS_Assignment_Train.csv")
View(data)

#Shape

dim(data)

#Duplicates
data[duplicated(data$Dress_CODE)] #no duplicates


#Converting each value to lowercase
data <- data.frame(lapply(data, tolower))
data

#Missing values

sapply(data, function(x) sum(x == "null"))

#Sleevelength contains 1 null value, extracting that row

data[data$SleeveLength == "null", ]  #Dress code = 100841
#as 4 variables contain null values, we will drop this row

data = data[data$Dress_CODE != "100841", ]

#checking for missing values again
sapply(data, function(x) sum(x == "null"))

#it can be seen that fabric type contains 210 missing values, so we can drop the column
data <- data[, -which(names(data) == "FabricType")]
data <- data[, -which(names(data) == "Decoration")]


#Extracting the row where rating = 0
data[data$Rating == 0, ]
data = data[data$Dress_CODE != "100586", ] #contains rating as 0 and many null values

#finding the datatype of Rating
class(data$Rating)  #character
data$Rating = as.numeric(data$Rating)
class(data$Rating)

#finding the median of Rating
median(data$Rating)

#finding the columns that contain 0
sapply(data, function(x) sum(x == "0"))
data[data$Price == 0, ]
data[data$Season == 0, ]  #2 null 1 0
data[data$Neckline == 0, ]
data[data$waiseline == 0, ]  #remove this row
data[data$Material == 0, ]
data[data$Pattern.Type == 0, ]


data = data[data$Dress_CODE != "s", ]
#replacing 0 with null
data$Price[data$Price == 0] <- "null"
data$Size[data$Size == 0] <- "null"
data$Season[data$Season == 0] <- "null"
data$Price[data$Price == 0] <- "null"
data$Price[data$Price == 0] <- "null"

sapply(data, function(x) sum(x == "null"))  
sapply(data, function(x) sum(x == 0))   #No zero in columns except rating and target


#finding the most frequent value in material, price, season, waiseline and pattern.type

freq_table <- table(data$Material)
names(freq_table)[which.max(freq_table)]  #cotton occurs maximum time

freq_table <- table(data$Price)
names(freq_table)[which.max(freq_table)] #average

freq_table <- table(data$Season)
names(freq_table)[which.max(freq_table)] #summer

freq_table <- table(data$Pattern.Type)
names(freq_table)[which.max(freq_table)] #solid

freq_table <- table(data$waiseline)
names(freq_table)[which.max(freq_table)] #natural


#replacing with the most frequent value

data$Material[data$Material == "null"] <- "cotton"
data[data$Material == "null", ]

data$Price[data$Price == "null"] <- "average"
data[data$Price == "null", ]

data$Season[data$Season == "null"] <- "summer"

data$Pattern.Type[data$Pattern.Type == "null"] <- "solid"

data$waiseline[data$waiseline == "null"] <- "natural"

#Label encoding Price and size

data$Price[data$Price == "medium"] <- "average"
data$Size[data$Size == "small"] <- "s"

data$Price[data$Price == "low"]               <- 1
data$Price[data$Price == "average"]           <- 2
data$Price[data$Price == "high"]              <- 3
data$Price[data$Price == "very-high"]         <- 4


data$Size[data$Size == "free"]                <- 1
data$Size[data$Size == "s"]                   <- 1
data$Size[data$Size == "m"]                   <- 2
data$Size[data$Size == "l"]                   <- 3
data$Size[data$Size == "xl"]                  <- 4

View(data)


