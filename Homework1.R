### Problem 1 

student_id <- c("S01", "S02", "S03", "S04", "S05", "S06")
section <- c("A", "B", "A", "B", "A", "B")
quiz1 <- c(82, 91, 76, 88, 95, 69)
quiz2 <- c(85, 89, 80, 92, 94, 74)
passed <- c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)

### Part A
# Convert section to factor
section <- factor(section, levels = c("A", "B"))

# Data Frame
students <- data.frame(student_id, section, quiz1, quiz2, passed)

# Matrix
score_matrix <- matrix(
  c(quiz1,quiz2),
  nrow = 6,
  ncol = 2,
  dimnames = list(student_id, c("quiz1", "quiz2"))
)

# List of courses
course_record <- list(
  course = "R Programming",
  scores = students,
  cutoffs = c(pass =70, excellent = 90)
)

typeof(section)
class(section)
length(section)
str(section)

typeof(students)
class(students)
dim(students)
str(students)

typeof(score_matrix)
class(score_matrix)
dim(score_matrix)

typeof(course_record)
class(course_record)
length(course_record)
str(course_record)

#Comparison
# A vector is made of objects that can be any size and adjusted. A factor is an augmented vector that can hold categorical data. 
# A list can hold objects of different types and lengths. A data frame is a list of same length vectors, while a matrix is an atomic vector with two dimensional attributes. 

# Part B

#S04's second quiz score from score_matrix
score_matrix["S04", "quiz2"]

# First two rows extracted
score_matrix[1:2, , drop = FALSE]
dim(score_matrix[1:2, , drop = FALSE]) 

# Extract course 
course_record["course"]
course_record[["course"]]
course_record$course

class(course_record["course"])
class(course_record[["course"]])
class(course_record$course)

# Explanation
# '[' Returns a sublist. '[[' returns the element inside the sublist. $ is short for '[[' with a name, does not accept variable holding the name. 

# Part C
students$average <- rowMeans(students[, c("quiz1", "quiz2")])

# True when average >= 90
students$excellent <- students$average >= 90

# Section A students with >= 80
sectiona_80 <- students[students$section == "A" & students$average >= 80, ]
sectiona_80

sectiona_80[, c("student_id", "section", "average")]


# Named numeric vector of all averages
avg_vec <- students$average
names(avg_vec) <- students$student_id
avg_vec

students

# Explanation :
# Every calculations operates on vectors. rowMeans() computes all 6 row means.
# '>=' compares every element of average to 90 days, the index '[' is a full-length TRUE/FALSE vector produced by element-wise.  


# Problem 2
csv_text <- "sample_id,site,temp_c,ph,status
M01,North,18.2,7.1,ok
M02,South,20.5,,ok
M03,North,NA,6.8,review
M04,East,22.1,7.4,ok
M05,South,19.7,7.0,review
M06,East,23.0,NA,ok
M07,North,17.8,6.9,ok
M08,South,21.2,7.2,ok"

#Part A
# Treat both blank fields and the text "NA" as missing.
measurements <- read.csv(text = csv_text, na.strings = c("", "NA"))

head(measurements)
str(measurements)
dim(measurements)
names(measurements)

colSums(is.na(measurements))

# filter
measurements_complete <- measurements[complete.cases(measurements), ]
measurements_complete

# Sample IDs removed by filter
removed_ids <- measurements$sample_id[!complete.cases(measurements)]
removed_ids

#Explanation
# NA means unknown x == NA so it returns NA for every element rather then true or false.
# is.na(x) is the proper test. 
c(1, NA, 3) == NA
is.na(c(1, NA, 3))

# Part B
# Convert site and status with no loop
measurements$site <- factor(measurements$site)
measurements$status <- factor(measurements$status)
levels(measurements$site)
levels(measurements$status)

#Fahrenheit
measurements$temp_f <- measurements$temp_c * 9 / 5 + 32

# Below 7 PH
measurements$ph_below_7 <- measurements$ph < 7
measurements

keep <- complete.cases(measurements) &
  measurements$site %in% c("North", "South") & 
  measurements$status == "ok"
subset_ns <- measurements[keep, ]

# Return sample_id, site, temp_c, temp_f, and ph
subset_ns[, c("sample_id", "site", "temp_c", "temp_f", "ph")]

# Mean temperature overall
mean(measurements$temp_c, na.rm = TRUE)
mean(measurements$temp_c[measurements$site == "South"], na.rm = TRUE)

# Part C
A <- matrix(1:4, nrow = 2)
B <- matrix(5:8, nrow = 2)

elementwise <- A * B
matmult <- A %*% B

elementwise
matmult

dim(elementwise)
dim(matmult)

# Explanation
# A * B multiplies each by the cell in that position. So the matrices must be same dimensions for this to be valid. 
# A %*% ends up with entry (i, i) is sum of row i of A times column j of B. So for this to work out number of columns in A, must be same as rows in matrix B.

# Problem 3
student_id <- paste0("P", sprintf("%02d", 1:8))
scores <- c(95, 82, NA, 67, 74, 88, 59, 91)

# Part A
grade_one <- function(
    score,
    a_min = 90,
    b_min = 80,
    c_min = 70,
    d_min = 60
) {
  if (is.na(score)) {
    return(NA_character_)
  }
  if (score >= a_min) {
    "A"
  } else if (score >= b_min) {
    "B"
  } else if (score >= c_min) {
    "C"
  } else if (score >= d_min) {
    "D"
  } else {
    "F"
  }
}

grade_one(NA)
grade_one(90)
grade_one(80)
grade_one(85)
grade_one(74)

# Part B
grades <- rep(NA_character_, length(scores))
for (i in seq_along(scores)) {
  # Store one grade here
  grades[i] <- grade_one(scores[i])
}
names(grades) <- student_id
grades

# i is the index in scores, taking values 1-8. 

# Part C-1
summarize_scores <- function(x, na.rm = TRUE, digits = 1) {
  c(
    n       = length(x),
    missing = sum(is.na(x)),
    mean    = round(mean(x, na.rm = na.rm), digits),
    sd      = round(sd(x, na.rm = na.rm), digits),
    min     = round(min(x, na.rm = na.rm), digits),
    max     = round(max(x, na.rm = na.rm), digits)
  )
}
summarize_scores(scores)
summarize_scores( x = scores, na.rm = TRUE, digits = 2 )

# Part C-2

plot_scores <- function(x, ...) {
  plot(seq_along(x), x, ...)
}

plot_scores(scores, type = "b", pch = 19, xlab = "Position", ylab = "Score", main = "Student Scores" )

#The y-axis is the score, and x-axis is index position 1-8. The missing value at 3 is skipped. 