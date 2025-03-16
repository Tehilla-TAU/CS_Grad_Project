
library(dplyr)

#### BONUS - REMOVE COLUMNS WITH HIGH NA ----
library(janitor)

clean_missing_data <- function(df, threshold = 0.2) {
  cols_to_keep <- names(df)[colMeans(is.na(df)) <= threshold]
  message(" הוסרו ", ncol(df) - length(cols_to_keep), " עמודות עם יותר מדי ערכים חסרים.")
  df |> select(all_of(cols_to_keep))
}

df <- read.csv("education_career_success.csv")
df <- clean_missing_data(df)

# סינון רק סטודנטים שלמדו מדעי המחשב
df <- df |> filter(Field_of_Study == "Computer Science")

# טיפול בערכים חסרים באמצעות חציון
numeric_vars <- c("Starting_Salary", "Internships_Completed", "Networking_Score")
df <- df |> mutate(across(all_of(numeric_vars), ~ifelse(is.na(.), ifelse(all(is.na(.)), 0, median(., na.rm = TRUE)), .)))

# פונקציה להסרת ערכים חריגים לפי IQR
remove_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  return(x >= (Q1 - 1.5 * IQR_value) & x <= (Q3 + 1.5 * IQR_value))
}

# החלת פונקציית הסרת החריגים
df <- df |> filter(if_all(all_of(numeric_vars), ~remove_outliers(.)))

#### יצירת משתנים חדשים ----
# הפיכת ציון נטוורקינג לרמות קטגוריות
df <- df |> mutate(
  Networking_Level = case_when(
    Networking_Score <= quantile(Networking_Score, 0.33, na.rm = TRUE) ~ "Low",
    Networking_Score <= quantile(Networking_Score, 0.67, na.rm = TRUE) ~ "Medium",
    TRUE ~ "High"
  )
)

# פונקציה ליצירת משתנה בינארי High_Salary
create_high_salary <- function(df, salary_column) {
  median_salary <- median(df[[salary_column]], na.rm = TRUE)
  df <- df |> mutate(High_Salary = ifelse(df[[salary_column]] > median_salary, 1, 0))
  return(df)
}

# החלת הפונקציה על הנתונים
df <- create_high_salary(df, "Starting_Salary")

# בחירת משתנים רלוונטיים
df <- df |> select(Starting_Salary, Internships_Completed, Networking_Score, 
                   Networking_Level, High_Salary)

# שמירת הנתונים לאחר כל העיבודים
saveRDS(df, file = "./data/filtered_data.rds")

# הצגת סיכום של הנתונים
summary(df)
