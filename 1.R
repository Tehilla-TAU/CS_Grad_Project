# טעינת ספריות
library(tidyverse)
library(ggdist)
library(ggplot2)

# טעינת הנתונים
df <- read.csv("education_career_success.csv")

# סינון רק לסטודנטים במדעי המחשב
df_cs <- df %>% filter(Field_of_Study == "Computer Science")

# 1. התפלגות השכר ההתחלתי כולל חציון וממוצע
ggplot(df_cs, aes(x = "", y = Starting_Salary)) +  
  geom_violin(fill = "lightblue", alpha = 0.5) +  # Violin Plot להצגת ההתפלגות
  geom_jitter(width = 0.2, alpha = 0.3, color = "black") +  # נקודות לפיזור הנתונים
  stat_summary(fun = median, geom = "crossbar", width = 0.3, color = "red", size = 0.7) +  # חציון (קו אדום)
  stat_summary(fun = mean, geom = "point", size = 3, color = "blue") +  # ממוצע (נקודה כחולה)
  labs(title = "התפלגות השכר ההתחלתי בקרב בוגרי מדעי המחשב",
       x = "", 
       y = "שכר התחלתי ($)") +
  theme_classic()

# 2. השפעת מספר ההתמחויות על השכר ההתחלתי
ggplot(df_cs, aes(x = factor(Internships_Completed), y = Starting_Salary, fill = factor(Internships_Completed))) +
  geom_violin(alpha = 0.3) +  # Violin להצגת הצפיפות
  geom_boxplot(width = 0.2, alpha = 0.7, outlier.shape = NA) +  # Boxplot עם פחות שקיפות
  geom_jitter(width = 0.2, alpha = 0.3, color = "black") +  # פיזור נתונים
  labs(title = "השפעת מספר ההתמחויות על השכר ההתחלתי",
       x = "מספר ההתמחויות",
       y = "שכר התחלתי ($)") +
  theme_classic()

# 3. השפעת דירוג האוניברסיטה על השכר ההתחלתי
ggplot(df_cs, aes(x = University_Ranking, y = Starting_Salary)) +
  geom_point(alpha = 0.3, color = "blue") +  # נקודות פיזור
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # קו מגמה ליניארי
  labs(title = "השפעת דירוג האוניברסיטה על השכר ההתחלתי",
       x = "דירוג האוניברסיטה",
       y = "שכר התחלתי ($)") +
  theme_classic()

# 4. התפלגות כישורי הנטוורקינג של הסטודנטים 
ggplot(df_cs, aes(x = Networking_Score)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black", alpha = 0.6) +
  labs(title = "התפלגות ציוני Networking",
       x = "ציון Networking",
       y = "מספר מקרים") +
  theme_classic()

# 5. התפלגות מספר ההתמחויות של הסטודנטים
ggplot(df_cs, aes(x = Internships_Completed)) +
  geom_histogram(binwidth = 1, fill = "purple", color = "black", alpha = 0.6) +
  stat_count(geom = "text", aes(label = ..count..), vjust = -0.5, color = "black") +  # הוספת מספרים
  labs(title = "התפלגות מספר ההתמחויות",
       x = "מספר ההתמחויות",
       y = "מספר מקרים") +
  theme_classic()

# 6. התפלגות דירוג האוניברסיטה בקרב בוגרי מדעי המחשב
ggplot(df_cs, aes(x = University_Ranking)) +
  geom_histogram(binwidth = 50, fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "התפלגות דירוג האוניברסיטה בקרב בוגרי מדעי המחשב",
       x = "דירוג האוניברסיטה",
       y = "מספר מקרים") +
  theme_classic()

# 7. התפלגות השכר לפי מגדר
ggplot(df_cs, aes(x = Starting_Salary, fill = Gender)) +
  geom_density(alpha = 0.5) +
  stat_summary(aes(y = 0), fun = median, geom = "point", size = 3, color = "black") +  # חציון של כל קבוצה
  labs(title = "התפלגות השכר ההתחלתי לפי מגדר",
       x = "שכר התחלתי ($)",
       y = "צפיפות") +
  theme_classic()

