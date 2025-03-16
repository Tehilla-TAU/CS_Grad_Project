# טעינת ספריות
library(dplyr)
library(ggplot2)
library(pROC)

# טעינת הנתונים
df <- readRDS("./data/filtered_data.rds")

####  רגרסיה לינארית מרובה ----
linear_model <- lm(Starting_Salary ~ Internships_Completed + Networking_Score, data = df)
summary(linear_model)

####  רגרסיה לוגיסטית ----
logistic_model <- glm(High_Salary ~ Internships_Completed + Networking_Score, data = df, family = binomial)
summary(logistic_model)

#### גרף: התפלגות השכר לפי מספר התמחויות ורמת נטוורקינג ----
ggplot(df, aes(x = factor(Internships_Completed), y = Starting_Salary, fill = Networking_Level)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2), alpha = 0.3) +
  scale_fill_manual(values = c("Low" = "red", "Medium" = "green", "High" = "blue")) +
  labs(title = "התפלגות השכר לפי מספר ההתמחויות ורמת נטוורקינג",
       x = "מספר ההתמחויות",
       y = "שכר התחלתי ($)",
       fill = "רמת נטוורקינג") +
  theme_minimal()

#### חיזוי הסתברויות מהמודל הלוגיסטי ----
df <- df |> mutate(predict_logistic = predict(logistic_model, type = "response"))

#### גרף: הסתברות לשכר גבוה לפי מספר התמחויות ורמת נטוורקינג ----
ggplot(df, aes(x = Internships_Completed, y = predict_logistic, color = Networking_Level)) +
  geom_jitter(alpha = 0.3, width = 0.2) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "הסתברות ניבוי לשכר גבוה לפי מספר התמחויות ורמת נטוורקינג",
       x = "מספר ההתמחויות",
       y = "הסתברות לשכר מעל החציון",
       color = "רמת נטוורקינג") +
  theme_minimal()

#### ROC ואבחון איכות המודל הלוגיסטי ----
roc_logistic <- roc(df$High_Salary, df$predict_logistic)
auc_value <- auc(roc_logistic)

# הצגת AUC
print(paste("AUC:", auc_value))

# ציור גרף ROC
plot(roc_logistic, col = "blue", main = "עקומת ROC למודל הרגרסיה הלוגיסטית")
legend("bottomright", legend = paste("AUC =", round(auc_value, 3)), col = "blue", lwd = 2)

