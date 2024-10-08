# Честь 1
# Задание 1
# Стандартное нормальное распределение
z_values <- qnorm(c(0.3, 0.5, 0.8, 0.368, 0.9, 0.95, 0.975, 0.98))
print(z_values)

# Задание 2
# Распределение Стьюдента с 10 степенями свободы
t_values <- qt(c(0.9, 0.95, 0.975), df=10)
print(t_values)

# Задание 3
# Хи-квадрат распределение с 15 степенями свободы
chi_squared_values <- qchisq(c(0.05, 0.1, 0.9, 0.95), df=15)
print(chi_squared_values)

# Задание 4
# Распределение Фишера с (3, 5) степенями свободы
f_values <- qf(c(0.9, 0.95), df1=3, df2=5)
print(f_values)

# Часть 2

library(DescTools)
# Данные о баллах за экзамен
scores <- c(10, 20, 30, 25, 20, 40, 30, 40, 20, 25)

# 1. Найти выборочные характеристики
mean_score <- mean(scores)
median_score <- median(scores)
var_score <- var(scores)
sd_score <- sd(scores)
# summary_scores <- summary(scores)

# 2. Построить 95% ди для среднего балла, если данные имеют нормальное распределение с дисперсией 16
conf_int_known_var <- MeanCI(scores, method = "classic", sd=sqrt(16), conf.level=0.95)
cat("95% доверительный интервал для среднего значения (известная дисперсия):")
cat("[", conf_int_known_var[1], ", ", conf_int_known_var[2], "]\n")

# 3. Построить 95% ди для среднего балла, если данные имеют нормальное распределение
conf_int_unknown_var <- MeanCI(scores, conf.level=0.95)
cat("95% доверительный интервал для среднего значения (неизвестная дисперсия):")
cat("[", conf_int_unknown_var[1], ", ", conf_int_unknown_var[2], ")]n")

# 4. Построить 95% ди для дисперсии балла, если данные имеют нормальное распределение
conf_int_variance <- VarCI(scores, conf.level=0.95)
cat("95% доверительный интервал для дисперсии:")
cat("[", conf_int_variance[1], ", ", conf_int_variance[2], "]\n")

# 5. Построить 95% ди для доли студентов с баллом больше, чем 25
prop <- sum(scores > 25) / length(scores)
conf_int_prop <- BinomCI(sum(scores > 25), length(scores), conf.level=0.95)
cat("95% доверительный интервал для доли студентов с баллом больше 25:")
cat("[", conf_int_prop[1], ", ", conf_int_prop[2], "]\n")

# Часть 3
# Считываем данные из файла xls
library("readxl")
dataset <- read_excel("Данные Титаник.xls")
data <- na.omit(dataset)

# 1 Задание
# 95% доверительный интервал
conf_interval_mean <- MeanCI(data$age, conf.level = 0.95, na.rm = TRUE)
cat('95% доверительный интервал для среднего возраста: [', conf_interval_mean[1], ',', conf_interval_mean[2], ']\n')

# Дисперсия и 95% доверительный интервал для дисперсии возраста
conf_interval_var <- VarCI(data$age, conf.level = 0.95, na.rm = TRUE)
cat('95% доверительный интервал для дисперсии возраста: [', conf_interval_var[1], ',', conf_interval_var[2], ']\n')

# Среднее и 95% доверительный интервал для среднего возраста при известном стандартном отклонении
age_mean <- mean(data$age)
conf_interval_known_sd <- age_mean + c(-1, 1) * qnorm(1 - 0.05/2) * (8 / sqrt(sum(!is.na(data$age))))
cat('95% доверительный интервал для среднего возраста при ср.квадратичном отклонении 8: [', conf_interval_known_sd[1], ',', conf_interval_known_sd[2], ']\n')

# Пропорция и 95% доверительный интервал для доли выживших
conf_interval_survived <- BinomCI(sum(data$survived == 1, na.rm = TRUE), sum(!is.na(data$survived)), conf.level = 0.95)
cat('95% доверительный интервал для доли людей, которые выжили: [', conf_interval_survived[1], ',', conf_interval_survived[2], ']\n')