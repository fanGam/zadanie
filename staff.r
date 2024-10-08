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
summary(scores)

# 2. Построить 95% ди для среднего балла, если данные имеют нормальное распределение с дисперсией 16
conf_int_known_var <- CI(mean_score, sigma=sqrt(16), n=length(scores), conf.level=0.95)
print(conf_int_known_var)

# 3. Построить 95% ди для среднего балла, если данные имеют нормальное распределение
conf_int_unknown_var <- t.test(scores, conf.level=0.95)$conf.int
print(conf_int_unknown_var)

# 4. Построить 95% ди для дисперсии балла, если данные имеют нормальное распределение
conf_int_variance <- VarTest(scores, conf.level=0.95)$conf.int
print(conf_int_variance)

# 5. Построить 95% ди для доли студентов с баллом больше, чем 25
prop <- sum(scores > 25) / length(scores)
conf_int_prop <- BinomCI(sum(scores > 25), length(scores), conf.level=0.95)
print(conf_int_prop)