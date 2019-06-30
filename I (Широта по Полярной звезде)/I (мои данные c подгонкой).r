# Функция для отображения угла, записанного в десятичной форме, в градусной мере

show_in_the_degree_system = function(x) {
  
  degrees = as.integer(x)
  print(degrees)
  
  minutes = (x - degrees) * 60
  print(as.integer(minutes))
  
  seconds = (minutes - as.integer(minutes)) * 60
  print(seconds)
  
  temp1 = degrees
  temp2 = minutes
  temp3 = seconds
  
}

# Функция для отображения угла, записанного в градусной мере, в часовой мере (не работает: пока не могу сообразить)

show_in_the_hour_system = function(temp1, temp2, temp3) {
  
  S = temp1 * 3600 + temp2 * 60 + temp3
  s = S / 15
  
  hours = as.integer(s / 3600)
  print(hours)
  
  minutes = as.integer((s - hours * 3600) / 60)
  print(minutes)
  
  seconds = s - hours * 3600 - minutes * 60
  print(seconds)
  
}

# Функция для отображения десятичного угла в часовой мере

show_in_the_hour_system_from_dec = function(x) {
  
  degrees = as.integer(x)
  minutes = (x - degrees) * 60
  seconds = (minutes - as.integer(minutes)) * 60
  minutes = as.integer(minutes)
  
  S = degrees * 3600 + minutes * 60 + seconds
  s = S / 15
  
  hours = as.integer(s / 3600)
  print(hours)
  
  hour_minutes = as.integer((s - hours * 3600) / 60)
  print(hour_minutes)
  
  hour_seconds = s - hours * 3600 - hour_minutes * 60
  print(hour_seconds)
  
}

# Первая строчка таблицы (в часовой мере) с переводом сразу в десятичные углы (через градусную)
# (T_1 - часы, T_2 - минуты, T_3 - секунды)
T_1 = c(19,     19,  19,   19,   19,   19,   19,   19)
T_2 = c( 4,     10,  16,   19,   32,   35,   38,   40)
T_3 = c(54.5, 39.5, 8.5, 14.5, 53.5, 49.5, 13.5, 53.5)

T_dec = T_1 * 15 + T_2 / 4 + T_3 / 240
T_dec

show_in_the_hour_system_from_dec(T_dec)

# Табличные значения для альфа (между дробными сутками 18.4 и 19.4) 
# с переводом в десятичные углы (запись в виде (часы, минуты, секунды))

alpha_18 = c(2, 54, 42.01)
alpha_19 = c(2, 54, 43.59)

alpha_18_dec = alpha_18[1] * 15 + alpha_18[2] / 4 + alpha_18[3] / 240
alpha_18_dec

alpha_19_dec = alpha_19[1] * 15 + alpha_19[2] / 4 + alpha_19[3] / 240
alpha_19_dec

# Табличное значение долготы (для площадки наблюдений; требует проверки)
# с переводом в десятичные углы

lambda = c(2, 1, 10.771)
lambda_dec = lambda[1] * 15 + lambda[2] / 4 + lambda[3] / 240
lambda_dec

# Средний момент по звёздному времени (S_lambda)

S_l = mean(T_dec)
S_l

# Момент наблюдения по звёздному гринвичскому времени

S = S_l - lambda_dec
S

# Дельта верхней кульминации (совпадает до минут на смежных днях)

alpha_0 = c(2, 54)
alpha_0_dec = alpha_0[1] * 15 + alpha_0[2] / 4
alpha_0_dec

# Вычисление n (для интерполяции)

n = (S - alpha_0_dec) / 360
n

# Интерполяция для альфа

alpha = alpha_18_dec + (alpha_19_dec - alpha_18_dec) * n
alpha

show_in_the_hour_system_from_dec(alpha)

# Табличные значения дельта (в градусной мере)

delta_18 = c(89, 20, 24.14)
delta_19 = c(89, 20, 24.00)

delta_18_dec = delta_18[1] + delta_18[2] / 60 + delta_18[3] / 3600
delta_18_dec

delta_19_dec = delta_19[1] + delta_19[2] / 60 + delta_19[3] / 3600
delta_19_dec

# Интерполяция для дельта

delta = delta_18_dec + (delta_19_dec - delta_18_dec) * n
delta

show_in_the_degree_system(delta)

# Поправку хронометра U считаем нулевой

# Вторая строчка таблицы (сразу в десятичном виде - для таблицы, вероятно, потребуется обратный
# перевод в часовую меру)

L2 = 180 + alpha
L2

show_in_the_hour_system_from_dec(L2)

# Третья строчка (разница первой и второй)

t = T_dec - L2
t

show_in_the_hour_system_from_dec(t)

# Четвёртая строчка (перед этим перевод в радианы)

t_rad = t * pi / 180

sin_val = (sin(t_rad/2))^2
sin_val

# Пятая строчка: lambda

phi_0 = c(59,56)
phi_0_dec = phi_0[1] + phi_0[2] / 60
phi_0_dec

phi_0_rad = phi_0_dec * pi / 180

dzeta_0 = 180 - (phi_0_dec + delta)
dzeta_0

dzeta_0_rad = dzeta_0 * pi / 180
dzeta_0_rad

delta_rad = delta * pi / 180
delta_rad

lambda_2 = 412530 * cos(phi_0_rad) * cos(delta_rad) / sin(dzeta_0_rad)
lambda_2

# Шестая строчка (ro)

ro = lambda_2 * sin_val
ro

# Седьмая строчка (r) (число угловых секунд)

r = ro + ro * ro * cos(dzeta_0_rad) / sin(dzeta_0_rad) / 412530 + ro * ro * ro * (1 / (sin(dzeta_0_rad))^2 - 2 / 3) / (2 * 206265^2)
r

show_in_the_degree_system(r / 3600)

# Дельта R (вычисляется в угловых минутах)

# Значения уровней 

# (bLL - до, левый край, левый круг)
# (aLR - после, левый край, правый круг)

bLL = c(11, 12, 12, 12)
bRL = c(23, 24, 24, 24)
aLL = c(12, 12, 12, 12)
aRL = c(24, 24, 24, 24)

bLR = c(14, 14, 14, 14)
bRR = c(26, 26, 26, 26)
aLR = c(14, 14, 14, 14)
aRR = c(26, 26, 26, 26)

# Средние значения по уровням между началами и концами наблюдений

mLL = (bLL + aLL) / 2
mRL = (bRL + aRL) / 2

mLR = (bLR + aLR) / 2
mRR = (bRR + aRR) / 2

delta_R_L = ((mLL + mRL) - 35) * 1.332
delta_R_L

delta_R_R = ((mLR + mRR) - 35) * 1.332
delta_R_R

# Восьмая строчка: L' и R'

# Вычисление L'

# Младшие штрихи первого микрометра

L_I_y_1 = c(30, 30, 30, 30)
L_I_y_2 = c(21, 20, 19, 18)
L_I_y_3 = c( 5, 13, 19, 50)

L_I_y_dec = L_I_y_1 + L_I_y_2 / 60 + L_I_y_3 / 3600
L_I_y_dec

# Старшие штрихи первого микрометра (далее по аналогии)

L_I_o_1 = c(30, 30, 30, 30)
L_I_o_2 = c(21, 20, 19, 18)
L_I_o_3 = c(1,  7,  22, 55)

L_I_o_dec = L_I_o_1 + L_I_o_2 / 60 + L_I_o_3 / 3600
L_I_o_dec

L_I_dec = (L_I_y_dec + L_I_o_dec) / 2
L_I_dec

show_in_the_degree_system(L_I_dec)

L_II_y_1 = c(210, 210, 210, 210)
L_II_y_2 = c( 21,  20,  19,  18)
L_II_y_3 = c(  1,   8,  20,  50)

L_II_y_dec = L_II_y_1 + L_II_y_2 / 60 + L_II_y_3 / 3600
L_II_y_dec

L_II_o_1 = c(210, 210, 210, 210)
L_II_o_2 = c( 21,  20,  19,  18)
L_II_o_3 = c(  5,  13,  22,  54)

L_II_o_dec = L_II_o_1 + L_II_o_2 / 60 + L_II_o_3 / 3600
L_II_o_dec

L_II_dec = (L_II_y_dec + L_II_o_dec) / 2
L_II_dec

show_in_the_degree_system(L_II_dec)

L_a = (L_I_dec + L_II_dec - 180) / 2 + delta_R_L / 3600
L_a

show_in_the_degree_system(L_a)

# Вычисление R'

R_I_y_1 = c(329, 329, 329, 329)
R_I_y_2 = c( 42,  42,  42,  43)
R_I_y_3 = c( 12,  34,  56,  22)

R_I_y_dec = R_I_y_1 + R_I_y_2 / 60 + R_I_y_3 / 3600
R_I_y_dec

R_I_o_1 = c(329, 329, 329, 329)
R_I_o_2 = c( 42,  42,  43,  43)
R_I_o_3 = c( 6,  41,   5,  33)

R_I_o_dec = R_I_o_1 + R_I_o_2 / 60 + R_I_o_3 / 3600
R_I_o_dec

R_I_dec = (R_I_y_dec + R_I_o_dec) / 2
R_I_dec

show_in_the_degree_system(R_I_dec)

R_II_y_1 = c(149, 149, 149, 149)
R_II_y_2 = c( 42,  42,  42,  43)
R_II_y_3 = c(  5,  30,  57,  21)

R_II_y_dec = R_II_y_1 + R_II_y_2 / 60 + R_II_y_3 / 3600
R_II_y_dec

R_II_o_1 = c(149, 149, 149, 149)
R_II_o_2 = c( 42,  42,  43,  43)
R_II_o_3 = c( 15,  42,   4,  31)

R_II_o_dec = R_II_o_1 + R_II_o_2 / 60 + R_II_o_3 / 3600
R_II_o_dec

R_II_dec = (R_II_y_dec + R_II_o_dec) / 2
R_II_dec

show_in_the_degree_system(R_II_dec)

R_a = (R_I_dec + R_II_dec + 180) / 2 + delta_R_R / 3600
R_a

show_in_the_degree_system(R_a)

# Девятая строчка: L и R

L = L_a + r[1:4] / 3600
L

show_in_the_degree_system(L)

R = R_a - r[5:8] / 3600
R

show_in_the_degree_system(R)

# Десятая строчка: (L) и (R)

LM = mean(L)
LM

show_in_the_degree_system(LM)

RM = mean(R)
RM

show_in_the_degree_system(RM)

# Одиннадцатая строчка: M_z

M_z = (LM + RM - 360) / 2
M_z

show_in_the_degree_system(M_z)

# Двенадцатая строчка: (L) - M_z

L12 = LM - M_z
L12

show_in_the_degree_system(L12)

# Тринадцатая строчка: refr (рефракция)

refr = 33
refr_dec = refr / 3600

# Четырнадцатая строчка: z (истинное зенитное расстояние)

z = L12 + refr_dec
z

show_in_the_degree_system(z)

# Пятнадцатая строчка: дельта (величина, которую мы получили интерполяцией)

delta
show_in_the_degree_system(delta)

# Шестнадцатая строчка: z + дельта

L16 = z + delta
L16

show_in_the_degree_system(L16)

# Семнадцатая строчка: фи

phi = 180 - L16
phi

show_in_the_degree_system(phi)

# Для проверки:

show_in_the_degree_system(L)
show_in_the_degree_system(LM)

show_in_the_degree_system(R)
show_in_the_degree_system(RM)
