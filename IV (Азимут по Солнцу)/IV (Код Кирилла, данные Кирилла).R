# Функция для отображения угла, записанного в десятичной форме, в градусной мере

show_in_the_degree_system = function(x) {
  
  degrees = as.integer(x)
  print(degrees)
  
  minutes = (x - degrees) * 60
  print(as.integer(minutes))
  
  seconds = (minutes - as.integer(minutes)) * 60
  print(seconds)
  
}

# Функция для отображения угла, записанного в градусной мере, в часовой мере

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

# Функция для перевода из часовой системы в десятичную с возвращением результата

transfer_from_hour_system_to_dec = function(v) {
  
  temp4 = v[1] * 15 + v[2] / 4 + v[3] / 240
  return(temp4)
  
}


# Функция для перевода из градусной системы в десятичную с возвращением результата

transfer_from_degree_system_to_dec = function(v) {
  
  temp4 = v[1] + v[2] / 60 + v[3] / 3600
  return(temp4)
  
}

# Моменты наблюдений по рабочему звёздному хронометру (в часовой мере два значения подряд)

# Первое наблюдение, левый круг

T_1_L = c(5, 4, 30, 5, 6, 53)
T_1_L_dec = c(transfer_from_hour_system_to_dec(T_1_L[1:3]), transfer_from_hour_system_to_dec(T_1_L[4:6]))

# Второе наблюдение, левый круг (далее по аналогии)

T_2_L = c(5, 9, 7, 5, 11, 28.5)
T_2_L_dec = c(transfer_from_hour_system_to_dec(T_2_L[1:3]), transfer_from_hour_system_to_dec(T_2_L[4:6]))

T_1_R = c(4, 40, 40.5, 4, 43, 8)
T_1_R_dec = c(transfer_from_hour_system_to_dec(T_1_R[1:3]), transfer_from_hour_system_to_dec(T_1_R[4:6]))

T_2_R = c(4, 50, 8.5, 4, 52, 30.5)
T_2_R_dec = c(transfer_from_hour_system_to_dec(T_2_R[1:3]), transfer_from_hour_system_to_dec(T_2_R[4:6]))

# Первая строчк: средние моменты наблюдений, T_(iL) и T_(iR)

T_1_L_mean = mean(T_1_L_dec)
T_2_L_mean = mean(T_2_L_dec)
T_1_R_mean = mean(T_1_R_dec)
T_2_R_mean = mean(T_2_R_dec)

show_in_the_hour_system_from_dec(T_1_L_mean)
show_in_the_hour_system_from_dec(T_2_L_mean)
show_in_the_hour_system_from_dec(T_1_R_mean)
show_in_the_hour_system_from_dec(T_2_R_mean)

# Вторая строчка: поправку времени хронометра (U_dec) считаем равной нулю

# Третья строчка: средние моменты наблюдений с учётом поправки хронометра

S_l = c(T_1_L_mean, T_2_L_mean, T_1_R_mean, T_2_R_mean)

show_in_the_hour_system_from_dec(S_l)

# Приведение к моментам наблюдения по звёздному гринвичскому времени

lambda = c(2, 1, 10.771)
lambda_dec = transfer_from_hour_system_to_dec(lambda)

S = S_l - lambda_dec
S

show_in_the_hour_system_from_dec(S)

# Четвёртая и пятая строчки: прямые восхождения и склонения (интерполирование по табличным данным)

    # Чтобы показать алгоритм, подставляю современные данные.

    # Видимое прямое восхождение (в часовой мере)

    alpha_24 = c(6,  9, 42.963)
    alpha_25 = c(6, 13, 52.360)
    
    alpha_24_dec = transfer_from_hour_system_to_dec(alpha_24)
    alpha_25_dec = transfer_from_hour_system_to_dec(alpha_25)
    
    
    # Часовые изменения уравнения времени на начало текущих и следующих суток
    # (у Витязева: teta_0 и teta_1)
    
    alpha_teta_24 = c(0, 0, -0.5368)
    alpha_teta_25 = c(0, 0, -0.5335)
    
    alpha_teta_24_dec = transfer_from_hour_system_to_dec(alpha_teta_24)
    alpha_teta_25_dec = transfer_from_hour_system_to_dec(alpha_teta_25)
    
    # Истинное звёздное время на 24 июня (табличное значение в часовой мере)
    
    S_24 = c(18, 7, 26.1669)
    S_24_dec = transfer_from_hour_system_to_dec(S_24) 
    
    # Множитель перевода звёздного времени в среднее
    nu = 0.0027304336

    # Поправка, с помощью которой всемирное время переводится в земное время (в часовой мере)
    
    delta_T = c(0,0,69,6)
    delta_T_dec = transfer_from_hour_system_to_dec(delta_T)
    
    # Здесь начинается та часть алгоритма, которая вызывает сомнение:
    
    # Разница S - S_0 (в моих обозначениях: S - S_24)
    
    S_diff = -(S - S_24_dec)
    show_in_the_hour_system_from_dec(S_diff)
    
    # Величина M
    
    M = S_diff * (1 - nu)
    show_in_the_hour_system_from_dec(M)
    
    # Величина M*
    
    M_ast_hour = M + delta_T_dec
    M_ast_long = c(c(15,1,36.515464), c(14,57,1.019746), c(15,25,19.868453), c(15,15,56.161830))
    M_ast = c(transfer_from_degree_system_to_dec(M_ast_long[1:3]), transfer_from_degree_system_to_dec(M_ast_long[4:6]),
             transfer_from_degree_system_to_dec(M_ast_long[7:9]), transfer_from_degree_system_to_dec(M_ast_long[10:12]))
    
    # Значение teta'_0
    
    alpha_teta_a = transfer_from_hour_system_to_dec(c(0, 0, 9.856)) - alpha_teta_24_dec
    show_in_the_hour_system_from_dec(alpha_teta_a)
    
    # Значение разницы teta_0 - teta_1
    
    alpha_teta_diff = alpha_teta_24_dec - alpha_teta_25_dec
    
    # Получение значения прямого восхождения
    
    alpha = alpha_24_dec + M_ast * (alpha_teta_a - M_ast / 48 * alpha_teta_diff)
    show_in_the_hour_system_from_dec(alpha)
    
    transfer_from_hour_system_to_dec(c(6,11,22.64))
    
    # Данное alpha из stellarium 6h11m22.64s
    # alpha = 92.84433
    
    alpha
    
    
    # Получение значения склонения
    
    # Видимое склонение (в часовой мере)
    
    delta_24 = c(23, 25,  0.52)
    delta_25 = c(23, 23, 50.23)
    
    delta_24_dec = transfer_from_degree_system_to_dec(delta_24)
    delta_25_dec = transfer_from_degree_system_to_dec(delta_25)
    
    # Часовое отклонение
    
    delta_teta_24 = c(0, 0, -2.413)
    delta_teta_25 = c(0, 0, -3.443)
    
    delta_teta_24_dec = transfer_from_degree_system_to_dec(delta_teta_24)
    delta_teta_25_dec = transfer_from_degree_system_to_dec(delta_teta_25)
    
    delta_teta_diff = delta_teta_25_dec - delta_teta_24_dec 
    
    delta = delta_24_dec + M_ast * (delta_teta_24_dec + M_ast / 48 * delta_teta_diff)
    
    show_in_the_degree_system(delta)
    
    
    transfer_from_degree_system_to_dec(c(23,24,30.3))
    # Данное alpha из stellarium delta=23.24'30.3"
    # delta = 23.40842
    
# Шестая строчка: t_i
    S
      
t = (S - alpha)
show_in_the_hour_system_from_dec(t)

    
# Седьмая строчка: sin(t_i)

t_rad = t * pi / 180

sin_val = sin(t_rad)
sin_val

# Восьмая строчка: cos(t_i)

cos_val = cos(t_rad)
cos_val

# Девятая строчка: m_i (с этого момента идут небольшие расхождения с Витязевым)

delta_rad = delta * pi / 180

m = cos(delta_rad) / sin(delta_rad) * 1.996522
m

# Десятая строчка: n_i

n = cos(delta_rad) / sin(delta_rad) * 1.728034
n

# Одиннадцатая строчка: tan(A_i)

tan_val = (-1 * m * sin_val) / (1 - n * cos_val)
tan_val

# Двенадцатая строчка: A_i

A = atan(tan_val)
A_dec = A * 180 / pi

show_in_the_degree_system(A_dec)

# Тринадцатая строчка: sin(A_i)

sin_val_2 = sin(A)
sin_val_2

# Четырнадцатая строчка: cos(delta_i)

cos_val_2 = cos(delta_rad)
cos_val_2

# Пятнадцатая строчка: sin(z_i)

sin_val_3 = sin_val / sin_val_2 * cos_val_2
sin_val_3

# Шестнадцатая строчка: z_i (там опечатки у Витязева)

z = asin(sin_val_3)
z_dec = z * 180 / pi

z

show_in_the_degree_system(z_dec)

# Семнадцатая строчка: z_L, z_R

z_L = mean(z_dec[1:2])
z_R = mean(z_dec[3:4])

show_in_the_degree_system(z_L)
show_in_the_degree_system(z_R)

z_L_rad = z_L * pi / 180
z_R_rad = z_R * pi / 180

# Восемнадцатая строчка: csc(z_L), csc(z_R)

csc_val_L = 1 / sin(z_L_rad)
csc_val_R = 1 / sin(z_R_rad)

csc_val_L
csc_val_R

# Девятнадцатая строчка: p

p = csc_val_L + csc_val_R
p

# Двадцатая строчка: q

q = 1 / 2 * (csc_val_L - csc_val_R)
q

# Двадцать первая строчка: KL', KR'

# Значения азимутов для наблюдений Солнца:



# Младшие штрихи первого микрометра

L_A_y_1 = c(140, 141)
L_A_y_2 = c(14, 54)
L_A_y_3 = c(55,  42)

L_A_y_dec = L_A_y_1 + L_A_y_2 / 60 + L_A_y_3 / 3600
L_A_y_dec

# Старшие штрихи первого микрометра (далее по аналогии)

L_A_o_1 = c(140, 141)
L_A_o_2 = c(14,  14)
L_A_o_3 = c(52,  44)

L_A_o_dec = L_A_o_1 + L_A_o_2 / 60 + L_A_o_3 / 3600
L_A_o_dec

L_A_dec = (L_A_y_dec + L_A_o_dec) / 2
L_A_dec


L_B_y_1 = c(320, 321)
L_B_y_2 = c(13, 53)
L_B_y_3 = c(22, 22)

L_B_y_dec = L_B_y_1 + L_B_y_2 / 60 + L_B_y_3 / 3600
L_B_y_dec

L_B_o_1 = c(320, 321)
L_B_o_2 = c(13, 53)
L_B_o_3 = c(22, 28)

L_B_o_dec = L_B_o_1 + L_B_o_2 / 60 + L_B_o_3 / 3600
L_B_o_dec

L_B_dec = (L_B_y_dec + L_B_o_dec) / 2
L_B_dec

KL_a = (L_A_dec + L_B_dec) / 2
KL_a

show_in_the_degree_system(KL_a)

# Вычисление R'

R_A_y_1 = c(311, 315)
R_A_y_2 = c(56,  11)
R_A_y_3 = c(30,  34)

R_A_y_dec = R_A_y_1 + R_A_y_2 / 60 + R_A_y_3 / 3600
R_A_y_dec

# Старшие штрихи первого микрометра (далее по аналогии)

R_A_o_1 = c(311, 315)
R_A_o_2 = c(56,  56)
R_A_o_3 = c(38,  34)

R_A_o_dec = R_A_o_1 + R_A_o_2 / 60 + R_A_o_3 / 3600
R_A_o_dec

R_A_dec = (R_A_y_dec + R_A_o_dec) / 2
R_A_dec


R_B_y_1 = c(131, 135)
R_B_y_2 = c(55, 10)
R_B_y_3 = c(25, 29)

R_B_y_dec = R_B_y_1 + R_B_y_2 / 60 + R_B_y_3 / 3600
R_B_y_dec

R_B_o_1 = c(131, 135)
R_B_o_2 = c(55, 10)
R_B_o_3 = c(30, 20)

R_B_o_dec = R_B_o_1 + R_B_o_2 / 60 + R_B_o_3 / 3600
R_B_o_dec

R_B_dec = (R_B_y_dec + R_B_o_dec) / 2
R_B_dec

KR_a = (R_A_dec + R_B_dec) / 2
KR_a

show_in_the_degree_system(KR_a)

# Двадцать вторая строчка: b_i

b = c(transfer_from_degree_system_to_dec(c(0, 0, 10.6275)), transfer_from_degree_system_to_dec(c(0, 0, 1.417)),
      transfer_from_degree_system_to_dec(c(0, 0, -21.255)), transfer_from_degree_system_to_dec(c(0, 0, -21.255)))
show_in_the_degree_system(b)

# Двадцать третяя строчка: delta_b

delta_b = b*cos(z)/sin(z)
show_in_the_degree_system(delta_b)

# Цена полуделения накладного уровня (в градусной мере)

tau_half = c(0, 0, 1.417)
tau_half_dec = transfer_from_degree_system_to_dec(tau_half)

# Разницы между правыми и левыми краями уровней до и после перекладки: i

# Двадцать четвёртая строчка: L', R'

L_a = KL_a + delta_b[1:2]
R_a = KR_a + delta_b[3:4]

show_in_the_degree_system(L_a)
show_in_the_degree_system(R_a)

# Двадцать пятая строчка: A (Да, двенадцатая строчка тоже была про A. Ну и что теперь.)

show_in_the_degree_system(A_dec)

# Двадцать шестая строчка: M_(iL), M_(iR)

M = c(L_a - A_dec[1:2], R_a - A_dec[3:4])
M

show_in_the_degree_system(M)

# Двадцать седьмая строчка: M_L, M_R

M_L = mean(M[1:2])
M_R = mean(M[3:4])

show_in_the_degree_system(M_L)
show_in_the_degree_system(M_R)

# Двадцать восьмая строчка: KL, KR

# Значения азимутов для наблюдений предмета:

# Вычисление KR

# Младшие штрихи первого микрометра

obj_L_A_y_1 = c(0, 0)
obj_L_A_y_2 = c(3,  4)
obj_L_A_y_3 = c(59,  26.5)

obj_L_A_y_dec = obj_L_A_y_1 + obj_L_A_y_2 / 60 + obj_L_A_y_3 / 3600
obj_L_A_y_dec

# Старшие штрихи первого микрометра (далее по аналогии)

obj_L_A_o_1 = c(0, 0)
obj_L_A_o_2 = c(3,  4)
obj_L_A_o_3 = c(58, 25.5)

obj_L_A_o_dec = obj_L_A_o_1 + obj_L_A_o_2 / 60 + obj_L_A_o_3 / 3600
obj_L_A_o_dec

obj_L_A_dec = (obj_L_A_y_dec + obj_L_A_o_dec) / 2
obj_L_A_dec


obj_L_B_y_1 = c(180, 180)
obj_L_B_y_2 = c(3,  3)
obj_L_B_y_3 = c(20,  24)

obj_L_B_y_dec = obj_L_B_y_1 + obj_L_B_y_2 / 60 + obj_L_B_y_3 / 3600
obj_L_B_y_dec

obj_L_B_o_1 = c(180, 180)
obj_L_B_o_2 = c( 3,  3)
obj_L_B_o_3 = c( 11,  27)

obj_L_B_o_dec = obj_L_B_o_1 + obj_L_B_o_2 / 60 + obj_L_B_o_3 / 3600
obj_L_B_o_dec

obj_L_B_dec = (obj_L_B_y_dec + obj_L_B_o_dec) / 2
obj_L_B_dec

K_R = (obj_L_A_dec + obj_L_B_dec) / 2
K_R

show_in_the_degree_system(K_R)

# Вычисление KL

obj_R_A_y_1 = c(180, 180)
obj_R_A_y_2 = c( 2,  2)
obj_R_A_y_3 = c( 41,  55)

obj_R_A_y_dec = obj_R_A_y_1 + obj_R_A_y_2 / 60 + obj_R_A_y_3 / 3600
obj_R_A_y_dec

obj_R_A_o_1 = c(180, 180)
obj_R_A_o_2 = c( 2,  2)
obj_R_A_o_3 = c( 48,  55)

obj_R_A_o_dec = obj_R_A_o_1 + obj_R_A_o_2 / 60 + obj_R_A_o_3 / 3600
obj_R_A_o_dec

obj_R_A_dec = (obj_R_A_y_dec + obj_R_A_o_dec) / 2
obj_R_A_dec


obj_R_B_y_1 = c(0, 0)
obj_R_B_y_2 = c(1,  1)
obj_R_B_y_3 = c(31, 32)

obj_R_B_y_dec = obj_R_B_y_1 + obj_R_B_y_2 / 60 + obj_R_B_y_3 / 3600
obj_R_B_y_dec

obj_R_B_o_1 = c(0, 0)
obj_R_B_o_2 = c(1, 1)
obj_R_B_o_3 = c(24, 37)

obj_R_B_o_dec = obj_R_B_o_1 + obj_R_B_o_2 / 60 + obj_R_B_o_3 / 3600
obj_R_B_o_dec

obj_R_B_dec = (obj_R_B_y_dec + obj_R_B_o_dec) / 2
obj_R_B_dec

K_L = (obj_R_A_dec + obj_R_B_dec) / 2
K_L

show_in_the_degree_system(K_L)

# Двадцать девятая строчка: L, R

L = mean(K_L)
R = mean(K_R)

show_in_the_degree_system(L)
show_in_the_degree_system(R)

# Тридцатая строчка: с_1 (в формуле указан плюс/минус)
R

c_1 = (L - (R - 180)) / 2

show_in_the_degree_system(c_1)

# Тридцать первая строчка: с_2

c_2 = (M_L - M_R) / p

show_in_the_degree_system(c_2)

# Тридцать вторая строчка: с

c = 1 / 2 * (c_1 + c_2)

show_in_the_degree_system(c)

# Тридцать третья строчка: M_s

M_s = (M_L + M_R) / 2 - q * c

show_in_the_degree_system(M_s)

# Тридцать четвёртая строчка: a (азимут) (в формуле указан плюс/минус)

a = (L + (R - 180)) / 2 - M_s

show_in_the_degree_system(a)
