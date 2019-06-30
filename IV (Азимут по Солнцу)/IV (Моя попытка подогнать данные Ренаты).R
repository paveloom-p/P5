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

T_1_L = c(6, 21, 45.5, 6, 24, 3)
T_1_L_dec = c(transfer_from_hour_system_to_dec(T_1_L[1:3]), transfer_from_hour_system_to_dec(T_1_L[4:6]))

# Второе наблюдение, левый круг (далее по аналогии)

T_2_L = c(6, 29, 6.5, 6, 31, 26)
T_2_L_dec = c(transfer_from_hour_system_to_dec(T_2_L[1:3]), transfer_from_hour_system_to_dec(T_2_L[4:6]))

T_1_R = c(6, 45, 47.5, 6, 48, 6)
T_1_R_dec = c(transfer_from_hour_system_to_dec(T_1_R[1:3]), transfer_from_hour_system_to_dec(T_1_R[4:6]))

T_2_R = c(6, 51, 40.5, 6, 53, 59)
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

    # Подгоняю под средние моменты у Витязева, поскольку не понимаю, как он их получил (в дальнейшем этот
    # блок нужно будет убрать)

#    T_1_L_mean_test = c(13, 27, 42.62)
#    T_2_L_mean_test = c(13, 34, 07.25)
#    T_1_R_mean_test = c(13, 49, 18.75)
#    T_2_R_mean_test = c(13, 55, 53.75)

#    T_1_L_mean = transfer_from_hour_system_to_dec(T_1_L_mean_test)
#    T_2_L_mean = transfer_from_hour_system_to_dec(T_2_L_mean_test)
#    T_1_R_mean = transfer_from_hour_system_to_dec(T_1_R_mean_test)
#    T_2_R_mean = transfer_from_hour_system_to_dec(T_2_R_mean_test)
  
#    show_in_the_hour_system_from_dec(T_1_L_mean)
#    show_in_the_hour_system_from_dec(T_2_L_mean)
#    show_in_the_hour_system_from_dec(T_1_R_mean)
#    show_in_the_hour_system_from_dec(T_2_R_mean)

# Вторая строчка: поправку времени хронометра (U_dec) считаем равной нулю, но для сохранения
# аутентичности эксперимента использую значение Витязева

#U = c(0, 1, 10.421)
U=c(0,0,0)
U_dec = transfer_from_hour_system_to_dec(U)


# Третья строчка: средние моменты наблюдений с учётом поправки хронометра

S_l = c(T_1_L_mean + U_dec, T_2_L_mean + U_dec, T_1_R_mean + U_dec, T_2_R_mean + U_dec)

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
    
    # Видимое склонение (в часовой мере)
    
    delta_24 = c(23, 25,  0.52)
    delta_25 = c(23, 23, 50.23)
    
    delta_24_dec = transfer_from_degree_system_to_dec(delta_24)
    delta_25_dec = transfer_from_degree_system_to_dec(delta_25)
    
    # Часовые изменения уравнения времени на начало текущих и следующих суток
    # (у Витязева: teta_0 и teta_1)
    
    alpha_teta_24 = c(0, 0, -0.5368)
    alpha_teta_25 = c(0, 0, -0.5335)
    
    alpha_teta_24_dec = transfer_from_hour_system_to_dec(alpha_teta_24)
    alpha_teta_25_dec = transfer_from_hour_system_to_dec(alpha_teta_25)
    
    delta_teta_24 = c(0, 0, -2.413)
    delta_teta_25 = c(0, 0, -3.443)
    
    delta_teta_24_dec = transfer_from_degree_system_to_dec(delta_teta_24)
    delta_teta_25_dec = transfer_from_degree_system_to_dec(delta_teta_25)
    
    # Истинное звёздное время на 24 июня (табличное значение в часовой мере)
    
    S_0 = c(18, 07, 26.1669)
    S_0_dec = transfer_from_hour_system_to_dec(S_0) 
    
    # Множитель перевода звёздного времени в среднее
    nu = 0.0027304336

    # Поправка, с помощью которой всемирное время переводится в земное время (в часовой мере)
    
    delta_T = c(0,0,70)
    delta_T_dec = transfer_from_hour_system_to_dec(delta_T)
    
    # Здесь начинается та часть алгоритма, которая вызывает сомнение:
    
    # Разница S - S_0 (в моих обозначениях: S - S_24)
    
    S_diff = 360 + (S - S_0_dec)
    S_diff
    
    show_in_the_hour_system_from_dec(S_diff)
    
    # Слагаемое nu * (S - S_0)
    
    S_fix = nu * S_diff
    
    # Величина M
    
    M = Mod(S_diff - S_fix)
    show_in_the_hour_system_from_dec(M)
    
    # Величина M*
    
    M_ast = M + delta_T_dec
    show_in_the_hour_system_from_dec(M_ast)
    
    M_ast_hour = M_ast / 15
    
    M_ast_hour
    
    # Значение teta'_0
    
    alpha_teta_a = transfer_from_hour_system_to_dec(c(0, 0, 9.856)) - alpha_teta_24_dec
    show_in_the_hour_system_from_dec(alpha_teta_a)
    
    # Значение разницы teta_0 - teta_1
    
    alpha_teta_diff = alpha_teta_24_dec - alpha_teta_25_dec
    
    # Получение значения прямого восхождения (берем только первое и последнее значения)
    
    alpha = alpha_24_dec + M_ast_hour * (alpha_teta_a + M_ast_hour / 48 * alpha_teta_diff)
    show_in_the_hour_system_from_dec(alpha)
    
    # получение alpha_2,  alpha_3 
    
    alpha[2] = alpha[1] + (T_2_L_mean - T_1_L_mean)/(T_2_R_mean - T_1_L_mean)*(alpha[4] - alpha[1])
    show_in_the_hour_system_from_dec(alpha[2])
    
    alpha[3] = alpha[1] + (T_1_R_mean - T_1_L_mean)/(T_2_R_mean - T_1_L_mean)*(alpha[4] - alpha[1])
    show_in_the_hour_system_from_dec(alpha[3])
    
    show_in_the_hour_system_from_dec(alpha)
    
    # Получение значения склонения
    
    delta = delta_24_dec + M_ast_hour * (delta_teta_24_dec + M_ast_hour / 48 * (delta_teta_25_dec - delta_teta_24_dec))
    
    show_in_the_degree_system(delta)
    
    # получение delta_2,  delta_3 
    
    delta[2] = delta[1] + (T_2_L_mean - T_1_L_mean)/(T_2_R_mean - T_1_L_mean)*(delta[4] - delta[1])
    show_in_the_degree_system(delta[2])
    
    delta[3] = delta[1] + (T_1_R_mean - T_1_L_mean)/(T_2_R_mean - T_1_L_mean)*(delta[4] - delta[1])
    show_in_the_degree_system(delta[3])
    
    show_in_the_degree_system(delta)
    
    
    # Поскольку нет данных, необходимых, чтобы получить результат Витязева, просто подставляю его же
    # готовый результат вручную
      
    # delta_1_test = c(3, 54, 34.5)
    # delta_2_test = c(3, 54, 28.4)
    # delta_3_test = c(3, 54, 13.9)
    # delta_4_test = c(3, 54,  7.6)
    
    # delta = c(transfer_from_degree_system_to_dec(delta_1_test), transfer_from_degree_system_to_dec(delta_2_test), transfer_from_degree_system_to_dec(delta_3_test), transfer_from_degree_system_to_dec(delta_4_test))
    
    
# Шестая строчка: t_i
      
t = S_l - alpha
show_in_the_hour_system_from_dec(t)

    # Опять же, для проверки следующих алгоритмов подставляю известные значения t_i:

    #t_1 = c(2, 5, 9.371)
    #t_2 = c(2, 11, 33.041)
    #t_3 = c(2, 26, 42.271)
    #t_4 = c(2, 33, 16.291)
    
    #t = c(transfer_from_hour_system_to_dec(t_1), transfer_from_hour_system_to_dec(t_2), transfer_from_hour_system_to_dec(t_3), transfer_from_hour_system_to_dec(t_4))
    
    #show_in_the_hour_system_from_dec(t)
    
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

  A_dec[1] = transfer_from_degree_system_to_dec(c(  4,  22,  48.9))
  A_dec[2] = transfer_from_degree_system_to_dec(c(  7,  12,     6))
  A_dec[3] = transfer_from_degree_system_to_dec(c( 13,  31,  56.1))
  A_dec[4] = transfer_from_degree_system_to_dec(c( 15,  44,  22.2))
  
  A = A_dec * pi / 180

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

# Вычисление KL'

# Младшие штрихи первого микрометра

L_A_y_1 = c(169, 172)
L_A_y_2 = c(20,  10)
L_A_y_3 = c(37,  22)

L_A_y_dec = L_A_y_1 + L_A_y_2 / 60 + L_A_y_3 / 3600
L_A_y_dec

# Старшие штрихи первого микрометра (далее по аналогии)

L_A_o_1 = c(169, 172)
L_A_o_2 = c(20,  10)
L_A_o_3 = c( 38,  18)

L_A_o_dec = L_A_o_1 + L_A_o_2 / 60 + L_A_o_3 / 3600
L_A_o_dec

L_A_dec = (L_A_y_dec + L_A_o_dec) / 2
L_A_dec


L_B_y_1 = c(349, 352)
L_B_y_2 = c( 19,  8)
L_B_y_3 = c( 28,  41)

L_B_y_dec = L_B_y_1 + L_B_y_2 / 60 + L_B_y_3 / 3600
L_B_y_dec

L_B_o_1 = c(349, 352)
L_B_o_2 = c( 19,   8)
L_B_o_3 = c( 26,  35)

L_B_o_dec = L_B_o_1 + L_B_o_2 / 60 + L_B_o_3 / 3600
L_B_o_dec

L_B_dec = (L_B_y_dec + L_B_o_dec) / 2
L_B_dec

KL_a = (L_A_dec + L_B_dec - 180) / 2
KL_a

show_in_the_degree_system(KL_a)

# Вычисление KR'

R_A_y_1 = c(358, 360)
R_A_y_2 = c( 31,  43)
R_A_y_3 = c( 36,  18)

R_A_y_dec = R_A_y_1 + R_A_y_2 / 60 + R_A_y_3 / 3600
R_A_y_dec

R_A_o_1 = c(358, 360)
R_A_o_2 = c( 31,  43)
R_A_o_3 = c( 43,  20)

R_A_o_dec = R_A_o_1 + R_A_o_2 / 60 + R_A_o_3 / 3600
R_A_o_dec

R_A_dec = (R_A_y_dec + R_A_o_dec) / 2
R_A_dec


R_B_y_1 = c(178, 180)
R_B_y_2 = c(30,  42)
R_B_y_3 = c(50,  42)

R_B_y_dec = R_B_y_1 + R_B_y_2 / 60 + R_B_y_3 / 3600
R_B_y_dec

R_B_o_1 = c(178, 180)
R_B_o_2 = c(30, 42)
R_B_o_3 = c(49, 44)

R_B_o_dec = R_B_o_1 + R_B_o_2 / 60 + R_B_o_3 / 3600
R_B_o_dec

R_B_dec = (R_B_y_dec + R_B_o_dec) / 2
R_B_dec

KR_a = (R_A_dec + R_B_dec + 180) / 2
KR_a

show_in_the_degree_system(KR_a)

# Двадцать вторая строчка: b_i

# Цена полуделения накладного уровня (в градусной мере)

tau_half = c(0, 0, 1.417)
tau_half_dec = transfer_from_degree_system_to_dec(tau_half)

# Разницы между правыми и левыми краями уровней до и после перекладки: i

# Значения уровней: (расположены парами (сначала все левые в столбце, потом все правые в столбце) 
# в порядке строчки таблицы: 1L, 2L, 1R, 2R)

L1 = c(26, 10, 8, 28)
R1 = c(16, 20, 17.5, 18)

L2 = c(9, 26.5, 27.5, 9)
R2 = c(19, 17, 17.5, 19)

# Максимальный номер штриха на уровне
m = 35

# Выбор знака у единицы зависит от положения нуля на уровне:
# если справа, то "-"; если слева, то "+".

i_1 = -1 * c(1, -1, -1, 1) * (m - (L1 + R1)) 
i_1 

i_2 = -1 * c(-1, 1, 1, -1) * (m - (L2 + R2))
i_2

b = (i_1 + i_2) / 2 * tau_half_dec 
b

show_in_the_degree_system(b)

# Двадцать третья строчка: delta_b_i

delta_b = b * cos(z) / sin_val_3
delta_b

show_in_the_degree_system(delta_b)

# Двадцать четвёртая строчка: L', R'

L_a = KL_a + delta_b[1:2]
R_a = KR_a + delta_b[3:4]

show_in_the_degree_system(L_a)
show_in_the_degree_system(R_a)

# Двадцать пятая строчка: A (Да, двенадцатая строчка тоже была про A. Ну и что теперь.)

show_in_the_degree_system(A_dec)

# Двадцать шестая строчка: M_(iL), M_(iR)

M = c(L_a - A_dec[1:2], R_a - A_dec[3:4]) - c(0, 0, 180, 180)
M

show_in_the_degree_system(M)

# Двадцать седьмая строчка: M_L, M_R

M_L = mean(M[1:2])
M_R = mean(M[3:4])

show_in_the_degree_system(M_L)
show_in_the_degree_system(M_R)

# Двадцать восьмая строчка: KL, KR

# Значения азимутов для наблюдений предмета:

# Вычисление KL

# Младшие штрихи первого микрометра

obj_L_A_y_1 = c(180, 180)
obj_L_A_y_2 = c( 2,  2)
obj_L_A_y_3 = c( 12,  8)

obj_L_A_y_dec = obj_L_A_y_1 + obj_L_A_y_2 / 60 + obj_L_A_y_3 / 3600
obj_L_A_y_dec

# Старшие штрихи первого микрометра (далее по аналогии)

obj_L_A_o_1 = c(180, 180)
obj_L_A_o_2 = c( 2,  2)
obj_L_A_o_3 = c( 10,  4)

obj_L_A_o_dec = obj_L_A_o_1 + obj_L_A_o_2 / 60 + obj_L_A_o_3 / 3600
obj_L_A_o_dec

obj_L_A_dec = (obj_L_A_y_dec + obj_L_A_o_dec) / 2
obj_L_A_dec


obj_L_B_y_1 = c(0, 0)
obj_L_B_y_2 = c( 1,  0)
obj_L_B_y_3 = c( 3,  56)

obj_L_B_y_dec = obj_L_B_y_1 + obj_L_B_y_2 / 60 + obj_L_B_y_3 / 3600
obj_L_B_y_dec

obj_L_B_o_1 = c(0, 0)
obj_L_B_o_2 = c( 1,  0)
obj_L_B_o_3 = c( 12,  48)

obj_L_B_o_dec = obj_L_B_o_1 + obj_L_B_o_2 / 60 + obj_L_B_o_3 / 3600
obj_L_B_o_dec

obj_L_B_dec = (obj_L_B_y_dec + obj_L_B_o_dec) / 2
obj_L_B_dec

K_L = (obj_L_A_dec + obj_L_B_dec + 180) / 2
K_L

show_in_the_degree_system(K_L)

# Вычисление KR

obj_R_A_y_1 = c(0, 0)
obj_R_A_y_2 = c( 3,  2)
obj_R_A_y_3 = c( 14,  56)

obj_R_A_y_dec = obj_R_A_y_1 + obj_R_A_y_2 / 60 + obj_R_A_y_3 / 3600
obj_R_A_y_dec

obj_R_A_o_1 = c(0, 0)
obj_R_A_o_2 = c( 3,  3)
obj_R_A_o_3 = c( 14,  2)

obj_R_A_o_dec = obj_R_A_o_1 + obj_R_A_o_2 / 60 + obj_R_A_o_3 / 3600
obj_R_A_o_dec

obj_R_A_dec = (obj_R_A_y_dec + obj_R_A_o_dec) / 2
obj_R_A_dec


obj_R_B_y_1 = c(180, 180)
obj_R_B_y_2 = c( 2,  1)
obj_R_B_y_3 = c( 26,  59)

obj_R_B_y_dec = obj_R_B_y_1 + obj_R_B_y_2 / 60 + obj_R_B_y_3 / 3600
obj_R_B_y_dec

obj_R_B_o_1 = c(180, 180)
obj_R_B_o_2 = c( 2,  2)
obj_R_B_o_3 = c( 24,  3)

obj_R_B_o_dec = obj_R_B_o_1 + obj_R_B_o_2 / 60 + obj_R_B_o_3 / 3600
obj_R_B_o_dec

obj_R_B_dec = (obj_R_B_y_dec + obj_R_B_o_dec) / 2
obj_R_B_dec

K_R = (obj_R_A_dec + obj_R_B_dec - 180) / 2
K_R

show_in_the_degree_system(K_R)

# Двадцать девятая строчка: L, R

L = mean(K_L)
R = mean(K_R)

show_in_the_degree_system(L)
show_in_the_degree_system(R)

# Тридцатая строчка: с_1 (в формуле указан плюс/минус)

c_1 = (L - (R + 180)) / 2

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

a = (L + (R + 180)) / 2 - M_s

show_in_the_degree_system(a)

# Для проверки:

delta_c = abs(c_1 - c_2)
show_in_the_degree_system(delta_c)

show_in_the_degree_system(A_dec)

show_in_the_degree_system(180 + A_dec)
show_in_the_hour_system_from_dec(S_l)

show_in_the_hour_system_from_dec(alpha)
show_in_the_degree_system(delta)

show_in_the_degree_system(transfer_from_degree_system_to_dec(c(184, 22, 48.9)) - 180)
show_in_the_degree_system(transfer_from_degree_system_to_dec(c(187, 12,  6.0)) - 180)
show_in_the_degree_system(transfer_from_degree_system_to_dec(c(193, 31, 56.1)) - 180)
show_in_the_degree_system(transfer_from_degree_system_to_dec(c(195, 44, 22.2)) - 180)

show_in_the_degree_system(a)
