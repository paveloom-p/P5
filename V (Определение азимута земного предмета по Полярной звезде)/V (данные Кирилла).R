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
  # Первая строчка: средние моменты наблюдений, T_(iL) и T_(iR)
  
  # Моменты наблюдений по рабочему звёздному хронометру (в часовой мере два значения подряд)
  
  # Первое наблюдение, левый круг
  
  
  T_1_L = c(16, 57, 46.5)
  T_1_L_dec = transfer_from_hour_system_to_dec(T_1_L)
  # Второе наблюдение, левый круг (далее по аналогии)
  
  T_2_L = c(17, 3, 33)
  T_2_L_dec = transfer_from_hour_system_to_dec(T_2_L)
  
  T_1_R = c(17, 26, 59.5)
  T_1_R_dec = transfer_from_hour_system_to_dec(T_1_R)

  T_2_R = c(17, 33, 37.5)
  T_2_R_dec = transfer_from_hour_system_to_dec(T_2_R)
  
  # Вторая строчка: поправку времени хронометра (U) считаем равной нулю
  
  T_L = 0.5*(T_1_L_dec+T_2_L_dec)
  T_R = 0.5*(T_1_R_dec+T_2_R_dec)
  
  show_in_the_hour_system_from_dec(T_L)
  show_in_the_hour_system_from_dec(T_R)
  
  # Третья строчка: z
  phi = 59.942248
  f = c(transfer_from_degree_system_to_dec(c(0,-33.6,0)), transfer_from_degree_system_to_dec(c(0,-30.6,0)))
  z = 90 - (phi + f)
  z
  show_in_the_degree_system(z)
  
  z_rad = z * pi / 180
  
  # 4
  
  cscz = 1/sin(z_rad)
  cscz
  
  # 5
  
  ctgz = 1/tan(z_rad)
  ctgz
  
  # 6
  
  p = cscz[1] + cscz[2]
  p
  
  # 7
  q = 0.5*(cscz[1] - cscz[2])
  q
  
  # 8
  
  # Значения азимутов для наблюдений Солнца:
  
  # Вычисление KL'
  
  # Младшие штрихи первого микрометра
  
  L_A_y_1 = c(0, 0)
  L_A_y_2 = c(30, 31)
  L_A_y_3 = c(45,  7)
  
  L_A_y_dec = L_A_y_1 + L_A_y_2 / 60 + L_A_y_3 / 3600
  L_A_y_dec
  
  # Старшие штрихи первого микрометра (далее по аналогии)
  
  L_A_o_1 = c(0, 0)
  L_A_o_2 = c(30, 31)
  L_A_o_3 = c(54, 2)
  
  L_A_o_dec = L_A_o_1 + L_A_o_2 / 60 + L_A_o_3 / 3600
  L_A_o_dec
  
  L_A_dec = (L_A_y_dec + L_A_o_dec) / 2
  L_A_dec
  
  
  L_B_y_1 = c(180, 180)
  L_B_y_2 = c(30, 31)
  L_B_y_3 = c(59, 11)
  
  L_B_y_dec = L_B_y_1 + L_B_y_2 / 60 + L_B_y_3 / 3600
  L_B_y_dec
  
  L_B_o_1 = c(180, 180)
  L_B_o_2 = c(30, 31)
  L_B_o_3 = c(49, 10)
  
  L_B_o_dec = L_B_o_1 + L_B_o_2 / 60 + L_B_o_3 / 3600
  L_B_o_dec
  
  L_B_dec = (L_B_y_dec + L_B_o_dec) / 2
  L_B_dec
  
  KL_a = (L_A_dec + L_B_dec + 180) / 2
  KL_a
  
  show_in_the_degree_system(KL_a)
  
  # Вычисление KR'
  
  R_A_y_1 = c(180, 180)
  R_A_y_2 = c(40,  43)
  R_A_y_3 = c(52,  14)
  
  R_A_y_dec = R_A_y_1 + R_A_y_2 / 60 + R_A_y_3 / 3600
  R_A_y_dec
  
  R_A_o_1 = c(180, 180)
  R_A_o_2 = c(40,  43)
  R_A_o_3 = c(56,  18)
  
  R_A_o_dec = R_A_o_1 + R_A_o_2 / 60 + R_A_o_3 / 3600
  R_A_o_dec
  
  R_A_dec = (R_A_y_dec + R_A_o_dec) / 2
  R_A_dec
  
  
  R_B_y_1 = c(0, 0)
  R_B_y_2 = c(41, 43)
  R_B_y_3 = c(4, 29)
  
  R_B_y_dec = R_B_y_1 + R_B_y_2 / 60 + R_B_y_3 / 3600
  R_B_y_dec
  
  R_B_o_1 = c(0, 0)
  R_B_o_2 = c(40, 43)
  R_B_o_3 = c(52, 29)
  
  R_B_o_dec = R_B_o_1 + R_B_o_2 / 60 + R_B_o_3 / 3600
  R_B_o_dec
  
  R_B_dec = (R_B_y_dec + R_B_o_dec) / 2
  R_B_dec
  
  KR_a = (R_A_dec + R_B_dec - 180) / 2
  KR_a
  
  show_in_the_degree_system(KR_a)
  
  # Двадцать вторая строчка: b_i
  
  # Цена полуделения накладного уровня (в градусной мере)
  
  tau_half = c(0, 0, 1.328)
  tau_half_dec = transfer_from_degree_system_to_dec(tau_half)
  
  # Разницы между правыми и левыми краями уровней до и после перекладки: i
  
  # Значения уровней: (расположены парами (сначала все левые в столбце, потом все правые в столбце) 
  # в порядке строчки таблицы: 1L, 2L, 1R, 2R)
  
  L1 = c(11, 10, 8.5, 8)
  R1 = c(20, 19, 17.5, 17)
  
  L2 = c(22.5, 20, 23, 23)
  R2 = c(13.5, 11, 14, 14)
  
  # Максимальный номер штриха на уровне
  m = 35
  
  # Выбор знака у единицы зависит от положения нуля на уровне:
  # если справа, то "-"; если слева, то "+".
  
  i_1 = c(1, -1, -1, 1)  * (m - (L1 + R1))
  i_1
  
  i_2 = c(-1, 1, 1, -1)  * (m - (L2 + R2))
  i_2
  
  b = (i_1 + i_2) / 2 * tau_half_dec 
  b
  
  show_in_the_degree_system(b)
  
  # Двадцать третья строчка: delta_b_i
  
  delta_b = b / tan(z_rad)
  delta_b
  
  show_in_the_degree_system(delta_b)
  
  # Двадцать четвёртая строчка: L', R'
  
  L_a = KL_a - delta_b[1:2]
  R_a = KR_a - delta_b[3:4] 
  
  show_in_the_degree_system(L_a)
  show_in_the_degree_system(R_a)
  
  S = 0.5*(T_L + T_R)

  show_in_the_hour_system_from_dec(S)
  
  # Табличные значения для альфа (между дробными сутками 18.4 и 19.4) 
  # с переводом в десятичные углы (запись в виде (часы, минуты, секунды))
  
  alpha_28 = c(2, 54, 56.81)
  alpha_29 = c(2, 54, 58.65)
  
  alpha_29_dec = alpha_29[1] * 15 + alpha_29[2] / 4 + alpha_29[3] / 240
  alpha_29_dec
  
  alpha_28_dec = alpha_28[1] * 15 + alpha_28[2] / 4 + alpha_28[3] / 240
  alpha_28_dec
  
  # Табличное значение долготы (для площадки наблюдений; требует проверки)
  # с переводом в десятичные углы
  
  
  # Дельта верхней кульминации (совпадает до минут на смежных днях)
  
  alpha_0 = c(2, 54)
  alpha_0_dec = alpha_0[1] * 15 + alpha_0[2] / 4
  alpha_0_dec
  
  # Вычисление n (для интерполяции)
  
  n = (S - alpha_0_dec) / 360
  n
  
  # Интерполяция для альфа
  
  alpha = alpha_28_dec + (alpha_29_dec - alpha_28_dec) * n
  alpha
  show_in_the_hour_system_from_dec(alpha)
  
  # Табличные значения дельта (в градусной мере)
  delta_18 = c(89, 20, 22.61)
  delta_19 = c(89, 20, 22.45)
  
  delta_18_dec = delta_18[1] + delta_18[2] / 60 + delta_18[3] / 3600
  delta_18_dec
  
  delta_19_dec = delta_19[1] + delta_19[2] / 60 + delta_19[3] / 3600
  delta_19_dec
  
  # Интерполяция для дельта
  
  delta = delta_18_dec + (delta_18_dec - delta_19_dec) * n
  delta
  show_in_the_degree_system(delta)
  
  t = c(T_1_L_dec, T_2_L_dec, T_1_R_dec, T_2_R_dec) - alpha
  show_in_the_hour_system_from_dec(t)
  
  t_rad = t * pi / 180
  
  sint = sin(t_rad)
  sint
  
  cost = cos(t_rad)
  cost
  
  delta_rad = delta * pi / 180
  
  m = 1.996522/tan(delta_rad)
  m
  n = 1.728034/tan(delta_rad)
  n
  
  # Одиннадцатая строчка: tan(A_i)
  
  tgA = (-1 * m * sint) / (1 - n * cost)
  tgA
  
  # Двенадцатая строчка: A_i
  
  A = atan(tgA)
  A_dec = A * 180 / pi + c(180, 180, 0, 0)
  A_dec
  show_in_the_degree_system(A_dec)
  
  show_in_the_degree_system(L_a)
  show_in_the_degree_system(R_a)
  
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
  
  # Вычисление KL
  
  # Младшие штрихи первого микрометра
  
  obj_L_A_y_1 = c(194, 194)
  obj_L_A_y_2 = c( 55,  55)
  obj_L_A_y_3 = c( 39,  31)
  
  obj_L_A_y_dec = obj_L_A_y_1 + obj_L_A_y_2 / 60 + obj_L_A_y_3 / 3600
  obj_L_A_y_dec
  
  # Старшие штрихи первого микрометра (далее по аналогии)
  
  obj_L_A_o_1 = c(194, 194)
  obj_L_A_o_2 = c( 55,  55)
  obj_L_A_o_3 = c( 43,  30)
  
  obj_L_A_o_dec = obj_L_A_o_1 + obj_L_A_o_2 / 60 + obj_L_A_o_3 / 3600
  obj_L_A_o_dec
  
  obj_L_A_dec = (obj_L_A_y_dec + obj_L_A_o_dec) / 2
  obj_L_A_dec
  
  
  obj_L_B_y_1 = c(14,  14)
  obj_L_B_y_2 = c(55,  55)
  obj_L_B_y_3 = c(40, 34)
  
  obj_L_B_y_dec = obj_L_B_y_1 + obj_L_B_y_2 / 60 + obj_L_B_y_3 / 3600
  obj_L_B_y_dec
  
  obj_L_B_o_1 = c(14,  14)
  obj_L_B_o_2 = c(55,  55)
  obj_L_B_o_3 = c(35.5, 34)
  
  obj_L_B_o_dec = obj_L_B_o_1 + obj_L_B_o_2 / 60 + obj_L_B_o_3 / 3600
  obj_L_B_o_dec
  
  obj_L_B_dec = (obj_L_B_y_dec + obj_L_B_o_dec) / 2
  obj_L_B_dec
  
  K_L = (obj_L_A_dec + obj_L_B_dec + 180) / 2
  K_L
  
  show_in_the_degree_system(K_L)
  
  # Вычисление KR
  
  obj_R_A_y_1 = c(14,  14)
  obj_R_A_y_2 = c(57,  57)
  obj_R_A_y_3 = c(1,  5)
  
  obj_R_A_y_dec = obj_R_A_y_1 + obj_R_A_y_2 / 60 + obj_R_A_y_3 / 3600
  obj_R_A_y_dec
  
  obj_R_A_o_1 = c(14,  14)
  obj_R_A_o_2 = c(57,  57)
  obj_R_A_o_3 = c(4, 13)
  
  obj_R_A_o_dec = obj_R_A_o_1 + obj_R_A_o_2 / 60 + obj_R_A_o_3 / 3600
  obj_R_A_o_dec
  
  obj_R_A_dec = (obj_R_A_y_dec + obj_R_A_o_dec) / 2
  obj_R_A_dec
  
  
  obj_R_B_y_1 = c(194,  194)
  obj_R_B_y_2 = c(57,  57)
  obj_R_B_y_3 = c(21,  16)
  
  obj_R_B_y_dec = obj_R_B_y_1 + obj_R_B_y_2 / 60 + obj_R_B_y_3 / 3600
  obj_R_B_y_dec
  
  obj_R_B_o_1 = c(194,  194)
  obj_R_B_o_2 = c(57,  57)
  obj_R_B_o_3 = c(19,  20)
  
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
  
  a = (L + (R - 180)) / 2 - M_s
  
  show_in_the_degree_system(a)

  