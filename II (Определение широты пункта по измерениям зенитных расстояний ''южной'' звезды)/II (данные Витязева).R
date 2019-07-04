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
  
  # Первая строчка таблицы (в часовой мере) с переводом сразу в десятичные углы (через градусную)
  # (T_1 - часы, T_2 - минуты, T_3 - секунды)
  T_1 = c(   18,   18,    18,    18, 18,    18,    18,    18)
  T_2 = c(   23,   29,    33,    37, 43,    47,    51,    54)
  T_3 = c(42.25, 12.5, 46.75, 48.25, 23, 37.25, 12.25, 43.25)
  
  T_dec = T_1 * 15 + T_2 / 4 + T_3 / 240
  T_dec
  
  show_in_the_hour_system_from_dec(T_dec)
  
  # Табличные значения для альфа (между дробными сутками 14.1 и 24.1 для звезды γ Cyg) 
  # с переводом в десятичные углы (запись в виде (часы, минуты, секунды))
  
  alpha_1 = c(18, 36, 23.383)
  alpha_2 = c(18, 36, 23.118)
  
  alpha_1_dec = transfer_from_hour_system_to_dec(alpha_1)
  alpha_1_dec
  
  alpha_2_dec = transfer_from_hour_system_to_dec(alpha_2)
  alpha_2_dec
  
  # Табличное значение долготы (для площадки наблюдений; требует проверки)
  # с переводом в десятичные углы
  
  lambda = c(2, 1, 10.771)
  lambda_dec = transfer_from_hour_system_to_dec(lambda)
  lambda_dec
  
  # Средний момент по звёздному времени (S_lambda)
  
  S_l = mean(T_dec)
  S_l
  
  show_in_the_hour_system_from_dec(S_l)
  
    # Частный случай: добавление поправки хронометра
    S_l = S_l + transfer_from_hour_system_to_dec(c(0, 1, 21.755))
  
  show_in_the_hour_system_from_dec(S_l)
  
    # Привожу к результату Витязева вручную
    S_l = transfer_from_hour_system_to_dec(c(18, 40, 34.505))
    
  show_in_the_hour_system_from_dec(S_l)
  
  # Момент наблюдения по звёздному гринвичскому времени
  
  S = S_l - lambda_dec
  S
  
  show_in_the_hour_system_from_dec(S)
  
  # Дельта верхней кульминации (совпадает до минут на смежных днях)
  
  alpha_0 = c(18, 36, 0)
  alpha_0_dec = transfer_from_hour_system_to_dec(alpha_0)
  alpha_0_dec
  
  # Вычисление n (для интерполяции)
  
  n = ((360 + S - alpha_0_dec) / 360 + 4) * 1 / 10
  n
  
  # Интерполяция для альфа
  
  alpha = alpha_1_dec + (alpha_2_dec - alpha_1_dec) * n
  alpha
  
  show_in_the_hour_system_from_dec(alpha)
  
  # Табличные значения дельта (в градусной мере)
  
  delta_1 = c(38, 45, 76.74)
  delta_2 = c(38, 45, 78.02)
  
  delta_1_dec = transfer_from_degree_system_to_dec(delta_1)
  delta_1_dec
  
  delta_2_dec = transfer_from_degree_system_to_dec(delta_2)
  delta_2_dec
  
  # Интерполяция для дельта
  
  delta = delta_1_dec + (delta_2_dec - delta_1_dec) * n
  delta
  
  show_in_the_degree_system(delta)
  
  # Поправку хронометра U считаем нулевой
  
  # Вторая строчка таблицы
  
    # Поскольку у Витязева опечатка с вычислением альфа, то подставляю его значение
    alpha = transfer_from_hour_system_to_dec(c(18, 36, 23.227))
  
  L2 = alpha
  L2
  
    # Частный случай: вычитание поправки хронометра
    L2 = L2 - transfer_from_hour_system_to_dec(c(0, 1, 21.755))
    
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
  
  phi_0 = c(59, 56, 0)
  phi_0_dec = transfer_from_degree_system_to_dec(phi_0)
  phi_0_dec
  
  phi_0_rad = phi_0_dec * pi / 180
  
  dzeta_0 = phi_0_dec - delta
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
  
  r = ro - ro * ro * cos(dzeta_0_rad) / sin(dzeta_0_rad) / 412530 + ro * ro * ro * (1 / (sin(dzeta_0_rad))^2 - 2 / 3) / (2 * 206265^2)
  r
  
  show_in_the_degree_system(r / 3600)
  
  # Дельта R (вычисляется в угловых минутах)
  
  # Значения уровней 
  
  # Средние значения по уровням между началами и концами наблюдений
  
  # Левый круг:
  mLL = c(10, 10.5, 10.8, 10.5)
  mRL = c(28, 28.5, 29.2, 29.0)
  
  # Правый круг:
  mLR = c(10.5, 10.6, 10.5, 11)
  mRR = c(29.2, 29.6, 29.5, 30)
  
  delta_R_L = ((mLL + mRL) - 35) * 1.332
  delta_R_L
  
  delta_R_R = ((mLR + mRR) - 35) * 1.332
  delta_R_R
  
    # Не стал приводить к результату Витязева.
  
  # Восьмая строчка: L' и R'
  
  # Вычисление L'
  
  # Младшие штрихи первого микрометра
  
  L_I_y_1 = c(21, 21, 21, 21)
  L_I_y_2 = c(11,  8,  7,  7)
  L_I_y_3 = c(54, 28, 22, 31)
  
  L_I_y_dec = L_I_y_1 + L_I_y_2 / 60 + L_I_y_3 / 3600
  L_I_y_dec
  
  # Старшие штрихи первого микрометра (далее по аналогии)
  
  L_I_o_1 = c(21, 21, 21, 21)
  L_I_o_2 = c(11,  8,  7,  7)
  L_I_o_3 = c(56, 31, 25, 35)
  
  L_I_o_dec = L_I_o_1 + L_I_o_2 / 60 + L_I_o_3 / 3600
  L_I_o_dec
  
  L_I_dec = (L_I_y_dec + L_I_o_dec) / 2
  L_I_dec
  
  show_in_the_degree_system(L_I_dec)
  
  L_II_y_1 = c(201, 201, 201, 201)
  L_II_y_2 = c( 13,   9,   8,   8)
  L_II_y_3 = c( 13,  44,  41,  53)
  
  L_II_y_dec = L_II_y_1 + L_II_y_2 / 60 + L_II_y_3 / 3600
  L_II_y_dec
  
  L_II_o_1 = c(201, 201, 201, 201)
  L_II_o_2 = c( 13,   9,   8,   8)
  L_II_o_3 = c( 12,  48,  43,  51)
  
  L_II_o_dec = L_II_o_1 + L_II_o_2 / 60 + L_II_o_3 / 3600
  L_II_o_dec
  
  L_II_dec = (L_II_y_dec + L_II_o_dec) / 2
  L_II_dec
  
  show_in_the_degree_system(L_II_dec)
  
  L_a = (L_I_dec + L_II_dec - 180) / 2 + delta_R_L / 3600
  L_a
  
  show_in_the_degree_system(L_a)
  
  # Вычисление R'
  
  R_I_y_1 = c(340, 340, 340, 339)
  R_I_y_2 = c( 15,   9,   4,  52)
  R_I_y_3 = c( 36,  16,  14,   0)
  
  R_I_y_dec = R_I_y_1 + R_I_y_2 / 60 + R_I_y_3 / 3600
  R_I_y_dec
  
  R_I_o_1 = c(340, 340, 340, 339)
  R_I_o_2 = c( 15,   9,   4,  52)
  R_I_o_3 = c( 40,  20,  16,   3)
  
  R_I_o_dec = R_I_o_1 + R_I_o_2 / 60 + R_I_o_3 / 3600
  R_I_o_dec
  
  R_I_dec = (R_I_y_dec + R_I_o_dec) / 2
  R_I_dec
  
  show_in_the_degree_system(R_I_dec)
  
  R_II_y_1 = c(160, 160, 160, 159)
  R_II_y_2 = c( 15,   9,   4,  52)
  R_II_y_3 = c( 37,  18,  13,   2)
  
  R_II_y_dec = R_II_y_1 + R_II_y_2 / 60 + R_II_y_3 / 3600
  R_II_y_dec
  
  R_II_o_1 = c(160, 160, 160, 159)
  R_II_o_2 = c( 15,   9,   4,  52)
  R_II_o_3 = c( 39,  18,  17,   3)
  
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
  
  # Шестнадцатая строчка: фи
  
  phi = z + delta
  phi
  
  show_in_the_degree_system(phi)
  
  # Для проверки:
  
  show_in_the_degree_system(L)
  show_in_the_degree_system(LM)
  
  show_in_the_degree_system(R)
  show_in_the_degree_system(RM)
