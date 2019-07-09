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
  
  # Функция для перевода из градусов в радианы
  
  rad = function(x) {
    temp5 = x * pi / 180
    return(temp5)
  }
  
  # Данные по инструменту
  
# R = 155.107 # 107
# R = 155.19  # 104
  R = 154.96  # 103
  
# tau2 = 1.336 # 107
# tau2 = 1.434 # 104
  tau2 = 1.225 # 103
  
  tau2_dec = tau2 / 3600
  
  # Если номер северной звезды меньше 1000 (звезда наблюдается в
  # нижней кульминации), измените знак на -1
  
  sign_val = 1
  
  # Вычисление величин по алгоритму, описанному выше таблиц с примером
  
  # Средние значения микрометров
  
  M_north = c(18.14, 18.21, 18.22, 18.265, 18.255, 18.25) 
  Mn = mean(M_north)
  Mn
  
  M_south = c(4.275, 4.220, 4.225, 4.240, 4.240)
  Ms = mean(M_south)
  Ms
  
  # Поправка за уровень
  
  value_1 = R * (Ms - Mn) / 2 # в секундах
  value_1
  
  # Среднее между суммами левого и правого краёв уровней до и после для северной и южной звезды
  
  JIuIIn = mean(c(24 + 32, 24 + 32))
  JIuIIn 
  
  JIuIIs = mean(c(22 + 30, 22 + 30))
  JIuIIs
  
  P = (JIuIIs - JIuIIn) * tau2 / 2 # в секундах
  P
  
  # Величины с верхней части журнала наблюдений;
  # большинство значений из таблицы с парами Талькотта.
  
  deltaN = c(70, 13.3, 0) # Звезда 1568 (беру значения из таблицы пар Талькотта на старую эпоху)
  deltaN_dec = transfer_from_degree_system_to_dec(deltaN)
  deltaN_rad = rad(deltaN_dec)
  
  tgdN = tan(deltaN_rad)
  tgdN
  
  deltaS = c(50, 3.2, 0) # Звезда 1598
  deltaS_dec = transfer_from_degree_system_to_dec(deltaS)
  deltaS_rad = rad(deltaS_dec)
  
  tgdS = tan(deltaS_rad)
  tgdS
  
  SN = c(19, 48.2, 0)
  SN_dec = transfer_from_hour_system_to_dec(SN)
  
  SS = c(20, 0.9, 0)
  SS_dec = transfer_from_hour_system_to_dec(SS)
  
  # Число нитей, на которых сделаны отсчёты микрометра
  thr_num_1 = 5
  thr_num_2 = 5
  
  Fn = (301.7^2 + 149.8^2 + 150.8^2 + 302.9^2 + 447.8^2) / 825060 / (thr_num_1 + 1)
  Fn # в секундах
    
  Fs = (447.0^2 + 301.7^2 + 150.8^2 + 302.9^2 + 447.8^2) / 825060 / (thr_num_2 + 1) 
  Fs # в секудах
  
  z_0 = c(10, 5.1, 0)
  z_0_dec = transfer_from_degree_system_to_dec(z_0)
  z_0_rad = rad(z_0_dec)
  
  sec2_z = (1 / cos(z_0_rad)) ^ 2
  sec2_z
  
  # Далее по алгоритму выше таблиц
 
  dRo = 2.826 * 10^(-4) * (Ms - Mn) * (R / 2) * sec2_z
  dRo
  
  Fs * tgdS
  Fn * tgdN
  
  zs_zn_half = (Ms - Mn) * (R / 2) + P + dRo + (Fs * tgdS + sign_val * Fn * tgdN)
  zs_zn_half
  
  # В последнем слагаемом знак минус берётся в том случае, когда северная звезда
  # наблюдается в нижней кульминации
  
  lamda = c(2, 1.18, 0)
  lamda_dec = transfer_from_hour_system_to_dec(lamda)
  
  S_l_dec = mean(c(SS_dec, SN_dec))
  show_in_the_hour_system_from_dec(S_l_dec)
  
  S = S_l_dec - lamda_dec
  show_in_the_hour_system_from_dec(S)
  
  n = S / 360
  n
  
  # Блок с редукционными величинами (интерполяция по табличным данным)
  
  tau_0 = 0.0031
  tau_1 = 0.0058
  
  A_A_0 = -6.235
  A_A_1 = -6.137
  
  B_B_0 = 3.337
  B_B_1 = 3.267
  
  C_0 = 3.77
  C_1 = 4.077
  
  D_0 = -19.728
  D_1 = -19.655
  
  tau = tau_0 + (tau_1 - tau_0) * n
  tau
  
  A_A = A_A_0 + (A_A_1 - A_A_0) * n
  A_A
  
  B_B = B_B_0 + (B_B_1 - B_B_0) * n
  B_B
  
  C = C_0 + (C_1 - C_0) * n
  C
  
  D = D_0 + (D_1 - D_0) * n
  D
  
  # Блок с полусуммами или полуразностями редукционных величин
  
  # Вектора из редукционных данных, взятых из таблицы
  # (сначала для северной звезды) (1568 / 1598)
  
  mu_a_v = c(0.040,  0.006)
  a_a_v =  c(0.456,  0.507)
  b_a_v =  c(0.8898, 0.8618)
  c_a_v =  c(0.9839, 0.9396)
  d_a_v =  c(0.4296, 0.3895)
  
  # Функция для вычисления полусуммы или полуразности редукционных величин
  # (после определения значения sign_val)
  
  half_sum_diff = function(x) {
    
    temp6 = (x[2] + sign_val * x[1]) / 2
    
  }
  
  mu_a = half_sum_diff(mu_a_v)
  mu_a
  
  a_a = half_sum_diff(a_a_v)
  a_a
  
  b_a = half_sum_diff(b_a_v)
  b_a
  
  c_a = half_sum_diff(c_a_v)
  c_a
  
  d_a = half_sum_diff(d_a_v)
  d_a
  
  # Четвертый блок таблицы вычислений
  
  # Здесь должны использоваться не deltaS_dec и delta_N_dec, а склонения,
  # посчитанные на эпоху наблюдений.
  
  deltaN_now = c(70, 19, 2.99)
  deltaS_now = c(50, 10, 23.01) 
  
  deltaN_now_dec = transfer_from_degree_system_to_dec(deltaN_now)
  deltaS_now_dec = transfer_from_degree_system_to_dec(deltaS_now)
  
  if (sign_val == 1) {
    
    d0 = 0.5 * (deltaS_now_dec + deltaN_now_dec)
    
  } else {
    
    d0 = 90 + 0.5 * (deltaS_now_dec - deltaN_now_dec)
    
  }
  
  d0
  show_in_the_degree_system(d0)
  
  dd0 = A_A * a_a + B_B * b_a + C * c_a + D * d_a + tau * mu_a
  dd0_dec = dd0 / 3600
  
  show_in_the_degree_system(dd0_dec)
  
  delta = d0 + dd0_dec
  show_in_the_degree_system(delta)
  
  zs_zn_half
  zs_zn_half_dec = zs_zn_half / 3600
  show_in_the_degree_system(zs_zn_half_dec)
  
  phi = delta + zs_zn_half_dec
  phi
  
  show_in_the_degree_system(phi)
  
  # Для проверки:
  
  # M_north - M_south
  # show_in_the_degree_system(value_1 / 3600)
  # show_in_the_degree_system(P / 3600)
  # show_in_the_degree_system((Fs * tgdS + sign_val * Fn * tgdN) / 3600)
  # show_in_the_degree_system(zs_zn_half_dec)
  