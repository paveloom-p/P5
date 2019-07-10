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
  
  # Поправка за уровень
  
  B_1 = c(12.0, 16.5) # Восточная звезда, до наблюдения
  A_1 = c(9.8,  15.5) # Восточная звезда, после наблюдения
  
  B_2 = c(14.1, 20)
  A_2 = c(15, 20.9)
  
  E = mean(c(B_1[1] + B_1[2], A_1[1] + A_1[2]))
  W = mean(c(B_2[1] + B_2[2], A_2[1] + A_2[2]))
  
  E
  W
  
  # Знак единицы определяется по правилу, указанному в учебнике
  
  delta_i = 1 * (W - E)
  delta_i
  
  # Моменты времени
    
  T_E_1 = c( 18, 18,   18, 18)
  T_E_2 = c( 15, 15,   15, 16)
  T_E_3 = c(3.5, 27, 45.5,  6)
  
  T_E_dec = T_E_1 * 15 + T_E_2 / 4 + T_E_3 / 240
  
  T_W_1 = c(18,  18,   18,   18,   18,   18, 18)
  T_W_2 = c(21,  21,   20,   20,   19,   19, 19)
  T_W_3 = c(25, 3.5, 42.5, 21.5, 58.5, 38.5, 18)
  
  T_W_dec = T_W_1 * 15 + T_W_2 / 4 + T_W_3 / 240
  
  show_in_the_hour_system_from_dec(1 / 2 * (T_W_dec[4:7] + T_E_dec))
  show_in_the_hour_system_from_dec(1 / 2 * (T_W_dec[4:7] - T_E_dec))
  
  T_E = mean(T_E_dec)
  T_W = mean(T_W_dec)
  
  show_in_the_hour_system_from_dec(T_E)
  show_in_the_hour_system_from_dec(T_W)
    
  # Средние значения полусумм и полуразностей моментов
    
  A_1 = mean(1 / 2 * (T_W_dec[4:7] + T_E_dec))
  A_2 = mean(1 / 2 * (T_W_dec[4:7] - T_E_dec))
  
  # Было: 274.4263
  A_1 = 274.20506

  show_in_the_hour_system_from_dec(A_1)  
  show_in_the_hour_system_from_dec(A_2)  
    
  # Далее строки из таблицы вычислений (первые значения берутся из текстового файла Трофимова,
  # здесь же я просто возьму готовое у Витязева)
    
  alpha = transfer_from_hour_system_to_dec(c(18, 20, 24.461)) # в часовой
  delta = c(37,  43,   14.44)  # в градусной
  eps   = c(-0, -24,  -23.33)  # в градусной
  beta  = c( 2,  55,   10.580) # в часовой
  
  show_in_the_hour_system_from_dec(A_2)
  
  t = transfer_from_hour_system_to_dec(beta) + A_2
  show_in_the_hour_system_from_dec(t)
  
  tg_delta = tan(rad(transfer_from_degree_system_to_dec(delta)))
  tg_delta
  
  tg_eps   = tan(rad(transfer_from_degree_system_to_dec(eps)))
  tg_eps
  
  tg_t = tan(rad(t))
  tg_t
  
  tg_m = tg_delta * tg_eps / tg_t
  tg_m
  
  m_rad = atan(tg_m)
  m = m_rad * 180 / pi
  
  m_sec = m * 3600
  m_sec
    
  cos_m = cos(rad(m))
  cos_m
  
  tg_phi = tan(rad(transfer_from_degree_system_to_dec(c(59, 56, 32.5))))
  tg_phi
  
  sin_t = sin(rad(t))
  sin_t
  
  sin_n = tg_eps * tg_phi / sin_t * cos_m
  sin_n
  
  n_rad = asin(sin_n)
  n = n_rad * 180 / pi  

  n_sec = n * 3600  
  n_sec

  r_sec = (n_sec - m_sec) / 15 # Часовые секунды
  r_sec
  
  r = r_sec / 240
  
  delta_i
  
  sec_phi = 1 / cos(rad(transfer_from_degree_system_to_dec(c(59, 56, 32.5))))
  sec_phi 
  
  tau_half = 1.225 # Цена полуделения талькоттовского уровня
  
  k_sec = tau_half * sec_phi / 30 # Часовые секунды
  k_sec  
  
  # Азимуты западной и восточной звёзд в моменты их прохождения через общий
  # круг высоты (по сути, из таблички с парами Цингера)
  
  A_W = transfer_from_degree_system_to_dec(c(70, 53, 0))
  
  csc_A_W = 1 / sin(rad(A_W))
  csc_A_W
  
  delta_u = k_sec * delta_i * csc_A_W # Часовые секунды
  delta_u  
  
  # z: также из эфемерид 
  
  z = transfer_from_degree_system_to_dec(c(35, 40, 0))
  
  cos_z = cos(rad(z))  
  cos_z
  
  delta_alpha = 0.021 * cos_z # Часовые секунды
  delta_alpha
  
  delta_u + delta_alpha
  
  show_in_the_hour_system_from_dec(A_1)

  show_in_the_hour_system_from_dec(alpha)
  
  u_k = alpha - A_1 + r + delta_u / 240 + delta_alpha / 240
  show_in_the_hour_system_from_dec(u_k)
  
#  U = u_k # Поправка, получаемая усреднением поправок u_k для разных пар Цингера

#  # Опорный момент приёма радиосигнала. Не знаю, откуда он.
#  T_op = transfer_from_hour_system_to_dec(c(20, 46, 47.515))
  
#  S_l = T_op + U
#  show_in_the_hour_system_from_dec(S_l)
  
#  # Возможно, тоже откуда-то взятая величина (отличается от той, что была ранее)
#  lambda_0 = transfer_from_hour_system_to_dec(c(2, 1, 10.771))
#  show_in_the_hour_system_from_dec(lambda_0)

#  # Гринвичское время в момент T. Тоже не знаю, откуда это.
#  S = transfer_from_hour_system_to_dec(c(18, 46, 28.856))

#  lambda_obs = S_l - S
#  show_in_the_hour_system_from_dec(lambda_obs)  

#  delta_lambda = lambda_0 - lambda_obs
#  show_in_the_hour_system_from_dec(delta_lambda)
  
  show_in_the_hour_system_from_dec(u_k)
  