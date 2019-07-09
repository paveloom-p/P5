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
  
  rad = function(x) {
    temp5 = x * pi / 180
    return(temp5)
  }
  
  R = 155.107 #107
  #R = 137.95 #1300
  
  tau2 = 1.336 #107
  #tau2_dec = c(0,0,1.3) #1300
  tau2_dec = transfer_from_degree_system_to_dec(c(0,0,1.336))
  
  M_north = c(14.135, 14.15, 14.155, 14.21, 14.13)
  Mn = mean(M_north)
  Mn
  
  M_south = c(4.60, 4.595, 4.71, 4.66, 4.65, 4.63)
  Ms = mean(M_south)
  Ms
  
  value_1 = R*(Ms - Mn)/2 #в секундах
  value_1
  
  JIuIIn = mean(c(22 + 28.5, 21.5 + 28))
  JIuIIn  
  
  JIuIIs = mean(c(19 + 25.5, 19 + 25.5))
  JIuIIs
  
  is_in = JIuIIs - JIuIIn
  is_in
  
  #Поправка за уровень
  P = (is_in)*tau2/2 #в секундах
  P
  
  deltaN = c(68, 44, 55.69)
  deltaN_dec = transfer_from_degree_system_to_dec(deltaN)
  deltaN_rad = rad(deltaN_dec)
  
  tgdN = tan(deltaN_rad)
  tgdN
  
  deltaS = c(50, 46, 37.41)
  deltaS_dec = transfer_from_degree_system_to_dec(deltaS)
  deltaS_rad = rad(deltaS_dec)
  
  tgdS = tan(deltaS_rad)
  tgdS
  
  SN = c(17,37.1,0)
  SN_dec = transfer_from_hour_system_to_dec(SN)
  
  Fn = (296.2^2 + 144.4^2 + 150.2^2 + 298.9^2)/(825060*5) # мб не 5, а 6
  Fn # в секундах
  
  SS = c(17,48.6,0)
  SS_dec = transfer_from_hour_system_to_dec(SS)
  
  Fs = (451.5^2 + 298.9^2 + 296.2^2 + 445.6^2)/(825060*5) # мб не 5, а 6
  Fs # в секудах
  
  z_0 = c(8,59.4,0)
  z_0_dec = transfer_from_degree_system_to_dec(z_0)
  z_0_rad = rad(z_0_dec)
  
  sec2_z = (1/cos(z_0_rad))^2
  sec2_z  
 
  dRo = 2.826 * 10^(-4) * (Ms - Mn) * (R/2) * sec2_z
  dRo  
  
  zs_zn_half = (Ms - Mn) * (R/2) + tau2/2 * is_in + dRo + (Fs * tgdS + Fn * tgdN)
  zs_zn_half
  
  lamda = c(2, 1.18, 0)
  lamda_dec = transfer_from_hour_system_to_dec(lamda)
  
  S_l_dec = mean(SS_dec, SN_dec)
  show_in_the_hour_system_from_dec(S_l_dec)
  
  S = S_l_dec - lamda_dec
  show_in_the_hour_system_from_dec(S)
  n = S/360
  n
  
  a_a_v = c(-0.101, -0.046)
  
  b_a_v = c(0.9949, 0.999)
  
  c_a_v = c(1.0844, 1.0481)
  
  d_a_v = c(-0.094, -0.0354)
  
  mu_a_v = c(0.323, 0.208)
  
  
  a_a = mean(a_a_v)
  a_a
  
  b_a = mean(b_a_v)
  b_a
  
  c_a = mean(c_a_v)
  c_a
  
  d_a = mean(d_a_v)
  d_a
  
  mu_a = mean(mu_a_v)
  mu_a
  
  A_A_0 = -6.235
  A_A_1 = -6.137
  
  B_B_0 = 3.337
  B_B_1 = 3.267
  
  C_0 = 3.77
  C_1 = 4.077
  
  D_0 = -19.728
  D_1 = -19.655
  
  tau_0 = 0.0031
  tau_1 = 0.0058
  
  A_A = A_A_0 + (A_A_1 - A_A_0) * n
  A_A
  B_B = B_B_0 + (B_B_1 - B_B_0) * n
  B_B
  C = C_0 + (C_1 - C_0) * n
  C
  D = D_0 + (D_1 - D_0) * n
  D
  tau = tau_0 + (tau_1 - tau_0) * n
  tau
  
  d0 = 0.5*(deltaN_dec+deltaS_dec)
  d0
  dd0 = A_A*a_a + B_B*b_a + C*c_a + D*d_a + tau*mu_a
  dd0_dec = transfer_from_degree_system_to_dec(c(0,0,dd0))
  
  delta = d0+dd0_dec
  show_in_the_degree_system(delta)
  
  zs_zn_half_dec = transfer_from_degree_system_to_dec(c(0,0,zs_zn_half))
  
  phi = delta + zs_zn_half
  phi
  