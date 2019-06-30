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

# Функции для вычисления зенитного расстояния и его значения, увеличенного на 180 градусов, (для левого круга)
# и вариации этих же функций для правого круга

refr = 33
refr_dec = refr / 3600
refr_dec

check_1 = function(x) {
  
  show_in_the_degree_system(90 - transfer_from_degree_system_to_dec(x) - refr_dec)
  
}

check_2 = function(x) {
  
  show_in_the_degree_system(270 - transfer_from_degree_system_to_dec(x) - refr_dec)
  
}

check_3 = function(x) {
  
  show_in_the_degree_system(360 - (90 - transfer_from_degree_system_to_dec(x)) - refr_dec)
  
}

check_4 = function(x) {
  
  show_in_the_degree_system(360 - (270 - transfer_from_degree_system_to_dec(x)) - refr_dec)
  
}

# L_i, R_i - значения высот, указанных в Stellarium

L_1 = c(59, 38, 24.2)
check_1(L_1)
check_2(L_1)

L_2 = c(59, 39, 16.1)
check_1(L_2)
check_2(L_2)

L_3 = c(59, 40, 6.2)
check_1(L_3)
check_2(L_3)

L_4 = c(59, 40, 34.7)
check_1(L_4)
check_2(L_4)

R_1 = c(59, 42, 42.9)
check_3(R_1)
check_4(R_1)

R_2 = c(59, 43, 10.9)
check_3(R_2)
check_4(R_2)

R_3 = c(59, 43, 33.8)
check_3(R_3)
check_4(R_3)

R_4 = c(59, 43, 59.5)
check_3(R_4)
check_4(R_4)
