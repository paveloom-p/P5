# Функция для отображения угла, записанного в десятичной форме, в градусной мере

show_in_the_degree_system = function(x) {
  
  degrees = as.integer(x)
  print(degrees)
  
  minutes = (x - degrees) * 60
  print(as.integer(minutes))
  
  seconds = (minutes - as.integer(minutes)) * 60
  print(seconds)
  
}

L1 = c(10.9, 20.4, 7.4, 18.3)
R1 = c(21.9, 9.3, 18.9, 6.9)

L2 = c(18.5, 10.1, 18.5, 6.1)
R2 = c(7.6, 21.2, 7.1, 17.6)

# Максимальный номер штриха на уровне
m = 35

# Выбор знака у единицы зависит от положения нуля на уровне:
# если справа, то "-"; если слева, то "+".

i_1 = 1 * c(1, -1, 1, -1) * (m - (L1 + R1))
i_1

i_2 = -1 * c(1, -1, 1, -1) * (m - (L2 + R2))
i_2

b = (i_1 + i_2) / 2 * (1.574 / 3600)
b

show_in_the_degree_system(b)
