import numpy as np
import matplotlib.pyplot as plt

f = open('point.txt', 'r')
data = f.readlines()
n = len(data)
x = np.zeros((n))
y = np.zeros((n))
y1 = np.zeros((n))

for i in range(0, n, 1):
    x[i], y[i], y1[i] = data[i].split()

print(x)
print(y)
print(y1)

# 開始畫圖
fig = plt.figure(1)
plt.plot(x, y, 'b-') #畫點 
plt.plot(x, y1, 'r-') #畫線 


# 定 X, Y軸, 標題
plt.xlabel('x', fontsize='20')
plt.ylabel('y', fontsize='20')
plt.title('Plot', fontsize='22')



plt.savefig('plot.png',dpi=300)
plt.close(fig)
