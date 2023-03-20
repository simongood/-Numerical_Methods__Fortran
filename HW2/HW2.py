import numpy as np
import matplotlib.pyplot as plt

f = open('RoundOffErro.txt', 'r')
data = f.readlines()
n = len(data)
x = np.zeros((n))
y = np.zeros((n))

for i in range(0, n, 1):
    x[i], y[i] = data[i].split()

print(x)
print(y)

# 開始畫圖
fig = plt.figure(1)
plt.plot(x, y, 'ro-')



# 定XY軸顯示方法(使用對數)
plt.xscale('log')
plt.yscale('log')

# 定XY軸最大小值
# plt.xlim(10**(-10), 10**0)
# plt.ylim(10**(-12), 10**0)

# 去邊框
# ax = plt.axes()
# ax.spines['top'].set_visible(False)
# ax.spines['right'].set_visible(False)



# 定 X, Y軸, 標題
plt.xlabel('Step size', fontsize='20')
plt.ylabel('Error', fontsize='20')
plt.title('Plot of error versus step size', fontsize='22')



plt.savefig('RoundOffErro.png',dpi=300)
plt.close(fig)
