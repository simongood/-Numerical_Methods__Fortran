import numpy as np
import matplotlib.pyplot as plt

f = open('countt.txt', 'r')
data = f.readlines()
n = len(data)
x = np.zeros((n))
y = np.zeros((n))

for i in range(0, n, 1):
    x[i], y[i] = data[i].split()

print(x)
print(y)
fig = plt.figure(1)

plt.plot(x, y, 'ro-')
plt.xlim((x[0],x[-1]))
plt.xlabel('x', fontsize='20')
plt.ylabel('y', fontsize='20')
plt.title('y(t)', fontsize='22')

plt.savefig('countt.png',dpi=300)
plt.close(fig)
