#simplePlots
import matplotlib.pyplot as plt
import numpy as np

x_data = np.array([0,2,4,6,8,10])
y_data = np.array([1,2,3,4,5,6])

fig,ax = plt.subplots(2,2,figsize=(6,6))
plt.subplots_adjust(hspace=0,wspace=0,top=0.95,right=0.90)

ax[0,0].plot(x_data,y_data,'o',c='r',label=r'x vs. y')

plt.show()
