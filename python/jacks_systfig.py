import matplotlib.pyplot as plt
import numpy as np

si_a = np.array([26,28,30,32,34])
n18_z = np.array([10,12,14,16,18,20])
si_E_2 = np.array([1.797,1.779,2.235,1.941,3.327])
si_E_4 = np.array([3.842,4.617,5.279,5.502,np.nan])
n18_E_2 = np.array([1.304,1.482,1.941,2.217,1.971,2.213])
n18_E_4 = np.array([3.010,3.381,5.502,4.689,4.414,5.816])

si_E_2_sm = np.array([1.897,1.932,2.265,2.053,5.245])
si_E_4_sm = np.array([4.365,4.608,5.334,5.885,8.279])
si_4_2_sm = si_E_4_sm / si_E_2_sm

n18_E_2_sm = np.array([1.623,1.592,2.053,2.131,1.818,np.nan])
n18_E_4_sm = np.array([2.991,3.394,5.885,4.835,4.492,np.nan])
n18_4_2_sm = n18_E_4_sm / n18_E_2_sm

si_be2 = np.array([0.0344,0.03267,0.02081,0.0113,0.0085])
si_be2_err = np.array([0.0025,0.00050,0.00064,0.0033,0.0033])

si32_be2_guillame = 311
si32_be2_guillame_err = 41

si32_be2_pronko = 160
si32_be2_pronko_err = 41

n18_be2 = np.array([0.0136,0.0273,0.0113,0.02083,0.0301,0.0096])
n18_be2_err = np.array([0.0023,0.0026,0.0033,0.00120,0.0016,0.0021])

n18_be2_imsrg = np.array([np.nan,np.nan,122,125,np.nan,np.nan])

si_be2_sm = np.array([0.4706E+02,0.8185E+02,0.4799E+02,0.4452E+02,0.3554E+02])
si_be2_sm *= 5

si_be2_imsrg = np.array([228,np.nan,122,107,np.nan])

si_q_sm = np.array([-7.67,18.93,2.13,13.49,11.01])
n18_q_sm = np.array([-1.98,-12.78,13.49,3.99,13.73,np.nan])
si_q_sm *= 0.01
n18_q_sm *= 0.01

si_q_imsrg = np.array([-0.0702,np.nan,0.0031,0.0887,np.nan])

n18_be2_sm = np.array([0.3607E+02,0.5216E+02,0.4452E+02,0.3759E+02,0.5314E+02,np.nan])
n18_be2_sm *= 5

si_q = np.array([np.nan,0.16,-0.05,np.nan,np.nan])
si_q_err = np.array([np.nan,0.03,0.06,np.nan,np.nan])

n18_q = np.array([np.nan,np.nan,np.nan,0.04,0.11,np.nan])
n18_q_err = np.array([np.nan,np.nan,np.nan,0.03,0.06,np.nan])

si_be2 *= 10000
si_be2_err *= 10000

n18_be2 *= 10000
n18_be2_err *= 10000

si_4_2 = si_E_4 / si_E_2
n18_4_2 = n18_E_4 / n18_E_2

fig,ax = plt.subplots(3,2,figsize=(6,8))
plt.subplots_adjust(hspace=0,wspace=0,top=0.95,right=0.90)

ax[0,0].plot(si_a,si_E_2,'o',c='r',label=r'$2^+_1$')
ax[0,0].plot(si_a,si_E_4,'o',c='b',label=r'$4^+_1$')
ax[0,0].plot(si_a,si_4_2,'o',c='k',label=r'$4^+_1/2^+_1$')
ax[0,0].plot(si_a,si_E_2_sm,'--',c='r')
ax[0,0].plot(si_a,si_E_4_sm,'--',c='b')
ax[0,0].plot(si_a,si_4_2_sm,'--',c='k')
ax[0,0].set_ylabel('Excitation Energy [MeV]',fontsize=14)
ax[0,0].tick_params(axis='y',labelsize=13)
ax[0,0].legend(fontsize=11)
ax[0,1].plot(n18_z,n18_E_2,'o',c='r')
ax[0,1].plot(n18_z,n18_E_4,'o',c='b')
ax[0,1].plot(n18_z,n18_4_2,'o',c='k')
ax[0,1].plot(n18_z,n18_E_2_sm,'--',c='r',label=r'$2^+_1$')
ax[0,1].plot(n18_z,n18_E_4_sm,'--',c='b',label=r'$4^+_1$')
ax[0,1].plot(n18_z,n18_4_2_sm,'--',c='k',label=r'$4^+_1/2^+_1$')
ax[0,1].legend(fontsize=11)
ax[0,1].set_ylabel(r'$R_{4/2}$',fontsize=14)
ax[0,1].tick_params(axis='y',labelsize=13)
ax[0,1].yaxis.set_label_position('right')
ax[0,1].yaxis.tick_right()
ax[0,0].set_ylim(0,8)#max(ax[0,0].get_ylim()[1],ax[0,1].get_ylim()[1]))
ax[0,1].set_ylim(0,8)#max(ax[0,0].get_ylim()[1],ax[0,1].get_ylim()[1]))


ax[1,0].errorbar(si_a,si_be2,yerr=si_be2_err,fmt='ro',label='Expt.')
ax[1,0].set_ylabel(r'$B(E2;0^+\rightarrow2^+) [e^2fm^4]$',fontsize=14)
ax[1,0].tick_params(axis='y',labelsize=13)
ax[1,0].plot(si_a,si_be2_sm,'--',c='k',label='Shell Model')
#ax[1,0].plot(si_a,si_be2_imsrg,'d',c='b',label='IM-SRG')
ax[1,0].errorbar(32.1,si32_be2_guillame,yerr=si32_be2_guillame_err,fmt='go',label='Guillame')
ax[1,0].errorbar(32.2,si32_be2_pronko,yerr=si32_be2_pronko_err,fmt='yo',label='Pronko')
ax[1,0].plot(si_a,si_be2_imsrg * (1/pow(0.7275,2)),'--',c='c',label='IM-SRG')
ax[1,0].legend(fontsize=11)
ax[1,1].errorbar(n18_z,n18_be2,yerr=n18_be2_err,fmt='ro',label=r'$2^+_1$')
ax[1,1].errorbar(14.1,si32_be2_guillame,yerr=si32_be2_guillame_err,fmt='go',label='Guillame')
ax[1,1].errorbar(14.2,si32_be2_pronko,yerr=si32_be2_pronko_err,fmt='yo',label='Pronko')
#ax[1,1].plot(n18_z,n18_be2_imsrg * (1/pow(0.7275,2)),'--',c='c')
ax[1,1].plot(n18_z,n18_be2_sm,'--',c='k')
ax[1,1].tick_params(axis='y',labelsize=13)
ax[1,1].yaxis.set_label_position('right')
ax[1,1].yaxis.tick_right()

ax[1,0].set_ylim(5,max(ax[1,0].get_ylim()[1],ax[0,1].get_ylim()[1]))
ax[1,1].set_ylim(5,max(ax[1,0].get_ylim()[1],ax[0,1].get_ylim()[1]))

ax[2,0].errorbar(si_a,si_q,yerr=si_q_err,fmt='ro')
ax[2,0].plot(si_a,si_q_sm,'--',c='k')
ax[2,0].plot(si_a,si_q_imsrg,'--',c='c')
ax[2,0].tick_params(axis='y',labelsize=13)
ax[2,0].set_ylabel(r'$Q_s(2^+_1) [eb]$',fontsize=14)
ax[2,1].errorbar(n18_z,n18_q,yerr=n18_q_err,fmt='ro')
ax[2,1].plot(n18_z,n18_q_sm,'--',c='k')
ax[2,1].tick_params(axis='y',labelsize=13)
ax[2,1].yaxis.set_label_position('right')
ax[2,1].yaxis.tick_right()

ax[2,0].plot([24,36],[0,0],'--',c='b')
ax[2,1].plot([8,22],[0,0],'--',c='b')

ax[2,0].set_xlim(ax[0,0].get_xlim())
ax[2,1].set_xlim(ax[0,1].get_xlim())

ax[2,0].set_xlabel('A',fontsize=14)
ax[2,1].set_xlabel('Z',fontsize=14)
ax[2,0].tick_params(axis='x',labelsize=13)
ax[2,1].tick_params(axis='x',labelsize=13)

ax[2,0].set_xticks(si_a)
ax[2,1].set_xticks(n18_z)

ax[2,0].set_ylim(min(ax[2,0].get_ylim()[0],ax[2,1].get_ylim()[0]),max(ax[2,0].get_ylim()[1],ax[2,1].get_ylim()[1]))
ax[2,1].set_ylim(min(ax[2,0].get_ylim()[0],ax[2,1].get_ylim()[0]),max(ax[2,0].get_ylim()[1],ax[2,1].get_ylim()[1]))

fig.align_labels(ax[:,0])
plt.show()

