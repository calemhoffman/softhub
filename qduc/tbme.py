import numpy as np
import matplotlib.pyplot as plt
from sympy.physics.wigner import wigner_6j

# =====================================================================
# 1. Input Data: USD, USDA, USDB Diagonal TBMEs (d3/2 x d5/2)
# =====================================================================
j1, j2 = 1.5, 2.5
J_values = np.array([1, 2, 3, 4])

# Diagonal matrix elements V_J in MeV
tbme_data = {
    'USD':  {'T0': [-6.5058, -3.8253, -0.5377, -4.5062], 'T1': [1.0334, -0.3248, 0.5894, -1.4497]},
    'USDA': {'T0': [-6.0099, -4.2117, -1.2124, -4.6189], 'T1': [0.6030, -0.0041, 0.6476, -1.3904]},
    'USDB': {'T0': [-5.9698, -4.2675, -1.2549, -4.5027], 'T1': [0.6126, -0.0300, 0.6551, -1.4069]},
}

# =====================================================================
# 2. Compute Semiclassical Angle cos(theta_12)
# =====================================================================
denom = 2.0 * np.sqrt(j1*(j1+1) * j2*(j2+1))
cos_theta = (J_values*(J_values+1) - j1*(j1+1) - j2*(j2+1)) / denom

# Dense array for smooth fitting curves
x_fit = np.linspace(-1.0, 0.8, 200)

# =====================================================================
# 3. Functions for Fits and Exact Multipole Projection E_k
# =====================================================================
def fit_parabola(x, y):
    """Fit V(x) = c0 + c1*x + c2*x^2 and return coefficients [c0, c1, c2]"""
    # np.polyfit returns [c2, c1, c0] for degree 2
    p = np.polyfit(x, y, 2)
    return p[2], p[1], p[0]  # c0, c1, c2

def extract_Ek(J_list, V_list, j1, j2):
    """Exact inverse Racah projection to extract E_k multipole strength."""
    E_k = {}
    k_max = int(min(2*j1, 2*j2))  # k = 0, 1, 2, 3
    for k in range(k_max + 1):
        Ek_val = 0.0
        for J, V_J in zip(J_list, V_list):
            w6j = float(wigner_6j(j1, j2, J, j2, j1, k))
            phase = (-1)**(int(j1 + j2 + J))
            Ek_val += (2*J + 1) * phase * w6j * V_J
        E_k[f'E_{k}'] = Ek_val
    return E_k

# =====================================================================
# 4. Perform Analysis and Print Table
# =====================================================================
print("=" * 72)
print(f"{'Interaction':<12} | {'Chan':<4} | {'c0 (MeV)':<10} | {'c1 (MeV)':<10} | {'c2 [Q.Q]':<10} | {'E_2 (Q2)':<10}")
print("=" * 72)

fit_results = {}
for model, data in tbme_data.items():
    fit_results[model] = {}
    for T_str, V_J in data.items():
        c0, c1, c2 = fit_parabola(cos_theta, V_J)
        E_k = extract_Ek(J_values, V_J, j1, j2)
        fit_results[model][T_str] = (c0, c1, c2, E_k)
        print(f"{model:<12} | {T_str:<4} | {c0:10.4f} | {c1:10.4f} | {c2:10.4f} | {E_k['E_2']:10.4f}")
print("=" * 72)

# =====================================================================
# 5. Plot Schiffer Parabolas (T=0 and T=1)
# =====================================================================
fig, axes = plt.subplots(1, 2, figsize=(13, 5.5), sharex=True)
colors = {'USD': '#d95f02', 'USDA': '#7570b3', 'USDB': '#1b9e77'}
markers = {'USD': 'o', 'USDA': 's', 'USDB': '^'}

for i, (T_str, title) in enumerate([('T0', 'Isoscalar Channel ($T=0$)'), ('T1', 'Isovector Channel ($T=1$)')]):
    ax = axes[i]
    for model in ['USD', 'USDA', 'USDB']:
        V_J = tbme_data[model][T_str]
        c0, c1, c2, _ = fit_results[model][T_str]
        
        # Plot fitted parabola
        y_fit = c0 + c1*x_fit + c2*(x_fit**2)
        ax.plot(x_fit, y_fit, color=colors[model], linestyle='--', alpha=0.7,
                label=f"{model} ($c_2={c2:.2f}$ MeV)")
        
        # Plot discrete TBME points
        ax.plot(cos_theta, V_J, color=colors[model], marker=markers[model], 
                linestyle='', markersize=8, zorder=5)
        
        # Label J states near points
        if model == 'USDB':
            for x, y, J in zip(cos_theta, V_J, J_values):
                ax.annotate(f"$J={J}^+$", (x, y), textcoords="offset points", 
                            xytext=(0, 10 if T_str=='T1' or J!=3 else -15), ha='center', fontsize=9)

    ax.set_title(title, fontsize=13, fontweight='bold')
    ax.set_xlabel(r"$\cos \theta_{12}$", fontsize=11)
    ax.set_ylabel("Diagonal TBME $V_J$ (MeV)", fontsize=11)
    ax.grid(True, linestyle=':', alpha=0.6)
    ax.legend(frameon=True, loc='best')

plt.tight_layout()
plt.show()