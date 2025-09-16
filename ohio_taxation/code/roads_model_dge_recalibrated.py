"""
Dynamic General Equilibrium Model for Road Maintenance

This module implements a dynamic general equilibrium model to analyze
the economic impacts and optimal strategies for road maintenance.

Author: Saani Rawat
Version: 1.0, Date: 08/10/2025
Version: 2.0, Date: 09/15/2025

Log:
    08/10/2025 - Initial model implementation
    08/11/2025 - Added detailed comments and parameter documentation
    08/13/2025 - Recalibrated model parameters to improve fit with empirical data
    09/15/2025 - Updated the model and implementation after adding housing as a storable asset

Dependencies:
    - numpy
    - scipy
    - matplotlib
    - pandas

Usage:
    python roads_model_dge_recalibrated.py
"""

# Recalibration & run evidence for Saani's DGE model with tau_H fixed at 0.015
# - Steady state calibration to match targets (-23% G, -11% q, -11% M)
# - Transitional dynamics with K fixed (as requested)
# - Prints parameter results and target hits
# - Compares model q path to empirical series (0..10 “years” in your mapping)

import numpy as np
import pandas as pd
from dataclasses import dataclass
from typing import Tuple, Dict, Any
from scipy.optimize import root, minimize, Bounds

# ------------- Fixed structural parameters -------------
beta = 0.96           # Discount factor
deltaK = 0.06         # Depreciation rate of capital
alpha_k = 0.30        # Capital share in production
kappa = 2.9           # Inverse Frisch elasticity of labor supply
theta = 2.0           # Elasticity of substitution between housing and other goods
alpha_h = 0.35        # Housing share in utility
Hbar = 1.0            # Fixed housing stock
A_TFP = 1.0          # Total factor productivity

# Derived
r_ss = deltaK + 1.0/beta - 1.0
Y_over_K_ss = r_ss / alpha_k

# Targets (tightened housing drop to -11%)
tG = -0.23
tq = -0.11
tM = -0.11

# Fix baseline property tax at 1.5%
tauH_fixed = 0.015

# -------------------- Helper fns --------------------
def A_of_G(G, eta):
    return G**eta

def phi_of_G(G, gamma):
    return gamma * max(0.0, 1.0 - G)

def ss_equations(x, pars, tau_H):
    n, K, G = x
    eta, gamma, tau_L, deltaG_norm, psi = pars

    if n <= 0 or K <= 0 or G <= 0:
        return np.array([1e6, 1e6, 1e6])

    # Production
    Yp = (K**alpha_k) * ((A_TFP * n)**(1.0 - alpha_k))
    Yr = Y_over_K_ss * K
    eq_prod = Yp - Yr
    Y = Yp
    w = (1.0 - alpha_k) * Y / n

    # Maintenance requirement at steady state (minimal-upkeep)
    M = deltaG_norm * (G**psi)

    # Resource constraint
    c = Y - deltaK * K - M
    if c <= 0:
        return np.array([1e6, 1e6, 1e6])

    # Effective housing price q = (1+tau_H) p_h
    q = alpha_h * (A_of_G(G, eta)) * c / Hbar

    # Government budget
    M_tax = tau_L * w * n + tau_H * (q / (1.0 + tau_H)) * Hbar
    eq_gov = M_tax - M

    # Labor FOC
    lhs = kappa * (1.0 + phi_of_G(G, gamma))**(1.0 + theta) * (n**theta)
    rhs = ((1.0 - tau_L) * w) / c
    eq_lab = lhs - rhs

    return np.array([eq_prod, eq_gov, eq_lab])

def solve_ss(pars, tau_H, x0=np.array([0.66, 3.1, 1.0])) -> Tuple[Any, Dict[str, float]]:
    fun = lambda x: ss_equations(x, pars, tau_H)
    sol = root(fun, x0, method='hybr', tol=1e-11)
    if not sol.success:
        return None, None

    n, K, G = sol.x
    eta, gamma, tau_L, deltaG_norm, psi = pars

    Y = (K**alpha_k) * ((A_TFP * n)**(1.0 - alpha_k))
    w = (1.0 - alpha_k) * Y / n
    M = deltaG_norm * (G**psi)
    c = Y - deltaK * K - M
    q = alpha_h * (A_of_G(G, eta)) * c / Hbar
    p_h = q / (1.0 + tau_H)

    return sol.x, dict(n=float(n), K=float(K), G=float(G), Y=float(Y), w=float(w),
                       M=float(M), c=float(c), q=float(q), p_h=float(p_h))

# Objective with tau_H fixed at 0.015
def objective(x):
    # x = [eta, gamma, tau_L, deltaG_norm, tau_H, psi]; tau_H must equal tauH_fixed
    eta, gamma, tau_L, deltaG_norm, tau_H, psi = x

    if not (0.05 <= eta <= 0.50 and 0.05 <= gamma <= 2.00 and 0.00 <= tau_L <= 0.35
            and 0.02 <= deltaG_norm <= 0.20 and abs(tau_H - tauH_fixed) < 1e-12
            and 0.20 <= psi <= 1.20):
        return 1e4

    pars = (eta, gamma, tau_L, deltaG_norm, psi)

    ss0, v0 = solve_ss(pars, tauH_fixed)
    if v0 is None:
        return 1e3

    ss1, v1 = solve_ss(pars, 0.0, x0=ss0)
    if v1 is None:
        return 1e3

    dG = v1['G']/v0['G'] - 1.0
    dq = v1['q']/v0['q'] - 1.0  # effective price drop
    dM = v1['M']/v0['M'] - 1.0

    # Put extra weight on housing to correct undershoot
    err = 2.0*(dG - tG)**2 + 4.0*(dq - tq)**2 + 1.0*(dM - tM)**2
    return err

# Start near your earlier optimum, but with tau_H fixed
x0 = np.array([0.333, 0.0505, 0.066, 0.054, tauH_fixed, 0.446])
bounds = Bounds(
    [0.05, 0.05, 0.00, 0.02, tauH_fixed, 0.20],
    [0.50, 2.00, 0.35, 0.20, tauH_fixed, 1.20]
)

res = minimize(objective, x0, method='L-BFGS-B', bounds=bounds, options=dict(maxiter=400))
res_success = res.success
x_star = res.x
err_star = res.fun

# if not res_success:
#     print("Calibration failed:", res.message)
# else:
eta, gamma, tau_L, deltaG_norm, tau_H, psi = x_star
pars = (eta, gamma, tau_L, deltaG_norm, psi)
ss0, v0 = solve_ss(pars, tauH_fixed)
ss1, v1 = solve_ss(pars, 0.0, x0=ss0)
dG = v1['G']/v0['G'] - 1.0
dq = v1['q']/v0['q'] - 1.0
dM = v1['M']/v0['M'] - 1.0


eta, gamma, tau_L, deltaG_norm, tau_H, psi
# (0.42044675630610195, 0.05720788533188241, 0.04640807566566141, 0.04268157329184511, 0.015, 0.44586411335082793)
ss0, v0
# >>> ss0, v0
# (array([0.65449737, 3.07082155, 0.75080137]), {'n': 0.6544973743757916, 'K': 3.0708215537329737, 'G': 0.7508013718204255, 'Y': 1.04066730432062, 'w': 1.1130176247371335, 'M': 0.03756137740077858, 'c': 0.818856633695863, 'q': 0.2540626538059184, 'p_h': 0.25030803330632356})
ss1, v1 
# >>> ss1, v1 
# (array([0.64719635, 3.03656603, 0.57811665]), {'n': 0.6471963486011582, 'K': 3.036566034626442, 'G': 0.5781166467852663, 'Y': 1.0290584895122956, 'w': 1.1130176247371335, 'M': 0.03342963697197449, 'c': 0.8134348904627345, 'q': 0.22611585689744268, 'p_h': 0.22611585689744268})
dG, dq, dM
(-0.23000054543914905, -0.10999962603643676, -0.10999970487553101)

print("=== Recalibration with tau_H fixed at 0.015 ===")
print("Success:", res_success, "| objective:", err_star)
print("Params:")
print({ "eta": float(eta), "gamma": float(gamma), "tau_L": float(tau_L),
        "deltaG_norm": float(deltaG_norm), "tau_H_baseline": float(tau_H),
        "psi": float(psi) })
print("\nBaseline steady state (with tau_H=0.015):")
print(v0)
print("\nCounterfactual steady state (tau_H=0):")
print(v1)
print("\nTarget hits (percent changes):")
print({ "dG": float(dG), "dq": float(dq), "dM": float(dM) })

# ----------------- Transitional dynamics (K fixed) -----------------
# Use calibrated parameters from above, if success
def static_block(G, tau_H, K, eta, gamma, tau_L, x0=None):
    if x0 is None:
        x0 = np.array([0.66, 0.81, 0.28])  # n, c, q

    def F(z):
        n, c, q = z
        if (n <= 0) or (c <= 0) or (q <= 0):
            return np.array([1e6, 1e6, 1e6])
        Y = (K**alpha_k) * ((A_TFP * n)**(1.0 - alpha_k))
        w = (1.0 - alpha_k) * Y / n
        M = tau_L * w * n + tau_H * (q/(1.0+tau_H)) * Hbar
        eq1 = kappa * (1.0 + phi_of_G(G, gamma))**(1.0 + theta) * (n**theta) - ((1.0 - tau_L) * w) / c
        eq2 = c - (Y - deltaK*K - M)
        eq3 = q - (alpha_h * A_of_G(G, eta) * c / Hbar)
        return np.array([eq1, eq2, eq3])

    sol = root(F, x0, method='hybr', tol=1e-12)
    if not sol.success:
        return None
    n, c, q = sol.x
    Y = (K**alpha_k) * ((A_TFP * n)**(1.0 - alpha_k))
    w = (1.0 - alpha_k) * Y / n
    M = tau_L * w * n + tau_H * (q/(1.0+tau_H)) * Hbar
    return dict(n=float(n), c=float(c), q=float(q), Y=float(Y), w=float(w), M=float(M))

def simulate_path(T, cut, pars, tau_H_baseline, K0, G0, phi_dyn=0.50):
    eta, gamma, tau_L, deltaG_norm, psi = pars
    G = np.zeros(T+1); n = np.zeros(T+1); c = np.zeros(T+1); q = np.zeros(T+1)
    Y = np.zeros(T+1); w = np.zeros(T+1); M = np.zeros(T+1)
    tauH = np.zeros(T+1)

    G[0] = G0; K = K0
    base_res = static_block(G[0], tau_H_baseline, K, eta, gamma, tau_L, x0=None)

    for t in range(T+1):
        tauH[t] = 0.0 if (cut and t >= 0) else tau_H_baseline
        x0loc = None if base_res is None else np.array([base_res['n'], base_res['c'], base_res['q']])
        res = static_block(G[t], tauH[t], K, eta, gamma, tau_L, x0=x0loc)
        if res is None:
            raise RuntimeError(f"Static solver failed at t={t}")
        n[t], c[t], q[t], Y[t], w[t], M[t] = res['n'], res['c'], res['q'], res['Y'], res['w'], res['M']
        if t < T:
            M_req = deltaG_norm * (G[t]**psi)
            shortfall = max(0.0, 1.0 - M[t]/M_req)
            dG_t = phi_dyn * shortfall
            G[t+1] = G[t]*(1.0 - dG_t)

    return dict(G=G, n=n, c=c, q=q, Y=Y, w=w, M=M, tauH=tauH)


ss0, v0 = solve_ss(pars, tauH_fixed)  # ensure we get K0, G0
G0 = v0['G']; K0 = v0['K']
T = 20
base = simulate_path(T, cut=False, pars=pars, tau_H_baseline=tauH_fixed, K0=K0, G0=G0, phi_dyn=0.50)
cutp = simulate_path(T, cut=True,  pars=pars, tau_H_baseline=tauH_fixed, K0=K0, G0=G0, phi_dyn=0.50)


# model results
fig2, axes2 = plt.subplots(2, 3, figsize=(15, 10))
fig2.suptitle('Percentage Changes from Baseline After Housing Tax Cut', fontsize=16)
time_axis = np.arange(0, T+1)
for i, (title, var) in enumerate(variables):
    row = i // 3
    col = i % 3
    ax = axes2[row, col]
    
    # Calculate percentage changes
    pct_change = (cutp[var] / base[var] - 1) * 100
    
    # Show jump from t=-1 (0% change) to t=0
    time_extended = np.concatenate([[-1], time_axis])
    pct_extended = np.concatenate([[0], pct_change])
    ax.plot(time_extended, pct_extended, 'r--', linewidth=2)
    
    ax.axhline(y=0, color='b', linestyle='-',  linewidth=2)
    ax.set_title(f'{title} (% Change)')
    ax.set_xlabel('Time')
    ax.set_ylabel('Percent Change')
    ax.grid(True, alpha=0.3)
    ax.set_xlim(-1, T)

plt.tight_layout()
plt.show()


# t-3 to t+10
beta_estimates = [5307, 166, -273, -4261, -3908, -11001, -14733, -21701, -21706, -17365, -15975, -21984, -19857, -16090]
# Standard errors for confidence interval calculation
standard_errors = [7341, 6943, 7391, 7955, 8719, 9405, 7989, 7747, 8751, 8355, 7248, 9074, 7751, 9027]
# Calculate 95% confidence intervals (±1.96 * standard error)
confidence_intervals = []
for i in range(len(beta_estimates)):
    lower_bound = beta_estimates[i] - 1.96 * standard_errors[i]
    upper_bound = beta_estimates[i] + 1.96 * standard_errors[i]
    confidence_intervals.append([lower_bound, upper_bound])

# average house value
average_house_value = 166000  

# Convert beta estimates to percentage changes
empirical_time = np.arange(-3, 11)  # t-3 to t+10
empirical_pct_changes = [(beta / average_house_value) * 100 for beta in beta_estimates]
# Convert confidence intervals to percentage terms
empirical_ci_pct = []
for ci in confidence_intervals:
    lower_pct = (ci[0] / average_house_value) * 100
    upper_pct = (ci[1] / average_house_value) * 100
    empirical_ci_pct.append([lower_pct, upper_pct])

# Set publication-quality style
plt.style.use('seaborn-v0_8-whitegrid')
plt.rcParams.update({
    'font.size': 12,
    'font.family': 'serif',
    'font.serif': ['Times New Roman', 'DejaVu Serif'],
    'axes.linewidth': 1.2,
    'axes.spines.top': False,
    'axes.spines.right': False,
    'axes.grid': True,
    'grid.alpha': 0.3,
    'legend.frameon': True,
    'legend.fancybox': True,
    'legend.shadow': True,
    'figure.dpi': 300
})

# Create comparison plot for housing prices
fig3, ax3 = plt.subplots(1, 1, figsize=(10, 6))

# Plot empirical estimates with confidence intervals
empirical_lower = [ci[0] for ci in empirical_ci_pct]
empirical_upper = [ci[1] for ci in empirical_ci_pct]

# Use professional color scheme
color_empirical = '#2E86AB'  # Professional blue
color_model = '#A23B72'      # Professional red/magenta
color_ci = '#2E86AB'         # Same as empirical for CI

ax3.fill_between(empirical_time, empirical_lower, empirical_upper, 
                alpha=0.25, color=color_ci, label='95% Confidence Interval')
ax3.plot(empirical_time, empirical_pct_changes, 'o-', linewidth=2.5, 
         markersize=7, color=color_empirical, markerfacecolor='white',
         markeredgewidth=2, markeredgecolor=color_empirical,
         label='Empirical Estimates')

# Plot model predictions using base and cutp (adjust time scale to match empirical)
# Show pre-treatment periods (t-3 to t-1) as 0% change for model
# Then show actual model results from t=0 onwards, treating each period as 6 months
pre_treatment_time = np.arange(-3, 0)  # t-3, t-2, t-1
pre_treatment_zeros = [0, 0, 0]  # Model shows no change before treatment

# For post-treatment, skip every other period to represent 6-month intervals
# post_treatment_indices = np.arange(0, min(len(cutp['q']), 21), 2)  # Every 2nd period
post_treatment_indices = np.arange(0, min(len(cutp['q']), 101), 10)  # Every 10th period
post_treatment_time = np.arange(0, len(post_treatment_indices))  # t=0 to t=10
model_pct_q = [(cutp['q'][t] / base['q'][t] - 1) * 100 for t in post_treatment_indices]

# Combine pre and post treatment data
full_model_time = np.concatenate([pre_treatment_time, post_treatment_time])
full_model_pct = np.concatenate([pre_treatment_zeros, model_pct_q])

ax3.plot(full_model_time, full_model_pct, '--', linewidth=3, 
         color=color_model, label='Model Prediction')

# Add reference lines
ax3.axhline(y=0, color='black', linestyle='-', alpha=0.7, linewidth=1)
ax3.axvline(x=-0.5, color='gray', linestyle=':', alpha=0.7, linewidth=1.5)

# Formatting for publication quality
ax3.set_xlabel('Years Relative to Property Tax Cut', fontsize=14, fontweight='bold')
ax3.set_ylabel('Housing Price Change (%)', fontsize=14, fontweight='bold')
ax3.set_title('Housing Price Response to Property Tax Cuts:\nModel vs. Empirical Evidence', 
              fontsize=16, fontweight='bold', pad=20)

# Customize legend
legend = ax3.legend(loc='upper right', fontsize=12, framealpha=0.85)
legend.get_frame().set_linewidth(1.2)

# Set axis limits and ticks
ax3.set_xlim(-3.5, 10.5)
ax3.set_xticks(np.arange(-3, 11, 2))
ax3.set_xticklabels([f'{x}' for x in np.arange(-3, 11, 2)])

# Add subtle background shading for pre/post treatment
ax3.axvspan(-3.5, -0.5, alpha=0.05, color='gray', label=None)
ax3.text(-2.7, ax3.get_ylim()[1] * 0.9, 'Pre-Treatment', 
         fontsize=10, style='italic', alpha=0.7)
ax3.text(2, ax3.get_ylim()[1] * 0.9, 'Post-Treatment', 
         fontsize=10, style='italic', alpha=0.7)

# Improve tick formatting
ax3.tick_params(axis='both', which='major', labelsize=11, width=1.2)
ax3.yaxis.set_major_formatter(plt.FuncFormatter(lambda x, p: f'{x:.1f}'))

plt.tight_layout()
plt.show()

# Save the plot to specified location
save_path = r"C:\Users\rawatsa\OneDrive - University of Cincinnati\Applied Economics Program\PhD\classes\summer 2025\dissertation_proposal\presentation\images\model_vs_empirical_estimates_recalibrated.png"
plt.savefig(save_path, dpi=300, bbox_inches='tight', facecolor='white', edgecolor='none')
print(f"Plot saved to: {save_path}")

# Reset matplotlib style to default
plt.rcdefaults()

# Print empirical vs model comparison
print("Empirical vs Model Housing Price Changes:")
print("Time\tEmpirical (%)\tModel (%)\tDifference")
for i, t in enumerate(empirical_time):
    if t >= 0 and t < len(model_pct_q):
        model_val = model_pct_q[t]
        empirical_val = empirical_pct_changes[i]
        diff = abs(model_val - empirical_val)
        print(f"{t}\t{empirical_val:.2f}\t\t{model_val:.2f}\t\t{diff:.2f}")
