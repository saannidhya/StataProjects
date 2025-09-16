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
    python roads_model_dge_house.py
"""

import numpy as np
from typing import Tuple, Dict, Any
from dataclasses import dataclass
import matplotlib.pyplot as plt
from scipy.optimize import root, minimize, Bounds

# ---------------- Fixed structural parameters ----------------
beta = 0.96
deltaK = 0.06
alpha_k = 0.30
kappa = 2.9
theta = 2.0
alpha_h = 0.35
Hbar = 1.0
A_TFP = 1.0
deltaH = 0.05  # NEW: housing depreciation for user-cost formula

# Derived
rho = 1.0 / beta - 1.0
r_ss = deltaK + rho
Y_over_K_ss = r_ss / alpha_k

# Targets
tG = -0.23
tq = -0.11
# tq = -0.09
tM = -0.11

tauH_fixed = 0.01  # baseline property tax

def A_of_G(G, eta):
    return G**eta

def phi_of_G(G, gamma):
    return gamma * max(0.0, 1.0 - G)

def ss_equations(x, pars, tau_H):
    n, K, G = x
    eta, gamma, tau_L, deltaG_norm, psi = pars
    if min(n, K, G) <= 0:
        return np.array([1e6, 1e6, 1e6])

    # Output and wages
    Yp = (K**alpha_k) * ((A_TFP * n)**(1.0 - alpha_k))
    Yr = Y_over_K_ss * K
    eq_prod = Yp - Yr
    Y = Yp
    w = (1.0 - alpha_k) * Y / n

    # Maintenance & resource constraint
    M = deltaG_norm * (G**psi)
    c = Y - deltaK*K - M
    if c <= 0:
        return np.array([1e6, 1e6, 1e6])

    # Housing user cost & effective price
    # q = (alpha_h * A_of_G(G, eta) * (c / Hbar)) / (rho + deltaH + tau_H)
    q = (beta/(1-beta*(1-deltaH-tau_H)))*(alpha_h * A_of_G(G, eta) * (c / Hbar))  # asset pricing formula

    # Government budget (labor & property tax finance maintenance)
    M_tax = tau_L * w * n + tau_H * (q / (1.0 + tau_H)) * Hbar
    eq_gov = M_tax - M

    # Labor FOC
    lhs = kappa * (1.0 + phi_of_G(G, gamma))**(1.0 + theta) * (n**theta)
    rhs = ((1.0 - tau_L) * w) / c
    eq_lab = lhs - rhs

    return np.array([eq_prod, eq_gov, eq_lab])

def solve_ss(pars, tau_H, x0=np.array([0.66, 3.1, 1.0])) -> Tuple[Any, Dict[str, float]]:
    fun = lambda x: ss_equations(x, pars, tau_H)
    sol = root(fun, x0, method='hybr', tol=1e-12)
    if not sol.success:
        return None, None
    n, K, G = sol.x
    eta, gamma, tau_L, deltaG_norm, psi = pars
    Y = (K**alpha_k) * ((A_TFP * n)**(1.0 - alpha_k))
    w = (1.0 - alpha_k) * Y / n
    M = deltaG_norm * (G**psi)
    c = Y - deltaK*K - M
    # q = (alpha_h * A_of_G(G, eta) * (c / Hbar)) / (rho + deltaH + tau_H)
    q = (beta/(1-beta*(1-deltaH-tau_H)))*(alpha_h * A_of_G(G, eta) * (c / Hbar)) 
    p_h = q / (1.0 + tau_H)
    return sol.x, dict(n=float(n), K=float(K), G=float(G), Y=float(Y), w=float(w),
                       M=float(M), c=float(c), q=float(q), p_h=float(p_h))

def objective(z, weights=(2.0, 6.0, 1.0), want_baseline_G_gt1=True):
    eta, gamma, tau_L, deltaG_norm, tau_H, psi = z
    if not (0.05 <= eta <= 0.90 and 0.05 <= gamma <= 2.00 and 0.00 <= tau_L <= 0.35
            and 0.02 <= deltaG_norm <= 0.50 and abs(tau_H - tauH_fixed) < 1e-9
            and 0.30 <= psi <= 1.50):
        return 1e5
    pars = (eta, gamma, tau_L, deltaG_norm, psi)
    ss0, v0 = solve_ss(pars, tauH_fixed)
    if v0 is None: return 1e4
    ss1, v1 = solve_ss(pars, 0.0, x0=ss0)
    if v1 is None: return 1e4

    dG = v1['G']/v0['G'] - 1.0
    dq = v1['q']/v0['q'] - 1.0
    dM = v1['M']/v0['M'] - 1.0

    wG, wq, wM = weights
    err = wG*(dG - tG)**2 + wq*(dq - tq)**2 + wM*(dM - tM)**2

    # Regularizers/soft constraints
    reg = 0.0
    if v0['c'] <= 0 or v1['c'] <= 0: reg += 1e3
    # prefer interior
    if not (0.2 < ss0[0] < 0.95 and 0.2 < ss1[0] < 0.95): reg += 10.0
    if not (0.5 < ss0[1] < 10.0 and 0.5 < ss1[1] < 10.0): reg += 10.0
    # softly encourage G0>1 and G1<1
    if want_baseline_G_gt1 and v0['G'] < 1.0005:
        reg += 5000.0 * (1.0005 - v0['G'])
    if v1['G'] > 0.999:
        reg += 2000.0 * (v1['G'] - 0.999)

    return err + reg

def calibrate(max_starts=30, seed=0):
    rng = np.random.default_rng(seed)
    bounds = Bounds(
        [0.05, 0.05, 0.00, 0.02, tauH_fixed, 0.30],
        [0.90, 2.00, 0.35, 0.50, tauH_fixed, 1.50]
    )
    best = None
    for i in range(max_starts):
        x0 = np.array([
            rng.uniform(0.2, 0.9),   # eta
            rng.uniform(0.05, 1.2),  # gamma
            rng.uniform(0.05, 0.35), # tau_L
            rng.uniform(0.05, 0.45), # deltaG_norm
            tauH_fixed,              # tau_H fixed
            rng.uniform(0.35, 1.10), # psi
        ])
        res = minimize(lambda z: objective(z, weights=(2.0, 6.0, 1.0)),
                       x0, method='L-BFGS-B', bounds=bounds,
                       options=dict(maxiter=1500))
        if (best is None) or (res.fun < best.fun):
            best = res
    # If the best line-search fails, accept it anyway as a candidate
    eta, gamma, tau_L, deltaG_norm, tau_H, psi = best.x
    pars = (eta, gamma, tau_L, deltaG_norm, psi)
    ss0, v0 = solve_ss(pars, tauH_fixed)
    ss1, v1 = solve_ss(pars, 0.0, x0=ss0)
    dG = v1['G']/v0['G'] - 1.0
    dq = v1['q']/v0['q'] - 1.0
    dM = v1['M']/v0['M'] - 1.0
    return best, (v0, v1, dG, dq, dM), (eta, gamma, tau_L, deltaG_norm, tau_H, psi)

# if __name__ == "__main__":
res, (v0, v1, dG, dq, dM), x = calibrate(max_starts=40, seed=42)

v0
v1
# res
dG
dq

# ss0, v0 = solve_ss(pars, tauH_fixed)
# ss1, v1 = solve_ss(pars, 0.0, x0=ss0)

# using the calibrated parameters
eta, gamma, tau_L, deltaG_norm, tau_H, psi = x
pars = (eta, gamma, tau_L, deltaG_norm, psi)

# >>> res.success
# True
# >>> v0
# {'n': 0.6563145915944907, 'K': 3.0793477144503023, 'G': 1.2461087621648834, 'Y': 1.043556725452603, 'w': 1.113017624737134, 'M': 0.2794813596238912, 'c': 0.5793145029616937, 'q': 2.4181826667383297, 'p_h': 2.394240264097356}
# >>> v1
# {'n': 0.6356975199690882, 'K': 2.982614938428819, 'G': 0.9605219026756749, 'Y': 1.010775062467545, 'w': 1.1130176247371342, 'M': 0.24751160996569235, 'c': 0.5843065561961235, 'q': 2.1536628681935093, 'p_h': 2.1536628681935093}
# >>> dG
# -0.22918293182775973
# >>> v1['G']/v0['G'] - 1.0
# -0.22918293182775973
# >>> v1['G']
# 0.9605219026756749
# >>> v0['G']
# 1.2461087621648834
# >>> v1['q']/v0['q'] - 1.0
# -0.1093878482313363
# >>> v1
# {'n': 0.6356975199690882, 'K': 2.982614938428819, 'G': 0.9605219026756749, 'Y': 1.010775062467545, 'w': 1.1130176247371342, 'M': 0.24751160996569235, 'c': 0.5843065561961235, 'q': 2.1536628681935093, 'p_h': 2.1536628681935093}
# >>> v0
# {'n': 0.6563145915944907, 'K': 3.0793477144503023, 'G': 1.2461087621648834, 'Y': 1.043556725452603, 'w': 1.113017624737134, 'M': 0.2794813596238912, 'c': 0.5793145029616937, 'q': 2.4181826667383297, 'p_h': 2.394240264097356}
# >>> dq
# -0.1093878482313363
# >>> eta, gamma, tau_L, deltaG_norm, tau_H, psi = x
# >>> pars = (eta, gamma, tau_L, deltaG_norm, psi)
# >>> pars
# (0.875772186194558, 0.47086436936075887, 0.3498186959356814, 0.25220810391480897, 0.46667748350776045)

v1['c']/v0['c'] - 1.0
# 0.008617172898155223. No change.
v1['Y']/v0['Y'] - 1.0
# -0.04867951517191982. Output drops, because labor supply drops.

print("Optimizer success:", res.success, "| message:", res.message)
print("Objective:", res.fun)
labels = ["eta","gamma","tau_L","deltaG_norm","tau_H","psi"]
for L,val in zip(labels, x):
    print(f"{L:>12s} = {val:.6f}")
print("\nBaseline SS (tau_H=0.015):")
for k,v in v0.items():
    print(f"  {k:>2s}: {v:.6f}")
print("\nCounterfactual SS (tau_H=0.000):")
for k,v in v1.items():
    print(f"  {k:>2s}: {v:.6f}")
print("\nTarget hits (percent changes):")
print(f"  dG = {dG:.6f} (target {tG})")
print(f"  dq = {dq:.6f} (target {tq})")
print(f"  dM = {dM:.6f} (target {tM})")

# >>> print("Optimizer success:", res.success, "| message:", res.message)
# Optimizer success: True | message: CONVERGENCE: RELATIVE REDUCTION OF F <= FACTR*EPSMCH
# >>> print("Objective:", res.fun)
# Objective: 2.2851813458131677e-05
# >>> labels = ["eta","gamma","tau_L","deltaG_norm","tau_H","psi"]
# >>> for L,val in zip(labels, x):
# ...     print(f"{L:>12s} = {val:.6f}")
# ...
#          eta = 0.875772
#        gamma = 0.470864
#        tau_L = 0.349819
#  deltaG_norm = 0.252208
#        tau_H = 0.010000
#          psi = 0.466677
# >>> print("\nBaseline SS (tau_H=0.015):")

# Baseline SS (tau_H=0.015):
# >>> for k,v in v0.items():
# ...     print(f"  {k:>2s}: {v:.6f}")
# ...
#    n: 0.656315
#    K: 3.079348
#    G: 1.246109
#    Y: 1.043557
#    w: 1.113018
#    M: 0.279481
#    c: 0.579315
#    q: 2.418183
#   p_h: 2.394240
# >>> print("\nCounterfactual SS (tau_H=0.000):")

# Counterfactual SS (tau_H=0.000):
# >>> for k,v in v1.items():
# ...     print(f"  {k:>2s}: {v:.6f}")
# ...
#    n: 0.635698
#    K: 2.982615
#    G: 0.960522
#    Y: 1.010775
#    w: 1.113018
#    M: 0.247512
#    c: 0.584307
#    q: 2.153663
#   p_h: 2.153663
# >>> print("\nTarget hits (percent changes):")

# Target hits (percent changes):
# >>> print(f"  dG = {dG:.6f} (target {tG})")
#   dG = -0.229183 (target -0.23)
# >>> print(f"  dq = {dq:.6f} (target {tq})")
#   dq = -0.109388 (target -0.11)
# >>> print(f"  dM = {dM:.6f} (target {tM})")
#   dM = -0.114390 (target -0.11)


# ----------------------------------------------------------------------#
# ----------------- Transitional dynamics (K fixed) -----------------
# ----------------------------------------------------------------------#


# -------- Helpers: user-cost price (owner occupied) and tau_H schedule

def q_star(c, G, tau_H, eta):
    # user-cost (target) price given current c,G,tau_H
    return (beta / (1.0 - beta * (1.0 - deltaH - tau_H))) * (alpha_h * A_of_G(G, eta) * (c / Hbar))

def tauH_path(t, cut, tau_H_baseline, tau_H_new=0.0, phi_tau=0.0):
    """
    tau_H schedule:
      - If phi_tau == 0.0: immediate jump (surprise cut at t=0)
      - Else: smooth phase-in: tau_H(t) = tau_H_new + (tau_H_baseline - tau_H_new)*exp(-phi_tau*t)
    """
    if not cut:
        return tau_H_baseline
    if phi_tau <= 0.0:
        return tau_H_new if t >= 0 else tau_H_baseline
    return tau_H_new + (tau_H_baseline - tau_H_new) * np.exp(-phi_tau * t)


# -------- One-period static block with moving K and q partial adjustment --------

def static_block(G, tau_H, K, eta, gamma, tau_L, K_target, q_last, lambda_q=0.3, kappaK=0.25, x0=None):
    """
    Unknowns per period: (n, c, q). Closures:
      - Investment rule toward K_target: I = deltaK*K + kappaK*(K_target - K)
      - Goods: c = Y - M - I
      - Gov:   M = tau_L * w * n + tau_H * q/(1+tau_H)
      - Housing partial adjustment: q = (1 - lambda_q)*q_last + lambda_q * q_star(c, G, tau_H, eta)

    Returns dict(..., K_next) with next capital.
    """
    if x0 is None:
        # start near previous period, will get overwritten by simulate_path
        x0 = np.array([0.66, 0.80, max(0.25, q_last)])

    I_rule = deltaK * K + kappaK * (K_target - K)

    def F(z):
        n, c, q = z
        if (n <= 0.0) or (c <= 0.0) or (q <= 0.0):
            return np.array([1e6, 1e6, 1e6])

        # Technology & wages
        Y = (K**alpha_k) * ((A_TFP * n)**(1.0 - alpha_k))
        w = (1.0 - alpha_k) * Y / n

        # Taxes → maintenance
        M = tau_L * w * n + tau_H * (q / (1.0 + tau_H)) * Hbar

        # 1) Labor FOC
        eq1 = kappa * (1.0 + phi_of_G(G, gamma))**(1.0 + theta) * (n**theta) - ((1.0 - tau_L) * w) / c

        # 2) Goods clearing with investment rule
        eq2 = c - (Y - M - I_rule)

        # 3) Housing partial-adjustment to user-cost target
        q_tgt = q_star(c, G, tau_H, eta)
        q_padj = (1.0 - lambda_q) * q_last + lambda_q * q_tgt
        eq3 = q - q_padj

        return np.array([eq1, eq2, eq3])

    sol = root(F, x0, method='hybr', tol=1e-12)
    if not sol.success:
        return None

    n, c, q = sol.x
    # Aggregates at solution
    Y = (K**alpha_k) * ((A_TFP * n)**(1.0 - alpha_k))
    w = (1.0 - alpha_k) * Y / n
    M = tau_L * w * n + tau_H * (q / (1.0 + tau_H)) * Hbar
    I = deltaK * K + kappaK * (K_target - K)
    K_next = (1.0 - deltaK) * K + I

    return dict(n=float(n), c=float(c), q=float(q), Y=float(Y), w=float(w),
                M=float(M), I=float(I), K_next=float(K_next))


# -------- Full simulation: moving K, q partial-adjustment, shortfall for G --------

def simulate_path(T, cut, pars, tau_H_baseline, K0, G0, v1, phi_dyn=0.50,
                  kappaK=0.25, lambda_q=0.3, phi_tau=0.0, check_consistency=True):
    """
    Args:
      cut: True → property tax cut per tauH_path; False → baseline
      v1 : counterfactual SS dict (from solve_ss(pars, 0.0)); we use v1['K'] as K_target if cut
      lambda_q in (0,1]: q partial-adjustment weight (lower = less impact jump)
      phi_tau ≥ 0: smoothing speed for tau_H phase-in (0 = immediate)
    """
    eta, gamma, tau_L, deltaG_norm, psi = pars

    if check_consistency and cut:
        # SS budget consistency check
        M_req_v1 = deltaG_norm * (v1['G'] ** psi)
        M_tax_v1 = tau_L * v1['w'] * v1['n']  # tau_H=0 in v1
        if abs(M_req_v1 - M_tax_v1) > 1e-6:
            raise RuntimeError(
                f"[Consistency] SS mismatch: M_req(v1)={M_req_v1:.6f} vs taxes(v1)={M_tax_v1:.6f}. "
                f"Ensure 'pars' here matches the ones used to compute v1."
            )

    # Target for capital
    K_target = v1['K'] if cut else K0

    # Allocate arrays
    G = np.zeros(T+1); K = np.zeros(T+1)
    n = np.zeros(T+1); c = np.zeros(T+1); q = np.zeros(T+1)
    Y = np.zeros(T+1); w = np.zeros(T+1); M = np.zeros(T+1); I = np.zeros(T+1)
    tauH = np.zeros(T+1)

    # Initial states
    G[0] = G0; K[0] = K0

    # Initial guess & q_last for t=0
    guess = np.array([max(1e-3, v0['n']), max(1e-3, v0['c']), max(1e-3, v0['q'])])
    q_last = v0['q']  # baseline price last observed before cut

    for t in range(T+1):
        tauH[t] = tauH_path(t, cut=cut, tau_H_baseline=tau_H_baseline, tau_H_new=0.0, phi_tau=phi_tau)

        res = static_block(
            G=G[t], tau_H=tauH[t], K=K[t],
            eta=eta, gamma=gamma, tau_L=tau_L,
            K_target=K_target, q_last=q_last,
            lambda_q=lambda_q, kappaK=kappaK, x0=guess
        )
        if res is None:
            raise RuntimeError(f"Static solver failed at t={t}")

        n[t], c[t], q[t], Y[t], w[t], M[t], I[t] = res['n'], res['c'], res['q'], res['Y'], res['w'], res['M'], res['I']

        # Prepare next-period state/guess
        guess  = np.array([n[t], c[t], q[t]])  # previous period computation (your preference)
        q_last = q[t]

        if t < T:
            # Capital and G updates
            K[t+1] = res['K_next']
            M_req = deltaG_norm * (G[t] ** psi)
            shortfall = max(0.0, 1.0 - M[t] / M_req)
            dG_t = phi_dyn * shortfall
            G[t+1] = G[t] * (1.0 - dG_t)

    # Quick convergence print
    if cut:
        print(
            f"\n--- Convergence check (last period vs v1) ---\n"
            f"K_T: {K[-1]:.6f}  vs v1.K: {v1['K']:.6f}\n"
            f"G_T: {G[-1]:.6f}  vs v1.G: {v1['G']:.6f}\n"
            f"M_T: {M[-1]:.6f}  vs v1.M(req): {(deltaG_norm * (v1['G'] ** psi)):.6f}\n"
            f"q_T: {q[-1]:.6f}  vs v1.q: {v1['q']:.6f}\n"
        )

    return dict(G=G, K=K, n=n, c=c, q=q, Y=Y, w=w, M=M, I=I, tauH=tauH)


# 1) Calibration and storing calibrated parameters
res, (v0, v1, dG, dq, dM), x = calibrate(max_starts=40, seed=42)
eta, gamma, tau_L, deltaG_norm, tau_H, psi = x
pars = (eta, gamma, tau_L, deltaG_norm, psi)

# 2) Recompute SS from these pars 
ss0, v0 = solve_ss(pars, tauH_fixed)
ss1, v1 = solve_ss(pars, 0.0, x0=ss0)

# 3) Simulate transitional dynamics
T = 100

# Baseline: no cut
base = simulate_path(
    T, cut=False, pars=pars, tau_H_baseline=tauH_fixed,
    K0=v0['K'], G0=v0['G'], v1=v0,
    phi_dyn=0.50, kappaK=0.25, lambda_q=0.3, phi_tau=0.0
)

# lambda_q=0.25 and a mild tau smoothing phi_tau=0.6
cutp = simulate_path(
    T, cut=True, pars=pars, tau_H_baseline=tauH_fixed,
    K0=v0['K'], G0=v0['G'], v1=v1,
    phi_dyn=0.50, kappaK=0.25,
    lambda_q=0.1,   # ↓ smaller immediate jump in q
    phi_tau=0.8,     # ↓ smooth τ_H over time; set 0.0 for instantaneous
    check_consistency=True
)

# plot model results
T = 100
fig2, axes2 = plt.subplots(2, 3, figsize=(15, 10))
fig2.suptitle('Percentage Changes from Baseline After Housing Tax Cut', fontsize=16)

# Plot every 10th period
plot_indices = np.arange(0, T+1, 10)
time_axis = plot_indices

variables = [("Maintenance (M)", "M"), ("Road Quality (G)", "G"), ("House Price (q)", "q"), ("Consumption (c)", "c"), ("Labor (n)", "n"), ("Output (Y)", "Y")]

for i, (title, var) in enumerate(variables):
    row = i // 3
    col = i % 3
    ax = axes2[row, col]
    
    # Calculate percentage changes for every 10th period
    pct_change = (cutp[var][plot_indices] / base[var][plot_indices] - 1) * 100
    
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

#---------------------------------------------------------------#
#                   Empirics vs Model
#---------------------------------------------------------------#

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
# save_path = r"C:\Users\rawatsa\OneDrive - University of Cincinnati\StataProjects\ohio_taxation\docs\JMP_draft\images\model_vs_empirical_estimates_house.png"
# plt.savefig(save_path, dpi=300, bbox_inches='tight', facecolor='white', edgecolor='none')
# print(f"Plot saved to: {save_path}")

# # Reset matplotlib style to default
plt.rcdefaults()
