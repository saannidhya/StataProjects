"""
Dynamic General Equilibrium Model for Road Maintenance

This module implements a dynamic general equilibrium model to analyze
the economic impacts and optimal strategies for road maintenance.

Author: Saani Rawat
Date: 08/10/2025
Version: 1.0

Log:
    08/10/2025 - Initial model implementation
    08/11/2025 - Added detailed comments and parameter documentation
    
Dependencies:
    - numpy
    - scipy
    - matplotlib
    - pandas

Usage:
    python roads_model_dge.py
"""

import numpy as np
import scipy as sp
import pandas as pd
import matplotlib.pyplot as plt
from scipy.optimize import root, minimize, Bounds

#===============================================================================================================#
# Model Name: Dynamic General Equilibrium Model for Road Maintenance
# Purpose: To explain the empirical findings from the paper, "The Effect of Local Road Tax Cuts on House Values"
#
# Model has Household sector, Production sector, Government sector, and Housing sector
#===============================================================================================================#




# ===== FIXED STRUCTURAL PARAMETERS =====
# Fixed parameters
beta = 0.96        # Discount factor (households prefer present consumption)
deltaK = 0.06      # Capital depreciation rate (6% per year)
alpha_k = 0.30     # Capital share in production (Cobb-Douglas parameter)
kappa = 2.9        # Labor disutility parameter (how much people dislike working)
theta = 2.0        # Labor supply elasticity parameter
alpha_h = 0.35     # Housing preference parameter
Hbar = 1.0         # Fixed housing stock (normalized to 1)
A_TFP = 1.0        # Total Factor Productivity (normalized to 1)


# ===== DERIVED STEADY-STATE RELATIONSHIPS =====
# Calculate the steady-state real interest rate and output-to-capital ratio
r_ss = deltaK + 1.0/beta - 1.0  # Real interest rate in steady state
Y_over_K_ss = r_ss / alpha_k     # Output-to-capital ratio (from profit maximization)

# ===== GOVERNMENT PRODUCTIVITY FUNCTIONS =====
def A_of_G(G, eta = 0.3393868666456796):
    """
    Government productivity function: how government spending affects economic productivity
    G: government spending on roads
    eta: elasticity parameter (how responsive productivity is to government spending)
    """    
    return G**eta

def phi_of_G(G, gamma = 0.050664941866382124):
    """
    Government disutility function: represents congestion/negative effects when government spending is low
    G: government spending
    gamma: congestion parameter
    When G < 1, there's additional disutility (congestion, poor roads)
    """    
    return gamma * max(0.0, 1.0 - G)
# def A_of_G(G): return G**eta
# def phi_of_G_commute(G): return gamma * max(0.0, 1.0 - G)


def ss_equations(x, pars, tau_H):
    # x = [n, K, G]
    n, K, G = x
    eta, gamma, tau_L, deltaG_norm, psi = pars

    # Ensure all variables are positive (economic constraint)
    if n <= 0 or K <= 0 or G <= 0:
        return np.array([1e6, 1e6, 1e6])
    
    # ===== PRODUCTION SECTOR =====
    # Production consistency
    Yp = (K**alpha_k) * ((A_TFP * n)**(1.0 - alpha_k)) # Actual production using Cobb-Douglas technology
    Yr = Y_over_K_ss * K # Required production for capital market equilibrium
    eq_prod = Yp - Yr # output gap. In equilibrium, this should be zero
    Y = Yp # Actual output
    w = (1.0 - alpha_k) * Y / n  # Wage rate from firm's profit maximization

    # ===== GOVERNMENT SECTOR =====
    # Road maintenance budget in steady state: M = deltaG_norm * G**psi
    M = deltaG_norm * (G**psi)

    # ===== HOUSEHOLD SECTOR =====
    # Resource constraint
    c = Y - deltaK * K - M
    if c <= 0:
        return np.array([1e6, 1e6, 1e6])
    
    # After-tax housing price in steady state
    q = alpha_h * (A_of_G(G, eta)) * c / Hbar

    # Government budget
    M_tax = tau_L * w * n + tau_H * (q / (1.0 + tau_H)) * Hbar
    eq_gov = M_tax - M # Government revenue = expenditure. No spending gap.

    # ===== HOUSEHOLD LABOR SUPPLY DECISION =====
    lhs = kappa * (1.0 + phi_of_G(G, gamma))**(1.0 + theta) * (n**theta) # Left side: Marginal disutility of labor (including commuting costs)
    rhs = ((1.0 - tau_L) * w) / c # Right side: Marginal utility of consumption from working
    eq_lab = lhs - rhs
    return np.array([eq_prod, eq_gov, eq_lab])

def solve_ss(pars, tau_H, x0=np.array([0.6, 3.0, 1.0])):
    """
    Solve for steady-state equilibrium given parameters and tax rates
    
    Inputs:
    pars: model parameters
    tau_H: housing tax rate
    x0: initial guess for [labor, capital, government spending]
    
    Returns: 
    - Equilibrium values for endogenous variables
    - Dictionary with all computed economic variables
    """    
    # Define function to find roots (where all equations = 0). This is the system of 9 steady-state equations defined in ss_equations
    fun = lambda x: ss_equations(x, pars, tau_H)

    # Solve system of nonlinear equations
    sol = root(fun, x0, method='hybr', tol=1e-11)
    if not sol.success:
        return None, None
    
    # Extract solution
    n, K, G = sol.x
    # Extract parameters
    eta, gamma, tau_L, deltaG_norm, psi = pars
    # Compute all economic variables at equilibrium
    Y = (K**alpha_k) * ((A_TFP * n)**(1.0 - alpha_k))
    w = (1.0 - alpha_k) * Y / n
    M = deltaG_norm * (G**psi)
    c = Y - deltaK * K - M
    q = alpha_h * (A_of_G(G, eta)) * c / Hbar
    p_h = q / (1.0 + tau_H)
    return sol.x, dict(n=n, K=K, G=G, Y=Y, w=w, M=M, c=c, q=q, p_h=p_h)

# setting baseline parameters
pars_baseline = (0.20, 0.50, 0.10, 0.08, 0.5) # eta, gamma, tau_L, deltaG_norm, psi
ss_baseline, v_baseline = solve_ss(pars_baseline, 0.035)
ss_baseline, v_baseline
# >>> ss_baseline
# array([0.66342131, 3.11269155, 1.08366319])
# >>> v_baseline
# {'n': 0.663421306585338, 'K': 3.112691551147814, 'G': 1.083663194411527, 'Y': 1.0548565812223136, 'w': 1.1130176247371348, 'M': 0.08327931582472188, 'c': 0.7848157723287229, 'q': 0.2791352162580314, 'p_h': 0.26969586111887095}

#======================================================================================
# ===== PARAMETER OPTIMIZATION =====
# Goal: To find optimal parameters that match empirical results
#======================================================================================

# ===== CALIBRATION TARGETS FROM EMPIRICAL PAPER =====
tG = -0.23   # Target: 23% decrease in government spending
tq = -0.09   # Target: 9% decrease in housing prices  
tM = -0.11   # Target: 11% decrease in road maintenance

# tG = -0.23   # Target: 23% decrease in government spending
# tq = -0.11   # Target: 11% decrease in housing prices  
# tM = -0.11   # Target: 11% decrease in road maintenance

def objective(x):
    """
    Objective function for parameter calibration
    
    Goal: Find parameters that make the model match empirical findings
    when housing tax is removed (tau_H goes from positive to 0)
    
    Input: x = [eta, gamma, tau_L, deltaG_norm, tau_H, psi] - parameters to calibrate

    Returns: Weighted sum of squared errors between model predictions and targets. Goal would be to minimize this value.
    """    
    # x = [eta, gamma, tau_L, deltaG_norm, tau_H, psi]
    eta, gamma, tau_L, deltaG_norm, tau_H, psi = x

    # Bounds safeguarding (optimizer enforces but we add basic guards)
    if not (0.05 <= eta <= 0.5 and 0.05 <= gamma <= 2.0 and 0.0 <= tau_L <= 0.35
            and 0.02 <= deltaG_norm <= 0.20 and 0.01 <= tau_H <= 0.06 and 0.2 <= psi <= 1.2):
        return 1e4

    pars = (eta, gamma, tau_L, deltaG_norm, psi) # need to encode them like this for solve_ss

    # ===== BASELINE EQUILIBRIUM (with housing tax) =====
    ss0, v0 = solve_ss(pars, tau_H)
    if v0 is None:
        return 1e3
    
    # ===== COUNTERFACTUAL EQUILIBRIUM (without housing tax) =====
    ss1, v1 = solve_ss(pars, 0.0, x0=ss0) # tau_H = 0
    if v1 is None:
        return 1e3
    
    # ===== CALCULATE PERCENTAGE CHANGES =====
    dG = v1['G']/v0['G'] - 1.0 # % change in government spending
    dq = v1['q']/v0['q'] - 1.0 # % change in after-tax housing prices
    dM = v1['M']/v0['M'] - 1.0 # % change in road maintenance

    # Minimize weighted squared errors
    # Weighted squared errors (prioritize housing and G a bit more)
    err = 2.0*(dG - tG)**2 + 3.0*(dq - tq)**2 + 1.0*(dM - tM)**2
    return err

# ===== PARAMETER OPTIMIZATION =====
# Initial guess for parameters [eta, gamma, tau_L, deltaG_norm, tau_H, psi]
x0 = np.array([0.20, 0.50, 0.10, 0.08, 0.032, 0.48])
bounds = Bounds([0.05, 0.05, 0.00, 0.02, 0.01, 0.20], # Lower bounds
                [0.50, 2.00, 0.35, 0.20, 0.06, 1.20]) # Upper bounds

# Solve optimization problem to find best-fitting parameters
res = minimize(objective, x0, method='L-BFGS-B', bounds=bounds, options=dict(maxiter=200))

res_success = res.success
# True
x_star = res.x # optimal eta, gamma, tau_L, deltaG_norm, tau_H, psi
# array([0.33316435, 0.05044514, 0.06582418, 0.05388631, 0.01882615,
#        0.44586469])
err_star = res.fun
# 1.4979213301349845e-12

# ===== COMPUTE FINAL RESULTS WITH OPTIMAL PARAMETERS =====
if res_success:
    eta, gamma, tau_L, deltaG_norm, tau_H, psi = x_star
    pars = (eta, gamma, tau_L, deltaG_norm, psi)
    ss0, v0 = solve_ss(pars, tau_H)
    ss1, v1 = solve_ss(pars, 0.0, x0=ss0)
    dG = v1['G']/v0['G'] - 1.0
    dq = v1['q']/v0['q'] - 1.0
    dM = v1['M']/v0['M'] - 1.0
    output = {
        "success": True,
        "params": {
            "eta": float(eta), "gamma": float(gamma),
            "tau_L": float(tau_L), "deltaG_norm": float(deltaG_norm),
            "tau_H_baseline": float(tau_H), "psi": float(psi)
        },
        "baseline": v0,
        "post_cut": v1,
        "drops": {"dG": float(dG), "dq": float(dq), "dM": float(dM)},
        "objective": float(err_star)
    }
# else:
#     output = {"success": False, "message": res.message}

eta, gamma, tau_L, deltaG_norm, tau_H, psi = x_star
pars = (eta, gamma, tau_L, deltaG_norm, psi)
ss0, v0 = solve_ss(pars, tau_H)
ss1, v1 = solve_ss(pars, 0.0, x0=ss0)
dG = v1['G']/v0['G'] - 1.0
dq = v1['q']/v0['q'] - 1.0
dM = v1['M']/v0['M'] - 1.0
output = {
    "success": True,
    "params": {
        "eta": float(eta), "gamma": float(gamma),
        "tau_L": float(tau_L), "deltaG_norm": float(deltaG_norm),
        "tau_H_baseline": float(tau_H), "psi": float(psi)
    },
    "baseline": v0,
    "post_cut": v1,
    "drops": {"dG": float(dG), "dq": float(dq), "dM": float(dM)},
    "objective": float(err_star)}
output
# {'success': True, 'params': {'eta': 0.3331643468732505, 'gamma': 0.050445140473638, 'tau_L': 0.06582418170705688, 'deltaG_norm': 0.05388630834536275, 'tau_H_baseline': 0.018826153106953587, 'psi': 0.44586468779486765}, 'baseline': {'n': 0.6635155678894374, 'K': 3.113133813646693, 'G': 0.999632479170457, 'Y': 1.0550064590691595, 'w': 1.113017624737133, 'M': 0.053877477389840514, 'c': 0.8143409528605174, 'q': 0.28498443007796476, 'p_h': 0.27971840849284507}, 'post_cut': {'n': 0.6545006520540965, 'K': 3.0708369321983104, 'G': 0.7697174779535838, 'Y': 1.040672515911651, 'w': 1.1130176247371337, 'M': 0.047950991749436, 'c': 0.8084713082303164, 'q': 0.2593357050615286, 'p_h': 0.2593357050615286}, 'drops': {'dG': -0.22999953083524027, 'dq': -0.09000044321515843, 'dM': -0.10999931562352716}, 'objective': 1.4979213301349845e-12}

#===============================================================================================
# Transitional Dynamics
#===============================================================================================

# Transitional dynamics simulation
# We'll simulate discrete-time dynamics with a state variable G_t (road quality) and predetermined capital K.
# At each t, given G_t and policy tau_H_t, we solve a static block for (n_t, Y_t, w_t, c_t, q_t, M_t)
# with K held constant at baseline steady-state K0 to focus on the maintenance/amenity channel.
# Then we update roads using: G_{t+1} = G_t * (1 - deltaG_t), where
# deltaG_t = phi * max(0, 1 - M_t / (deltaG_norm * G_t**psi))
#
# We'll run two simulations to check: (1) constant baseline policy (should remain at steady state),
# (2) a permanent levy cut at t=0, and plot 15-year transitions suitable for an event-study style plot.
#

# Use the calibrated parameters from the previous step
beta = 0.96
deltaK = 0.06
alpha_k = 0.30
kappa = 2.9
theta = 2.0
alpha_h = 0.35
Hbar = 1.0
A_TFP = 1.0

# Calibrated knobs that matched targets
# 9% target
eta = 0.3393868666456796
gamma = 0.050664941866382124
tau_L = 0.06808813799763494
deltaG_norm = 0.05488423321246541
tau_H_baseline = 0.019542912305940068
psi = 0.4458662209501801

# Dynamics parameter for roads
phi_dyn = 0.50  # sensitivity of depreciation to under-maintenance (consistent with appendix table scale)

# def A_of_G(G): return G**eta
# def phi_of_G_commute(G): return gamma * max(0.0, 1.0 - G)

# Static block solver given (G, tau_H, K)
def static_block(G, tau_H, K, x0=None):
    """
    Solves the static equilibrium for a given period in the transitional dynamics
    
    Inputs:
    G: Road quality/government spending level (state variable)
    tau_H: Housing tax rate (policy variable)
    K: Capital stock (held fixed during transition)
    x0: Initial guess for solver [labor, consumption, housing price]
    
    Returns: Dictionary with equilibrium values for all economic variables
    """
    # Unknowns: n (labor), c (consumption), q (after-tax housing price)
    # We'll use three equations:
    # (i) Labor FOC: kappa (1+phi(G))^(1+theta) n^theta = ((1 - tau_L) w) / c
    # (ii) Goods/resource: c = Y - deltaK*K - M, with M from gov budget M = tau_L w n + tau_H * (q/(1+tau_H)) * Hbar
    # (iii) Housing FOC: q = alpha_h * A(G) * c / Hbar
    # Given K and n, Y and w are pinned down by production.
    
    # Set default initial guess if not provided
    if x0 is None:
        x0 = np.array([0.65, 0.80, 0.27])  # initial guesses (n, c, q)

    def F(vars):
        """
        System of equations that must equal zero in equilibrium
        vars: [n, c, q] - labor, consumption, after-tax housing price
        """
        n, c, q = vars  # Unpack decision variables
        
        # Ensure all variables are economically valid (positive)
        if n <= 0 or c <= 0 or q <= 0:
            return np.array([1e6, 1e6, 1e6])  # Large penalty for invalid values
        
        # ===== PRODUCTION SECTOR CALCULATIONS =====
        Y = (K**alpha_k) * ((A_TFP * n)**(1.0 - alpha_k))  # Cobb-Douglas production function
        w = (1.0 - alpha_k) * Y / n  # Wage rate from firm's profit maximization (MPL = wage)
        
        # ===== GOVERNMENT BUDGET CALCULATION =====
        M = tau_L * w * n + tau_H * (q / (1.0 + tau_H)) * Hbar  # Total government revenue = labor tax + housing tax
        
        # ===== THREE EQUILIBRIUM CONDITIONS =====
        # EQUATION 1: Labor supply optimality condition
        # Left side: Marginal disutility of labor (including road quality effects)
        # Right side: Marginal utility of consumption from working (after-tax wage benefit)
        eq1 = kappa * (1.0 + phi_of_G(G, gamma))**(1.0 + theta) * (n**theta) - ((1.0 - tau_L) * w) / c
        
        # EQUATION 2: Resource constraint (goods market clearing)
        # Consumption equals output minus investment and government spending
        eq2 = c - (Y - deltaK * K - M)
        
        # EQUATION 3: Housing price determination (from household optimization)
        # Housing price depends on consumption, road quality, and housing preferences
        eq3 = q - (alpha_h * A_of_G(G, eta) * c / Hbar)
        
        return np.array([eq1, eq2, eq3])  # Return system of equations

    # Solve the system of nonlinear equations
    sol = root(F, x0, method='hybr', tol=1e-12, options=dict(maxfev=2000))
    
    # Check if solver converged to a solution
    if not sol.success:
        return None  # Return None if no solution found
    
    # Extract solution variables
    n, c, q = sol.x
    
    # ===== COMPUTE ALL EQUILIBRIUM VALUES =====
    Y = (K**alpha_k) * ((A_TFP * n)**(1.0 - alpha_k))  # Output
    w = (1.0 - alpha_k) * Y / n  # Wage rate
    M = tau_L * w * n + tau_H * (q / (1.0 + tau_H)) * Hbar  # Government revenue/road maintenance
    
    # Return dictionary with all computed economic variables
    return dict(n=n, c=c, q=q, Y=Y, w=w, M=M)

def roads_next(G, M):
    """
    Updates road quality for the next period based on current maintenance spending 
    This function models road depreciation as dependent on maintenance adequacy.
    If maintenance is below required levels, roads deteriorate faster.
    
    Inputs:
    G: Current road quality/government spending level
    M: Actual maintenance spending in current period
    
    Returns:
    G_next: Road quality in next period
    deltaG_t: Depreciation rate applied this period
    M_req: Required maintenance for zero depreciation
    shortfall: Fraction of required maintenance that was not provided
    """
    # Calculate required maintenance for zero-depreciation based on current road quality
    # This uses the flexible relationship: M_required = deltaG_norm * G^psi
    M_req = deltaG_norm * (G**psi)  # Required maintenance to keep roads at current quality
    
    # Calculate maintenance shortfall as a fraction of required maintenance
    # If M >= M_req, shortfall = 0 (fully funded)
    # If M < M_req, shortfall > 0 (underfunded, roads will deteriorate)
    shortfall = max(0.0, 1.0 - M / M_req)  # Fraction of required maintenance not provided
    
    # Calculate depreciation rate based on maintenance shortfall
    # phi_dyn controls sensitivity: higher phi_dyn means faster deterioration when underfunded
    deltaG_t = phi_dyn * shortfall  # Depreciation rate (0 if fully funded, positive if underfunded)
    
    # Update road quality for next period using depreciation
    # G_next = G * (1 - depreciation_rate)
    G_next = G * (1.0 - deltaG_t)  # Road quality next period (lower if underfunded)
    
    # Return all computed values for analysis and debugging
    return G_next, deltaG_t, M_req, shortfall

# First: compute steady state under baseline policy to set initial K and G
# We re-use the steady-state equations to get K0 and G0 (from previous solution),
# but to keep it consistent with this static block, we solve for K0 such that the static block implied M equals M_req
# This is already true at the calibrated baseline; we just plug numbers.
# For simplicity in this script, we'll take K0 and G0 from the calibrated steady state earlier:
G0 = v_baseline['G']
K0 = v_baseline['K']


# Verify baseline static block reproduces steady values
baseline_static = static_block(G0, tau_H_baseline, K0)
baseline_static

# Simulate dynamics: (1) constant baseline policy, (2) levy cut at t=0
T = 20

def simulate_path(cut=False):
    G = np.zeros(T+1)
    n = np.zeros(T+1)
    c = np.zeros(T+1)
    q = np.zeros(T+1)
    Y = np.zeros(T+1)
    w = np.zeros(T+1)
    M = np.zeros(T+1)
    tauH_path = np.zeros(T+1)

    G[0] = G0
    K = K0  # hold capital fixed

    for t in range(T+1):
        tauH_path[t] = 0.0 if (cut and t >= 0) else tau_H_baseline
        if baseline_static is not None:
            x0 = np.array([baseline_static['n'], baseline_static['c'], baseline_static['q']])
        else:
            x0 = None
        res = static_block(G[t], tauH_path[t], K, x0=x0)
        if res is None:
            raise RuntimeError(f"Static solver failed at t={t}")
        n[t], c[t], q[t], Y[t], w[t], M[t] = res['n'], res['c'], res['q'], res['Y'], res['w'], res['M']
        if t < T:
            G[t+1], _, _, _ = roads_next(G[t], M[t])

    return dict(G=G, n=n, c=c, q=q, Y=Y, w=w, M=M, tauH=tauH_path)

baseline_path = simulate_path(cut=False)
# >>> baseline_path
# {'G': array([1.03793926, 1.03793926, 1.03793926, 1.03793926, 1.03793926,
#        1.03793926, 1.03793926, 1.03793926, 1.03793926, 1.03793926,
#        1.03793926, 1.03793926, 1.03793926, 1.03793926, 1.03793926,
#        1.03793926, 1.03793926, 1.03793926, 1.03793926, 1.03793926,
#        1.03793926]), 'n': array([0.66351483, 0.66351483, 0.66351483, 0.66351483, 0.66351483,
#        0.66351483, 0.66351483, 0.66351483, 0.66351483, 0.66351483,
#        0.66351483, 0.66351483, 0.66351483, 0.66351483, 0.66351483,
#        0.66351483, 0.66351483, 0.66351483, 0.66351483, 0.66351483,
#        0.66351483]), 'c': array([0.8124144, 0.8124144, 0.8124144, 0.8124144, 0.8124144, 0.8124144,
#        0.8124144, 0.8124144, 0.8124144, 0.8124144, 0.8124144, 0.8124144,
#        0.8124144, 0.8124144, 0.8124144, 0.8124144, 0.8124144, 0.8124144,
#        0.8124144, 0.8124144, 0.8124144]), 'q': array([0.28796135, 0.28796135, 0.28796135, 0.28796135, 0.28796135,
#        0.28796135, 0.28796135, 0.28796135, 0.28796135, 0.28796135,
#        0.28796135, 0.28796135, 0.28796135, 0.28796135, 0.28796135,
#        0.28796135, 0.28796135, 0.28796135, 0.28796135, 0.28796135,
#        0.28796135]), 'Y': array([1.05500529, 1.05500529, 1.05500529, 1.05500529, 1.05500529,
#        1.05500529, 1.05500529, 1.05500529, 1.05500529, 1.05500529,
#        1.05500529, 1.05500529, 1.05500529, 1.05500529, 1.05500529,
#        1.05500529, 1.05500529, 1.05500529, 1.05500529, 1.05500529,
#        1.05500529]), 'w': array([1.11301762, 1.11301762, 1.11301762, 1.11301762, 1.11301762,
#        1.11301762, 1.11301762, 1.11301762, 1.11301762, 1.11301762,
#        1.11301762, 1.11301762, 1.11301762, 1.11301762, 1.11301762,
#        1.11301762, 1.11301762, 1.11301762, 1.11301762, 1.11301762,
#        1.11301762]), 'M': array([0.05580307, 0.05580307, 0.05580307, 0.05580307, 0.05580307,
#        0.05580307, 0.05580307, 0.05580307, 0.05580307, 0.05580307,
#        0.05580307, 0.05580307, 0.05580307, 0.05580307, 0.05580307,
#        0.05580307, 0.05580307, 0.05580307, 0.05580307, 0.05580307,
#        0.05580307]), 'tauH': array([0.01954291, 0.01954291, 0.01954291, 0.01954291, 0.01954291,
#        0.01954291, 0.01954291, 0.01954291, 0.01954291, 0.01954291,
#        0.01954291, 0.01954291, 0.01954291, 0.01954291, 0.01954291,
#        0.01954291, 0.01954291, 0.01954291, 0.01954291, 0.01954291,
#        0.01954291])}
cut_path = simulate_path(cut=True)
# >>> cut_path
# {'G': array([1.03793926, 0.98590474, 0.94655215, 0.91616579, 0.89259367,
#        0.8742386 , 0.8599023 , 0.84867741, 0.83987137, 0.8329521 ,
#        0.82750853, 0.82322166, 0.81984303, 0.81717855, 0.81507622,
#        0.81341678, 0.81210651, 0.81107169, 0.81025426, 0.80960844,
#        0.80909815]), 'n': array([0.66209458, 0.66164603, 0.66039711, 0.65943612, 0.65869265,
#        0.65811494, 0.65766447, 0.6573122 , 0.65703613, 0.65681938,
#        0.65664896, 0.65651481, 0.65640913, 0.65632581, 0.65626009,
#        0.65620822, 0.65616727, 0.65613494, 0.6561094 , 0.65608922,
#        0.65607328]), 'c': array([0.81642821, 0.81595242, 0.8146271 , 0.81360681, 0.81281717,
#        0.8122034 , 0.81172468, 0.81135027, 0.8110568 , 0.81082636,
#        0.81064516, 0.81050253, 0.81039015, 0.81030154, 0.81023165,
#        0.81017649, 0.81013294, 0.81009855, 0.81007139, 0.81004993,
#        0.81003297]), 'q': array([0.28938405, 0.28421078, 0.27985345, 0.27642487, 0.27372437,
#        0.27159566, 0.26991666, 0.26859172, 0.26754579, 0.26671988,
#        0.26606754, 0.26555221, 0.26514506, 0.26482333, 0.26456908,
#        0.26436815, 0.26420934, 0.26408382, 0.26398461, 0.26390619,
#        0.2638442 ]), 'Y': array([1.05342401, 1.0529244 , 1.05153275, 1.05046141, 1.04963225,
#        1.04898776, 1.04848508, 1.04809194, 1.04778377, 1.0475418 ,
#        1.04735153, 1.04720176, 1.04708376, 1.04699072, 1.04691733,
#        1.04685941, 1.04681368, 1.04677757, 1.04674904, 1.04672651,
#        1.0467087 ]), 'w': array([1.11373335, 1.1139598 , 1.11459139, 1.11507843, 1.11545585,
#        1.11574951, 1.11597874, 1.11615812, 1.1162988 , 1.1164093 ,
#        1.11649621, 1.11656465, 1.11661857, 1.1166611 , 1.11669465,
#        1.11672113, 1.11674203, 1.11675854, 1.11677159, 1.11678189,
#        1.11679003]), 'M': array([0.05020798, 0.05018416, 0.05011784, 0.05006677, 0.05002725,
#        0.04999654, 0.04997258, 0.04995384, 0.04993915, 0.04992762,
#        0.04991855, 0.04991141, 0.04990579, 0.04990135, 0.04989786,
#        0.0498951 , 0.04989292, 0.04989119, 0.04988984, 0.04988876,
#        0.04988791]), 'tauH': array([0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
#        0., 0., 0., 0.])}

# Quick checks: baseline should be flat (steady)
flat_checks = {
    "G_var": float(np.var(baseline_path['G'])),
    "n_var": float(np.var(baseline_path['n'])),
    "c_var": float(np.var(baseline_path['c'])),
    "q_var": float(np.var(baseline_path['q'])),
    "M_var": float(np.var(baseline_path['M'])),
}

#==========================================================================================#
#               Model Plot: Dynamic Transition
#==========================================================================================#

# Create transition graphs comparing baseline and cut paths
fig, axes = plt.subplots(2, 3, figsize=(15, 10))
fig.suptitle('Dynamic Transition: Baseline vs Housing Tax Cut', fontsize=16)

# Time axis for plotting
time_axis = np.arange(T+1)

# Variable labels and their corresponding data
variables = [
    ('Road Maintenance Budget (M)', 'M'),
    ('Road Infrastructure (G)', 'G'),
    ('Housing Price (q)', 'q'),
    ('Consumption (c)', 'c'),
    ('Labor (n)', 'n'), 
    ('Output (Y)', 'Y')
]

# Plot each variable
for i, (title, var) in enumerate(variables):
    row = i // 3
    col = i % 3
    ax = axes[row, col]
    
    # Plot baseline path - extend to t=-1 for continuity
    time_extended = np.concatenate([[-1], time_axis])
    baseline_extended = np.concatenate([[baseline_path[var][0]], baseline_path[var]])
    ax.plot(time_extended, baseline_extended, 'b-', linewidth=2, label='Baseline')
    
    # Plot tax cut path with jump at t=0
    # Create separate segments: pre-jump (t=-1 to t=0) and post-jump (t=0 onwards)
    # Add a point at t=-1 with baseline value to show the jump
    cut_extended = np.concatenate([[baseline_path[var][0]], cut_path[var]])
    ax.plot(time_extended, cut_extended, 'r--', linewidth=2, label='Tax Cut')
    
    ax.set_title(title)
    ax.set_xlabel('Time')
    ax.legend()
    ax.grid(True, alpha=0.3)
    ax.set_xlim(-1, T)

plt.tight_layout()
plt.show()

#==========================================================================================#
#               Model Plot: Dynamic Transition (Percentage Changes)
#==========================================================================================#
# Create percentage change plots relative to baseline
fig2, axes2 = plt.subplots(2, 3, figsize=(15, 10))
fig2.suptitle('Percentage Changes from Baseline After Housing Tax Cut', fontsize=16)

for i, (title, var) in enumerate(variables):
    row = i // 3
    col = i % 3
    ax = axes2[row, col]
    
    # Calculate percentage changes
    pct_change = (cut_path[var] / baseline_path[var] - 1) * 100
    
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

# Print summary statistics
print("Flat check results (should be near zero for baseline):")
for key, val in flat_checks.items():
    print(f"{key}: {val:.8f}")

print(f"\nImmediate impact (t=0 to t=1) percentage changes:")
for title, var in variables:
    immediate_change = (cut_path[var][1] / baseline_path[var][1] - 1) * 100
    print(f"{title}: {immediate_change:.2f}%")

print(f"\nFinal impact (t={T}) percentage changes:")
for title, var in variables:
    final_change = (cut_path[var][T] / baseline_path[var][T] - 1) * 100
    print(f"{title}: {final_change:.2f}%")


# pct_change = (cut_path[var] / baseline_path[var] - 1) * 100

# cut_path['q']

## Empirical numbers (hardcoded - come directly from the paper)

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

# Plot model predictions (adjust time scale to match empirical)
# Show pre-treatment periods (t-3 to t-1) as 0% change for model
# Then show actual model results from t=0 onwards, treating each period as 6 months
pre_treatment_time = np.arange(-3, 0)  # t-3, t-2, t-1
pre_treatment_zeros = [0, 0, 0]  # Model shows no change before treatment

# For post-treatment, skip every other period to represent 6-month intervals
post_treatment_indices = np.arange(0, min(len(cut_path['q']), 21), 2)  # Every 2nd period
post_treatment_time = np.arange(0, len(post_treatment_indices))  # t=0 to t=10
model_pct_q = [(cut_path['q'][t] / baseline_path['q'][t] - 1) * 100 for t in post_treatment_indices]

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
legend = ax3.legend(loc='upper right', fontsize=6, framealpha=0.85)
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
# plt.show()

# Save the plot to specified location
save_path = r"C:\Users\rawatsa\OneDrive - University of Cincinnati\Applied Economics Program\PhD\classes\summer 2025\dissertation_proposal\presentation\images\model_vs_empirical_estimates.png"
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


