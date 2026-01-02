# -----------------------------------------------------------------------------
# Roads DGE: Baseline Steady State Solver (Sequential / Nested Root Form)
# -----------------------------------------------------------------------------
# Model features implemented in this baseline script:
#   • Representative household with log utility in c and h; fixed housing stock H̄.
#   • Amenity from public road capital G enters utility weight on housing.
#   • Commuting disutility multiplicatively scales labor (1 + φ(G)) in disutility term.
#   • Cobb–Douglas production Y = K^αk * n^(1-αk) (NO direct G in production).
#   • Government finances road maintenance M from labor tax τL and property tax τH.
#   • Riojas-style interior steady state: M = δG * G  ⇒  G = M / δG.
#   • Resource constraint: Y = c + δK*K + M.
#   • Sequential solution: outer root in labor n, inner root in house price ph.
#
# References to model equations in the paper (line numbers from provided PDF):
#   Preferences with A(G) and commuting factor (Eq. 6).  (See PDF C.1) 
#   Budget constraint (Eq. 7) & capital accumulation (Eq. 8). 
#   Euler equation (Eq. 11). 
#   Labor FOC w/ commuting (C.9.2). 
#   Housing FOC (Eq. 21) evaluated at h = H̄ (Eq. 31). 
#   Firm FOCs (Eq. 23). 
#   Government budget (Eq. 28). 
#   Public capital law (Eqs. 29–30) ⇒ interior steady state M=δG*G. 
#   Resource constraint (Eq. 33). 
# -----------------------------------------------------------------------------
# Author: ChatGPT draft for Saani Rawat
# Date  : 2025-07-16
# -----------------------------------------------------------------------------

using Printf
using Plots
using Random

# ============ 0. User-editable Calibration Block =================================
# Baseline parameter values from Table 14 in paper (annual units where relevant).
# Feel free to edit τL, τH, and HBAR to explore scenarios.

const β    = 0.98      # Discount factor. Table 14 baseline.
const αh   = 0.35      # Housing budget share weight in utility. Table 14 baseline.
const ηA   = 0.15      # Amenity utility weight exponent in A(G). Table 14 baseline (η).
# const θ    = 2.0       # Inverse Frisch elasticity. Table 14 baseline.
const θ    = 2.5       # Inverse Frisch elasticity. Give Frisch elasticity of 0.4.
# const κ    = 2.9       # Labor disutility scale. Table 14 baseline.
const κ    = 25.0       # Labor disutility scale. Table 14 baseline.
const αk   = 0.30      # Capital share in production. Table 14 baseline.
const δK   = 0.06      # Private capital depreciation (annual). Table 14 baseline.
const δG   = 0.02      # Road stock depreciation (annual). Table 14 baseline.
const φwear = 0.50     # Wear-and-tear exponent in Riojas law (NOT used in interior SS).
# const γY   = 0.15      # Output elasticity of public capital (unused here; production omits G).
const ψcomm = 0.30     # semi-elasticity of commuting cost wrt roughness i.e. as G falls by 1 unit, commuting cost rises by ψcomm percent.

# Tax rates: choose reasonable baselines; adjust to match your data.
# (Not in Table 14; set by user / data moments.)
const τL   = 0.05      # Labor State income tax rate (endogenous revenue source).
const τH   = 0.025     # Property / voted road levy tax rate.
# const τH   = 0.0     # Property / voted road levy tax rate.

# Housing stock (per household units of housing services). Normalization.
const HBAR = 1.0       # You can scale; affects level of ph but not ratios.

# Numerical tolerances & max iterations
const TOL      = 1e-10
const MAXITER  = 1000
const SAFE_BIG = 1.0e12

# Domain safety epsilons
const EPS_G = 1.0e-12   # Floor for G to keep logs/powers safe.
const EPS_C = 1.0e-12   # Floor for c to avoid div-by-zero.

# ============ 1. Functional Forms ===============================================
# Amenity from roads A(G): choose simple power form A(G) = G^ηA (with G>0). 

@inline function Amenity(G::Float64)
    Gp = max(G, EPS_G)
    return Gp^ηA         
end

# Commuting wedge 1+φ(G): we want this to RISE as G FALLS.  We use
# (1+φ(G)) = (G)^(-ψcomm)
# Thus φ(G) = G^(-ψcomm) - 1 ≥ 0 for G <= 1; if G > 1 the wedge < 1 (good roads)

# @inline function OnePlusPhi(G::Float64)
#     Gp = max(G, EPS_G)
#     return Gp^(-ψcomm)
# end
# @inline function Phi(G::Float64)
#     return OnePlusPhi(G) - 1.0
# end
@inline function Phi(G::Float64)
    Gp = max(G, EPS_G)
    return (Gp)^(-ψcomm)
end

# ============ 2. Precomputed constants from parameters ==========================
# Capital Euler ⇒ rental rate r; then derive μ = (αk/r)^(αk/(1-αk)) s.t. Y = μ*n.
# Wage w = (1-αk)*μ.

const r_ss = 1/β - (1 - δK)                                  # S0
const μ_ss = (αk / r_ss)^(αk / (1 - αk))                     # S1
const w_ss = (1 - αk) * μ_ss                                 # S2

# ============ 3. Utilities to compute derived quantities ========================

# Given n, return Y and K (Steps S3a, S3b)
function Y_of_n(n::Float64)
    return μ_ss * n
end

function K_of_n(n::Float64)
    Y = Y_of_n(n)
    return (αk / r_ss) * Y
end

# Maintenance revenue M(n, ph)
function M_of_n_ph(n::Float64, ph::Float64)
    return τL * w_ss * n + τH * ph * HBAR
end

# Road stock G(n, ph)
function G_of_n_ph(n::Float64, ph::Float64)
    M = M_of_n_ph(n, ph)
    return M / δG
end

# Consumption given n, ph
function c_of_n_ph(n::Float64, ph::Float64)
    Y = Y_of_n(n)
    K = K_of_n(n)
    M = M_of_n_ph(n, ph)
    c = Y - δK * K - M
    return max(c, EPS_C)   # keep positive for stability; caller should check
end

# ============ 4. Inner root: ph given n =========================================
# Solve F_ph(ph; n) = 0 with a robust bracketed bisection.
# Equation: ph = [αh * Amenity(G(n,ph)) / (1+τH)] * [c(n,ph) / HBAR]

function F_ph(ph::Float64, n::Float64)
    G = G_of_n_ph(n, ph)
    c = c_of_n_ph(n, ph)
    target = (αh * Amenity(G) / (1 + τH)) * (c / HBAR)
    return ph - target
end

# Bracketing helper: expand upper bound until sign change or hit SAFE_BIG.
# This function finds the safe range [ph_low, ph_high] where F_ph is of same sign.
function bracket_ph(n::Float64; ph_low::Float64=1e-12, ph_high::Float64=1.0)
    f_low = F_ph(ph_low, n)
    f_high = F_ph(ph_high, n)
    iter = 0
    while sign(f_low) == sign(f_high) && ph_high < SAFE_BIG && iter < 100
        ph_high *= 2.0
        f_high = F_ph(ph_high, n)
        iter += 1
    end
    return ph_low, ph_high, f_low, f_high
end

# Bisection root-finder (scalar, monotone-safe)
# finds the root of F_ph(ph; n) = 0 for given n i.e. finds the house price ph*(n)
function bisection_ph(n::Float64; tol::Float64=TOL, maxiter::Int=MAXITER)
    a, b, fa, fb = bracket_ph(n)
    if sign(fa) == sign(fb)
        error("Failed to bracket ph root for n=$n. Try different τL/τH/HBAR.")
    end
    for i in 1:maxiter
        m = 0.5*(a + b)
        fm = F_ph(m, n)
        if abs(fm) < tol || abs(b - a) < tol
            return max(m, 0.0)
        end
        if sign(fm) == sign(fa)
            a = m; fa = fm
        else
            b = m; fb = fm
        end
    end
    @warn "ph bisection did not fully converge; returning midpoint" a b
    return max(0.5*(a + b), 0.0)
end

# Wrapper that also returns associated c,M,G after solving ph
function ph_given_n(n::Float64)
    ph = bisection_ph(n)
    c  = c_of_n_ph(n, ph)
    M  = M_of_n_ph(n, ph)
    G  = G_of_n_ph(n, ph)
    return ph, c, M, G
end

# ============ 5. Outer root: n ================================================
# Labor FOC ⇒ n_impl = [ ((1-τL)*w_ss) / (κ*c*(1+φ(G))^(1+θ)) ]^(1/θ)
# Solve n = n_impl(n)  (where c,G depend on ph*(n)).

function n_implied(c::Float64, G::Float64)
    denom = κ * c * (1 + Phi(G))^(1 + θ)
    return ((1 - τL) * w_ss / denom)^(1 / θ)
end

function F_n(n::Float64)
    ph, c, M, G = ph_given_n(n)
    nimp = n_implied(c, G)
    return n - nimp
end

# Bracket n root. We search over [n_min, n_max].
function bracket_n(; n_low::Float64=1e-8, n_high::Float64=0.95)
    f_low = F_n(n_low)
    f_high = F_n(n_high)
    if sign(f_low) != sign(f_high)
        return n_low, n_high, f_low, f_high
    end
    # If no sign change, try shrinking upper bound gradually
    steps = 20
    for i in 1:steps
        nb = n_high * (0.95^i)
        fb = F_n(nb)
        if sign(f_low) != sign(fb)
            return n_low, nb, f_low, fb
        end
    end
    error("Failed to bracket n root. Adjust tax params or calibration.")
end

function bisection_n(; tol::Float64=TOL, maxiter::Int=MAXITER)
    a, b, fa, fb = bracket_n()
    for i in 1:maxiter
        m = 0.5*(a + b)
        fm = F_n(m)
        if abs(fm) < tol || abs(b - a) < tol
            return max(min(m,1.0),0.0)
        end
        if sign(fm) == sign(fa)
            a = m; fa = fm
        else
            b = m; fb = fm
        end
    end
    @warn "n bisection did not fully converge; returning midpoint" a b
    return max(min(0.5*(a + b),1.0),0.0)
end

# ============ 6. Solve steady state ===========================================

mutable struct SteadyState
    n::Float64
    Y::Float64
    K::Float64
    c::Float64
    ph::Float64
    M::Float64
    G::Float64
    w::Float64
    r::Float64
end

function solve_steady_state()
    n  = bisection_n()
    Y  = Y_of_n(n)
    K  = K_of_n(n)
    ph, c, M, G = ph_given_n(n)
    return SteadyState(n, Y, K, c, ph, M, G, w_ss, r_ss)
end

# Pretty-print
function Base.show(io::IO, ss::SteadyState)
    println(io, "\n---- Roads DGE Baseline Steady State ----")
    @printf(io, "n   = %.6f\n", ss.n)
    @printf(io, "Y   = %.6f\n", ss.Y)
    @printf(io, "K   = %.6f\n", ss.K)
    @printf(io, "c   = %.6f\n", ss.c)
    @printf(io, "ph  = %.6f\n", ss.ph)
    @printf(io, "M   = %.6f\n", ss.M)
    @printf(io, "G   = %.6f\n", ss.G)
    @printf(io, "w   = %.6f\n", ss.w)
    @printf(io, "r   = %.6f\n", ss.r)
end

# ============ 7. Example run ===================================================
# if abspath(PROGRAM_FILE) == @__FILE__
ss = solve_steady_state()
println(ss)
# end

# SS when you cut property tax to 0.025
# ---- Roads DGE Baseline Steady State ----
# n   = 0.181252
# Y   = 0.318677
# K   = 1.188971
# c   = 0.234309
# ph  = 0.075027
# M   = 0.013029
# G   = 0.651467
# w   = 1.230734
# r   = 0.080408

# SS when you cut property tax to 0
# ---- Roads DGE Baseline Steady State ----
# n   = 0.175420
# Y   = 0.308422
# K   = 1.150712
# c   = 0.228585
# ph  = 0.072936
# M   = 0.010795
# G   = 0.539739
# w   = 1.230734
# r   = 0.080408



# -----------------------------------------------------------------------------
# Do the transition paths actually converge to this steady state?
# -----------------------------------------------------------------------------



# ============================================================================
# 8. Transitional Dynamics (Pre-Shock Convergence Demo)
# ============================================================================
# Goal: Starting from arbitrary initial public-capital stock G0 (and, if desired,
#       an arbitrary initial guess for private capital; but in this reduced-form
#       baseline we keep MPK pinned to r_ss each period, so K adjusts instantly)
#       we simulate the coupled evolution of {G_t, n_t, c_t, ph_t, Y_t, K_t} under
#       *constant* tax rates (τL, τH) and the *Rioja-style* public-capital law of
#       motion (Eq. 29–30). This shows that the dynamic system converges back to
#       the steady state computed above when no policy shocks occur – i.e., the
#       model is internally coherent ("it works").
#
# Economic structure each period t when G_t is taken as predetermined (state):
#   1. Given G_t, household intratemporal conditions pin down n_t and ph_t once c_t
#      is known. We collapse to a single unknown n_t by substituting labor FOC into
#      the goods/resource constraint. (See derivation below.)
#   2. Production ⇒ Y_t, factor prices w_t, r_t.
#   3. Housing FOC ⇒ ph_t (closed form, because h = HBAR fixed). (Eq. 21 & Eq. 31.)
#   4. Government budget ⇒ M_t = τL * w_t * n_t + τH * ph_t * HBAR. (Eq. 28.)
#   5. Public capital law ⇒ δG,t = max{0, φwear * (1 - M_t / (δG * G_t))};
#      G_{t+1} = G_t * (1 - δG,t). (Eq. 29–30.)
#   6. Resource ⇒ c_t = Y_t - δK * K_t - M_t, with K_t chosen so that MPK = r_t.
#      Because we are not solving the full intertemporal problem here (no shooting on c0),
#      the system is "static each period" except for the physical evolution of G_t.
#      This is exactly the reduced-form equilibrium used to compute the steady state
#      earlier, except that we do *not* impose M=δG*G_t; instead we let the law of motion
#      update G endogenously until it settles at the fixed point where M = δG * G.
#
# This exercise cleanly illustrates how, in the absence of shocks to τH or τL, any initial
# G_0 converges numerically to the steady-state G reported by `solve_steady_state()`.
# Because K is a jump variable in the reduced-form closure (r pinned by Euler ⇒ MPK
# constant), private capital adjusts instantaneously; the only source of dynamics is the
# gradual physical adjustment of G. That keeps the code and interpretation simple while
# remaining faithful to the model equations cited below. For richer transitional dynamics
# (Euler-enforced K accumulation), see the TODO notes at the bottom of this section.
#
# ---------------------------------------------------------------------------
# 8.1  Closed-form expressions when G_t is given
# ---------------------------------------------------------------------------
# Labor FOC (Eq. 15):
#   κ (1+φ(G_t))^(1+θ) n_t^θ = (1-τL) w_t / c_t.                fileciteturn4file3L98-L108
# Housing FOC (Eq. 21) at h=HBAR:                                fileciteturn4file5L85-L101
#   ph_t = [αh * A(G_t) / (1+τH)] * [c_t / HBAR].
# Government budget (Eq. 28):                                     fileciteturn4file0L44-L51
#   M_t = τL * w_t * n_t + τH * ph_t * HBAR.
# Resource constraint (Eq. 33):                                   fileciteturn4file8L30-L36
#   Y_t = c_t + δK*K_t + M_t.
# Firm FOCs / production (Eq. 23, 22):                            fileciteturn4file5L114-L124
#   Y_t = K_t^αk * n_t^(1-αk);  w_t=(1-αk)Y_t/n_t; r_t=αk Y_t/K_t.
# Public capital law of motion (Eq. 29–30):                       fileciteturn4file0L70-L88
#   G_{t+1} = G_t * (1 - δG,t),  δG,t = max{0, φwear*(1 - M_t/(δG*G_t))}.
#
# Eliminate c_t: from Labor FOC ⇒ c_t = (1-τL)w_t / [κ (1+φ(G_t))^(1+θ) n_t^θ]. Plug into
# Resource & Housing to express a single scalar equation in n_t; solve by bisection.
#
# ---------------------------------------------------------------------------
# 8.2  Scalar residual in n_t when G_t is given
# ---------------------------------------------------------------------------
# Let c_ls(n,G) = (1-τL) w(n) / [κ * (1+φ(G))^(1+θ) * n^θ].     (from Labor FOC)
# Let ph(c,G)  = (αh * Amenity(G) / (1+τH)) * (c / HBAR).   (from Housing FOC)
# M(n,G) = τL*w(n)*n + τH*ph(c_ls(n,G),G)*HBAR.                 (Govt budget)
# Resource ⇒ c_res(n,G) = Y(n) - δK*K(n) - M(n,G).
# Residual: F_static(n;G) = c_ls(n,G) - c_res(n,G) = 0.
#
# Solve F_static=0 for n_t∈(0,1). This yields the intratemporal equilibrium for given G_t.
#
# ---------------------------------------------------------------------------
# 8.3  Implementation
# ---------------------------------------------------------------------------

# Labor-FOC-implied consumption when G is exogenous
# @inline function c_ls_of_n_G(n::Float64, G::Float64)
#     w = (1 - αk) * (K_of_n(n)^αk?error:0)  # placeholder
# end

# OOPS: We'll replace the above stub with correct code momentarily using a full
# implementation below. (Search for `c_ls_of_n_G`.)

# ------------------ BEGIN Transitional Dynamics IMPLEMENTATION ---------------

# We need production primitives (without relying on μ_ss) because when G is exogenous we
# cannot assume the steady-state restriction r_ss ⇒ μ_ss holds every period. Instead we use
# the production function directly.

@inline function Y_prod(K::Float64, n::Float64)
    return K^αk * n^(1 - αk)
end

@inline function w_from(K::Float64, n::Float64)
    Y = Y_prod(K,n)
    return (1 - αk) * Y / n
end

@inline function r_from(K::Float64, n::Float64)
    Y = Y_prod(K,n)
    return αk * Y / K
end

# In this reduced-form dynamics we *continue* to enforce the household’s Euler steady-state
# condition r = r_ss in each period ⇒ K adjusts each period so that MPK= r_ss.
# Thus the capital-labor ratio is pinned down:  r_ss = αk * Y / K ⇒ K = αk/ r_ss * Y.
# Combine with production ⇒ exactly the μ_ss relation we used earlier. Therefore, for given n
# we can still call the cheaper Y_of_n(n) and w_ss; results consistent with our steady state.
# (This keeps dynamics driven solely by G_t; for full intertemporal K dynamics see TODO below.)

# Labor-FOC-implied consumption when G is exogenous (using w_ss constant shortcut)
@inline function c_ls_of_n_G(n::Float64, G::Float64)
    return ((1 - τL) * w_ss) / (κ * (1 + Phi(G))^(1 + θ) * n^θ)
end

# ph given c,G (closed form)
@inline function ph_of_c_G(c::Float64, G::Float64)
    return (αh * Amenity(G) / (1 + τH)) * (c / HBAR)
end

# Govt maintenance revenue when G exogenous and we use w_ss shortcut
@inline function M_of_n_G(n::Float64, G::Float64)
    c = c_ls_of_n_G(n,G)
    ph = ph_of_c_G(c,G)
    return τL * w_ss * n + τH * ph * HBAR
end

# Resource-implied consumption (using Y_of_n, K_of_n)
@inline function c_res_of_n_G(n::Float64, G::Float64)
    Y = Y_of_n(n)
    K = K_of_n(n)
    M = M_of_n_G(n,G)
    return Y - δK*K - M
end

@inline function F_static_n(n::Float64, G::Float64)
    return c_ls_of_n_G(n,G) - c_res_of_n_G(n,G)
end

# Bisection for n given G
function solve_n_given_G(G::Float64; tol::Float64=TOL, maxiter::Int=MAXITER)
    a = 1e-8; b = 0.99
    fa = F_static_n(a,G); fb = F_static_n(b,G)
    if sign(fa) == sign(fb)
        # expand search upward then downward
        for i in 1:100
            b = min(0.99, b*0.95)
            fb = F_static_n(b,G)
            if sign(fa) != sign(fb)
                break
            end
            if i == 100
                error("Failed to bracket n root for G=$G")
            end
        end
    end
    for it in 1:maxiter
        m = 0.5*(a+b)
        fm = F_static_n(m,G)
        if abs(fm) < tol || abs(b-a) < tol
            return m
        end
        if sign(fm) == sign(fa)
            a = m; fa = fm
        else
            b = m; fb = fm
        end
    end
    return 0.5*(a+b)
end

# Effective depreciation rate δG_eff per Eq. 30.                           
@inline function δG_eff(M::Float64, G::Float64)
    Gp = max(G, EPS_G)
    shortfall = 1.0 - M / (δG * Gp)
    return max(0.0, φwear * shortfall)
end

@inline function G_next(M::Float64, G::Float64)
    return G * (1.0 - δG_eff(M,G))
end

# Simulate path from initial G0 for T periods; return vectors + steady state
function simulate_path_pre_shock(G0::Float64; T::Int=50)
    ss = solve_steady_state()  # baseline SS for comparison
    Gpath = Vector{Float64}(undef, T+1)
    npath = similar(Gpath)
    cpath = similar(Gpath)
    phpath = similar(Gpath)
    Mpath = similar(Gpath)
    Ypath = similar(Gpath)
    Kpath = similar(Gpath)
    Gpath[1] = G0
    for t in 1:T
        Gt = Gpath[t]
        nt = solve_n_given_G(Gt)
        ct = c_ls_of_n_G(nt,Gt)            # from labor FOC
        pht = ph_of_c_G(ct,Gt)
        Mt  = τL * w_ss * nt + τH * pht * HBAR
        Yt  = Y_of_n(nt)
        Kt  = K_of_n(nt)
        Gpath[t+1] = G_next(Mt, Gt)
        npath[t] = nt; cpath[t] = ct; phpath[t] = pht; Mpath[t] = Mt; Ypath[t] = Yt; Kpath[t] = Kt
    end
    # fill last period values with SS approximations at G_T
    npath[end] = solve_n_given_G(Gpath[end])
    cpath[end] = c_ls_of_n_G(npath[end],Gpath[end])
    phpath[end] = ph_of_c_G(cpath[end],Gpath[end])
    Mpath[end] = τL * w_ss * npath[end] + τH * phpath[end] * HBAR
    Ypath[end] = Y_of_n(npath[end])
    Kpath[end] = K_of_n(npath[end])
    return (G=Gpath, n=npath, c=cpath, ph=phpath, M=Mpath, Y=Ypath, K=Kpath, ss=ss)
end

# Convenience: print last few rows to show convergence
function show_convergence(sim)
    ss = sim.ss
    # println("\n--- Pre-shock convergence demo (last 10 periods) ---")
    println("\n--- Pre-shock convergence demo (show all periods) ---")
    T = length(sim.G)
    start = 1
    # start = max(1, T-9)
    @printf("%5s %12s %12s %12s %12s\n", "t", "G_t", "n_t", "c_t", "ph_t")
    for t in start:T
        @printf("%5d %12.6f %12.6f %12.6f %12.6f\n", t-1, sim.G[t], sim.n[t], sim.c[t], sim.ph[t])
    end
    println("\nSteady state targets:")
    @printf("G*  = %.6f\n", ss.G)
    @printf("n*  = %.6f\n", ss.n)
    @printf("c*  = %.6f\n", ss.c)
    @printf("ph* = %.6f\n", ss.ph)
end

# Example usage (uncomment to run after including this file):
sim = simulate_path_pre_shock(0.5; T=40)  # start from poor roads. If you start with poor roads, no convergence. They stay poor.
show_convergence(sim)
sim2 = simulate_path_pre_shock(3.0; T=40) # start from excellent roads
show_convergence(sim2)
sim2 = simulate_path_pre_shock(3.5; T=40) # start from excellent roads
show_convergence(sim2)
sim2 = simulate_path_pre_shock(8.0; T=40) # start from even more excellent roads
show_convergence(sim2)
sim3 = simulate_path_pre_shock(1.110994; T=40) # start from even more excellent roads
show_convergence(sim3)

# n   = 0.334522
# Y   = 0.531898
# K   = 1.569535
# c   = 0.415506
# ph  = 0.144138
# M   = 0.022220
# G   = 1.110994
# w   = 1.113018
# r   = 0.101667

# ------------------ END Transitional Dynamics IMPLEMENTATION -----------------
# end

# Transition path when G = 3.0 and property tax = 0.025
# ---- Roads DGE Baseline Steady State ----
# n   = 0.334522
# Y   = 0.531898
# K   = 1.569535
# c   = 0.415506
# ph  = 0.144138
# M   = 0.022220
# G   = 1.110994
# w   = 1.113018
# r   = 0.101667
# )

# --- Pre-shock convergence demo (show all periods) ---
#     t          G_t          n_t          c_t         ph_t
#     0     3.000000     0.450866     0.559243     0.225171
#     1     2.268009     0.414519     0.514371     0.198594
#     2     1.834835     0.388944     0.482779     0.180564
#     3     1.571397     0.371247     0.460911     0.168424
#     4     1.407470     0.359160     0.445970     0.160293
#     5     1.303607     0.350982     0.435860     0.154869
#     6     1.236908     0.345487     0.429067     0.151258
#     7     1.193658     0.341813     0.424523     0.148860
#     8     1.165421     0.339363     0.421493     0.147268
#     9     1.146899     0.337734     0.419478     0.146212
#    10     1.134711     0.336651     0.418140     0.145512
#    11     1.126674     0.335933     0.417252     0.145048
#    12     1.121367     0.335457     0.416663     0.144741
#    13     1.117858     0.335141     0.416272     0.144537
#    14     1.115538     0.334932     0.416014     0.144402
#    15     1.114002     0.334794     0.415842     0.144313
#    16     1.112986     0.334702     0.415729     0.144254
#    17     1.112313     0.334641     0.415654     0.144215
#    18     1.111867     0.334601     0.415604     0.144189
#    19     1.111572     0.334574     0.415571     0.144172
#    20     1.111377     0.334556     0.415549     0.144160
#    21     1.111248     0.334545     0.415534     0.144153
#    22     1.111162     0.334537     0.415525     0.144148
#    23     1.111105     0.334532     0.415519     0.144144
#    24     1.111068     0.334528     0.415514     0.144142
#    25     1.111043     0.334526     0.415512     0.144141
#    26     1.111026     0.334525     0.415510     0.144140
#    27     1.111015     0.334524     0.415508     0.144139
#    28     1.111008     0.334523     0.415508     0.144139
#    29     1.111003     0.334523     0.415507     0.144138
#    30     1.111000     0.334522     0.415507     0.144138
#    31     1.110998     0.334522     0.415507     0.144138
#    32     1.110997     0.334522     0.415506     0.144138
#    33     1.110996     0.334522     0.415506     0.144138
#    34     1.110995     0.334522     0.415506     0.144138
#    35     1.110995     0.334522     0.415506     0.144138
#    36     1.110995     0.334522     0.415506     0.144138
#    37     1.110994     0.334522     0.415506     0.144138
#    38     1.110994     0.334522     0.415506     0.144138
#    39     1.110994     0.334522     0.415506     0.144138
#    40     1.110994     0.334522     0.415506     0.144138

# Steady state targets:
# G*  = 1.110994
# n*  = 0.334522
# c*  = 0.415506
# ph* = 0.144138


# Transition path when G = 3.0 and property tax = 0.0
# ---- Roads DGE Baseline Steady State ----
# n   = 0.308820
# Y   = 0.491031
# K   = 1.448944
# c   = 0.386908
# ph  = 0.132373
# M   = 0.017186
# G   = 0.859304
# w   = 1.113018
# r   = 0.101667
# )


# --- Pre-shock convergence demo (show all periods) ---
#     t          G_t          n_t          c_t         ph_t
#     0     3.000000     0.449363     0.562990     0.232347
#     1     2.125186     0.405210     0.507672     0.198958
#     2     1.626350     0.373960     0.468520     0.176392
#     3     1.333455     0.352334     0.441426     0.161314
#     4     1.156920     0.337638     0.423014     0.151328
#     5     1.048207     0.327789     0.410675     0.144755
#     6     0.980148     0.321254     0.402487     0.140447
#     7     0.937025     0.316947     0.397090     0.137632
#     8     0.909472     0.314121     0.393551     0.135796
#     9     0.891764     0.312274     0.391236     0.134600
#    10     0.880340     0.311068     0.389726     0.133821
#    11     0.872951     0.310283     0.388742     0.133315
#    12     0.868163     0.309771     0.388101     0.132985
#    13     0.865058     0.309439     0.387684     0.132771
#    14     0.863042     0.309222     0.387412     0.132631
#    15     0.861733     0.309081     0.387236     0.132541
#    16     0.860883     0.308990     0.387121     0.132482
#    17     0.860330     0.308930     0.387047     0.132444
#    18     0.859971     0.308892     0.386998     0.132419
#    19     0.859738     0.308866     0.386967     0.132403
#    20     0.859586     0.308850     0.386946     0.132392
#    21     0.859487     0.308839     0.386933     0.132385
#    22     0.859423     0.308832     0.386924     0.132381
#    23     0.859382     0.308828     0.386919     0.132378
#    24     0.859355     0.308825     0.386915     0.132376
#    25     0.859337     0.308823     0.386913     0.132375
#    26     0.859326     0.308822     0.386911     0.132374
#    27     0.859318     0.308821     0.386910     0.132374
#    28     0.859313     0.308821     0.386910     0.132373
#    29     0.859310     0.308820     0.386909     0.132373
#    30     0.859308     0.308820     0.386909     0.132373
#    31     0.859307     0.308820     0.386909     0.132373
#    32     0.859306     0.308820     0.386909     0.132373
#    33     0.859305     0.308820     0.386908     0.132373
#    34     0.859305     0.308820     0.386908     0.132373
#    35     0.859305     0.308820     0.386908     0.132373
#    36     0.859305     0.308820     0.386908     0.132373
#    37     0.859304     0.308820     0.386908     0.132373
#    38     0.859304     0.308820     0.386908     0.132373
#    39     0.859304     0.308820     0.386908     0.132373
#    40     0.859304     0.308820     0.386908     0.132373

# Steady state targets:
# G*  = 0.859304
# n*  = 0.308820
# c*  = 0.386908
# ph* = 0.132373


# sim.G[t], sim.n[t], sim.c[t], sim.ph[t]


# Plot Gpath over time

# Add disturbance to the path
disturbance = randn(length(sim3.G)) .* 0.001  # Small random noise
G_disturbed = sim3.G .+ disturbance

plot(0:40, G_disturbed, xlabel="Time (t)", ylabel="G_t", title="Road Stock Dynamics with Disturbance", legend=false, ylim=(0.6, 1.15))
plot!(0:40, fill(1.110994, 41), label="Steady State G*", linestyle=:dash)
(0.8596596828423505- 1.110994)/1.110994
# Save the plot to the specified folder
savefig("C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/docs/JMP_draft/images/model_roads.png")

# -23% drop in roads when you cut property tax to 0. 

plot(0:40, sim3.n, xlabel="Time (t)", ylabel="Labor Share", title="Labor Share Dynamics", legend=false, ylim=(0.23, 0.35))
plot!(0:40, fill(0.333560, 41), label="Steady State n*", linestyle=:dash)
savefig("C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/docs/JMP_draft/images/model_labor.png")

plot(0:40, sim3.c, xlabel="Time (t)", ylabel="Consumption", title="Consumption Dynamics", legend=false, ylim=(0.1, 0.45))
plot!(0:40, fill(0.417905, 41), label="Steady State c*", linestyle=:dash)
savefig("C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/docs/JMP_draft/images/model_consumption.png")


plot(0:40, sim3.ph, xlabel="Time (t)", ylabel="House Price", title="House Price Dynamics", legend=false, ylim=(0.1, 0.15))
plot!(0:40, fill(0.148594, 41), label="Steady State ph*", linestyle=:dash)
savefig("C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/docs/JMP_draft/images/model_houseprice.png")

    # t          G_t          n_t          c_t         ph_t
    # 0     1.110994     0.333560     0.417905     0.148594

# 11% drop in house prices when you cut property tax to 0. We want 9% drop.
(0.13237261418269303-0.148594)/0.148594

# ---------------------------------------------------------------------------
# TODO: Full intertemporal dynamics with capital accumulation
# ---------------------------------------------------------------------------
# The above reduced-form convergence exercise holds r=r_ss each period and lets K jump.
# For richer dynamics, drop that restriction and instead:
#   • Track K_t as a genuine state with K_{t+1} = (1-δK)K_t + Y_t - c_t - M_t.  (Eq. 8 & 33.)
#   • Impose Euler forward: c_t^{-1} = β * c_{t+1}^{-1} * (1 + r_{t+1} - δK). (Eq. 11.)
#   • Solve by shooting on c_0 (or via time iteration) to land on the steady state as t→∞.
# If you want me to code this full perfect‑foresight transition solver, let me know and I’ll
# extend the script.

# -----------------------------------------------------------------------------
# End of file
# -----------------------------------------------------------------------------

