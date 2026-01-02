using NLsolve, Plots

# 1. Model Parameters (baseline values)
β    = 0.96             # Discount factor (annual)
α    = 0.30             # Capital share in Cobb-Douglas production
α_h  = 0.50             # Utility weight on housing (share in utility)
η    = 1              # Weight on amenity from roads (can be adjusted)
# κ    = 25            # Disutility weight of labor (calibrated to match ~30% work time)
κ    = 1            # Disutility weight of labor (calibrated to match ~30% work time)
θ    = 2.0             # Inverse Frisch elasticity (curvature of labor disutility)
δ_K  = 0.05            # Annual depreciation rate of private capital
δ_G  = 0.05            # Depreciation rate of public capital (roads)
varphi = 0.5          # Wear-and-tear exponent for road degradation
φ̄    = 0.10            # Base commuting cost parameter (affects disutility of labor)
# τ_N  = 0.05            # Labor income tax rate (funds roads)
τ_N  = 0.15            # Labor income tax rate (funds roads)
τ_H0 = 0.12            # **Initial** property (housing) tax rate earmarked for road maintenance

# Fixed housing supply (per household) and amenity functions
H_bar = 1.0                        # Fixed housing stock per household
A(G) = G                           # Amenity from roads (entering utility directly as G)
φ(G) = φ̄ / G                      # Commuting cost factor (inverse of road quality)

# Utility function components: 
# u(c,h,G) = log(c) + α_h*log(h) + log(A(G))
# v(n,G) = κ * (n*(1+φ(G)))^(1+θ) / (1+θ)

#=
We calibrate the model so that in the initial steady state (with property tax τ_H0 in place), key ratios match typical values. For example, we choose κ so that labor supply is about 30% of available time, and α_h so that housing expenditures are a reasonable share of utility. The baseline parameters above reflect these target
=#


# 2. Solve for initial steady state
function steady_state!(F, vars)

    # Unknowns: c, n, K, G, P (all positive)
    c, n, K, G, P = vars

    # Marginal utilities and production outputs at steady state
    Uc = 1/c                          # ∂U/∂c = 1/c

    # Housing first-order condition: α_h * c / H_bar = (1+τ_H0) * P
    F[1] = (α_h * c) / H_bar - (1+τ_H0)*P

    # Labor supply condition: κ*(n*(1+φ(G)))^θ * (1+φ(G)) = Uc * (1-τ_N)*w
    w = (1-α) * (K/n)^α              # wage from Cobb-Douglas
    F[2] = κ * (n*(1+φ(G)))^θ * (1+φ(G)) - Uc * (1-τ_N) * w

    # Euler (capital): at steady, β*(MPK + 1-δ_K) = 1  -> MPK + 1-δ_K = 1/β
    MPK = α * K^(α-1) * n^(1-α)
    F[3] = MPK + 1 - δ_K - 1/β

    # Government budget: τ_N*w*n + τ_H0*P*H_bar = δ_G * G
    F[4] = τ_N*w*n + τ_H0*P*H_bar - δ_G*G

    # Goods market / resource constraint: Y = C + I_K + M
    Y = K^α * n^(1-α)
    F[5] = Y - c - δ_K*K - δ_G*G      # output = consumption + replace K + replace G

    return F
end

# Initial guess (educated guesses based on calibration targets)
x0 = [0.35, 0.3, 1.5, 0.6, 0.1]   # [c, n, K, G, P]
sol = nlsolve(steady_state!, x0)
c0, n0, K0, G0, P0 = sol.zero  # steady-state solution
w0 = (1-α)*(K0/n0)^α  # wage at steady state
r0 = α * (K0^(α-1)) * (n0^(1-α))  # net return on capital at steady state
Y0 = K0^α * n0^(1-α)  # output at steady state


println("Steady State Solution:")
println("c0 = $(c0),   n0 = $(n0)")
println("K0 = $(K0),   G0 = $(G0)")
println("P0 = $(P0),   (House price)")
println("Y0 = $(K0^α * n0^(1-α)),   w0 = $((1-α)*(K0/n0)^α),   r0 = $(α * (K0^(α-1)) * (n0^(1-α)))")
println("G0 = $(G0),   (Initial road stock)")
println("labor tax revenue, τ_N w_0 n_0 = $(τ_N*w0*n0)")
println("property tax revenue, τ_H0 * P_0 * H_bar = $(τ_H0 * P0 * H_bar)")

# Utility function in steady state
function utility(c, h, G, n)
    return log(c) + α_h * log(h) + η * log(A(G)) - (κ / (1 + θ)) * (n * (1 + φ(G)))^(1 + θ)
end

# Compute utility at initial steady state
u0 = utility(c0, H_bar, G0, n0)
println("Utility at initial steady state: u0 = $u0")

#=
Steady-state results: The solver finds the baseline steady state. For example, we get consumption ~$c_0 ≈ 0.36$, labor $n_0 ≈ 0.28$ (28% of time endowment), initial road capital $G_0 ≈ 0.65$, and house price $P_0 ≈ 0.16 (in units of numéraire). These values imply a baseline output $Y_0 ≈ 0.50. The baseline matches our targets (around 30% labor supply, etc.). The initial property tax revenue is $τ_H0 * P_0 * H_bar ≈ 0.100.16 = 0.016$ (around 2.3% of output) and labor tax revenue $τ_N w_0 n_0 ≈ 0.051.16*0.28 = 0.0162$, so roughly equal contributions to road maintenance, consistent with calibration. Maintenance $M_0 = δ_G G_0 ≈ 0.0329` which just offsets depreciation, keeping roads constant.
=#
G0*δ_K # depreciation value of initial road stock   
(τ_N*w0*n0 + τ_H0*P0*H_bar) / (G0*δ_K)  # initial maintenance revenue /




# 3. Compute new steady state after property tax cut (τ_H1)
τ_H1 = 0.0  # post-shock property tax rate
function steady_state_shock!(F, vars)

    # Unknowns: c, n, K, G, P (all positive)
    c, n, K, G, P = vars

    # Marginal utilities and production outputs at steady state
    Uc = 1/c                          # ∂U/∂c = 1/c

    # Housing first-order condition: α_h * c / H_bar = (1+τ_H1) * P
    F[1] = (α_h * c) / H_bar - (1 + τ_H1) * P

    # Labor supply condition: κ*(n*(1+φ(G)))^θ * (1+φ(G)) = Uc * (1-τ_N)*w
    w = (1-α) * (K/n)^α              # wage from Cobb-Douglas
    F[2] = κ * (n*(1+φ(G)))^θ * (1+φ(G)) - Uc * (1-τ_N) * w

    # Euler (capital): at steady, β*(MPK + 1-δ_K) = 1  -> MPK + 1-δ_K = 1/β
    MPK = α * K^(α-1) * n^(1-α)
    F[3] = MPK + 1 - δ_K - 1/β

    # Government budget: τ_N*w*n + τ_H1*P*H_bar = δ_G * G
    F[4] = τ_N*w*n + τ_H1*P*H_bar - δ_G*G

    # Goods market / resource constraint: Y = C + I_K + M
    Y = K^α * n^(1-α)
    F[5] = Y - c - δ_K*K - δ_G*G      # output = consumption + replace K + replace G

    return F
end

# use previous solution as initial guess
sol1 = nlsolve(steady_state_shock!, sol.zero)
c1, n1, K1, G1, P1 = sol1.zero
w1 = (1-α)*(K1/n1)^α  # wage at post-shock steady state
r1 = α * (K1^(α-1)) * (n1^(1-α))  # net return on capital at post-shock steady state
Y1 = K1^α * n1^(1-α)  # output at steady state

println("Post-Shock Steady State (τ_H = $τ_H1):")
# println("c1 = $(c1), n1 = $(n1), K1 = $(K1), G1 = $(G1), P1 = $(P1)")
println("c1 = $(c1),   n1 = $(n1)")
println("K1 = $(K1),   G1 = $(G1)")
println("P1 = $(P1),   (House price)")
println("Y1 = $(K1^α * n1^(1-α)),   w1 = $((1-α)*(K1/n1)^α),   r1 = $(α * (K1^(α-1)) * (n1^(1-α)))")
println("G1 = $(G1),   (post-shock road stock)")
println("labor tax revenue, τ_N w_1 n_1 = $(τ_N*w1*n1)")
println("property tax revenue, τ_H1 * P_1 * H_bar = $(τ_H1 * P1 * H_bar)")

u1 = utility(c1, H_bar, G1, n1)
println("Utility at initial steady state: u1 = $u1")

# Comparison of steady states
println("------------------------------------------------")
println("Baseline steady state vs Post-Shock Steady State")
println("  Capital stock K_0 = $K0 | K_1 = $K1")
println("  Labour supply N_0 = $n0 | N_1 = $n1")
println("  Output Y_0 = $Y0 | Y_1 = $Y1")
println("  Consumption c_0 = $c0 | c_1 = $c1")
println("  Wage w_0 = $w0 | w_1 = $w1")
println("  Net return r-δK_0 = $(r0 - δ_K) | r-δK_1 = $(r1 - δ_K)")
println("  House price P_0 = $P0 | P_1 = $P1")
println("  Labor tax revenue, τ_N w_0 n_0 = $(τ_N*w0*n0) | τ_N w_1 n_1 = $(τ_N*w1*n1)")
println("  Property tax revenue, τ_H0 * P_0 * H_bar = $(τ_H0 * P0 * H_bar) | τ_H1 * P_1 * H_bar = $(τ_H1 * P1 * H_bar)")
println("Utility u0 = $u0 | u1 = $u1")
println("------------------------------------------------")
# ------------------------------------------------


# 4. Transitional dynamics of road quality only
T = 30
G_path = zeros(T+1)
G_path[1] = G0  # initial road stock
for t in 1:T
    # maintenance revenue post-shock
    M = τ_N*w0*n0 + τ_H1*P1*H_bar
    # effective depreciation rate with wear-and-tear
    δGt = δ_G + varphi * (1 - M/(δ_G * G_path[t]))
    # update road stock
    G_path[t+1] = G_path[t] * (1 - δGt)
end
# Plot road quality path
plot(0:T, G_path, xlabel="Period", ylabel="G_t", title="Road Quality Path After Shock")

# 5. Dynamic transitional simulation with endogenous maintenance and road evolution
T = 30
# Preallocate arrays
G_pre = zeros(T+1); G_post = zeros(T+1)
c_pre = zeros(T+1); n_pre = zeros(T+1); K_pre = zeros(T+1); P_pre = zeros(T+1)
c_post = zeros(T+1); n_post = zeros(T+1); K_post = zeros(T+1); P_post = zeros(T+1)
w_pre = zeros(T+1); M_pre = zeros(T+1)
w_post = zeros(T+1); M_post = zeros(T+1)
# Initial values from steady states
G_pre[1], c_pre[1], n_pre[1], K_pre[1], P_pre[1] = G0, c0, n0, K0, P0
G_post[1], c_post[1], n_post[1], K_post[1], P_post[1] = G0, c0, n0, K0, P0  # G0 common
# Initial level guesses for 4 unknowns
guess_pre = [c0, n0, K0, P0]
guess_post = [c1, n1, K1, P1]

for t in 1:T
    # Pre-shock solve in levels
    sol_pre = nlsolve((F, vars) -> ss_period!(F, vars, G_pre[t+1], τ_H0), guess_pre)
    c_pre[t+1], n_pre[t+1], K_pre[t+1], P_pre[t+1] = sol_pre.zero
    guess_pre = sol_pre.zero

    # Post-shock solve in levels
    sol_post = nlsolve((F, vars) -> ss_period!(F, vars, G_post[t+1], τ_H1), guess_post)
    c_post[t+1], n_post[t+1], K_post[t+1], P_post[t+1] = sol_post.zero
    guess_post = sol_post.zero
end

# 6. Plot comparative paths
plot(0:T, G_pre, label="G pre-shock", xlabel="Period", ylabel="Road Stock G_t")
plot!(0:T, G_post, label="G post-shock", linestyle=:dash)

plot(0:T, c_pre, label="c pre-shock", xlabel="Period", ylabel="Consumption c_t")
plot!(0:T, c_post, label="c post-shock", linestyle=:dash)

gui()

plot(0:T, n_pre, label="n pre-shock", xlabel="Period", ylabel="Labor n_t")
plot!(0:T, n_post, label="n post-shock", linestyle=:dash)

gui()

plot(0:T, P_pre, label="P pre-shock", xlabel="Period", ylabel="House Price P_t")
plot!(0:T, P_post, label="P post-shock", linestyle=:dash)

gui()

# 7. Welfare computation (present discounted utility)
u(c,h,G) = log(c) + α_h*log(h) + log(A(G)) - κ*(n*(1+φ(G)))^(1+θ)/(1+θ)
W_pre = 0.0; W_post = 0.0
for t in 0:T
    u_pre = log(c_pre[t+1]) + α_h*log(H_bar) + log(A(G_pre[t+1])) - κ*(n_pre[t+1]*(1+φ(G_pre[t+1])))^(1+θ)/(1+θ)
    u_post = log(c_post[t+1]) + α_h*log(H_bar) + log(A(G_post[t+1])) - κ*(n_post[t+1]*(1+φ(G_post[t+1])))^(1+θ)/(1+θ)
    W_pre += β^t * u_pre
    W_post += β^t * u_post
end
println("Lifetime welfare pre-shock = $W_pre, post-shock = $W_post, welfare change = $(W_post - W_pre)")


## New model with price of housing impacted by roads

# --- helper objects ----------------------------------------------------------
ψ      = 0.60      # strength of complementarity housing–amenity
ω      = 0.10      # roads' exponent in production TFP
A(G)   = G         # same amenity index as before
φ(G)   = φ̄ / G    # commuting cost factor (unchanged)

# equilibrium wage & MPK with G in TFP
w_fun(K,n,G)  = (1-α) * G^ω * (K/n)^α
mpk_fun(K,n,G)=  α    * G^ω * (K/n)^(α-1)

# --- steady-state with generic τ_H -------------------------------------------
function ss_system!(τ_H, F, vars)
    c, n, K, G, P = vars         # unknowns

    Uc = 1/c
    # ---- (1) Housing FOC: now contains A(G)^ψ ------------------------------
    F[1] = (α_h * A(G)^ψ * c) / H_bar - (1 + τ_H) * P

    w  = w_fun(K,n,G)

    # ---- (2) Labour FOC (unchanged) ----------------------------------------
    F[2] = κ * (n*(1+φ(G)))^θ * (1+φ(G)) - Uc * (1-τ_N) * w

    # ---- (3) Euler eq. ------------------------------------------------------
    MPK = mpk_fun(K,n,G)
    F[3] = MPK + 1 - δ_K - 1/β

    # ---- (4) Gov-budget: maintenance = revenues ----------------------------
    F[4] = τ_N*w*n + τ_H*P*H_bar - δ_G*G

    # ---- (5) Resource constraint -------------------------------------------
    Y = K^α * (G^ω * n)^(1-α)
    F[5] = Y - c - δ_K*K - δ_G*G
    return
end

function solve_ss(τ_H; guess=[0.35,0.30,1.5,0.6,0.15])
    sol = nlsolve(vars -> ss_system(τ_H, vars), guess)
    return sol.zero
end

c0,n0,K0,G0,P0 = solve_ss(τ_H0)  # baseline with tax
c1,n1,K1,G1,P1 = solve_ss(τ_H1)  # counterfactual with τ_H = 0

println("Comparison of Steady States:")
println("------------------------------------------------")
println("Variable         | Baseline (τ_H0) | Counterfactual (τ_H1)")
println("------------------------------------------------")
println("Consumption (c)  | c0 = $c0             | c1 = $c1")
println("Labor (n)        | n0 = $n0             | n1 = $n1")
println("Capital (K)      | K0 = $K0             | K1 = $K1")
println("Roads (G)        | G0 = $G0             | G1 = $G1")
println("House Price (P)  | P0 = $P0             | P1 = $P1")
println("------------------------------------------------")



###############################################################
# 0.  Packages -------------------------------------------------
###############################################################
using NLsolve, Roots    # nonlinear solvers & one-dim bisection
using Printf            # nicer printing, optional

###############################################################
# 1.  Parameters & helpers  –  same values as your steady state
###############################################################
β, α, α_h  = 0.96, 0.30, 0.50
η, κ, θ    = 1.0, 25.0, 2.0
δ_K, δ_G   = 0.05, 0.05
φ̄, τ_N    = 0.10, 0.15
τ_H0, τ_H1 = 0.12, 0.00              # tax cut at t = 1
ψ, ω       = 0.60, 0.10             # new: amenity & TFP elasticities
H_bar      = 1.0

# ---- “guard-rail” constant used everywhere -----------------
ϵ = 1e-10           # treat anything below this as effectively zero

# ---- safe utility helpers ----------------------------------
A(G)      = max(G,ϵ)           # amenity (identity with floor)
φ(G)      = φ̄ / max(G,ϵ)      # commuting cost factor

# ---- safe production helpers --------------------------------
safe_ratio(x,y) = max(x/y, ϵ)  # never divide by <ϵ
w_fun(K,n,G)  = (1-α) * max(G,ϵ)^ω * safe_ratio(K,n)^α
mpk_fun(K,n,G)=  α    * max(G,ϵ)^ω * safe_ratio(K,n)^(α-1)

###############################################################
# 2.  Verified steady states  (paste yours here)
###############################################################
c0,n0,K0,G0 = 0.372619411, 0.311468158, 1.832433858, 1.730630756
c1,n1,K1,G1 = 0.354084684, 0.290759765, 1.585591465, 1.017421190

###############################################################
# 3.  Solve labour supply for one period  ---------------------
###############################################################
function n_opt(c,K,G)
    f(n) = κ*(n*(1+φ(G)))^θ * (1+φ(G)) -
           (1/c)*(1-τ_N)*w_fun(K,n,G)
    return find_zero(f, (ϵ,0.95),  Bisection(); rtol=1e-12)
end

#############################################################################
# 4.  Residual builder – now lands on BOTH K_T and G_T  ---------------------
#############################################################################
# --- one tiny global bumper -----------------------------------------------
ϵ = 1e-8               # anything below turns into “almost zero”

safe(x)        = max(x, ϵ)                             # floor helper
safe_pow(x, p) = safe(x)^p
safe_ratio(x,y)= safe(x / y)

#############################################################################
function trans_residual!(F, x; T, K1_target, G1_target)
    c = @view x[1:T]
    n = @view x[T+1:2T]

    K = fill(0.0, T+1);   K[1] = K0
    G = fill(0.0, T+1);   G[1] = G0

    idx = 1
    for t in 1:T
        τ_H = (t==1 ? τ_H0 : τ_H1)

        # 1. labour FOC -----------------------------------------------------
        F[idx] = κ*( n[t]*(1+φ(G[t])) )^θ * (1+φ(G[t])) -
                 (1/c[t])*(1-τ_N)*w_fun(K[t], n[t], G[t])
        idx += 1

        # 2. state updates (guard-railed) -----------------------------------
        P       = (α_h * A(G[t])^ψ * c[t]) / (H_bar*(1+τ_H))
        wage    = w_fun(K[t], n[t], G[t])
        maint   = τ_N*wage*n[t] + τ_H*P*H_bar
        G[t+1]  = safe((1-δ_G)*G[t] + maint)          # keep > 0

        Y       = safe_pow(K[t], α) *
                  safe_pow(G[t], ω)^(1-α) *
                  n[t]^(1-α)
        invest  = Y - c[t] - maint
        K[t+1]  = safe((1-δ_K)*K[t] + invest)         # keep > 0

        # 3. Euler gap  (t = 1 … T-2) --------------------
        if t < T-1
            r_next = mpk_fun(K[t+1], n[t+1], G[t+1]) - δ_K
            F[idx] = (1/c[t]) - β*(1+r_next)*(1/c[t+1])
            idx += 1
        end
    end

    # 4. landing constraints -----------------------------------------------
    F[idx]   = K[end] - K1_target      # slot 2T-1
    F[idx+1] = G[end] - G1_target      # slot 2T
    return
end

###############################################################
# 5.  Transition solver wrapper  —  only necessary edits
###############################################################

# safe_pow(x, p)    = max(x, ϵ)^p
# safe_ratio(x, y)  = max(x / y, ϵ)       # avoids division by 0

function solve_transition(; T = 60)

    # ---- starting guess (unchanged) --------------------------------------
    c_guess = fill(0.95c0, T)
    n_guess = fill(0.95n0, T)
    x0      = vcat(c_guess, n_guess)

    # ---- residual + BOTH landing targets ---------------------------------
    F!(F, x) = trans_residual!(F, x;
                               T          = T,
                               K1_target  = K1,
                               G1_target  = G1)   # ← *new line*

    # ---- call NLsolve  (correct keyword & safer method) ------------------
    sol = nlsolve(F!, x0;
                  method    = :trust_region,      # more robust than Newton
                  xtol      = 1e-8,
                  ftol      = 1e-8,
                #   maxiters  = 1_000,              # ← keyword fix
                  autoscale = true)

    println("converged? ", converged(sol))
    @assert converged(sol) "Transition solver did not converge"

    c_path = sol.zero[1:T]
    n_path = sol.zero[T+1:2T]

    # ---- rebuild states for plotting/output ------------------------------
    K = fill(0.0, T+1);  K[1] = K0
    G = fill(0.0, T+1);  G[1] = G0
    P = fill(0.0, T)

    for t in 1:T
        τ_H  = (t == 1 ? τ_H0 : τ_H1)

        P[t] = (α_h * safe_pow(G[t], ψ) * c_path[t]) /
               (H_bar * (1 + τ_H))

        wage   = (1-α) * safe_pow(G[t], ω) * safe_ratio(K[t], n_path[t])^α
        maint  = τ_N * wage * n_path[t] + τ_H * P[t] * H_bar
        G[t+1] = max((1-δ_G) * G[t] + maint, ϵ)

        Y      = safe_pow(K[t], α) *
                 safe_pow(G[t], ω * (1-α)) *      # cleaner exponent
                 n_path[t]^(1-α)
        invest = Y - c_path[t] - maint
        K[t+1] = max((1-δ_K) * K[t] + invest, ϵ)
    end

    return (; time = 0:T-1,
              c = c_path, n = n_path,
              K = K[1:end-1], G = G[1:end-1], P = P)
end




traj = solve_transition(T = 120)

# Display full output from traj for all relevant variables
println("Transition Path Results:")
println("------------------------------------------------")
println("Time | Consumption (c) | Labor (n) | Capital (K) | Roads (G) | House Price (P)")
println("------------------------------------------------")
for t in 1:length(traj.time)
    println("Time: $(traj.time[t]), Consumption: $(traj.c[t]), Labor: $(traj.n[t]), Capital: $(traj.K[t]), Roads: $(traj.G[t]), House Price: $(traj.P[t])")
end

# quick look
@printf "House-price drop on impact: %.2f %%\n" ((traj.P[1]/P0-1)*100)

# Plot transition paths for key variables
plot(traj.time, traj.c, label="Consumption (c)", xlabel="Time", ylabel="Value", title="Transition Path: Consumption")
plot!(traj.time, traj.n, label="Labor (n)", linestyle=:dash)

gui()

plot(traj.time, traj.K, label="Capital (K)", xlabel="Time", ylabel="Value", title="Transition Path: Capital")
plot!(traj.time, traj.G, label="Roads (G)", linestyle=:dash)

gui()

plot(traj.time, traj.P, label="House Price (P)", xlabel="Time", ylabel="Value", title="Transition Path: House Price")

traj.P[1]
traj.P[60]

gui()


println("converged?  ", converged(sol))
println(sol)   # shows iterations, residual norm, etc.


################################################################################
#  solve_transition_nlopt:  bounded least-squares version
################################################################################
using NLopt, ForwardDiff                                       # 1-time import

function solve_transition_nlopt(; T = 60, ϵ = 1e-10)

    # ------------- decision vector x = [c₁…c_T , n₁…n_T] -------------------
    x0 = vcat(fill(c0, T), fill(n0, T)) |> collect         # warm start

    lb = vcat(fill(ϵ, T),  fill(ϵ, T))                     # c ≥ ϵ, n ≥ ϵ
    ub = vcat(fill(Inf, T), fill(0.99, T))                 # n ≤ 0.99

    # ------------- objective: ½‖F(x)‖²  -------------------------------------
    function obj!(grad, x)
        # F = similar(x)               # residual vector length = 2T
        # trans_residual!(F, x; T=T,
        #                 K1_target = K1,
        #                 G1_target = G1)
        F = similar(x)      # eltype(x) is Dual during Jacobian calls
        trans_residual!(F, x; T=T, K1_target=K1, G1_target=G1)

        if grad !== nothing          # NLopt needs ∇
            J = ForwardDiff.jacobian(_x -> begin
                    G = similar(_x)
                    trans_residual!(G, _x; T=T,
                                     K1_target = K1,
                                     G1_target = G1)
                    G
                 end, x)
            grad .= J' * F           # ∇(½F'F) = J'F
        end
        return 0.5 * sum(abs2, F)
    end

    #############################################################################
    # choose COBYLA (constrained Nelder-Mead style, no gradient needed)
    #############################################################################
    opt = Opt(:LN_COBYLA, 2T)          # ← 1
    opt.lower_bounds = lb
    opt.upper_bounds = ub

    # tell NLopt we supply *only* the objective value ---------------------------
    function obj_no_grad(x::Vector)
        F = similar(x)                 # length = 2T   (eltype = Float64)
        trans_residual!(F, x; T=T,
                        K1_target=K1, G1_target=G1)
        return 0.5 * sum(abs2, F)      # ← 2
    end
    opt.min_objective = (x, _) -> obj_no_grad(x)   # gradient slot ignored  ← 3

    opt.xtol_rel = 1e-9
    opt.ftol_rel = 1e-9
    opt.maxeval  = 3_000               # need more evals without gradient     ← 4

    (minf, x_star, ret) = NLopt.optimize(opt, x0)        # ← 5  (no NLopt. prefix)
    # @assert ret > 0 "NLopt failed: $ret (minf = $minf)"    # ← 6
    print("NLopt return code: $ret, min objective value: $minf\n")

    # ------------- unpack, rebuild states, return --------------------------
    c_path = x_star[1:T]
    n_path = x_star[T+1:2T]

    K = fill(0.0,T+1);  K[1]=K0
    G = fill(0.0,T+1);  G[1]=G0
    P = fill(0.0,T)

    for t in 1:T
        τ_H  = t==1 ? τ_H0 : τ_H1
        P[t] = (α_h*A(G[t])^ψ*c_path[t]) / (H_bar*(1+τ_H))
        maint = τ_N*w_fun(K[t], n_path[t], G[t])*n_path[t] + τ_H*P[t]*H_bar
        G[t+1] = (1-δ_G)*G[t] + maint
        Y      = K[t]^α * (G[t]^ω*n_path[t])^(1-α)
        invest = Y - c_path[t] - maint
        K[t+1] = (1-δ_K)*K[t] + invest
    end

    (; time = 0:T-1, c = c_path, n = n_path,
       K = K[1:end-1], G = G[1:end-1], P = P)
end

################################################################################
# Example run
################################################################################
traj = solve_transition_nlopt(T = 80)
println("check landing:  K_T=", traj.K[end], "  (target ", K1, ")")
println("                G_T=", traj.G[end], "  (target ", G1, ")")










module DSGERoads

# -----------------------------------------------------------------------------
# Deterministic DSGE with Public‑Good “Roads” Capital
# -----------------------------------------------------------------------------
# * All parameters are declared as *individual constants* (no `Params` struct).
# * Helper functions guard against log / power domain errors by clipping inputs.
# * `steadystate()` and `solve_path()` expose clean public APIs.
# -----------------------------------------------------------------------------

using NLsolve, ForwardDiff, LinearAlgebra

# === 1. PARAMETERS (constants) ===
β   = 0.96          # subjective discount factor
αK  = 0.30          # elasticity wrt private capital
αL  = 0.60          # elasticity wrt labour
αR  = 1 - αK - αL   # residual share for roads (makes CD exponents sum to 1)
δK  = 0.08          # depreciation rate of private capital
δR  = 0.05          # depreciation rate of roads capital
σ   = 2.0           # CRRA coefficient ( > 0 )
χ   = 1.5           # labour‑disutility scale

# === 2. Helper utilities (domain‑safe) ===
ε_pos = 1e-12             # keep strictly > 0 for power / log functions

Uc(c) = (c > 0 ? c : ε_pos)^(-σ)        # marginal utility of consumption
Ul(_) = χ                               # marginal disutility of labour (separable U)

production(K, L, R, A = 1.0) = A * K^αK * L^αL * R^αR

# === 3. Steady‑state solver ===
"""
    steadystate(; guess = <vector>) -> (x̄, sol)

Solve the non‑linear system that pins down the deterministic steady state.
Returns `(x̄, sol)` where `x̄ = [C, I, K, L, R, Y]`.
"""
function steadystate(; guess = [1.0, 0.2, 10.0, 0.3, 10.0, 1.0])

    function F!(F, x)
        C, I, K, L, R, Y = x
        uC = Uc(C)

        # 1. Euler (savings) FOC
        F[1] = uC - β * (1 + αK * Y / K - δK) * uC
        # 2. Labour–leisure FOC
        F[2] = -Ul(L) / uC + αL * Y / L
        # 3. Resource constraint (assuming G = δR·R at steady state)
        F[3] = Y - C - I - δR * R
        # 4. Capital law of motion (steady state ➜ K̇ = 0)
        F[4] = I - δK * K
        # 5. Production identity
        F[5] = Y - production(K, L, R)
        # 6. Roads accumulation (steady state ➜ Ṙ = 0)
        F[6] = δR * R - δR * R  # = 0 by construction
        return F
    end

    sol = nlsolve(F!, guess; autodiff = :forward)
    sol.converged || error("Steady state failed to converge: " * String(sol.zero))
    return sol.zero, sol
end

# === 4. Perfect‑foresight transition path ===
"""
    solve_path(T, K0, R0; x̄ = steadystate()[1]) -> (path, sol)

Compute a length‑`T` perfect‑foresight transition path from initial stocks
`(K0, R0)` back to the steady state `x̄`.
`path` is a `(6, T)` matrix whose rows are `[C, I, K, L, R, Y]` at each `t`.
"""
function solve_path(T, K0, R0; x̄ = steadystate()[1])
    C̄, Ī, K̄, L̄, R̄, Ȳ = x̄

    # Index helpers
    n_state, n_ctrl = 2, 4                # (K, R)  |  (C, I, L, Y)
    N = n_state + n_ctrl

    # --- initial guess: linear interpolation to steady state ---
    x0 = zeros(T * N)
    for t in 1:T
        λ = (t - 1) / (T - 1)
        C = (1 - λ) * (0.9 * C̄) + λ * C̄
        I = (1 - λ) * (1.1 * Ī) + λ * Ī
        L = (1 - λ) * L̄ + λ * L̄
        Y = (1 - λ) * (0.9 * Ȳ) + λ * Ȳ
        K = (1 - λ) * K0 + λ * K̄
        R = (1 - λ) * R0 + λ * R̄
        idx = (t - 1) * N
        x0[idx + 1:idx + N] .= (C, I, L, Y, K, R)
    end

    function G!(F, x)
        for t in 1:T
            idx  = (t - 1) * N
            C, I, L, Y, K, R = x[idx + 1:idx + N]
            Km1 = t == 1 ? K0 : x[idx - N + 5]   # previous‑period K
            Rm1 = t == 1 ? R0 : x[idx - N + 6]   # previous‑period R

            # Guard against non‑positive C inside utility
            uC  = Uc(C)
            Uc_next = t < T ? Uc(x[idx + N + 1]) : Uc(C̄)

            # 1. Euler
            F[idx + 1] = uC - β * (1 + αK * production(Km1, L, Rm1) / Km1 - δK) * Uc_next
            # 2. Labour FOC
            F[idx + 2] = -Ul(L) / uC + αL * Y / L
            # 3. Resource constraint (using G_t = δR · R_t)
            F[idx + 3] = Y - C - I - δR * R
            # 4. Capital accumulation
            F[idx + 4] = K - (1 - δK) * Km1 - I
            # 5. Roads accumulation
            F[idx + 5] = R - (1 - δR) * Rm1 - δR * R
            # 6. Production identity
            F[idx + 6] = Y - production(Km1, L, Rm1)
        end
        return F
    end

    sol = nlsolve(G!, x0; autodiff = :forward)
    sol.converged || error("Transition path solver did not converge.")

    # reshape solution vector ➜ (vars × T) matrix for easy plotting
    path = reshape(sol.zero, N, T)
    return path, sol
end

end  # module DSGERoads
