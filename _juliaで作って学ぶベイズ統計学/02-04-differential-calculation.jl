# Section 2.4  微分計算
# Subsection ---
# 2.4.1 1変数関数の微分 
# 2.4.2 多変数関数の微分
# 2.4.3 自動微分

using Plots

# ---- 2.4.1 1変数関数の微分 ----
f(x) = -(x + 1) * (x - 1)

# 小さい変化量を与えることで、近似的に微分係数を求める方法を数値微分とする。
# この方法は関数の値さえ分かれば、導関数を近似的に求めることができるが、
# 計算上の誤差もあるため実用上使える場面は限られる。
h = 1.0e-10
ft(a) = (f(a + h) - f(a)) / h

xs = range(-1, 1, length = 100)


f.(xs)
ft.(xs)

plot(f.(xs))
plot(ft.(xs))

# ---- 2.4.2 多変数関数の微分 ----
L = 10
xs1 = range(-1, 1, length = L)
xs2 = range(-1, 1, length = L)

f2(x) = -(x .+ 1)' * (x .- 1)
∇f2(x) = -2x

z_book = [f2([x1 x2]) for x1 in xs1, x2 in xs2]

z = zeros(L, L) # L×Lの行列を初期化する
for i in 1:L, j in 1:L
    z[i,j] = f2([xs1[i], xs2[j]])
end

contour(xs1, xs2, z')

vec1 = zeros(L, L) # L×Lの行列を初期化する

vec1 = [∇f2([x1, x2])[1] for x1 in xs1, x2 in xs2]
vec2 = [∇f2([x1, x2])[2] for x1 in xs1, x2 in xs2]

quiver(repeat(xs1, 1, L), repeat(xs2', 1, L), quiver = (vec1, vec2))
# quiver(x,y,quiver=(u,v))
# make a quiver (vector field) plot. 
# The ith vector extends from (x[i],y[i]) to (x[i] + u[i], y[i] + v[i])

# 2.4.3 自動微分 
using ForwardDiff

# using 
# using Chain: @chain