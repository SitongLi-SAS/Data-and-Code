import pandas as pd
import numpy as np
from scipy.special import gamma
from scipy.special import gamma
from scipy.optimize import least_squares
import matplotlib.pyplot as plt

def mittag_leffler(alpha, z, terms=50):
    """
    计算 E_alpha(z) ≈ ∑_{k=0}^{terms-1} z^k / Gamma(alpha*k + 1)
    terms 越大精度越高，但会更慢。
    """
    return sum((z**k) / gamma(alpha*k + 1) for k in range(terms))



# 1. 加载数据
df = pd.read_csv('R15_cleaned_data.csv')
t = df['t'].values
S_real = df['gdp_per_capita'].values
S0 = S_real[0]

# 2. 模型函数定义
def S_abc_linear(params, ti):
    alpha, k, M = params
    return k*(1-alpha)/M*(1 + alpha/(1-alpha)*ti**alpha/gamma(alpha+1)) + S0

def S_abc_exp(params, ti):
    alpha, k, M = params
    denom = M - k*(1-alpha)
    lam = k*alpha/denom
    return S0/denom * mittag_leffler(alpha, lam*ti**alpha)

def S_cfc_linear(params, ti):
    alpha, k, B = params
    return S0 + k*ti/B

def S_cfc_exp(params, ti):
    alpha, k, B = params
    return S0 * np.exp(k*ti/B)

def S_caputo_linear(params, ti):
    alpha, k = params
    return S0 + k*ti**alpha/gamma(alpha+1)

def S_caputo_exp(params, ti):
    alpha, k = params
    return S0 * mittag_leffler(alpha, k*ti**alpha)

# 3. 拟合函数
def fit_model(model_func, initial_params, bounds):
    def residuals(params):
        return np.array([model_func(params, ti) - Si for ti, Si in zip(t, S_real)])
    return least_squares(residuals, initial_params, bounds=bounds)

# 4. 初始值和约束
models = [
    ("ABC Linear",  S_abc_linear,   [0.36, 1.46, 2.38], ([0,0,0], [1, np.inf, np.inf])),
    ("ABC Exponential", S_abc_exp,  [0.36, 1.46, 2.38], ([0,0,0], [1, np.inf, np.inf])),
    ("CFC Linear",  S_cfc_linear,   [0.41, 2.36, 14.19], ([0,0,0], [1, np.inf, np.inf])),
    ("CFC Exponential", S_cfc_exp,  [0.41, 2.36, 14.19], ([0,0,0], [1, np.inf, np.inf])),
    ("Caputo Linear",  S_caputo_linear, [0.50, 0.24], ([0,0], [1, np.inf])),
    ("Caputo Exponential", S_caputo_exp, [0.50, 0.24], ([0,0], [1, np.inf]))
]

# 5. 执行拟合并输出
results = {}
for name, func, p0, bounds in models:
    res = fit_model(func, p0, bounds)
    sse = np.sum(res.fun**2)
    results[name] = (res.x, sse)
    print(f"{name} -> params: {res.x}, SSE: {sse:.2f}")

# 6. 绘制对比
plt.figure(figsize=(8,5))
plt.plot(df['year'], S_real, 'ko', label='Observed')
for name, func, _, _ in models:
    params, _ = results[name]
    fitted = [func(params, ti) for ti in t]
    plt.plot(df['year'], fitted, label=name)
plt.xlabel('Year')
plt.ylabel('GDP per Capita')
plt.title('R15: Observed vs Fitted Models')
plt.legend()
plt.show()
