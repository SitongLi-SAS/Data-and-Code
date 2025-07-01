# -*- coding: utf-8 -*-
import pandas as pd
import numpy as np
from scipy.stats import shapiro, rankdata, norm, chi2
import statsmodels.api as sm
from linearmodels.panel import PooledOLS, PanelOLS, RandomEffects, compare

# -----------------------------
# 诊断函数
# -----------------------------
def pesaran_cd(residuals):
    df = residuals.unstack(level=0) if isinstance(residuals, pd.Series) else residuals.copy()
    T, N = df.shape
    if N < 2:
        return {'stat': np.nan, 'p_value': np.nan}
    df_cen = df - df.mean(axis=0)
    s = df_cen.std(axis=0, ddof=0)
    corr_mat = (df_cen.T @ df_cen) / (T * np.outer(s, s))
    iu = np.triu_indices(N, k=1)
    sum_rho = corr_mat.values[iu].sum()
    cd_stat = np.sqrt(2 * T / (N * (N - 1))) * sum_rho
    pval = 2 * (1 - norm.cdf(abs(cd_stat)))
    return {'stat': float(cd_stat), 'p_value': float(pval)}

def friedman_cd(residuals):
    df = residuals.unstack(level=0) if isinstance(residuals, pd.Series) else residuals.copy()
    T, N = df.shape
    if N < 2:
        return {'stat': np.nan, 'p_value': np.nan}
    rank_matrix = np.vstack([rankdata(df.iloc[i, :]) for i in range(T)])
    Rbar = (N + 1) / 2
    S = (12 / (T * N * (N + 1))) * np.sum((rank_matrix - Rbar) ** 2)
    pval = 1 - chi2.cdf(S, N - 1)
    return {'stat': float(S), 'p_value': float(pval)}

def slope_homogeneity(residuals, exog):
    return {'stat': np.nan, 'p_value': np.nan}

# -----------------------------
# 1. 读取面板数据
# -----------------------------
csv_path = 'R9_data.csv'
df = pd.read_csv(csv_path)
df = df.set_index(['Country', 'Year'])

# -----------------------------
# 2. 描述性统计 (Table 2)
# -----------------------------
desc = df[['TINN', 'EC', 'FinD', 'GDP1', 'OPP', 'CO2']].describe().T
desc = desc.rename(columns={
    'count': 'Obs', 'mean': 'Mean', 'std': 'Std.Dev',
    'min': 'Min', 'max': 'Max'
})
print("\n===== 描述性统计 (Table 2) =====\n")
print(desc[['Obs', 'Mean', 'Std.Dev', 'Min', 'Max']].round(3))

# -----------------------------
# 3. 相关系数矩阵 (Table 3)
# -----------------------------
corr = df[['TINN', 'EC', 'FinD', 'GDP1', 'OPP', 'CO2']].corr()
print("\n===== 相关系数矩阵 (Table 3) =====\n")
print(corr.round(3))

# -----------------------------
# 4. 面板诊断检验
# -----------------------------
pooled = PooledOLS.from_formula('CO2 ~ 1 + TINN + EC + FinD + GDP1 + OPP', data=df)\
                .fit(cov_type='robust')
resid = pooled.resids
cd = pesaran_cd(resid)
fr = friedman_cd(resid)
slope = slope_homogeneity(resid, df[['TINN', 'EC', 'FinD', 'GDP1', 'OPP']])
print("\n===== Panel Diagnostics =====\n")
print(f"Pesaran’s CD statistic:  {cd['stat']:.3f}, p-value = {cd['p_value']:.4f}")
print(f"Friedman’s statistic:   {fr['stat']:.3f}, p-value = {fr['p_value']:.4f}")
print(f"Slope Homogeneity Δ:    {slope['stat']}, p-value = {slope['p_value']}")

# -----------------------------
# 5. Hausman Test (FE vs RE)
# -----------------------------
re_mod = RandomEffects.from_formula('CO2 ~ 1 + TINN + EC + FinD + GDP1 + OPP', data=df).fit()
fe_mod = PanelOLS.from_formula('CO2 ~ 1 + TINN + EC + FinD + GDP1 + OPP + EntityEffects', data=df)\
               .fit(cov_type='clustered', cluster_entity=True)
hausman = compare({'RE': re_mod, 'FE': fe_mod})
print("\n===== Hausman Test (FE vs RE) =====\n")
print(hausman)

# -----------------------------
# 6. 三个固定效应模型 (Table 8)
# -----------------------------
mod1 = PanelOLS.from_formula('CO2 ~ 1 + TINN + EC + FinD + GDP1 + EntityEffects', data=df)\
              .fit(cov_type='clustered', cluster_entity=True)
print("\n===== Model 1 (Fixed Effects) =====\n")
print(mod1.summary)

mod2 = PanelOLS.from_formula('CO2 ~ 1 + TINN + EC + FinD + GDP1 + OPP + EntityEffects', data=df)\
              .fit(cov_type='clustered', cluster_entity=True)
print("\n===== Model 2 (Fixed Effects) =====\n")
print(mod2.summary)

if 'TINN_OPP_orth' not in df.columns:
    inter_raw = df['TINN'] * df['OPP']
    X2 = sm.add_constant(df[['TINN', 'EC', 'FinD', 'GDP1', 'OPP']])
    df['TINN_OPP_orth'] = sm.OLS(inter_raw, X2).fit().resid

mod3 = PanelOLS.from_formula('CO2 ~ 1 + TINN + EC + FinD + GDP1 + OPP + Q("TINN_OPP_orth") + EntityEffects', data=df)\
              .fit(cov_type='clustered', cluster_entity=True)
print("\n===== Model 3 (Fixed Effects with Interaction) =====\n")
print(mod3.summary)

# -----------------------------
# 7. Shapiro–Wilk 正态性检验
# -----------------------------
resid3 = mod3.resids.reset_index(drop=True)
W, pval = shapiro(resid3)
print("\n===== Shapiro–Wilk 正态性检验 =====\n")
print(f"W statistic = {W:.4f}, p-value = {pval:.4f}")
