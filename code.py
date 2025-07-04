# Environment and package versions:
# Python       3.11.x
# pandas       1.5.x
# numpy        1.23.x
# matplotlib   3.6.x
# scipy        1.9.x

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from scipy.special import gamma

# 1. Load the regenerated R15_data.csv (from /mnt/data/)
df = pd.read_csv('R15_data.csv')  # columns: Year, Real, t

# 2. Ensure correct types and filter 1972–2007
df['Year'] = df['Year'].astype(int)
df['Real'] = df['Real'].astype(float)
df['t']    = df['t'].astype(int)
df = df[(df['Year'] >= 1972) & (df['Year'] <= 2007)].reset_index(drop=True)

# 3. Mittag–Leffler function (100-term truncation)
def mittag_leffler(alpha, z, terms=100):
    s = 0.0
    for k in range(terms):
        s += (z**k) / gamma(alpha*k + 1)
    return s

# 4. Model parameters (Table 1)
alpha_ABC = 0.3584; k_ABC = 1.4569; M_alpha = 2.3773
alpha_CFC = 0.4095; k_CFC = 2.3602; B_alpha = 14.1943
alpha_Cap = 0.5008; k_Cap = 0.2404
k_int     = 0.0807

# initial condition S0
S0 = df.loc[df['t'] == 0, 'Real'].iat[0]

# 5. Analytic solutions
def S_ABC(t):
    coef = k_ABC*alpha_ABC/(M_alpha - k_ABC*(1-alpha_ABC))
    return (S0/(M_alpha - k_ABC*(1-alpha_ABC))) * mittag_leffler(alpha_ABC, coef*t**alpha_ABC)

def S_CFC(t):
    denom = B_alpha - k_CFC*(1-alpha_CFC)
    return (B_alpha*S0/denom) * np.exp(alpha_CFC*k_CFC*t/denom)

def S_Caputo(t):
    return S0 * mittag_leffler(alpha_Cap, k_Cap*t**alpha_Cap)

def S_int(t):
    return S0 * np.exp(k_int*t)

# 6. Compute predictions
df['ABC']     = df['t'].apply(S_ABC)
df['CFC']     = df['t'].apply(S_CFC)
df['Caputo']  = df['t'].apply(S_Caputo)
df['Integer'] = df['t'].apply(S_int)

# 7. Table 2: Key years comparison
years = [1979,1981,1982,1987,1989,1992,1995,1996,1997,1998,2004,2005,2006,2007]
comp = df[df['Year'].isin(years)][['Year','ABC','Caputo','CFC','Integer','Real']]
print("Table 2: Model Predictions vs Real Data")
print(comp.to_string(index=False, float_format='%.2f'))

# 8. Table 3: SSE and Efficiency
def sse_eff(real, pred):
    res = real - pred
    sse = np.sum(res**2)/len(res)
    eff = (np.sum(real**2)-np.sum(res**2))/np.sum(real**2)*100
    return sse, eff

metrics = {m: sse_eff(df['Real'], df[m]) for m in ['ABC','Caputo','CFC','Integer']}
m_df = pd.DataFrame([{'Model':m,'SSE':metrics[m][0],'Efficiency (%)':metrics[m][1]} for m in metrics])
print("\nTable 3: Statistical Evaluation of Models")
print(m_df.to_string(index=False, float_format='%.4e'))

# 9. Figure 1: Four subplots
fig, ax = plt.subplots(2,2,figsize=(12,8), sharex=True, sharey=True)
# ABC
ax[0,0].plot(df['Year'], df['Real'], label='Real')
ax[0,0].plot(df['Year'], df['ABC'], label='ABC')
ax[0,0].set_title('ABC vs Real'); ax[0,0].legend(); ax[0,0].set_ylabel('GDP per Capita')
# Caputo
ax[0,1].plot(df['Year'], df['Real'], label='Real')
ax[0,1].plot(df['Year'], df['Caputo'], label='Caputo')
ax[0,1].set_title('Caputo vs Real'); ax[0,1].legend()
# CFC
ax[1,0].plot(df['Year'], df['Real'], label='Real')
ax[1,0].plot(df['Year'], df['CFC'], label='CFC')
ax[1,0].set_title('CFC vs Real'); ax[1,0].legend()
ax[1,0].set_xlabel('Year'); ax[1,0].set_ylabel('GDP per Capita')
# Integer
ax[1,1].plot(df['Year'], df['Real'], label='Real')
ax[1,1].plot(df['Year'], df['Integer'], label='Integer')
ax[1,1].set_title('Integer-order vs Real'); ax[1,1].legend()
ax[1,1].set_xlabel('Year')
plt.tight_layout()
plt.show()
# Save Figure 1
fig.savefig('Figure1_R15_comparison.png', dpi=300)
plt.show()

# 10. Figure 2: All models vs Real
plt.figure(figsize=(10,6))
for col in ['Real','ABC','Caputo','CFC','Integer']:
    plt.plot(df['Year'], df[col], label=col)
plt.xlabel('Year'); plt.ylabel('GDP per Capita')
plt.title('Figure 2: All Models vs Real Data')
plt.legend(); plt.tight_layout(); plt.show()
# Save Figure 2
fig.savefig('Figure2_R15_all_models.png', dpi=300)
plt.show()