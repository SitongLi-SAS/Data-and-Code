import pandas as pd
import matplotlib.pyplot as plt

# 1. 读取数据并校验差值
df = pd.read_csv('R7_data.csv')

# 重新计算绝对差和相对差，确保一致性
df['Diff_abs'] = df['WDI_GDP_2005'] - df['PWT_GDP_2005']
df['Diff_pct'] = (df['Diff_abs'] / df['PWT_GDP_2005']) * 100

# 2. 筛选显著差异国家：abs(Diff_abs) >= 2500 OR abs(Diff_pct) >= 25
threshold_mask = (df['Diff_abs'].abs() >= 2500) | (df['Diff_pct'].abs() >= 25)
df_diff = df[threshold_mask].copy()

# 按照 WDI < PWT 与 WDI > PWT 分组
df_diff['Group'] = df_diff.apply(
    lambda row: 'WDI < PWT' if row['WDI_GDP_2005'] < row['PWT_GDP_2005'] else 'WDI > PWT',
    axis=1
)

# 统计各组数量
count_wdi_lt_pwt = (df_diff['Group'] == 'WDI < PWT').sum()
count_wdi_gt_pwt = (df_diff['Group'] == 'WDI > PWT').sum()

print(f"Number of countries where WDI < PWT: {count_wdi_lt_pwt}")
print(f"Number of countries where WDI > PWT: {count_wdi_gt_pwt}")

# 查看筛选结果
print("\nFiltered countries with significant differences:")
print(df_diff[['Country', 'PWT_GDP_2005', 'WDI_GDP_2005', 'Diff_abs', 'Diff_pct', 'Group']])
df_diff.to_csv('R7_difference_output.csv', index=False)

# 3. 绘制散点图：PWT vs WDI，并添加 ±25% 参考线
plt.figure(figsize=(8, 6))
plt.scatter(df['PWT_GDP_2005'], df['WDI_GDP_2005'])
plt.xlabel('PWT GDP per Capita (2005)')
plt.ylabel('WDI GDP per Capita (2005)')
plt.title('Scatter: PWT vs WDI GDP per Capita (2005)')

# 绘制 ±25% 参考线
x_vals = df['PWT_GDP_2005']
plt.plot(x_vals, x_vals * 1.25, linestyle='--')
plt.plot(x_vals, x_vals * 0.75, linestyle='--')
plt.savefig('R7_scatter_plot.png', dpi=300, bbox_inches='tight')
plt.show()

