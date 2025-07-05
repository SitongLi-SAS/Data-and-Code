# Environment and Package Requirements:
# - Python 3.8+
# - pandas >= 1.0
# - No additional packages required.

import pandas as pd

# Display configuration for full DataFrame output
pd.set_option('display.max_rows', None)
pd.set_option('display.max_columns', None)
pd.set_option('display.width', 1000)
pd.options.display.float_format = '{:,.2f}'.format

# ------------------------------------------------------------------------------
# STEP 1: Load the three datasets
# ------------------------------------------------------------------------------
wdi11 = pd.read_csv('comparison_wdi2011_vs_pwt2005.csv')
wdi12 = pd.read_csv('comparison_wdi2012_vs_pwt2005.csv')
icp   = (
    pd.read_csv('R7_data2.csv')
      .rename(columns={'iso3': 'iso', 'gdp_pc_ppp_icp': 'icp_ppp'})
)

# ------------------------------------------------------------------------------
# STEP 2: Define a helper function to generate tables A and B
# ------------------------------------------------------------------------------
def make_table(df, base_col, comp_col, prefix):
    """
    Given a DataFrame and two column names (base and compare), compute:
      - abs_diff: absolute difference (base - compare)
      - rel_pct: percentage difference relative to compare
    Then split entries into:
      A: |rel_pct| >= 25%
      B: remaining entries with |abs_diff| >= 2500
    Finally, split each of A and B into:
      - negative diff (base < compare)
      - positive diff (base > compare)
    Returns: ((A_neg, A_pos), (B_neg, B_pos))
    """
    # Compute differences (use precomputed if available)
    if f'{prefix}_abs_diff' in df.columns:
        abs_diff = df[f'{prefix}_abs_diff']
        rel_pct  = df[f'{prefix}_rel_diff_pct']
    else:
        abs_diff = df[base_col] - df[comp_col]
        rel_pct  = abs_diff / df[comp_col] * 100

    # Prepare a temporary DataFrame
    tmp = df[['iso', comp_col, base_col]].copy()
    tmp['abs_diff'] = abs_diff
    tmp['rel_pct']  = rel_pct.round(1)

    # Filter A and B groups
    group_A = tmp[tmp['rel_pct'].abs() >= 25]
    group_B = tmp[~tmp.index.isin(group_A.index) & (tmp['abs_diff'].abs() >= 2500)]

    # Split each group into negative and positive differences
    def split(df_part):
        negative = df_part[df_part['abs_diff'] < 0].sort_values('abs_diff')
        positive = df_part[df_part['abs_diff'] > 0].sort_values('abs_diff', ascending=False)
        return negative, positive

    return split(group_A), split(group_B)

# ------------------------------------------------------------------------------
# STEP 3: Generate Tables 1 through 4
# ------------------------------------------------------------------------------
# Table 1: WDI 2012 vs PWT 7.1
tbl1_A, tbl1_B = make_table(
    wdi12, base_col='wdi_ppp_2005', comp_col='pwt71_rgdpch', prefix='wdi2012_pwt71'
)

# Table 2: WDI 2011 vs PWT 7.0
tbl2_A, tbl2_B = make_table(
    wdi11, base_col='wdi_ppp_2005', comp_col='pwt70_rgdpch', prefix='wdi2011_pwt70'
)

# Table 3: PWT 7.1 vs ICP 2005
merged3 = pd.merge(wdi12[['iso','pwt71_rgdpch']], icp, on='iso', how='inner')
tbl3_A, tbl3_B = make_table(
    merged3, base_col='icp_ppp', comp_col='pwt71_rgdpch', prefix=''
)

# Table 4: WDI 2012 vs ICP 2005
merged4 = pd.merge(wdi12[['iso','wdi_ppp_2005']], icp, on='iso', how='inner')
tbl4_A, tbl4_B = make_table(
    merged4, base_col='icp_ppp', comp_col='wdi_ppp_2005', prefix=''
)

# ------------------------------------------------------------------------------
# STEP 4: Print each table in Markdown format
# ------------------------------------------------------------------------------
def print_markdown_section(table_tuple, table_number, label):
    neg, pos = table_tuple
    print(f"\n=== Table {table_number}{label} ===\n")
    print("---- Base < Compare ----")
    print(neg.to_markdown(index=False))
    print("\n---- Base > Compare ----")
    print(pos.to_markdown(index=False))

# Print Table 1A and 1B
print_markdown_section(tbl1_A, 1, "A (|rel_pct| >= 25%)")
print_markdown_section(tbl1_B, 1, "B (|abs_diff| >= 2500)")

# Print Tables 2, 3, and 4
print_markdown_section(tbl2_A, 2, "A (|rel_pct| >= 25%)")
print_markdown_section(tbl2_B, 2, "B (|abs_diff| >= 2500)")
print_markdown_section(tbl3_A, 3, "A (|rel_pct| >= 25%)")
print_markdown_section(tbl3_B, 3, "B (|abs_diff| >= 2500)")
print_markdown_section(tbl4_A, 4, "A (|rel_pct| >= 25%)")
print_markdown_section(tbl4_B, 4, "B (|abs_diff| >= 2500)")

# ------------------------------------------------------------------------------
# STEP 5: Combine all subtables and save to a single CSV
# ------------------------------------------------------------------------------
combined_records = []
tables_info = [
    (1, tbl1_A, tbl1_B, 'pwt71_rgdpch', 'wdi_ppp_2005'),
    (2, tbl2_A, tbl2_B, 'pwt70_rgdpch', 'wdi_ppp_2005'),
    (3, tbl3_A, tbl3_B, 'pwt71_rgdpch', 'icp_ppp'),
    (4, tbl4_A, tbl4_B, 'wdi_ppp_2005', 'icp_ppp'),
]

for table_num, tblA, tblB, comp_col, base_col in tables_info:
    for df_group, part_label, sign_label in [
        (tblA[0], 'A', 'base<compare'),
        (tblA[1], 'A', 'base>compare'),
        (tblB[0], 'B', 'base<compare'),
        (tblB[1], 'B', 'base>compare'),
    ]:
        tmp = df_group.copy()
        tmp = tmp.rename(columns={comp_col: 'compare', base_col: 'base'})
        tmp['table'] = table_num
        tmp['part']  = part_label
        tmp['sign']  = sign_label
        combined_records.append(tmp)

# Concatenate and save
combined_df = pd.concat(combined_records, ignore_index=True)
combined_df.to_csv('results.csv', index=False)

print("\nCombined results saved to results.csv")
