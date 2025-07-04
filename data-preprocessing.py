import os
import zipfile
from pathlib import Path
import pandas as pd
from io import BytesIO

def load_pwt(zip_path: str, ver: str, year: int = 2005) -> pd.DataFrame:
    with zipfile.ZipFile(zip_path, 'r') as zf:
        csvs = [n for n in zf.namelist() if n.lower().endswith('.csv')]
        if not csvs:
            raise FileNotFoundError(f"PWT ZIP '{zip_path}' missing CSV")
        df = pd.read_csv(zf.open(csvs[0]))
    df2 = df.loc[df['year'] == year, ['isocode', 'rgdpch']].copy()
    df2.columns = ['iso', f'pwt{ver}_rgdpch']
    return df2

def load_wdi(zip_path: str, year: str = '2005') -> pd.DataFrame:
    with zipfile.ZipFile(zip_path, 'r') as zf:
        excels = [n for n in zf.namelist() if n.lower().endswith(('.xls', '.xlsx'))]
        if not excels:
            raise FileNotFoundError(f"WDI ZIP '{zip_path}' missing Excel")
        raw = zf.read(excels[0])
        xls = pd.ExcelFile(BytesIO(raw))
        data_sheets = [s for s in xls.sheet_names if 'data' in s.lower()]
        if not data_sheets:
            raise KeyError(f"No sheet with 'data' found; sheets: {xls.sheet_names}")
        sheet = data_sheets[0]
        temp = pd.read_excel(xls, sheet_name=sheet, header=None)
        header_row = next((i for i, r in temp.iterrows() if 'Country Code' in r.values), None)
        if header_row is None:
            raise KeyError("Header row with 'Country Code' not found")
        df = pd.read_excel(xls, sheet_name=sheet, header=header_row)
    df.columns = [str(c).strip() for c in df.columns]
    ind_candidates = [c for c in df.columns if str(c).lower().strip() in ('series code','indicator code')]
    if not ind_candidates:
        raise KeyError(f"Indicator column not found; columns: {df.columns.tolist()}")
    ind_col = ind_candidates[0]
    if year not in df.columns.astype(str):
        raise KeyError(f"Year column '{year}' not found; columns: {df.columns.tolist()}")
    out = df.loc[df[ind_col] == 'NY.GDP.PCAP.PP.KD', ['Country Code', year]].copy()
    out.columns = ['iso', f'wdi_ppp_{year}']
    return out

def main():
    archives = {
        'pwt70': 'pwt70_06032011version.zip',
        'pwt71': 'pwt71_11302012version.zip',
        'wdi2011': 'WDIandGDF_excel_2011_12.zip',
        'wdi2012': 'WDI_excel_2012_12.zip'
    }

    # Load PWT 7.0 & 7.1
    df70 = load_pwt(archives['pwt70'], '70')
    df71 = load_pwt(archives['pwt71'], '71')

    # Use PWT7.1 as base, left join PWT7.0 & WDI
    df = df71.copy()
    df = df.merge(df70, on='iso', how='left')
    df['pwt_abs_diff'] = df['pwt71_rgdpch'] - df['pwt70_rgdpch']
    df['pwt_rel_diff_pct'] = df['pwt_abs_diff'] / df['pwt70_rgdpch'] * 100

    # Load WDI and merge
    missing = {}
    for key in ('wdi2011','wdi2012'):
        wdi = load_wdi(archives[key], year='2005')
        df = df.merge(wdi, on='iso', how='left')
        # record missing
        miss = df.loc[df[f'wdi_ppp_2005'].isna(), 'iso'].tolist()
        missing[key] = miss

    # Combine list of countries with any missing
    any_missing = set(missing['wdi2011'] + missing['wdi2012'] +
                      df.loc[df['pwt70_rgdpch'].isna(), 'iso'].tolist())
    print("Countries with missing data:", sorted(any_missing))

    # Drop rows with any missing in key columns
    cols_to_check = ['pwt70_rgdpch','pwt71_rgdpch','wdi_ppp_2005']
    df_clean = df.dropna(subset=cols_to_check)

    # Compute diffs for WDI
    for key in ('wdi2011','wdi2012'):
        df_clean[f'{key}_pwt70_abs_diff'] = df_clean[f'wdi_ppp_2005'] - df_clean['pwt70_rgdpch']
        df_clean[f'{key}_pwt70_rel_diff_pct'] = df_clean[f'{key}_pwt70_abs_diff'] / df_clean['pwt70_rgdpch'] * 100
        df_clean[f'{key}_pwt71_abs_diff'] = df_clean[f'wdi_ppp_2005'] - df_clean['pwt71_rgdpch']
        df_clean[f'{key}_pwt71_rel_diff_pct'] = df_clean[f'{key}_pwt71_abs_diff'] / df_clean['pwt71_rgdpch'] * 100

    # Save results
    df_clean.to_csv('final_comparison.csv', index=False)
    print("Saved final_comparison.csv with", len(df_clean), "rows")

if __name__ == '__main__':
    main()
