import zipfile
import pandas as pd
from scipy.stats.mstats import winsorize

# ——— CONFIGURATION ———
zip_path = r'WDI_excel_2024_09_25.zip'
out_path = r'R9_cleaned.data.csv'
# ——————————————————————————

# Mapping from WDI codes to our variable names
indicators = {
    'IT.NET.USER.ZS':    'TINN',
    'EG.FEC.RNEW.ZS':    'EC',
    'TX.VAL.ICTG.ZS.UN': 'FinD',
    'NY.GDP.MKTP.KD.ZG': 'GDP1',
    'EN.ATM.CO2E.PC':    'CO2',
    'NE.TRD.GNFS.ZS':    'OPP'
}
countries = ['BRA', 'RUS', 'IND', 'CHN', 'ZAF']
years = [str(y) for y in range(2004, 2024)]
cols = list(indicators.values())

# 1. Load raw WDI panel
with zipfile.ZipFile(zip_path) as z:
    candidates = [n for n in z.namelist() if n.lower().endswith('.csv')] \
               or [n for n in z.namelist() if n.lower().endswith(('.xls','xlsx'))]
    if not candidates:
        raise FileNotFoundError("No CSV/Excel in ZIP")
    data_file = candidates[0]

    # preview to detect header
    with z.open(data_file) as f:
        if data_file.lower().endswith('.csv'):
            preview = pd.read_csv(f, header=None, nrows=20,
                                  dtype=str, encoding='latin1', engine='python')
        else:
            preview = pd.read_excel(f, header=None, nrows=20, dtype=str)
    header_row = next(
        (i for i, row in preview.iterrows()
         if {'Country Code','Indicator Code'}.issubset(set(row.dropna()))),
        None
    )
    if header_row is None:
        raise ValueError("Header row not found")

    # read full sheet
    with z.open(data_file) as f:
        if data_file.lower().endswith('.csv'):
            raw = pd.read_csv(f, header=header_row, dtype=str,
                              encoding='latin1', engine='python')
        else:
            raw = pd.read_excel(f, header=header_row, dtype=str)

raw.columns = raw.columns.str.strip()

# 2. Filter to BRICS × indicators × years
sub = raw.loc[
    raw['Country Code'].isin(countries) &
    raw['Indicator Code'].isin(indicators),
    ['Country Name','Country Code','Indicator Code'] + years
].copy()

# 3. Melt to long form
long = (
    sub
    .melt(
        id_vars=['Country Name','Country Code','Indicator Code'],
        value_vars=years,
        var_name='Year',
        value_name='Value'
    )
    .dropna(subset=['Value'])
)
long['Year'] = long['Year'].astype(int)
long['Value'] = pd.to_numeric(long['Value'], errors='coerce')

# 4. Pivot back to wide
df = (
    long
    .pivot_table(
        index=['Country Name','Country Code','Year'],
        columns='Indicator Code',
        values='Value',
        aggfunc='first'
    )
    .reset_index()
    .rename(columns=indicators)
)
df = df.set_index(['Country Name','Country Code','Year'])

# 5. Impute missing with interpolation + country mean
def impute_panel(s: pd.Series) -> pd.Series:
    s = s.sort_index().interpolate(method='linear', limit_direction='both')
    return s.fillna(s.mean())

for col in cols:
    df[col] = df.groupby(level='Country Code')[col].transform(impute_panel)

# 6. Winsorize extremes at 1%/99%
for col in cols:
    # winsorize returns a masked array; convert back to numpy float
    df[col] = winsorize(df[col], limits=[0.01, 0.01]).data

# 7. Final check
assert not df[cols].isnull().any().any(), "Missing values remain!"

# 8. Export cleaned panel
df = df.reset_index()
df.to_csv(out_path, index=False, encoding='utf-8-sig')
print(f"Cleaned panel saved to: {out_path}")
