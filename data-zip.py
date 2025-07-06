import zipfile
import pandas as pd

# Configuration
zip_path = r'WDI_excel_2024_09_25.zip'
out_path = r'R9_data.csv'

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

# 1. Load raw WDI panel from ZIP
with zipfile.ZipFile(zip_path) as z:
    candidates = [n for n in z.namelist() if n.lower().endswith('.csv')] \
               or [n for n in z.namelist() if n.lower().endswith(('.xls','xlsx'))]
    data_file = candidates[0]
    with z.open(data_file) as f:
        # Read with header row auto-detected by pandas
        raw = pd.read_csv(f, header=0, encoding='latin1')

# 2. Filter to BRICS countries, indicators and years
sub = raw.loc[
    raw['Country Code'].isin(countries) &
    raw['Indicator Code'].isin(indicators),
    ['Country Name', 'Country Code', 'Indicator Code'] + years
].copy()

# 3. Melt to long form and drop rows without data
long = (
    sub.melt(
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
df_raw = (
    long.pivot_table(
        index=['Country Name','Country Code','Year'],
        columns='Indicator Code',
        values='Value',
        aggfunc='first'
    )
    .reset_index()
    .rename(columns=indicators)
)

# 5. Export raw panel without any imputation or winsorization
df_raw.to_csv(out_path, index=False, encoding='utf-8-sig')
print(f"Raw panel saved to: {out_path}")
