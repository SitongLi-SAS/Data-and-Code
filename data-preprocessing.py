import zipfile
import pandas as pd

# Path to the downloaded WDI Excel archive
zip_path = 'WDI_excel_2018_07_27.zip'

# 1. Read the "Data" sheet from the embedded Excel file
with zipfile.ZipFile(zip_path, 'r') as z:
    with z.open('WDIEXCEL.xlsx') as f:
        wdi_df = pd.read_excel(f, sheet_name='Data')

# 2. Filter for GDP per capita (current US$) for United Kingdom
gdp_df = wdi_df[
    (wdi_df['Indicator Code'] == 'NY.GDP.PCAP.CD') &
    (wdi_df['Country Name'] == 'United Kingdom')
]

# 3. Extract columns for years 1972–2007
years = [str(y) for y in range(1972, 2008)]
uk_series = gdp_df[years].T.reset_index()
uk_series.columns = ['Year', 'Real']

# 4. Convert types and compute t
uk_series['Year'] = uk_series['Year'].astype(int)
uk_series['Real'] = uk_series['Real'].astype(float)
uk_series['t'] = uk_series['Year'] - 1972

# 5. Save to new R15_data.csv
output_path = 'R15_data.csv'
uk_series.to_csv(output_path, index=False)

print(f"Successfully generated new data file: {output_path}")
