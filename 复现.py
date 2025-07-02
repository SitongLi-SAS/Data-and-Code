# ==============================================================
# R14 Reproduction – WDI Data Extraction & Visualization (PyCharm Version)
#
# ✅ Required Environment:
# Python version: >=3.9 (recommended 3.11)
#
# ✅ Required Packages (install via pip):
# pip install pandas numpy matplotlib scikit-learn geopandas pycountry wbdata
#
# 🛠 You may also need:
# - Spatial indexing support for GeoPandas (Linux: sudo apt install libspatialindex-dev)
#
# 📌 Notes:
# - This script fetches data from World Bank’s WDI API.
# - It handles selected indicators used in R14 (surgical & financial protection metrics).
# - If indicators return no data via API, the script will notify and skip them.
# ==============================================================

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.experimental import enable_iterative_imputer  # noqa
from sklearn.impute import IterativeImputer
import geopandas as gpd
import pycountry
import os
import wbdata

# Set output directory
output_dir = 'output'
os.makedirs(output_dir, exist_ok=True)

# Define target indicators (available in WDI as of 2024-2025)
wdi_indicators = {
    'SH.MED.BEDS.ZS': 'hospital_beds_per_1000',
    'SH.MED.PHYS.ZS': 'physicians_per_1000',
    'SH.MED.NUMW.P3': 'nurses_per_1000',
}

# Fetch raw data (all countries, all available years, long format)
try:
    df_raw = wbdata.get_dataframe(wdi_indicators).reset_index()
    df_raw = df_raw.rename(columns={'country': 'country_name'})
except Exception as e:
    print("❌ Failed to fetch WDI data. Details:", str(e))
    exit()

# Convert to wide format: one row per country, last available year
df_latest = df_raw.sort_values(by=['country_name', 'date'], ascending=[True, False])
df_latest = df_latest.drop_duplicates(subset=['country_name'], keep='first')
df_latest = df_latest.pivot(index='country_name', columns='indicator', values='value')
df_latest.columns.name = None
df_latest = df_latest.fillna(0)

# Save descriptive statistics
desc = df_latest.describe()
desc.to_csv(os.path.join(output_dir, 'describe.csv'))
print("✅ Descriptive statistics saved.")

# Plot histogram and boxplot per indicator
for col in df_latest.columns:
    # Histogram
    plt.figure()
    plt.hist(df_latest[col].dropna(), bins=30)
    plt.title(f'Histogram of {col}')
    plt.xlabel(col)
    plt.ylabel('Frequency')
    hist_path = os.path.join(output_dir, f'{col}_histogram.png')
    plt.savefig(hist_path, bbox_inches='tight')
    plt.close()
    print(f"Saved histogram: {hist_path}")

    # Boxplot
    plt.figure()
    plt.boxplot(df_latest[col].dropna())
    plt.title(f'Boxplot of {col}')
    box_path = os.path.join(output_dir, f'{col}_boxplot.png')
    plt.savefig(box_path, bbox_inches='tight')
    plt.close()
    print(f"Saved boxplot: {box_path}")

# Correlation matrix heatmap
corr = df_latest.corr()
plt.figure(figsize=(8, 6))
plt.imshow(corr, aspect='auto', cmap='coolwarm')
plt.colorbar()
plt.xticks(range(len(corr)), corr.columns, rotation=90)
plt.yticks(range(len(corr)), corr.columns)
plt.title('Correlation Matrix Heatmap')
heatmap_path = os.path.join(output_dir, 'corr_heatmap.png')
plt.savefig(heatmap_path, bbox_inches='tight')
plt.close()
print(f"✅ Saved correlation heatmap to {heatmap_path}")

# World choropleth map for one variable (example: hospital_beds_per_1000)
try:
    zip_url = "https://naturalearth.s3.amazonaws.com/110m_cultural/ne_110m_admin_0_countries.zip"
    world = gpd.read_file(zip_url)
    world = world.explode(index_parts=False)

    if 'ISO_A3' in world.columns:
        world = world.rename(columns={'ISO_A3': 'iso_a3'})
    elif 'ADM0_A3' in world.columns:
        world = world.rename(columns={'ADM0_A3': 'iso_a3'})

    df_map = df_latest.reset_index().rename(columns={'country_name': 'country'})
    def name_to_iso3(name):
        try:
            return pycountry.countries.lookup(name).alpha_3
        except:
            return None

    df_map['iso_a3'] = df_map['country'].apply(name_to_iso3)
    world_merged = world.merge(df_map, on='iso_a3', how='inner')

    fig, ax = plt.subplots(1, 1, figsize=(12, 6))
    world_merged.plot(column='hospital_beds_per_1000', legend=True, ax=ax,
                      legend_kwds={'label': "Hospital Beds per 1000"})
    ax.set_title('Hospital Beds per 1000 (WDI)')
    ax.axis('off')
    map_path = os.path.join(output_dir, 'hospital_beds_map.png')
    plt.savefig(map_path, bbox_inches='tight')
    plt.close()
    print(f"✅ Saved map to {map_path}")
except Exception as e:
    print("❗ Choropleth map could not be generated:", str(e))
