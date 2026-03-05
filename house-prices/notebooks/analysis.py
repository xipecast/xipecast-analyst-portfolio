import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

# Settings
pd.set_option('display.max_columns', None)
sns.set_theme(style='whitegrid')

# Load data
train = pd.read_csv('data/train.csv')
test = pd.read_csv('data/test.csv')

print('Train shape:', train.shape)
print('Test shape:', test.shape)

# First 5 rows
print(train.head())

# Column names and data types
print(train.info())

# Basic statistics
print(train.describe())

# prettier summary
print("=== SHAPE ===")
print(f"Rows: {train.shape[0]}, Columns: {train.shape[1]}")

print("\n=== COLUMN TYPES ===")
print(f"Numerical: {len(train.select_dtypes(include=np.number).columns)}")
print(f"Categorical: {len(train.select_dtypes(include='str').columns)}")

print("\n=== SALE PRICE STATS ===")
print(f"Min:  ${train['SalePrice'].min():,}")
print(f"Max:  ${train['SalePrice'].max():,}")
print(f"Mean: ${train['SalePrice'].mean():,.0f}")

print("\n=== MISSING VALUES ===")
missing = (train.isnull().sum() / len(train) * 100)
missing = missing[missing > 0].sort_values(ascending=False)
print(missing)

fig, axes = plt.subplots(1, 2, figsize=(12, 4))

# Distribution
axes[0].set_title('SalePrice Distribution')
sns.histplot(train['SalePrice'], kde=True, ax=axes[0])

# Log distribution
axes[1].set_title('Log SalePrice Distribution')
sns.histplot(np.log1p(train['SalePrice']), kde=True, ax=axes[1])

plt.tight_layout()
plt.savefig('notebooks/saleprice_distribution.png')
print('Plot saved!')
# Top correlations with SalePrice
corr = train.select_dtypes(include=np.number).corr()
top_features = corr['SalePrice'].sort_values(ascending=False).head(11)
print("\n=== TOP CORRELATED FEATURES ===")
print(top_features)

# Heatmap of top features
plt.figure(figsize=(10, 8))
top_cols = top_features.index
sns.heatmap(
    train[top_cols].corr(),
    annot=True,
    fmt='.2f',
    cmap='coolwarm'
)
plt.title('Top Feature Correlations')
plt.tight_layout()
plt.savefig('notebooks/correlations.png')
print('Correlation plot saved!')

fig, axes = plt.subplots(2, 2, figsize=(12, 10))

# Overall Quality vs Price
sns.boxplot(x='OverallQual', y='SalePrice', data=train, ax=axes[0,0])
axes[0,0].set_title('Quality vs Price')

# Living Area vs Price
sns.scatterplot(x='GrLivArea', y='SalePrice', data=train, ax=axes[0,1])
axes[0,1].set_title('Living Area vs Price')

# Garage Cars vs Price
sns.boxplot(x='GarageCars', y='SalePrice', data=train, ax=axes[1,0])
axes[1,0].set_title('Garage Cars vs Price')

# Year Built vs Price
sns.scatterplot(x='YearBuilt', y='SalePrice', data=train, ax=axes[1,1])
axes[1,1].set_title('Year Built vs Price')

plt.tight_layout()
plt.savefig('notebooks/relationships.png')
print('Relationships plot saved!')

# Add this to your script
print(train[train['GrLivArea'] > 4000][['GrLivArea', 'SalePrice']])

# Visualize outliers more clearly
fig, ax = plt.subplots(figsize=(10, 6))
sns.scatterplot(x='GrLivArea', y='SalePrice', data=train, ax=ax)

# Add labels to suspicious points
outliers = train[
    (train['GrLivArea'] > 4000) & 
    (train['SalePrice'] < 300000)
]
for idx, row in outliers.iterrows():
    ax.annotate(f"Id:{row['Id']}", (row['GrLivArea'], row['SalePrice']))

plt.title('Living Area vs Price - Outlier Investigation')
plt.savefig('notebooks/outliers.png')
print('Outlier plot saved!')

# Print the suspicious ones
print("\n=== SUSPICIOUS OUTLIERS ===")
print(train[train['GrLivArea'] > 4000][['Id','GrLivArea', 'SalePrice', 'OverallQual']])

print("\n=== CHEAP BUT HUGE HOUSES ===")
print(train[
    (train['GrLivArea'] > 4000) & 
    (train['SalePrice'] < 300000)
][['Id', 'GrLivArea', 'SalePrice', 'OverallQual', 'Neighborhood']])