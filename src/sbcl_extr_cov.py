import pandas as pd
from sys import argv

assert len(argv) == 2
covhtml = argv[1]

df = pd.read_html(covhtml)[0]
dff = df.drop([0, 1]).drop(columns=[0])
for i in range(1, len(dff.columns) + 1):
    dff[i] = dff[i].str.extract(r'(\d+(.\d+)?)', expand=True).dropna().astype('float64')
dff.dropna(inplace=True)
dff.columns = "cov_exp cov_tot cov_per banch_cov branch_tot branch_per".split(' ')

def mean(axis, rm_epsilon=3):
    return dff[dff[axis] > rm_epsilon][axis].mean()

print(f"Expression coverage: {mean('cov_per')}\nBranch coverage: {mean('branch_per')}")
