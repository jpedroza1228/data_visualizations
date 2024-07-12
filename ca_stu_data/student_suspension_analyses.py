import numpy as np
import pandas as pd
import janitor


data = pd.read_csv('/Users/cpppe/Desktop/github_projects/data_visualizations/ca_stu_data/suspension_data_2012.csv')

data.groupby(["CountyName"]).mean(["Total Suspensions"])

data = data.clean_names(
  case_type = "snake"
)

data["district_code"] = data.groupby(["county_name", "academic_year"]).cumcount()+1
data["school_code"] = data.groupby(["county_name", "academic_year", "district_code"]).cumcount()+1


