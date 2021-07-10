

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.44780bed-1635-4ef8-bfe0-21987ddcb0de"),
    data_partner_grades=Input(rid="ri.foundry.main.dataset.24d76af2-1c70-4423-9f66-eaafc7b7d60c")
)
def final_grades(data_partner_grades):
    import pandas as pd
    import re as re

    pandas_df = data_partner_grades.toPandas()

    pandas_df['date_shift']=pandas_df['max_num_shift_days'].apply(pd.to_numeric, errors='coerce')

    return pandas_df

