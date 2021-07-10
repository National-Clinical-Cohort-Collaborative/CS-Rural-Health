import pandas as pd

@transform_pandas(
    Output(rid="ri.vector.main.execute.ac452cb8-5902-4044-a251-a10617f9ee6d"),
    s_interact_invars=Input(rid="ri.foundry.main.dataset.56725eae-2284-4cb3-a920-ac17601fcc18")
)
def unnamed_4(s_interact_invars):
    

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.c58607a0-5f4b-4882-a060-ad93dc631a26"),
    s_interact_NOTinvars=Input(rid="ri.foundry.main.dataset.7dc6d77a-edbf-4c00-834d-ac149c707e21"),
    s_interact_invars=Input(rid="ri.foundry.main.dataset.56725eae-2284-4cb3-a920-ac17601fcc18")
)
def unnamed_5(s_interact_invars, s_interact_NOTinvars):
    local_df1 = (s_interact_invars)
    local_df2 = s_interact_NOTinvars
    
   
    print(local_df1)
    return local_df1

