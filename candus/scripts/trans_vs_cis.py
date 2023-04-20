# -*- coding: utf-8 -*-
import pandas as pd
import numpy as np
import plotly.express as px
import json


def trans_vs_cis(df, q):
    """
    Displays a plotly express bar chart comparing transgender vs cisgender individuals for a particular question.
    :param df: pandas.DataFrame containing at least the following columns—'TRANS_CIS', 'SEX', and q
    :param q: a string of the question to be visualized; must be documented in `codebook` and `questions` dict
    :returns: a plotly express figure (bar chart)
    """
    df = df.copy()
    df = df[['TRANS_CIS', q, 'SEX']].astype(str)
    with open('scripts/codebook.json', 'r') as f:
        codebook = json.load(f)
    with open('scripts/questions.json', 'r') as f:
        questions = json.load(f)
    df['TRANS_CIS'] = df['TRANS_CIS'].map(codebook['TRANS_CIS'])
    df[q] = df[q].map(codebook[q])
    dfpivot = df.pivot_table(
        index=q,
        columns='TRANS_CIS',
        values='SEX',
        aggfunc='count'
    )
    dfpivot = dfpivot.reindex(codebook[q].values())
    dfprop = dfpivot / dfpivot.sum()
    if 'Planned missing' in dfprop.index:
        dfprop = dfprop.drop(['Planned missing'])
    dfmelt = dfprop.reset_index().melt(id_vars=q)
    fig = px.bar(
        dfmelt,
        x=q,
        y='value',
        color='TRANS_CIS',
        barmode='group',
        color_discrete_sequence=['#bababa', '#fca2bb'],
        title=q + ': ' + questions[q],
        text=np.round(dfmelt['value'], 4)
    )
    fig.update_yaxes(
        title='proportion'
    )
    fig.update_xaxes(
        title=''
    )
    fig.update_traces(textposition='outside', textfont_size=9)
    return fig 
