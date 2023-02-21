# -*- coding: utf-8 -*-

import pandas as pd


def tsv_to_csv(tsv_fp, new_fp):
    """Converts the tsv, latin1 encoding files from ICPSR to csv, utf-8 encoding"""
    df = pd.read_csv(tsv_fp, sep='\t', encoding='latin1')
    df.to_csv(new_fp, sep=',', encoding='utf-8')


tsv_to_csv('../ICPSR_37938_TransPop/DS0001/37938-0001-Data.tsv', '../data/transpop.csv')
tsv_to_csv('../ICPSR_37938_TransPop/DS0003/37938-0003-Data.tsv', '../data/cisgender.csv')
tsv_to_csv('../ICPSR_37938_TransPop/DS0005/37938-0005-Data.tsv', '../data/combined.csv')
