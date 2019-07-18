# -*- coding: utf-8 -*-
"""
Created on Mon Jan 29 19:07:28 2018

@author: Bokkin Wang
"""
import os
import csv
import tablib
os.chdir('D:/bigdatahw/上财东正杯/竞赛数据-20171114')
print(os.getcwd())
csv_reader = csv.reader(open('contest_ext_crd_qr_recorddtlinfo.tsv', encoding='utf-8'))
csv=list(csv_reader)
csv=[row[0].split('\t') for row in csv]
data = tablib.Dataset(*csv)  
myfile = open('D:/bigdatahw/上财东正杯/竞赛数据-20171114/contest_ext_crd_qr_recorddtlinfo.csv', 'w')  
myfile.write(data.csv)  
myfile.close()  