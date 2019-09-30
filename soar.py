import pandas as pd
import numpy as np
import os

def cleanSoarExam (data\
                   , examNum\
                   , fileType='flat'\
                   , colSpec = [(0,9),(10,21),(22,28),(29,30),(30,32),(32,34),(34,36),(36,38),(39,41),(41,68)]\
                  , soarSessions=[]
                  , nkeys = 1):
    #import pandas as pd
    #import numpy as np
    #import os
    if fileType != 'flat':
        print("\n{} fileType is not currently supported.".format(fileType))
    else:
        try:
            df=pd.read_fwf(data,colSpec,
                           names=['tuid','last','first',
                                  'middle','unnamed1','unnamed2',
                                  'unnamed3','soar','ncorrect','item'])
        except:
            "print(\nCould not find file {} or {} was not acceptable value for colSpec)".format(data,colSpec)
        #try:
        #    df.columns = ['tuid','last','first','middle','unnamed1','unnamed2','unnamed3','soar','ncorrect','item']
        #except:
        #    print("\nColumn number != 10.\n{}".format(len(df.columns)))
        try:
            df['examNumber']=examNum
            numbers=pd.Series(list(range(28))).astype(str)
            itemNames= 'item_'+ numbers[1:]
            itemData=df.item.apply(lambda i: pd.Series(list(i)))
            itemData.columns=itemNames
            df=df.merge(itemData,'outer',left_index=True,right_index=True).drop('item',axis=1)
        except:
            print("\nUnhandled exception encountered.")
        if soarSessions ==np.nan:
            df.loc['soarType']='other'
        else:
            df.loc[df.soar.isin(soarSessions),'soarType']='mine'
            df.loc[~df.soar.isin(soarSessions),'soarType']='other'
            df.loc[(df['tuid'].str.contains('|'.join(['NNN','KEY'])))|(df['first'].str.contains('|'.join(['NNN'])))|(df['last'].str.contains('|'.join(['NNN']))),'soarType']='key'
        if df.loc[df['soarType']=='key'].shape[0]!= nkeys:
            print("Found {} keys in the df; keys should be confirmed manually".format(df.loc[df['soarType']=='key'].shape[0]))
        dfkey = pd.DataFrame()
        for soar in df.soar.unique():
            tmp = df.loc[df.soarType=='key']
            tmp['soar'] = soar
            dfkey = dfkey.append(tmp)
        df = df.append(dfkey)
    return df

def countwrong (df,col,ver,truthcol = 'soarType',truthind = 'key',compind = ['mine'],vercol = 'version'):
    try:
        truth = df.loc[(df[truthcol]==truthind)&(df[vercol] == ver),col]
    except:
        print("failed to generate 'truth' - truthcol:{};truthind:{};vercol:{};ver:{};col:{}"\
        .format(truthcol,truthind,vercol,ver,col))
    try:
        comp = df.loc[(df[truthcol].isin(compind))&(df[vercol] == ver),col]
    except:
        print("failed to generate 'comp' - truthcol:{};truthind:{};vercol:{};ver:{};col:{}"\
        .format(truthcol,truthind,vercol,ver,col))
    try:
        res = pd.DataFrame(data ={'item':[col],'number':[(comp != truth[0]).sum()]})
    except:
        print("failed to generate 'res' - truth:{}".format(truth))
    return res

def countwrong_allcol(df,ver,itemcols,sortfields = ['number','item'],
                      truthcol = 'soarType',truthind = 'key',compind = 'mine',vercol = 'version'):
    tmp = pd.DataFrame()
    for col in itemcols:
        tmp = tmp.append(countwrong(df= df, col = col, ver = ver))
    tmp = tmp.sort_values(sortfields,ascending=False).reset_index(drop = True)
    return tmp

def returnwrong (df,col,ver,truthcol = 'soarType',truthind = 'key',compind = ['mine'],vercol = 'version'):
    try:
        truth = df.loc[(df[truthcol]==truthind)&(df[vercol] == ver),col]
    except:
        print("failed to generate 'truth' - truthcol:{};truthind:{};vercol:{};ver:{};col:{}"\
        .format(truthcol,truthind,vercol,ver,col))
    try:
        comp = df.loc[(df[truthcol].isin(compind))&(df[vercol] == ver),col]
    except:
        print("failed to generate 'comp' - truthcol:{};truthind:{};vercol:{};ver:{};col:{}"\
        .format(truthcol,truthind,vercol,ver,col))
    try:
        df[col] = (comp != truth[0])
        res = df[col]
    except:
        print("failed to generate 'res' - truth:{}".format(truth))
    return res

def letters2num(x,letters2num_dict = {1:'D',2:'C',3:'B',4:'A',' ':'Space',np.nan:'Blank'}):
    try:
        x = int(x)
        x = letters2num_dict[int(x)]
        pass
    except:
        x = x
    return x
