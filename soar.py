import pandas as pd
import numpy as np
import os

def cleanSoarExam (data\
                   , examNum\
                   , fileType='flat'\
                   , colSpec = [(0,9),(10,21),(22,28),(29,30),(30,32),(32,34),(34,36),(36,38),(39,41),(41,68)]\
                  , soarSessions=[]):
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
            df.loc[df.tuid=='NNNNNNNNN','soarType']='key'
    return df

def countwrong (df,col,ver,truthcol = 'soarType',truthind = 'key',compind = ['mine'],vercol = 'version'):
    truth = df.loc[(df[truthcol]==truthind)&(df[vercol] == ver),col]
    comp = df.loc[(df[truthcol].isin(compind))&(df[vercol] == ver),col]
    res = pd.DataFrame(data ={'item':[col],'number':[(comp != truth[0]).sum()]})
    return(res)

def countwrong_allcol(df,ver,itemcols,sortfields = ['number','item'],
                      truthcol = 'soarType',truthind = 'key',compind = 'mine',vercol = 'version'):
    tmp = pd.DataFrame()
    for col in itemcols:
        tmp = tmp.append(countwrong(df= df, col = col, ver = ver))
    tmp = tmp.sort_values(sortfields,ascending=False).reset_index(drop = True)
    return tmp
	
	