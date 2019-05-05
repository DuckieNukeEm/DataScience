import os
import pandas as pd

def find_cols_to_keep(df):
	"""walks through each column in a data frame, and determines
	if the columns only has one value, two values (that basically say 
	the same thing) or is empty"""
	
	col_names = list(df.columns.values)
	cols_to_keep = []
	cols_to_reject = []
	blank_list = ['not available in demo dataset', 'not set']

	
	for i in xrange(len(col_names)):
		element_count = df.iloc[:,i].nunique()
		element_list = list(df.iloc[:,i].unique())
		
		if element_count < 2:
			cols_to_reject.append(col_names[i])
			next
		elif element_count == 2 and sorted(element_list) == blank_list:
			cols_to_reject.append(col_names[i])
			next			
		else:
			cols_to_keep.append(col_names[i])
		
	return (cols_to_keep, cols_to_reject)

def split_columns(df, sep = '|'):
	df_cols = df.dtypes
	df_cols = df_cols[df_cols == object].index
	for cols in df_cols:
		print(cols)
		try:
			df[cols] = df[cols].str.decode('ascii','ignore').str.encode('utf-8','ignore')
			counter = df[cols].map(lambda x: str.count(str(x), '|')).max()
		except TypeError:
			print('ran into TypeError on %s trying to get a count...:/' % cols)
			continue
		except UnicodeEncodeError:
			print('Ran into an encoding error on %s  trying to encode shit...:/' % cols)
			continue
		except AttributeError:
			print('Ran into a goddamn Attribute Error Now. jesuth christh')
			continue
		if counter > 0:
			new_col_names = [cols+'.' +str(x) for x in xrange(counter + 1)]
			try:
				df[new_col_names] = df[cols].str.split('|',expand=True)
				del df[cols]
			except TypeError:
				print('Type Error while attempting to split columns')
	return(df)

mypath = '/home/beltain/R/Data/GACRV/'

onlyfiles = [f for f in os.listdir(mypath) if os.path.isfile(os.path.join(mypath, f))]

for filez in onlyfiles:
    print('Working on File: %s' % filez)
    if filez[0:8] == 'training':
        if 'df_train' in locals():
            df_train = df_train.append(pd.read_csv(os.path.join(mypath, filez), encoding='utf-8', low_memory = False), ignore_index = True)
        else:
            df_train = pd.read_csv(os.path.join(mypath, filez), encoding='utf-8', low_memory = False)
    elif filez[0:5] == 'test ':
        if 'df_test' in locals():
            df_test = df_test.append(pd.read_csv(os.path.join(mypath, filez), encoding='utf-8', low_memory = False), ignore_index = True)
        else:
            df_test = pd.read_csv(os.path.join(mypath, filez), low_memory = False, encoding='utf-8')
    else:
        print('Not a file we are Using')

#df_train = split_columns(df_train) #Got way to much data that was pretty useless
#df_test = split_columns(df_test)

df_train_keep, df_train_reject = find_cols_to_keep(df_train)
df_test_keep, df_test_reject = find_cols_to_keep(df_test)

df_train = df_train[df_train_keep]
df_test = df_test[df_test_keep]

df_train.to_csv(os.path.join(mypath, 'Train.csv'), index = False, encoding = 'utf-8')
df_test.to_csv(os.path.join(mypath, 'Test.csv'),  index = False, encoding = 'utf-8')
