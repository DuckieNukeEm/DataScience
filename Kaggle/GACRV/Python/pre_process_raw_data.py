import pandas as pd
import os
from ast import literal_eval
import json


def flatten_json(df, cols_to_keep = None, Encode_Hits = False):
	"""A function to flatten the json part of the file, will take some time to process"""
	
	JSON_COLUMNS = ['device', 'geoNetwork', 'totals', 'trafficSource']
    #now going to be working on the 'hits' columns
	df = flatten_hits(df, Encode = Encode_Hits)
	
	df = flatten_CD(df)
    #flattening the files    
	for column in JSON_COLUMNS:
		print(column)
		column_as_df = pd.io.json.json_normalize(df[column])
		df = df.drop(column, axis=1).merge(column_as_df, right_index=True, left_index=True)
    

    
    #now I'm going to walk through each jason_columns and remove the 
    #fields that aren't needed
	if cols_to_keep is not None:
		df = df[cols_to_keep]
    
	return df

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
	



def get_promotion(df):
	"""Hunction to pull out the relevant promotion infomraiton and dumping the rest"""
	try:
		if 'promotion' in df.columns:
			df['promoId']  = df['promotion'].apply(lambda x: '|'.join([p['promoId'] for p in x] if type(x) == list else []))
			df['promoName']  = df['promotion'].apply(lambda x: '|'.join([p['promoName'] for p in x] if type(x) == list else []))
			del df['promotion']
		return(df)
	except:
		print("Wasn't able to pull out promotions")
	

def get_product(df):
	"""Hunction to get the product detail"""
	try:
		if 'product' in df.columns:
			#print(chunk['product'][0])
			df['ProductPrice'] = df['product'].apply(lambda x: '|'.join([p['productPrice'] for p in x] if type(x) == list else []))
			df['v2ProductName'] = df['product'].apply(lambda x: '|'.join([p['v2ProductName'] for p in x] if type(x) == list else []))
			df['v2ProductCategory'] = df['product'].apply(lambda x: '|'.join([p['v2ProductCategory'] for p in x] if type(x) == list else []))
			del df['product']
		return(df)
	except:
		print("wasn't able to pull out products")

             
			
def flatten_hits(df, Encode = False):
	"""A special function to flatten hits"""

	df_literal = df['hits'].copy()
	df_literal[df_literal == "[]"] = "[{}]"
	df_literal = df_literal.apply(literal_eval)

	df_hold = pd.io.json.json_normalize(df_literal.str[0])
	df_hold = get_promotion(df_hold)
	df_hold = get_product(df_hold)	
	df_hold.columns = ['Entrance.' + x for x in df_hold.columns]
	df = pd.concat([df, df_hold], axis = 1)
		
	df_literal = df_literal.str[1]
	#df_hold = pd.io.json.json_normalize(df_literal.str[1])
	df_hold = pd.io.json.json_normalize(df_literal[df_literal.apply(type) == dict] )
	df_hold = get_promotion(df_hold)
	df_hold = get_product(df_hold)	
	df_hold.columns = ['exit.' + x for x in df_hold.columns]
	df_hold.index = df_literal[df_literal.apply(type) == dict].index

	df = pd.concat([df, df_hold], axis=1, join_axes=[df.index])

	del df['hits']

	if Encode:
		for column in df.columns:
			for idx in df[column].index:
				x = df.get_value(idx,column)
				try:
					x = unicode(x.encode('utf-8','ignore'),errors ='ignore') if type(x) == unicode else unicode(str(x),errors='ignore')
					df.set_value(idx,column,x)
				except Exception:
					print 'encoding error: {0} {1}'.format(idx,column)
					df.set_value(idx,column,'')
					continue
	
	return(df)


def flatten_CD(df):	
	"""flattens the CustomDimension field, :/"""
	df_hold = df['customDimensions'].copy()
	df_eval = df_hold[df_hold != '[]'].apply(literal_eval)
	
	df_eval = pd.io.json.json_normalize(df_eval.str[0])
	df_eval.index = df_hold[df_hold != '[]']
	
	df_eval.columns = ['customDimensions.' + str(x) for x in df_eval.columns]
	df = pd.concat([df, df_eval], axis = 1, join_axes = [df.index])
	
	del df['customDimensions']
	
	return(df)
	
	
def read_da_file(csv_path = '/home/asmodi/.cache/.fr-ACwCO3/train_v2.csv', 
				nrows = 1e5,
				skiprows = 0,
				headers = None,
				col_names = None) : 
	""" Hunction to load the data frame, buid to do batch processing"""
	
	df = pd.read_csv(csv_path, 
		header = headers,
		names = col_names,
		converters={3: json.loads, #'device'
					5: json.loads, #geoNetworks
					 8 : json.loads, #totals
					 9 : json.loads}, #trafficSource
		dtype = {4: 'str'}, # Important!! FullVisitiorId
		skiprows = skiprows,
		nrows = nrows,
		encoding = 'utf-8-sig')
	return(df)
		

def write_da_file(df, file_name, file_loc = '/home/asmodi/Code/data/', index = False, try_encoding = False):
	file_loc = file_loc + file_name + '.csv'
	if try_encoding:
		for cols  in df.columns:
			if df[cols].dtype == object:
				try:
					df[cols] = df[cols].str.decode('utf-8', 'ignore').str.encode('utf-8')
				except:
					print('cant safely convert to utf-8 of col %s' % cols)
					continue
	df.to_csv(path_or_buf = file_loc, index = False, encoding = 'utf-8-sig')


if __name__ == "__main__":
	CHUNK_SIZE = 10000
	SKIP_ITTER = 0

	if False: #if processing training
		MAX_ITER = 1708338/CHUNK_SIZE 
		csv_file = '/home/beltain/R/Data/GACRV/train_v2.csv'
		out_name = 'training chunk '
	else:
		MAX_ITER = 401590/CHUNK_SIZE
		csv_file = '/home/beltain/R/Data/GACRV/test_v2.csv'
		out_name = 'test chunk '
	
	col_names = ['channelGrouping',
 'customDimensions',
 'date',
 'device',
 'fullVisitorId',
 'geoNetwork',
 'hits',
 'socialEngagementType',
 'totals',
 'trafficSource',
 'visitId',
 'visitNumber',
 'visitStartTime']

	
	while SKIP_ITTER <= MAX_ITER:
#	for chunk in df_itter:

		print('Working on current_itteration %i' % SKIP_ITTER)
		
		df = read_da_file(csv_file, nrows = CHUNK_SIZE, skiprows = CHUNK_SIZE * SKIP_ITTER, headers = 0, col_names = col_names)
		#df.columns = col_names
	#	print(df.columns)
		
		df = flatten_json(df)
		cur_name = out_name + str(SKIP_ITTER)
		
		write_da_file(df, file_name = cur_name,  file_loc = '/home/beltain/R/Data/GACRV/',try_encoding = True)
		
		SKIP_ITTER = SKIP_ITTER + 1
		
		
		
