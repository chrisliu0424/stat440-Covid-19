import pandas as pd
import numpy as np
import re
# Import data
data = pd.read_csv("train2.txt")
test_data = pd.read_csv("test2.txt")
baseline = pd.read_csv("baseline.txt",sep=',')

def correct_one(age):
	"""
	Helper function for correct_age
	Function returns the average of the input range
	i.e if age = "50-59", it will return 55
	"""
	pattern = '-'
	# print(re.search(pattern,age))
	if age =='nan':
		return np.NaN
	if re.search(pattern,age):
		return int(re.split(pattern,age)[0])+5
	return int(float(age))	
	

def correct_age(df):
	"""
	Function to clean age column and make it an integer column
	Input: Pandas series of the age column
	Output: Pandas series of the age column with integer type
	"""
	df['age'] = df['age'].astype('str')
	df['age'] = df['age'].apply(correct_one)
	return df

def data_processing(df):
	"""
	Function apply all the data processing steps
	Output a df for fitting the model
	It will do the following data processing steps
	1. Make date column to datetime Object
	2. Exclude ['symptoms','confirmed','city'] (make the model simplier)
	3. Make age an integer(some are a range of numbers)(some are float)(some are NaN)
	   If it's a range, we will take the middle value of the range
	   fillna with the age mean
	"""
	df['confirmed'] = pd.to_datetime(df['confirmed'])
	if 'outcome' in list(df.columns):
		df = df.drop(['outcome','symptoms','confirmed','city'], axis=1)	
	else:
		df = df.drop(['symptoms','confirmed','city'], axis=1)	
	df = correct_age(df)
	df = df.fillna(df.mean())
	return df

data = data_processing(data)
test_data = data_processing(test_data)





my_cols = set(data.columns)
my_cols.remove('duration')
X = data[my_cols]
y = data['duration']


X_encoded = pd.get_dummies(X, columns=['sex'])
print(X_encoded.head())




# test_data[['Id','duration']].to_csv('prediction.csv',index=False)
print(test_data.head())
test_data['age']

