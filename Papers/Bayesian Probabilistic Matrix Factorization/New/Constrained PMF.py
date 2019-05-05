""" This is an implementation of the following paper:

[PAPER NAME] [AUTHORES] [PUBLICATIONS YEAR] 

Permission is granted for anyone to copy, use, modify, or distribute this
program and accompanying programs and documents for any purpose, provided
this copyright notice is retained and prominently displayed, along with
a note saying that the original programs are available from our web page.
Web Page: https://www.cs.cmu.edu/~rsalakhu/
Code found at: http://www.utstat.toronto.edu/~rsalakhu/BPMF.html
The programs and documents are distributed without any warranty, express or
implied.  As the programs were written for research purposes only, they have
not been tested to the degree that would be advisable in any important
application.  All use of these programs is entirely at the user's own risk.

"""
import pandas as pd
import numpy as np
import os as os
from math import sqrt

#setting hyper paramereters:

epsilon=50.0 # Learning rate 
ll = 0.01 # Regularization parameter 
momentum=0.8 #and what the hell is this bullshit....this 'momentum???'
epoch=1
maxepoch=10

num_of_batches = 9
N = 100000 #number of training triplests



PATH = os.getcwd() + "/"

# First I'm going to build an actual program to read through the data


#reading in data
df_train = pd.read_table(PATH + 'train_vec.csv', sep = ",")

df_test = pd.read_table(PATH + 'test_vec.csv', sep = ",")

mean_rating = df_train.agg({'rating':'mean'}).values
#mean_rating = df_train[:,2].mean()

pairs_train = len(df_train)
pairs_test = len(df_test)

num_of_movies = df_train['movie_id'].max()
num_of_users = df_train['user_id'].max()
num_of_feat = 10

print("Movie Count: %i \t User Count: %i" % (num_of_movies,num_of_users))
###
# Setting up the two DxM matrices and intilizing them
###

m_x_d = np.random.rand(num_of_movies, num_of_feat) * 0.1
w_x_d = np.random.rand(num_of_movies, num_of_feat) * 0.1
u_x_d = np.random.rand(num_of_users, num_of_feat) * 0.1


###
# Setting up the gradiant change variables
###

m_x_d_chng = np.zeros((num_of_movies, num_of_feat))
w_x_d_chng = np.zeros((num_of_movies, num_of_feat))
u_x_d_chng = np.zeros((num_of_users, num_of_feat))

###
# Before we start, you have to remeber, the movies are labeld 1 to 1000
# so they are litterally treated as index, in this case you'll need to shift
# the index by 1 to alling to the start at 0 index, so that's wat I'm doing below
###

df_train.iloc[:,[0,1]] = df_train.iloc[:,[0,1]] - 1
df_test.iloc[:,[0,1]] = df_test.iloc[:,[0,1]] - 1

### Now doing the big loop
for epoch in xrange(maxepoch):
	#need to randomly permute the rows
	perm_index = np.random.permutation(range(0,pairs_train))
	
	for batch in xrange(num_of_batches):
		print("working on epoch %i - batch %i \n" % (epoch, batch))
		#possible efficens for the below would be to create the matrix then repopulate
		#every time
		batch_perm_index = perm_index[range((batch - 1) * N,batch*N)]
		
		np_train = df_train.iloc[batch_perm_index].values.copy() 
	
		
		np_ratings = np_train[:,2].copy() * 1.00 - mean_rating
		
		####
		# COmputer prediction ---- honeslty not really sure why we are doing
		# this step
		###
		u_x_d = 
		

		pred_out = np.sum(u_x_d[np_train[:,0]] * m_x_d[np_train[:,1]], 1) 
		
		rating_error =  (pred_out - np_ratings)
		
		Err = 0.5 * sum(\
		np.power(rating_error,2) + \
		ll*np.sum(np.power(u_x_d[np_train[:,0]],2) + \
		np.power(m_x_d[np_train[:,1]],2),1) )
		
		####
		# COmputing the graidinat (first part of the gradient)
		###
		#Lots of transpose, whant to find a very efficent way to do this one!
		dx_u = (rating_error * 2 * m_x_d[np_train[:,1]].transpose()).transpose() +  ll * u_x_d[np_train[:,0]]
		dx_m = (rating_error * 2 * u_x_d[np_train[:,0]].transpose()).transpose() +  ll * m_x_d[np_train[:,1]]
		
		
		### second part of the gradient
		for ii in xrange(N):
			# I really want to find amore effecient way to do this!!!!
			u_x_d_chng[np_train[ii,0]] = u_x_d_chng[np_train[ii,0]] + epsilon/(N * momentum) * dx_u[ii]
			m_x_d_chng[np_train[ii,1]] = m_x_d_chng[np_train[ii,1]] + epsilon/(N * momentum) * dx_m[ii]
		u_x_d_chng = u_x_d_chng * momentum
		m_x_d_chng = m_x_d_chng * momentum
		
		
		# now we're updating the gradiant
		m_x_d = m_x_d - m_x_d_chng
		u_x_d = u_x_d - u_x_d_chng
	
		###
		# Computing the prediction after the update
		###
		
		
		pred_out = np.sum(u_x_d[np_train[:,0]] * m_x_d[np_train[:,1]], 1) 
			
		rating_error =  (pred_out - np_ratings)
		
		Err = 0.5 * sum(\
		np.power(rating_error,2) + \
		ll*np.sum(np.power(u_x_d[np_train[:,0]],2) + \
		np.power(m_x_d[np_train[:,1]],2),1) )
		

		
		
		###
		# Computing error on the validation set
		###
		
		np_test = df_test.values 
	
			
		pred_out = np.sum(u_x_d[np_test[:,0]] * m_x_d[np_test[:,1]], 1) + mean_rating #(why are we adding tht back in 
		
		adj_ind = np.where(pred_out > 5)[0]
		pred_out[adj_ind] = 5
		
		
		adj_ind = np.where(pred_out < 1)[0]
		pred_out[adj_ind] = 1
		
		rating_error =  (pred_out - np_test[:,2])
		
		Err = 0.5 * sum(\
		np.power(rating_error,2) + \
		ll*np.sum(np.power(u_x_d[np_test[:,0]],2) + \
		np.power(m_x_d[np_test[:,1]],2),1) )
		print('mean delta is %f' % np.mean(rating_error))
		print('Valdation error is %f\n' % 	sqrt(sum(np.power(rating_error,2))))
		
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
