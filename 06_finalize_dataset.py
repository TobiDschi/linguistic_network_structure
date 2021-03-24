# -*- coding: utf-8 -*-
"""
Created on Fri Jul 31 11:58:57 2020

@author: tobia
"""

import pandas as pd
import ast
import statistics
import numpy as np

def prod(iterable):
    product = 1
    for element in iterable:
        if element != 0:
            product = product * element
        elif element == 0:
            product = product * 1
    return product

#define the geometrical mean function
def geom_mean(array):
    product = prod(array)
    try:
        return product ** (1/len(array))
    except ZeroDivisionError:
        return float('nan')

#import the first dataset (the data on the messages)
print("importing data")
filepath = ""
message_data = pd.read_csv(filepath) #open analysis_data.csv
#exclude the first index_column that is generated upon model 
message_data = message_data.iloc[:,1:]

#instantiate variable
similarity_values = list()

#the elements in the similarities column are stored as a str rather than lists
#so we have to recover them as floats with ast evaluation
print("evaluating similarity strings in message data")
counter = 1
for str_ in message_data['similarities']:
    try:
        similarity_values.append(ast.literal_eval(str_))
        
    #value error occurs for nans, so exception for the value error
    except ValueError:
        similarity_values.append('nan')
    print("appending sim val nº " + str(counter))
    counter = counter + 1    
    
message_data['similarities'] = similarity_values

#match the model information on the subgraph columns
full_data = message_data

#mean and median similarity
mean_similarity = list()
median_similarity = list()
similarity_sd = list()
similarity_var = list()
geom_mean_simil = list()

counter = 0
for list_ in full_data['similarities']:
    try:
        mean_similarity.append(statistics.mean(list_))
        median_similarity.append(statistics.median(list_))
        similarity_sd.append(statistics.stdev(list_))
        similarity_var.append(statistics.variance(list_))
        geom_mean_simil.append(geom_mean(np.array(list_)))
    except TypeError:
        mean_similarity.append(float('nan'))
        median_similarity.append(float('nan'))
        similarity_sd.append(float('nan'))
        similarity_var.append(float('nan'))
        geom_mean_simil.append(float('nan'))
    print("appending measures of similarity nº " + str(counter))
    counter = counter + 1
    
full_data['mean_similarity'] = mean_similarity
full_data['median_similarity'] = median_similarity
full_data['similarity_sd'] = similarity_sd
full_data['similarity_var'] = similarity_var
full_data['geom_mean_similarity'] = geom_mean_simil

#get the similarity to the central node as well
similarity_to_central = list()
nodes = list()

agg = message_data.groupby(message_data['subgraph'])
counter = 0
for subgraph, subcorpus in agg:
    index = 0
    index_counter = 0
    for node in subcorpus['central']:
        if node == True:
            index = index_counter
            break
        else:
            index_counter = index_counter + 1
    for list_ in subcorpus['similarities']:
        try:
            similarity_to_central.append(list_[index])
        except IndexError:
            similarity_to_central.append(float('nan'))
    for node in subcorpus['id']:
        nodes.append(node)
    print("analysing sims to central ini thread nº " + str(counter))
    counter = counter + 1
    
similarity_to_central = pd.DataFrame(data = {"id" : nodes, "sim_to_central": similarity_to_central})

full_data.merge(similarity_to_central, on = "id")

filepath = ""
full_data.to_csv() #create final_dataset.csv
print("data assembled and saved to file. exit exe")
