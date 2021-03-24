# -*- coding: utf-8 -*-
"""
Created on Sun Apr 26 19:54:16 2020

@author: tobia
"""

import gensim
import pandas as pd
import numpy as np
import os
from gensim.models.coherencemodel import CoherenceModel
import time
from gensim import corpora
import ast

#define the value extraction for latter correction of the dataframes
def extract_value(array):
    index_counter = 0
    for element in array:
        try:
            array[index_counter] = element.iloc[0]
        except AttributeError:
            array[index_counter] = element
        index_counter = index_counter + 1

#define the delta function
def delta(array):
    new_array = list()
    for n in range(len(array)):
        try:
            new_array.append(array[n+1] - array[n])
        except IndexError:
            break
    return np.array(new_array)

#prepare lists
ldamodels = list()

#open the data that previous programs have prepared
filepath = ""
data = pd.read_csv(filepath) #open network_data_prep.csv
data = data.iloc[:,1:]


model_data = data[['id', 'subgraph', 'prep_text', 'text_data', 'text', 'direct_neighbors']]
agg = model_data.groupby(model_data['subgraph'])

#prepare the variables for the similarity matrices that we are going to assemble later on
identifiers = list()
similarities = list()
sim_to_neighbors = list()

#prepare the variables for the model information that we are going to assmeble later on
modelNames = list()
modelCoherences = list()
modelTopicNumbers = list()
thread_ids = list()

#iterate over threads (groups aggradted by data.groupby("subgraph")), make various models and chose the best one
#the idea here is to find a model that is as coherent as possible while having as few topics as possible 
counter = 1
print("start topic modelling")
for subgraph, subcorpus in agg: #how to access the lenth of a chunk
    
    #prepare the variables to make more than one model and then compare them with regards to topic/coherence ratio
    models = list()
    coherences = list()
    topic_nums = list()
    
    #before we can build the dictionary and corpus we need to convert the string representation
    #of the word lists in the pandas series within subcorpus['text_data'] into a 
    #real list, we use the ast library for this
    text_data = list()
    for str_ in subcorpus['text_data']:
        text_data.append(ast.literal_eval(str_))
    
    #then build the corpus and the dictionary for each topic
    dictionary = corpora.Dictionary(text_data)  
    corpus = [dictionary.doc2bow(text) for text in text_data] #include the number of tokens in a corpus in the data to exclude sparse threads
 
    #make n = number of nodes models per thread but at least 5 and maximum 50 
    #we assume that by and large we can have as many topics as we have contributions
    #to the thread. this is maybe not a well-read assumtion but makes production
    #of models feasible, also because this is kind of a micro blogging site    
    #this represents an array 1 through the number of nodes in the network
    topics = np.array(range(1,subcorpus.shape[0]+1))
    
    #make sure not too many models are tried
    if len(topics) >= 50:
        node_count = np.array(range(1,51))
    
    #make sure enough models are tried
    elif len(topics) <= 5:
        node_count = np.array(range(1,6))
    
    #one model per count if we are in the middle range of things.
    else:
        node_count = topics
    
    for i in range(len(node_count)):   #change this to the size of the topic number array
        
        #prepare variables and train model
        topic_num = node_count[i]
        topic_nums.append(node_count[i])
        model = gensim.models.ldamodel.LdaModel(corpus, num_topics = topic_num, id2word=dictionary, passes = 15)
        print(str((i+1)/len(node_count) * 100) + " % of models for thread generated")
        models.append(model)
        
        #find coherence as measure for best model later on
        coherence_model = CoherenceModel(model = model, texts = text_data, dictionary = dictionary, coherence = "c_v")
        coherences.append(coherence_model.get_coherence())
        print(str((i+1)/len(node_count) * 100) + " % of coherences of thread models analysed")
        print("number of topics: " + str(topic_num))
        print("coherence " + str(coherence_model.get_coherence()))

    #join the models and their coherences together in order to chose the best one
    model_choice = pd.DataFrame(data = {"model":models, "coherence":coherences, "topic_num":topic_nums})
    models = np.array(models)
    coherences = np.array(coherences)
    topic_nums = np.array(topic_nums)
    ratio = list(coherences/topic_nums)
    model_choice['ratio'] = ratio
    model_choice['coherence'] = coherences
    
    #choose the best model
    #we use the formula DELTA(x)/DELTA(y) to calculate the first derivation of the function
    #and then choose the point at the highest to find the best possible model
    #optimizing for a high coherence and a low number of topics modelled
    delta_x = delta(topic_nums)
    delta_y = delta(coherences)
    
    #we then use the delta values to calculate the first derivation
    derivate = delta_y / delta_x
    
    #choose the best model
    #if the first model has the highest coherence, just use it, otherwise dismiss it. 
    if model_choice['coherence'].iloc[0] == max(model_choice['coherence']):
        
        #append the best model
        best_model = model_choice.iloc[0]
        
        #then add information about the model to the variables
        modelNames.append(str(best_model['model']))
        modelCoherences.append(best_model['coherence'])
        modelTopicNumbers.append(best_model['topic_num'])
        thread_ids.append(subgraph)
  
       
    else:
        #else use the derivate to find the best model (the one in which the jump is the highest)
        derivate_index = np.array(range(model_choice.shape[0]))
        derivate_plus_zero = np.append([0], derivate)
        derivate_data = pd.DataFrame(data = {'index': derivate_index, 'derivate': derivate_plus_zero})
        def_index = derivate_data['index'].loc[derivate_data['derivate'] == max(derivate_data['derivate'])]
        best_model = model_choice.iloc[def_index]
        
        #then add information about the model to the variables
        modelNames.append(str(best_model['model']))
        modelCoherences.append(best_model['coherence'])
        modelTopicNumbers.append(best_model['topic_num'])
        thread_ids.append(subgraph)
        
    #debugging
    print(best_model)
    
    #append the prepared lists
    #use the iloc sclicer to access the model as such, otherwise a pandas series is returned
    #sometimes there are empty data frames being returned (when coherences are very similar and rounded too high)
    #this is the result of the try/except formulation above
    #also: separate the best model from the dataframe it is embedded in so we can use it later
    try:
        ldamodels.append(best_model['model'].iloc[0])
        best_model = best_model['model'].iloc[0]
    
    #this is when the above try was successfull
    except AttributeError:
        ldamodels.append(best_model['model'])
        best_model = best_model['model']
    
    #then calculate similarity
    second_counter = 1
    index = gensim.similarities.docsim.MatrixSimilarity(best_model[corpus])
    
    #how to store the similarities in the data frame, this gon' be important
    ids = list()
    for ind, document in subcorpus.iterrows():
        ids.append(document['id'])
        
    for ind, document in subcorpus.iterrows():
        doc = document['text']
        
        #an attribute error can arise due to the fact that unpresent text ist stored as a NaN float
        try:
            vec_bow = dictionary.doc2bow(doc.split())
        
        #just use the questionmark stored in the text_data column
        except AttributeError:
            doc = document['text_data']
            vec_bow = dictionary.doc2bow(doc.split())
        
        vec_lda = best_model[vec_bow]
        try:
            sims = index[vec_lda]
        
        #no idea where this index error comes from. maybe take care of later
        except IndexError:
            sims = float('nan')
            
        print(str(second_counter/subcorpus.shape[0]*100) + " % of similarities in thread calculated")
        #store similarities together with the identifier in the list such that we can later on join them
        #together to construct our similarity matrix in a data frame
        
        identifiers.append(subcorpus['id'][ind])
        try:
            similarities.append(list(sims))  #at this stage, we might runto a problem later, when we try
        #to do calculations, because we have no exact identifier as to which similarities refers
        #to which other text... maybe position in the nested list can give us the clue, but I am not sure.
        except TypeError:
            similarities.append(float('nan'))
         
        #generate similarity variable only for the direct neighbors
        sim_data = pd.DataFrame(data = {"node":ids, "sim_to_doc":sims})
        neighbor_sims = sim_data.loc[sim_data['node'].isin(ast.literal_eval(document['direct_neighbors'])),]
        mean_neighbor_sim = np.mean(neighbor_sims['sim_to_doc'])
        sim_to_neighbors.append(mean_neighbor_sim)
        
        #increment counter variable inside the similarity loop
        second_counter = second_counter + 1
    
    #debug
    print('\n' + str(counter/len(agg)*100) + "% of threads modelled")
    print('similarities: ' + str(sims))
    print(str(time.localtime()) + '\n')
    print('next thread')
    
    #increment counter variable (index and position in the loop)
    counter = counter + 1

#assemble similaritymatrix
similarity_data = pd.DataFrame(data = {'id' : identifiers,
                                       'similarities' : similarities,
                                       'sim_to_neighbor' : sim_to_neighbors})

#put it together with the normal data
data_sim_Data = data.merge(similarity_data, on = 'id', how = 'left')

#put together a dataframe that tells us about the number of topics in each thread and the coherence of the model
#here we have to add an identifier that goes together with the model info so that we can merge both data frames later on
modelInformation = pd.DataFrame(data = {'model_name' : modelNames,
                                        'model_coherence': modelCoherences,
                                        'model_topic_numbers': modelTopicNumbers,
                                        'subgraph': thread_ids})

#there is an error in the code that accesses the models incorrectly from the model_choice
#dataframe. here, we extract the correct value from the arrays in order to make a correct dataframe
extract_value(modelInformation['model_coherence'])
extract_value(modelInformation['model_topic_numbers'])

#merge data on the models together with the rest of the data
analysisData = data_sim_Data.merge(modelInformation, on = 'subgraph', how = 'left')

filepath = ""
analysisData.to_csv(filepath) #create analysis_data.csv
print('data merged and saved to hard drive')

#as a backup, export this data extra
filepath = ""
modelInformation.to_csv(filepath) #create models_extra_info.csv

#save the models
counter = 1
for model in ldamodels:
    filepath = ""
    newpath = filepath + str(counter) + "\\"
    if not os.path.exists(newpath):
        os.makedirs(newpath)
    model.save(newpath + "topicmodel" + str(counter) + ".model")
    print("model " + str(counter) + " saved")
    counter = counter + 1
print("all models saved")


