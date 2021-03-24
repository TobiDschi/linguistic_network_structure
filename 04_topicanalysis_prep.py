# -*- coding: utf-8 -*-
"""
Created on Sun Apr 26 19:54:16 2020

@author: tobia
"""

import spacy
import pandas as pd
import numpy as np
import nltk
from nltk.stem import SnowballStemmer
from spacy.lang.es import Spanish

nltk.download('wordnet') 
spacy.load("es_core_news_md")
nltk.download('stopwords')

#open csv file to construct the data
data = pd.read_csv('') #load network_data.csv

mom = list()
for time in data['retrieved']:
    if time[38] == "1":
        mom.append(2021)
    elif time[38] == "4":
        mom.append(2020)
    else:
        print("ERROR!")
        
data['mom'] = mom

data['subgraph'] = np.where(data['mom'] == 2021, data['subgraph'].astype(str) + "_2021", data['subgraph'].astype(str))

data = data.iloc[:,1:21] 

prep_text = np.array(data['text'])

#correct for doubled exeptional threads

manual_stopwords = [ #append this list to take out all the stuff that we cannot process for the later topic modelling
                    ">",
                    ">>"
                    "negrin",
                    "negrines",
                    "negrín",
                    "negro",
                    "negros",
                    "anon",
                    "URL",
                    "number",
                    ",",
                    ".",
                    "?",
                    "¿",
                    "@",
                    "¡",
                    "!",
                    "(",
                    ")",
                    "q",
                    "ser",
                    ] #for the strings that start with digits in the beginning of every post

parser = Spanish()
stemmer = SnowballStemmer('spanish')
stop_list = nltk.corpus.stopwords.words('spanish')
[stop_list.append(word) for word in manual_stopwords]
es_stop = set(stop_list)

for index in (range(0,len(prep_text))):
    prep_text[index] = str(prep_text[index]).replace('>', ' ')
    prep_text[index] = str(prep_text[index]).replace('<', ' ')
    prep_text[index] = str(prep_text[index]).replace('0', ' ')
    prep_text[index] = str(prep_text[index]).replace('1', ' ')
    prep_text[index] = str(prep_text[index]).replace('2', ' ')
    prep_text[index] = str(prep_text[index]).replace('3', ' ')
    prep_text[index] = str(prep_text[index]).replace('4', ' ')
    prep_text[index] = str(prep_text[index]).replace('5', ' ')
    prep_text[index] = str(prep_text[index]).replace('6', ' ')
    prep_text[index] = str(prep_text[index]).replace('7', ' ')
    prep_text[index] = str(prep_text[index]).replace('8', ' ')
    prep_text[index] = str(prep_text[index]).replace('9', ' ')
    prep_text[index] = str(prep_text[index]).replace('(OP)', ' ')
    prep_text[index] = str(prep_text[index]).replace('/', ' ')
    prep_text[index] = str(prep_text[index]).replace('\\', ' ')
    prep_text[index] = str(prep_text[index]).replace('(', ' ')
    prep_text[index] = str(prep_text[index]).replace(')', ' ')
    prep_text[index] = str(prep_text[index]).replace('*', ' ')
    prep_text[index] = str(prep_text[index]).replace('[', ' ')
    prep_text[index] = str(prep_text[index]).replace(']', ' ')
    prep_text[index] = str(prep_text[index]).replace(';', ' ')
    prep_text[index] = str(prep_text[index]).replace('^', ' ')
    prep_text[index] = str(prep_text[index]).replace('"', ' ')
    prep_text[index] = str(prep_text[index]).replace('!', ' ')
    prep_text[index] = str(prep_text[index]).replace('¡', ' ')
    prep_text[index] = str(prep_text[index]).replace('?', ' ')
    prep_text[index] = str(prep_text[index]).replace('¿', ' ')
    prep_text[index] = str(prep_text[index]).replace('@', ' ')
    prep_text[index] = str(prep_text[index]).replace('.', ' ')
    prep_text[index] = str(prep_text[index]).replace('#', ' ')
    prep_text[index] = str(prep_text[index]).replace('&', ' ')
    prep_text[index] = str(prep_text[index]).replace('%', ' ')
    prep_text[index] = str(prep_text[index]).replace('https:', ' ')
    prep_text[index] = str(prep_text[index]).replace('http:', ' ')
    prep_text[index] = str(prep_text[index]).replace(':', ' ')
    prep_text[index] = str(prep_text[index]).replace('`', ' ')  
    prep_text[index] = str(prep_text[index]).replace('-', ' ')
    prep_text[index] = str(prep_text[index]).replace('=', ' ')
    prep_text[index] = str(prep_text[index]).replace('\'', ' ')
    prep_text[index] = str(prep_text[index]).replace('xd', ' ')
    print(str(index)+ ': nº of lines replaced')

#tokenizer and lemmatizer 
def tokenize(text):
    lda_tokens = list()
    tokens = parser(text) 
    
    #### one possibility for adaptation is: [lda_tokens.append(token.lower_) for token in tokens if token.isalpha()] ########
    for token in tokens:
        if token.orth_.isspace():
            continue
        elif token.like_url:
            lda_tokens.append('URL')
        elif token.orth_.isdigit():
            lda_tokens.append('number')
        else:
            lda_tokens.append(token.lower_) 
    return lda_tokens
        
#a good idea could be a curseword stoplist
def prepare_text_for_lda(text):
    lemmas = tokenize(text)
    lemmas = [token for token in lemmas if token not in es_stop]
    lemmas = [stemmer.stem(token) for token in lemmas]
    return lemmas
    
#make the analyzable data strucutre
data['prep_text'] = prep_text
bodies = data['prep_text']
bodies = list(bodies)
text_data = list()
thread_len = list()

#tokenize the different bodies
counter = 1
for body in bodies:
    
    #try to tokenize
    try:
        text_data.append(prepare_text_for_lda(body))
        print(str(counter) + 'bodies tokenized and lemmatized')
    
    #handles empty nodes
    except TypeError:
        text_data.append([])
        print('empty text node')
        print(str(counter) + 'bodies tokenized and lemmatized')
    counter = counter + 1

#dictionary = corpora.Dictionary(text_data) #those couple of lines are going to be excluded in the final version 
#corpus = [dictionary.doc2bow(text) for text in text_data] #we just keep them as reference until we have made the corpora per topic
#pickle.dump(corpus, open('C:\\Users\\tobia\\Desktop\\Working Copies\\Projects\\phd thesis\\code\\graphs\\corpus.pkl', 'wb'))
#pickle.dump(text_data, open('C:\\Users\\tobia\\Desktop\\Working Copies\\Projects\\phd thesis\\code\\graphs\\lemmatisedtext.pkl', 'wb'))
#dictionary.save('C:\\Users\\tobia\\Desktop\\Working Copies\\Projects\\phd thesis\\code\\graphs\\dictionary.gensim')

#set in a character for missing text
new_text_data = list()
for list_ in text_data:
    if len(list_) == 0:
        new_text_data.append(['?'])
    elif list_ == ['nan']:
        new_text_data.append(['?'])
    elif len(list_) != 0:
        new_text_data.append(list_)

#here we can add all the stuff that we will need to calculate later such as message lenght, mean centralities per topic and so on
#operate on the text_data array rathern than the text data dataframe column
data['text_data'] = new_text_data
filepath = ""
data.to_csv('', index = True) #create network_data_prep.csv
print("data saved. FINISH")
