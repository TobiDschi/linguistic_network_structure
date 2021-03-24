# -*- coding: utf-8 -*-
"""
Created on Sun Apr 19 23:49:33 2020

@author: tobia
"""

import networkx as nx
import pandas as pd

#import basic data structure from pickle
filepath = ''
print("importing network from" + filepath)
comment_network = nx.read_gpickle(filepath)

#create empty subgraph list
subgraphs = list()
single_nodes = list()
print("seperating subgraphs")

#loop through connected components to identify single threads and make a subgraph for each one
for component in nx.connected_components(comment_network):
    subgraph = comment_network.subgraph(component)
    
    #append subgraphs list only if the subgraph has more than one contribution
    if(subgraph.number_of_nodes() > 1):
        subgraphs.append(comment_network.subgraph(component))
    else:
         single_nodes.append(comment_network.subgraph(component))  

#report total number of subgraphs    
print("done! \n" + str(len(subgraphs)) + " subgraphs in the network")       

#prepare the arraylists
threads = list()
nodes = list()
text = list()
community = list()
central = list()
retrieved = list()
neighbors = list()
degree_centralities = list()
betweenness_centralities = list()
distance_from_central = list()
direct_neighbors = list()

#loop through the subgraphs and nodes to make the index arrays
print("constructing list arrays for construction of data index")
exeptional_threads = 1
for subgraph in subgraphs:
    
    #select central node
    for node, attr in subgraph.nodes(data = True):
        if attr['central'] == True:
            selected_node = node
            break
        else:
            pass
            
    #print central node as thread name 
    #sometimes a central node was surpressed due to unhandable content 
    #in that case make sure to avoid doubles
    if selected_node not in threads:
        for n in range(len(subgraph.nodes())):
            threads.append(selected_node)    
    
    #in case this happens, just append a node to the "threads" argument 
    else:
        for n in range(len(subgraph.nodes())):
            threads.append('exeptional thread no' + str(exeptional_threads))
        exeptional_threads = exeptional_threads + 1
        
    #print node names as nodes for index
    for node in subgraph.nodes():
        nodes.append(node)

counter = 1

#loop through the subgraphs and nodes to make the content for the dataframe
print("convert network structure into data frame")
for subgraph in subgraphs:
    
    #make a matrix of all the different paths that run through the subgraph
    distances = dict(nx.all_pairs_shortest_path_length(subgraph))
    
    #iterate over key value pairs for each subgrpah, reflecting the information on each node
    print("extracting info from subgraph no " + str(counter))
    for node, attr in subgraph.nodes(data = True):
        
        #changes can made be here to change the data contained in the data frame
        #calculate the distance for the node from the central node
        central_node = [n for n, a in subgraph.nodes(data = True) if a['central'] == True]
        try:
            distance_from_central.append(distances[node][central_node[0]])
        except IndexError:
            distance_from_central.append(float('nan'))
        
        #append all the other attributes of the data
        text.append(attr['text'])
        community.append(attr['community'])
        central.append(attr['central'])
        retrieved.append(attr['retrieved'])
        neighbors.append(len(list(nx.neighbors(subgraph, node))))
        direct_neighbors.append(list(nx.neighbors(subgraph,node)))
    counter = counter + 1


#loop through the subgraphs and nodes to calculate centralities and append the corresponding lists
counter = 1
for subgraph in subgraphs:
    print("calculating betweenness measures for subgraph no " + str(counter))
    
    #append the measures of degree centrality to the lists
    for key, value in nx.degree_centrality(subgraph).items():
        degree_centralities.append(value)
    
    #append measures of betweenness centrality to the lists
    for key, value in nx.betweenness_centrality(subgraph).items():
        betweenness_centralities.append(value)
    counter = counter + 1

#instantiate the subgraph variables
p_regular = list()
p_mean = list()
diameter = list()
graphtype = list()
incomplete_subgraph = list()
ids = list()
network_density = list()

#loop through the subgraphs to append p, p_regularity and diameter
counter = 1
for subgraph in subgraphs:
    print("measuring graph properties for subgraph nº" + str(counter))
    
    #instantiate variable to determine p
    ps = list()
    
    #loop through nodes to determine all the neighbors that they have
    for node in subgraph.nodes():
        
        #instantiate variable nn to collect the number of neighbors
        nn = list()
        
        #collect number of neighbors with the nbs generator object
        nbs = nx.neighbors(subgraph, node)
        for nb in nbs:
            nn.append(1)
            
        #then sum up the neighbors and append the ps list to uperate with
        ps.append(sum(nn))
        
    #determine p, and p regularity
    if all(n == ps[0] for n in ps):
        p_regular.append(True)
        p_mean.append(sum(ps)/len(ps))
    else:
        p_regular.append(False)
        p_mean.append(sum(ps)/len(ps))
        
    #determine the diameter using the longest shortest path of all the possible n pairs
    lengths_of_shortest_paths = list()
    
    #instantiate generator object as a dictionary
    all_shortest_paths = dict(nx.all_pairs_shortest_path_length(subgraph))
    
    #extract only values of values of dictionary
    for dict_ in all_shortest_paths.values():
        lengths = dict_.values()
        for length in lengths:
            lengths_of_shortest_paths.append(length)
    
    #take longest shortest paths to define the diameter of the network
    diameter.append(max(lengths_of_shortest_paths))
    
    #then calculate network density (as defined by edges / possible edges)
    number_of_edges = len(subgraph.edges())
    possible_edges = (len(subgraph.nodes()) * (len(subgraph.nodes())-1)  ) / 2
    network_density.append(number_of_edges / possible_edges)
    
    #update counter variable
    counter = counter + 1
    
print("measurements finished")

#loop through the subgraphs to append some of the properties that we need for our classification
counter = 1
exeptions = 1
for subgraph in subgraphs:
    print("classifying network type for subgraph nº" + str(counter))
    
    #measure the graphtype
    #first find the central node
    for node, attr in subgraph.nodes(data = True):
        if attr['central'] == True:
            center_node = node
            break
        
    #use the center_node as an identifyer later on:
    if center_node not in ids:
        ids.append(center_node)
    else: 
        ids.append('exeptional thread no' + str(exeptions))
        exeptions = exeptions + 1
        
    #instantiate helping variables for this task
    yes = list()
    no = list()
    
    #then classify the graph according to mathematical properties
    #first, if all the edges contain the central node, the network is "centralized"
    for edge in subgraph.edges(data = True):
        if center_node in edge:
            yes.append(1)
        else:
            no.append(1)
    
    #decide on this type:
    if sum(no) == 0:
        graphtype.append('centralized')
        incomplete_subgraph.append(False)
        counter = counter + 1
        continue
    
    #if it's not centralized go on with the algorithm
    else:
        pass

    #then try and see if it a linear network
    paths = list()
    pathlens = list()
    all_nodes = list()    
    
    #this is defined as a network in which the path from n0 to the most distant node contains all nodes of the subgraph
    
    for node in subgraph.nodes():
        all_nodes.append(node)
        
        #there is a NodeNotFound error if the subgraph doesn't have a center_node in which case, we have to mark this as a faulty graph
        try:
            path_gen = nx.all_simple_paths(subgraph, center_node, node)
            for path in path_gen:
                paths.append(path)
                pathlens.append(len(path))
                
        except:
            
            #append that this is incomplete and go back to the rest of the algorithm
            incomplete_subgraph.append(True)
            break
    
    #zip them together
    all_paths = pd.DataFrame(data = {"path" : paths,"pathlength" : pathlens})
    
    #make the decision based on the question of whether the longest path contains all nodes of the network
    try:
        longest_path = all_paths.loc[all_paths['pathlength'] == max(all_paths['pathlength'])]['path'].iloc[0]
        if set(longest_path) == set(all_nodes):
            
            #set value to linear network:
            graphtype.append('linear network')
            incomplete_subgraph.append(False)
            counter = counter + 1
            continue
        
        else:
            pass
    
    #if this doesn't work: pass because there should be a true in the incomplete_subgraph thing    
    except ValueError:
        graphtype.append('none') 
        counter = counter + 1
        continue
    
    #then try and see if it is a decentralized network
    #this is defined as a network in which there is exactly one path for each node to get to the center node         
    #instantiate helping variables
    number_of_paths = list()
    
    #then make decision
    #don't run algorithm for central node
    for node in subgraph.nodes():
        if node == center_node:
            continue
        
        #find possible paths add them together and see if there is any node that has more than one path
        else:
            possible_paths = list()
            paths = nx.all_simple_paths(subgraph, center_node, node)   
            
            #with the object installed find the number of paths and then sum them up
            for path in paths:
                possible_paths.append(1)
            number_of_paths.append(sum(possible_paths))
    
    #then if all the numbers in numbers of paths are equal to 1, classify as decentralized
    if all(path == 1 for path in number_of_paths) == True:
        graphtype.append('decentralized')
        incomplete_subgraph.append(False)
        counter = counter + 1
        continue
    
    #else go on with the algorithm
    else:
        pass

    #instantiate some variables
    number_of_components = list()
    
    #now we have to check whether this is about a true distributed system
    #the definition is now, that even if you take out a node, there must be a path from any node to any other node
    #this translates to: no matter which node you take away there should still be only one component to make up the graph
    for node in subgraph.nodes():
        
        #unfreeze the subgraph object and take away the node in question
        unfrozen_graph = nx.Graph(subgraph)
        unfrozen_graph.remove_node(node)
        
        #instantiate a counter of components
        components = list()
        
        #then check if we still have only one component
        for component in nx.connected_components(unfrozen_graph):
            components.append(1)
        
        #take all the components of the subgraph that exist
        #with this node removed and add them together
        number_of_components.append(sum(components))
        
    #then check if the number_of_components is 1 in each and every instance
    if all(component == 1 for component in number_of_components) == True:
        graphtype.append('real distributed')
        incomplete_subgraph.append(False)
        counter = counter + 1
        continue
        
    else:
        
        #we have to check if we can leave this like this or if it has to be more carefully defined. 
        #some experiments should be enough
        graphtype.append('non-real distributed')
        incomplete_subgraph.append(False)
        counter = counter + 1

print("classification finished, assembling data frame")
 
#make the index for the data frame
#tuples = list(zip(threads, nodes))
#index = pd.MultiIndex.from_tuples(tuples, names=['thread', 'node'])

#make a data frame from the data produced before
print("assembling data frame")
data = pd.DataFrame(data = {'id': nodes, 
                            'subgraph': threads, 
                            'text' : text,
                            'community' : community ,
                            'retrieved' : retrieved,
                            'degree_centrality': degree_centralities,
                            'betweenness_centrality' : betweenness_centralities,
                            'central' : central,
                            'neighbors': neighbors,
                            'distance_from_central' : distance_from_central,
                            'direct_neighbors' : direct_neighbors})

print("calculating graph centrality")

graph_centralities_neighbor = list()
graph_centralities = list()
center_equals_most_central = list()
center_equals_highest_degree = list()
id_ = list()

agg = data[['subgraph', 'id', 'degree_centrality', 'neighbors', 'central']].groupby('subgraph')

counter = 1
exeptions = 1

for identifier, info in agg:
    print('calculating graph centrality for graph nº ' + str(counter))
    
    #first find the central node
    for x, node in info.iterrows():
        if node['central'] == True:
            cent_node = node['subgraph']
            id_.append(cent_node)
            break
        else:
            pass
        
    #if a central node is found, there should be exactly as many elements in
    #the id_ list as we have iterations through the loop. we can use this to
    #check whether a central node has been found
    if len(id_) != counter:
        
        #add an exeption and update the exeption variable
        id_.append('exeptional thread no' + str(exeptions))
        exeptions = exeptions + 1
    
    #centralization as calculated by integral degree (number of neighbors in our case))
    most_neighbors_node = info['id'].loc[info['neighbors'] == max(info['neighbors'])]
    real_central_node = info['id'].loc[info['central'] == True]
    
    #check whether there is only one highest degree node, if there isn't it's necessarily a false statement
    if len(most_neighbors_node) != 1:
        center_equals_highest_degree.append(False)
        
    #check if there is a real central node and that it's not an exceptional thread
    elif len(real_central_node) != 1:
        center_equals_highest_degree.append(False)
      
    #or we just look up if the real_central_node (origin node) is actually the most_centralized node
    else:
        try:
            if most_neighbors_node.iloc[0] == real_central_node.iloc[0]:
                center_equals_highest_degree.append(True)
            else:
                center_equals_highest_degree.append(False)
        
        #when there is no central node in the network the above comparison doesn't work
        #because only identically labled Series object can be compared 
        except ValueError:
            center_equals_highest_degree.append(False)

    #then we go on to actually calculate the graph centrality according to the defined formula
    no_of_nodes = len(info)
    most_neighbors = max(info['neighbors'])
    substracts = list()
    
    #the following represents the formula to CD(G)
    for value in info['neighbors']:
        substracts.append(most_neighbors - value)
    
    try:
        graph_centralities_neighbor.append(sum(substracts) / ((no_of_nodes * no_of_nodes) - (no_of_nodes * 3) + 2))
    
    #in the case of two-node-networks, there is a zerodivision error due to the
    #quotient formula H = n^2 - 3n + 2
    except ZeroDivisionError:
        graph_centralities_neighbor.append(float('nan'))
    
    ### then calculate the other graph centrality measure
    
    #we define the node with the highest centralization as the most centralized node
    most_centralized_node = info['id'].loc[info['degree_centrality'] == max(info['degree_centrality'])]
    real_central_node = info['id'].loc[info['central'] == True]
    
    #sometimes this returns more than one most centralized node in this case 
    #we handle the the problem by saying that the center doesn't equate with the most central
    if len(most_centralized_node) != 1:
        center_equals_most_central.append(False)
        
    elif len(real_central_node) != 1:
         center_equals_most_central.append(False)
      
    #or we just look up if the real_central_node (origin node) is actually the most_centralized node
    else:
        try:
            if most_centralized_node.iloc[0] == real_central_node.iloc[0]:
                center_equals_most_central.append(True)
            else:
                center_equals_most_central.append(False)
        
        #when there is no central node in the network the above comparison doesn't work
        #because only identically labled Series object can be compared 
        except ValueError:
            center_equals_most_central.append(False)
            
    #then we go on to actually calculate the graph centrality according to the defined formula
    no_of_nodes = len(info)
    most_central_centrality = max(info['degree_centrality'])
    substracts = list()
    
    #the following represents the formula to CD(G)
    for value in info['degree_centrality']:
        substracts.append(most_central_centrality - value)
    
    try:
        graph_centralities.append(sum(substracts) / ((no_of_nodes * no_of_nodes) - (no_of_nodes * 3) + 2))
    
    #in the case of two-node-networks, there is a zerodivision error due to the
    #quotient formula H = n^2 - 3n + 2
    except ZeroDivisionError:
        graph_centralities.append(float('nan'))
        
    #update counter variable
    counter = counter + 1

print('assembling dataframe of centrality data')
#make the dataframe for centrality_data information
centrality_data = pd.DataFrame(data = {'subgraph': id_,
                                       'graph_centrality': graph_centralities,
                                       'center_most_central': center_equals_most_central,
                                       'integer_graph_centrality': graph_centralities_neighbor,
                                       'center_most_neighbors': center_equals_highest_degree})

print('assembling dataframe for thread information')
#make the dataframe for the thread information
thread_data = pd.DataFrame(data = {'subgraph' : ids,
                                   'graphtype': graphtype,
                                   'p-regular': p_regular,
                                   'p': p_mean,
                                   'diameter': diameter,
                                   'incomplete': incomplete_subgraph})

print("combining centralization and thread info")
centr_thread_data = thread_data.merge(centrality_data, on = 'subgraph')

print("combining centralization, thread and other data")
all_data = data.merge(centr_thread_data, on = 'subgraph', how = 'left')
print("all data assembled")

filepath = ""
all_data.to_csv(filepath, index = True) # create network_data.csv
print("data frame saved")

