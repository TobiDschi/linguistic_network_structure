# -*- coding: utf-8 -*-
"""
Created on Mon Apr 13 11:31:53 2020

@author: tobia
"""
import networkx as nx

#import network
filepath = ''
print("importing network from" + filepath)
comment_network = nx.read_gpickle(filepath)

#define function to be applied in different shapes
def surpress_problematic_nodes(problem = None, location = None, G = comment_network): #parameters require strings: what to look for in a node and where to look for it. example (surpress_problematic_nodes(problem = "$$", location = 'text'))
    
    #detect problematic content 
    counter = 1
    good_nodes = 0
    bad_nodes = 0
    problematic_nodes = []
    
    #start debugging
    print("Checking for problematic nodes")
    for (node, attr) in G.nodes(data = True):
        
        #check whether text attribute contains $ signs and surpress them from the network
        if(problem in attr[location]):
            print("problematic node found")
            problematic_nodes.append(node)
            bad_nodes = bad_nodes + 1
        
        else:
            print("node looks fine")
            good_nodes = good_nodes + 1
    
        counter = counter + 1
    
    #report on nodes
    print(str(good_nodes) + "  nodes in the network")
    print(str(bad_nodes) + " problematic nodes collected from the network")
    print(str(len(G.nodes())) + " nodes in the network at collection end")
    print(str(len(G.edges())) + " edges in the network at collection end")
    print("collection of faulty nodes finished")

    #remove problematic nodes 
    G.remove_nodes_from(problematic_nodes)
    print(str(bad_nodes) + " faulty nodes erased from the network")
    print(str(len(G.nodes())) + " nodes in the network after removal of problematic nodes")
    print(str(len(G.edges())) + " edges in the network after removal of problematic nodes")

#execute funtions according to the problems that have risen
surpress_problematic_nodes(problem = "$$", location = 'text')

#save pickle to specified filepath
filepath = "" #create comment_network.gpickle
print("saving file to " + str(filepath))
nx.write_gpickle(comment_network, filepath)
print("exe finnished")
