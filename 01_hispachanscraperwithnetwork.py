# -*- coding: utf-8 -*-
"""
Created on Thu Jul 11 10:13:10 2019

@author: Tobias
"""

import requests as rs 
from bs4 import BeautifulSoup
import networkx as nx
import time 

comment_network = nx.Graph() #setup of an empty graph for network analysis later on

urls = [
        'https://www.hispachan.org/ar/',
        #'https://www.hispachan.org/bo/',
        'https://www.hispachan.org/cc/',
        'https://www.hispachan.org/cl/',
        'https://www.hispachan.org/co/',
       #'https://www.hispachan.org/ec/',
        'https://www.hispachan.org/es/',
        'https://www.hispachan.org/mx/',
        'https://www.hispachan.org/pe/',
        'https://www.hispachan.org/uy/',
        'https://www.hispachan.org/ve/',
        'https://www.hispachan.org/hu/',
        'https://www.hispachan.org/k/',
        'https://www.hispachan.org/m/',
        'https://www.hispachan.org/pol/',
        'https://www.hispachan.org/q/',
        'https://www.hispachan.org/t/'
        ] 

new_urls = []

#localize the div class 'numeros' to find the additional pages of each thread
print("starting to collect urls")
for url in urls: 
    req = rs.get(url)
    html_content = req.text
    
    #make soup
    soup = BeautifulSoup(html_content, 'html.parser')    
    numeros = soup.find('div', class_ = 'numeros')
    
    #raise error and report if no page numbers are found
    try:
        links = numeros.find_all('a')
    except AttributeError:
        print("no numbers found in " + url)
        next
    
    #append the urls of the consecutive pages to the list of new urls
    for a in range(0,7):
        new_urls.append(links[a]['href'])

#append the links from the newly created url list to the original list 'urls'
for link in new_urls:
    urls.append(link)

#control programm
print("url collection finnished")

#controlling variables
counter = 1

#loop through the urls in order to get content
for url in urls:
    print("retrieving threads from" + url)
    req = rs.get(url)
    html_content = req.text
    soup = BeautifulSoup(html_content,'html.parser')
    
    #localize the different threads in the html file and loop through them
    for thread in soup.find_all('div', class_ = 'thread'):
        
        #controll progess
        print("appending network... thread no " + str(counter))
        counter = counter + 1
        
        #find thread including all of the comments and make soup
        comment_html = thread.find('a', class_ = 'reflink2')['href']
        comm_req = rs.get(comment_html)
        comment_html_content = comm_req.text
        comment_soup = BeautifulSoup(comment_html_content, 'html.parser')
       
        #add the central node of each thread to the graph including the textbody and the url of the community as node info, we also need to know whether a node is an origin node (central), and when the node was revrieved from the internet
        comment_network.add_node(comment_soup.find('span', class_ = 'reflink').a['href'],
                                 text = comment_soup.find('div', class_ = 'thread').blockquote.get_text(strip = True),
                                 community = url, 
                                 central = True,
                                 retrieved = time.localtime())
        
        #localize all comments in soup and loop through them
        for comment in comment_soup.find_all('td', class_ = 'reply'):
            
            #add nodes for each comment in the soup and include text body and community url for comparability 
            comment_network.add_node(comment.find('span', class_ = 'reflink').a['href'], 
                                     text = comment.div.blockquote.get_text(strip = True),
                                     community = url,
                                     central = False,
                                     retrieved = time.localtime())
    
            #add edge between comment and mentioned post/comment; if there is no mentioned one add the edge to the original comment
            try:
                
                #check whether href in comment redirects to a url within the same thread
                if(url[0:29] in comment.div.blockquote.a['href']):
                    comment_network.add_edge(comment.find('span', class_ = 'reflink').a['href'], comment.div.blockquote.a['href'])
                else:
                    comment_network.add_edge(comment.find('span', class_ = 'reflink').a['href'], comment_html)
                    print("avoiding reference to link out of thread. Redirecting Edge to OP")
            
            #if there is no href, a TypeError will be raised.
            except TypeError:
                comment_network.add_edge(comment.find('span', class_ = 'reflink').a['href'], comment_html)
                print("TypeError: don't worry. Redirecting Edge to OP")

#collect faulty nodes (missing values or attributes) from the network, iterating over all node and attribute combinations
counter = 1
good_nodes = 0
bad_nodes = 0
faulty_nodes = []

#start debugging
print("Checking for faulty nodes")
for (node, attr) in comment_network.nodes(data = True):
    
    #check whether all attributes (except the text attribute, this is normal on 4chan) are != None
    try:
        if((attr['central'] != None) & (attr['community'] != None)):
            print("node information complete")
            good_nodes = good_nodes + 1
        
        #this should never be the case. I leave it here just in case though
        else:
            faulty_nodes.append(node)
            print("collecting faulty node")
            bad_nodes = bad_nodes + 1
    
    #if an attribute doesn't exist for a node, this will raise a key error and handled by excluding node from the network
    except(KeyError):
        faulty_nodes.append(node)
        print("collecting faulty node")
        bad_nodes = bad_nodes + 1
    counter = counter + 1

#report on nodes
print(str(good_nodes) + " full nodes in the network")
print(str(bad_nodes) + " faulty nodes collected from the network")
print(str(len(comment_network.nodes())) + " nodes in the network at collection end")
print(str(len(comment_network.edges())) + " edges in the network at collection end")
print("collection of faulty nodes finished")
    
#remove faulty nodes 
comment_network.remove_nodes_from(faulty_nodes)
print(str(bad_nodes) + " faulty nodes erased from the network")
print(str(len(comment_network.nodes())) + " nodes in the network after removal of faulty nodes")
print(str(len(comment_network.edges())) + " edges in the network at collection end")

#save pickle to specified filepath
filepath = "" # create comment_network.gpickle
print("saving file to " + str(filepath))
nx.write_gpickle(comment_network, filepath)
print("exe finnished")
