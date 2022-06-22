#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Feb 20 22:22:03 2020

@author: Ryan1
"""
#Code was writtne on MacOS. File write doesn't work on windows so has been #'d out

#Importing Beautiful to parse HTML
from bs4 import BeautifulSoup
#Import Requests to make http requests for urls provided
import requests
#Import re to use regular expressions to help cleanse the text
import re
#Import natural language toolkit to support with tokenization, lemmatizing etc
import nltk
#Punkt was downloaded from nltk to support with cleansing punctuation
nltk.download('punkt')
#sent tokenize for tokenizing sentences from a body of text, word tk for words
from nltk.tokenize import sent_tokenize, word_tokenize
#stopwords imported to support removal of common stopwords that have no meaning/use
from nltk.corpus import stopwords
nltk.download('stopwords')
#Lemmatizer was imported as lemmatization was conducted instead of stemming
from nltk.stem.wordnet import WordNetLemmatizer
#Wordnet was downloaded to support part of speech tagging in the lemmatization
from nltk.corpus import wordnet
nltk.download('wordnet')
#downloaded ap tagger to conduct the part of speech tags
nltk.download('averaged_perceptron_tagger')
#imported pos_tag which uses ap tagger
from nltk import pos_tag
#sklearn was brought in to perform tf-idf which is not currenlty supported by nltk
from sklearn.feature_extraction.text import TfidfVectorizer
#pandas was imported to provide a data frame for the tf-idf
import pandas as pd

#Websites to feed into the pipeline are added to this list
websites = ["http://www.multimediaeval.org/mediaeval2019/memorability/",
       "https://sites.google.com/view/siirh2020/"]

#the run_pipeline function triggers the whole pipeline by taking the websites list
#as its parameter, looping through each website to scrape, lemmatize, pre-process (clean)
#and collect all the text from each document into one list to put through the TF-IDF
def run_pipeline(websites):
    #TFIDF empty list created ready to collect all cleansed text(documents)
    TFIDF_ready = []
    count = 1
    #forloop loops through the websites list/parameter to apply all steps
    for url in websites:
        #output is the file generated from the get_output function
        #parsing through the webpage and returning
        #relevant text and stripping out unneeded HTML code which should avoid going
        #through the pipeline
        output = get_output(url)
        #writes the output from the parsing into a file on the working directory
        #output_filename = "HTML Parse: Website " + str(count)
        #with open('%s.txt' % output_filename, 'w') as f:
           #f.write(output)
           #f.flush()
           #f.close()
        
        #lemmatizing runs the lemmatization function using output from the HTML parsing
        lemmatizing = lemmatization(output)
        #writes the output from the lemmatization into a file on the working directory
        #lemm_filename = "Lemmatize: Website " + str(count)
        #file1 = open('%s.txt' % lemm_filename, 'w')
        #looping has to be done as the return of lemmatizing are strings inside a list
        #for item in lemmatizing:
            #file1.write("%s\n" % item)
        #file1.flush()
        #file1.close()
        
        #the pre-process takes the lemmatized body of text and applies different cleansing
        #functions such as removing whitespaces, punctuation, stop words and so forth.
        clean_text = pre_processing(lemmatizing)
        #writes the output from the pre-processing into a file on the working directory
        #pre_process_filename = "Preprocessing: Website " + str(count)
       # file2 = open('%s.txt' % pre_process_filename, 'w')
        #looping has to be done as the return of pre-processing are strings inside a list
       # for item in clean_text:
            #file2.write("%s\n" % item)
       # file2.flush()
       # file2.close()
        
        #This extends the empty list above with all the clean text from processing each
        #website through the pipeline
        TFIDF_ready.extend(clean_text)
        count += 1
    #TFIDF takes the TFIDF list once all websites have been processed and runs the function
    #to calculate the TFIDF and export it as a csv
    TFIDF(TFIDF_ready)
        
        
def get_output(url):
    #sends a get request to the url of the website that is specified
    request_webpage = requests.get(url)
    #Extracts the status code, informing user if request was successful (200) or whether
    #there was an error of some sort
    website_status = request_webpage.status_code
    if website_status == 200:
        print('Website Request: Sucessful')
    elif website_status != 200:
        print('Error: Check request')
    #requests the content of the webpage and brings in beautiful soup to parse the information
    html_page = request_webpage.content
    soup = BeautifulSoup(html_page, 'html.parser')
    #soup is used to find all the text from the webpage
    scraped_text = soup.find_all(text=True)
    
    #Here the html to be removed has been specified. I took a general approach to removing
    #tags to be suitable for different websites. Links have been removed to avoid noise
    #in the pipeline.
    output = ''
    removed_html = [
    	'a',
    	'link',
    	'header',
    	'html',
    	'meta',
    	'head', 
    	'input',
    	'script',
        'style',
        'title',
        'noscript',
        'body',
        'span',
        '[document]',
    ]
    #looping through all the scraped text, the parent name of the parse tree is checked
    #against the removed html list and if it's not the text is added to output.
    for text in scraped_text:
    	if text.parent.name not in removed_html:
    		output += '{} '.format(text)
    return output
#'--------------------------------------------------------------'
    
def lemmatization(output):
    #The lemmatization function takes the htmp parsed output as a parameter
    #A dictionary has been created with a mapping from the treebank pos tags to the
    #corresponding accepted wordnet tags that will be accepted by the lemmatizer
    lem_map = {
        'CC':wordnet.NOUN, 
        'CD':wordnet.NOUN,            
        'DT':wordnet.NOUN,                   
        'EX':wordnet.ADV,         
        'FW':wordnet.NOUN,          
        'IN':wordnet.ADV, 
        'JJ':wordnet.ADJ,           
        'JJR':wordnet.ADJ,    
        'JJS':wordnet.ADJ,          
        'LS':wordnet.NOUN,         
        'MD':wordnet.NOUN,          
        'NN':wordnet.NOUN,       
        'NNS':wordnet.NOUN,         
        'NNP':wordnet.NOUN,          
        'NNPS':wordnet.NOUN,
        'PDT':wordnet.ADJ,          
        'POS':wordnet.NOUN,            
        'PRP':wordnet.NOUN,  
        'PRP$':wordnet.NOUN,   
        'RB':wordnet.ADV,        
        'RBR':wordnet.ADV,       
        'RBS':wordnet.ADV,    
        'RP':wordnet.ADJ,
        'SYM':wordnet.NOUN,
        'TO':wordnet.NOUN,
        'UH':wordnet.NOUN,
        'VB':wordnet.VERB,
        'VBD':wordnet.VERB,
        'VBG':wordnet.VERB,
        'VBN':wordnet.VERB,
        'VBP':wordnet.VERB,
        'VBZ':wordnet.VERB,
        'WDT':wordnet.NOUN,
        'WP':wordnet.NOUN,
        'WP$':wordnet.NOUN,
        'WRB':wordnet.NOUN,
        '$':wordnet.NOUN,
        '#':wordnet.NOUN,
        '“':wordnet.NOUN,
        '”':wordnet.NOUN,
        '``':wordnet.NOUN,
        ' '' ':wordnet.NOUN,
        "''":wordnet.NOUN,
        '(':wordnet.NOUN,
        ')':wordnet.NOUN,
        ',':wordnet.NOUN,
        '.':wordnet.NOUN,
        ':':wordnet.NOUN
        }

    #tokenizes the output text into sentences which will feed into the lemmatizer
    sentence_token = sent_tokenize(output)
    lemm = WordNetLemmatizer()
    #list comprehension to create enough empty lists for the lemmatized words to append to
    lemm_words = [[] for _ in range(len(sentence_token))]
    count = 0
    #looping through each tokenized sentence to apply pos tags and lemmatize per sentence
    for sentence in sentence_token:
        #each sentence is tokenized into words and pos tagged
        word_token = word_tokenize(sentence)
        pos_tagger = pos_tag(word_token)
        #for loop feeds each word and pos tag per sentence to the lemmatizer
        #This is done as lemmatizing is highly dependent on context which is provided by
        #the sentence
        for i in range(len(pos_tagger)):
            lemmatized = lemm.lemmatize(pos_tagger[i][0], lem_map[pos_tagger[i][1]])
            lemm_words[count].append(lemmatized)
        count += 1
    #joins the lists of lists created with the newly lemmatized words and transforms into
    #a format that is more suitable to pre-processing
    lemm_transform = ' '.join(map(lambda a: ' '.join(a), lemm_words))
    lemmatized_text = sent_tokenize(lemm_transform)
    return(lemmatized_text)

#-------------------------------------------------------------------

def pre_processing(lemmatized_text):
    #pre-processing takes the lemmatized text and applies different techniques to help
    #clear the noise from the text
    
    #This removes punctuation from the lemmatized text
    remove_punct = list(filter(lambda punct: nltk.tokenize.punkt.PunktToken(punct).is_non_punct, (lemmatized_text)))
    #This changes the case to lower as capitalised words and lower case will be seen
    #as different words
    lowercase = [sentence.lower() for sentence in remove_punct]
    #this removes any brackets
    remove_brack = [re.sub(r'[[\](){}]', '', bracket) for bracket in lowercase]
    #This removes any special characters and numbers that might get counted as a term
    remove_special_numchar = [re.sub(r'[^a-zA-z\s]', '', numchar) for numchar in remove_brack]
    #This removes extra whitespaces
    remove_wspace = [" ".join(wspace.split()) for wspace in remove_special_numchar]
    #This compares a list of identified English Stopwords and removes them from the text
    stop_words = set(stopwords.words('English'))
    filtered_stop_w = [stop_w for stop_w in remove_wspace if stop_w not in stop_words]
    #Text is now cleaned/processed
    clean_text = filtered_stop_w
    return(clean_text)
    
#-------------------------------------------------------------------   
def TFIDF(TFIDF_ready):
    #TFIDF makes use of all the websites passed through the pipeline, using tokenized
    #sentences in place of documents to help calculate weights for the idf
    tfidf_vect = TfidfVectorizer()
    got_tfidf = tfidf_vect.fit_transform(TFIDF_ready)
    #Pandas has been used to apply the tfidf into a dataframe. More useful visually and
    #to extract information
    tfidf = pd.DataFrame(got_tfidf.toarray())
    #Gets the column names which here will be a column per word.
    tfidf.columns = tfidf_vect.get_feature_names()
    print(tfidf)
    #tfidf is then exported to CSV
    tfidf.to_csv('TFIDF_Matrix.csv')
    

#-------------------------------------------------------------------    


