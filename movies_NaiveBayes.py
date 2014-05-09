import argparse
import re
import os
import csv
from operator import itemgetter
import random
import math


# Stop word list
stopWords = ['a', 'able', 'about', 'across', 'after', 'all', 'almost', 'also',
			 'am', 'among', 'an', 'and', 'any', 'are', 'as', 'at', 'be',
			 'because', 'been', 'but', 'by', 'can', 'cannot', 'could', 'dear',
			 'did', 'do', 'does', 'either', 'else', 'ever', 'every', 'for',
			 'from', 'get', 'got', 'had', 'has', 'have', 'he', 'her', 'hers',
			 'him', 'his', 'how', 'however', 'i', 'if', 'in', 'into', 'is',
			 'it', 'its', 'just', 'least', 'let', 'like', 'likely', 'may',
			 'me', 'might', 'most', 'must', 'my', 'neither', 'no', 'nor',
			 'not', 'of', 'off', 'often', 'on', 'only', 'or', 'other', 'our',
			 'own', 'rather', 'said', 'say', 'says', 'she', 'should', 'since',
			 'so', 'some', 'than', 'that', 'the', 'their', 'them', 'then',
			 'there', 'these', 'they', 'this', 'tis', 'to', 'too', 'twas', 'us',
			 've', 'wants', 'was', 'we', 'were', 'what', 'when', 'where', 'which',
			 'while', 'who', 'whom', 'why', 'will', 'with', 'would', 'yet',
			 'you', 'your']


def getFileContent(filename):
    """
    retrieve file content
    """
    input_file = open(filename, 'r')
    text = input_file.read()
    input_file.close()
    return text


def parseArgument():
    """
    Code for parsing arguments
    """
    parser = argparse.ArgumentParser(description='Parsing a file.')
    parser.add_argument('-d', nargs=1, required=True)
    args = vars(parser.parse_args())
    return args


def createTrainingDict(sampleDict, trainingDict):
    """
    Given sample dictionary, takes training filenames and creates dictionary 
    that maps words to their probabilities
    """
    for item in ('pos', 'neg'):
        for file in sampleDict[item]['train']:
            text = getFileContent(file)
            words = text.split()
            for word in words:
                if word not in trainingDict[item]:
                    trainingDict[item][word] = 1
                else:
                    trainingDict[item][word] += 1
    
    pos_V = len(trainingDict['pos'])
    neg_V = len(trainingDict['neg'])
    pos_count = 0
    neg_count = 0

    for key, value in trainingDict['pos'].iteritems():
        pos_count += value
    for key, value in trainingDict['neg'].iteritems():
        neg_count += value
    
    # Calculate probabilities
    for key, value in trainingDict['pos'].iteritems():
        pwc = (value + 1) / (pos_count + pos_V + float(1))
        trainingDict['pos'][key] = pwc
        
    for key, value in trainingDict['neg'].iteritems():
        pwc = (value + 1) / (neg_count + pos_V + float(1))
        trainingDict['neg'][key] = pwc
        
    return trainingDict


def test(sampleDict, trainingDict):
    """
    Given sample dictionary and training data, tests each document to
    predict its class
    """
    
    testDict = {'pos': [], 'neg': []}
    
    for item in ('pos', 'neg'):
        for file in sampleDict[item]['test']:
            reviewDict = {}
            pdc = {'pos': math.log(1.0/2), 'neg': math.log(1.0/2)}
            
            text = getFileContent(file)
            words = text.split()
            for word in words:
                if word not in stopWords and word.isalpha() is True:
                    if word not in reviewDict:
                        reviewDict[word] = 1
                    else:
                        reviewDict[word] += 1
            
            for word in reviewDict:
                if word in trainingDict['pos']:
                    pdc['pos'] += (math.log(trainingDict['pos'][word]) * reviewDict[word])
                else:
                    pdc['pos'] += (math.log(trainingDict['neg']['unk']) * reviewDict[word])
            
            for word in reviewDict:
                if word in trainingDict['neg']:
                    pdc['neg'] += (math.log(trainingDict['neg'][word]) * reviewDict[word])
                else:
                    pdc['neg'] += (math.log(trainingDict['neg']['unk']) * reviewDict[word])
                        
            if pdc['pos'] > pdc['neg']:
                testDict[item].append('pos')
            elif pdc['neg'] > pdc['pos']:
                testDict[item].append('neg')
    
    return testDict
    

def iteration(directory):
    training_probs = {'pos': {'unk': 0}, 'neg': {'unk': 0}}
    filename_dict = {'pos': [], 'neg': []}
    samples = {'pos': {}, 'neg': {}}
    
    for item in ('pos', 'neg'):
        filepath_list = []
        dir = os.path.join(directory, item)
        for thing in os.listdir(dir):
            filepath = os.path.join(dir, thing)
            filepath_list.append(filepath)
        filename_dict[item] = filepath_list
    
    for item in ('pos', 'neg'):
        sample = random.sample(filename_dict[item], 667)
        unsampled = []
        
        samples[item]['train'] = sample
        for file in filename_dict[item]:
            if file not in sample:
                unsampled.append(file)
        samples[item]['test'] = unsampled
    
    trainDict = createTrainingDict(samples, training_probs)
    testDict = test(samples, trainDict)
    
    pos_correct = 0.0
    neg_correct = 0.0
        
    for i in testDict['pos']:
        if i == 'pos':
            pos_correct += 1
    for i in testDict['neg']:
        if i == 'neg':
            neg_correct += 1
            
    postest = len(samples['pos']['test'])
    negtest = len(samples['neg']['test'])
    accuracy = (100 * (pos_correct + neg_correct) / (postest + negtest))
    
    print "num_pos_test_docs:", postest
    print "num_pos_training_docs:", len(samples['pos']['train'])
    print "num_pos_correct_docs:", pos_correct
    
    print "num_neg_test_docs:", negtest
    print "num_neg_training_docs:", len(samples['neg']['train'])
    print "num_neg_correct_docs:", neg_correct
    
    print "accuracy:", accuracy
    print "\n"
    
    return accuracy

def main():
    args = parseArgument()
    directory = args['d'][0]
    acc_count = 0
    for i in range(1, 4):
        print "Iteration", i
        acc_count += iteration(directory)
    print "avg_accuracy:", (acc_count / 3.0)

main()
