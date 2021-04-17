#!/usr/bin/env python3
"""
student.py

UNSW ZZEN9444 Neural Networks and Deep Learning

You may modify this file however you wish, including creating additional
variables, functions, classes, etc., so long as your code runs with the
a3main.py file unmodified, and you are only using the approved packages.

You have been given some default values for the variables stopWords,
wordVectors, trainValSplit, batchSize, epochs, and optimiser, as well as
a basic tokenise function.  You are encouraged to modify these to improve
the performance of your model.

The variable device may be used to refer to the CPU/GPU being used by PyTorch.
You may change this variable in the config.py file.

You may only use GloVe 6B word vectors as found in the torchtext package.
"""

import torch
import torch.nn as tnn
import torch.nn.functional as F
import torch.optim as toptim
from torchtext.vocab import GloVe

################################################################################
##### The following determines the processing of input data (review text) ######
################################################################################

def tokenise(sample):
    """
    Called before any processing of the text has occurred.
    """
    punct_list = '!"#$%&\'()*+,-./:;<=>?@[\\]^_`{|}~0123456789' # list of punctuation/numbers to remove
    processed = " ".join("".join([" " if char in punct_list else char for char in sample]).split()).split() # split into words (tokens) and removes punctuation/numbers
    return processed

def preprocessing(sample):
    """
    Called after tokenising but before numericalising.
    """
    sample = [j.lower() for j in sample]                     # lowercase words 
    sample = [i for i in sample if len(i) > 1]               # only keep words with more than 1 character
    return sample

def postprocessing(batch, vocab):
    """
    Called after numericalising but before vectorising.
    """
    return batch

stopWords = {'i', 'me', 'my', 'myself', 'we', 'our', 'ours', 'ourselves', 'you', 
            "you're", "you've", "you'll", "you'd", 'your', 'yours', 'yourself', 'yourselves', 
            'he', 'him', 'his', 'himself', 'she', "she's", 'her', 'hers', 'herself', 'it', 
            "it's", 'its', 'itself', 'they', 'them', 'their', 'theirs', 'themselves', 'what', 
            'which', 'who', 'whom', 'this', 'that', "that'll", 'these', 'those', 'am', 'is', 
            'are', 'was', 'were', 'be', 'been', 'being', 'have', 'has', 'had', 'having', 'do', 
            'does', 'did', 'doing', 'a', 'an', 'the', 'and', 'but', 'if', 'or', 'because', 'as', 
            'until', 'while', 'of', 'at', 'by', 'for', 'with', 'about', 'against', 'between', 
            'into', 'through', 'during', 'before', 'after', 'to', 'from', 
            'in', 'out', 'on', 'off', 'again', 'further', 'then', 'once', 
            'here', 'there', 'when', 'where', 'why', 'how', 'all', 'any', 'both', 'each', 
            'other', 'some', 'such', 'nor', 'only', 'own', 'same', 
            'so', 'than', 'too', 'very', 's', 't', 'can', 'will', 'just', 
            "should've", 'now', 'd', 'll', 'm', 'o', 're', 've', 'y', 'ain', 'aren', "aren't", 
            'doesn', "doesn't", 'hasn', 
            "hasn't", 'haven', "haven't", 'isn', "isn't", 'ma', 'mightn', "mightn't", 'mustn', 
            "mustn't", 'needn', "needn't", 'shan', "shan't"}
# The above list of NLTK stopwords was generated using the folloiwng code:
# import nltk
# nltk.download()
# from nltk.corpus import stopwords
# stop_word_list = stopwords.words("english")
# print(stop_word_list)
# removed some words from this list such as: "no", "not", "won", "won't", 'don', "don't", 'couldn', "couldn't",
# 'didn', "didn't", 'wasn', "wasn't",'weren', "weren't", 'above', 'below', 'up', 'down', 'over',
#  'under', 'should','shouldn', "shouldn't", 'few', 'more', 'most', 'wouldn', "wouldn't", 'hadn', "hadn't"

wordVectors = GloVe(name='6B', dim=100) # Changed from dim=50

################################################################################
####### The following determines the processing of label data (ratings) ########
################################################################################

def convertNetOutput(ratingOutput, categoryOutput):
    """
    Your model will be assessed on the predictions it makes, which must be in
    the same format as the dataset ratings and business categories.  The
    predictions must be of type LongTensor, taking the values 0 or 1 for the
    rating, and 0, 1, 2, 3, or 4 for the business category.  If your network
    outputs a different representation convert the output here.
    """
    ratingOutput = torch.round(torch.sigmoid(ratingOutput)).long()  # Apply sigmoid activation before long conversion
    categoryOutput = torch.argmax(categoryOutput, dim=1)            # Apply argmax activation before long conversion
    return ratingOutput, categoryOutput

################################################################################
###################### The following determines the model ######################
################################################################################

class network(tnn.Module):
    """
    Class for creating the neural network.  The input to your network will be a
    batch of reviews (in word vector form).  As reviews will have different
    numbers of words in them, padding has been added to the end of the reviews
    so we can form a batch of reviews of equal length.  Your forward method
    should return an output for both the rating and the business category.
    """
    def __init__(self):
        super(network, self).__init__()
        # Bidirectional LSTM Layer
        self.input_to_lstm = tnn.LSTM(100, 250, batch_first=True, num_layers=2, bidirectional=True) 
        self.lstm_dropout = tnn.Dropout(p=0.15)        # 15% dropout rate
        # First FC Layer
        self.lstm_to_fc1_r = tnn.Linear(250, 64)        
        self.lstm_to_fc1_c = tnn.Linear(250, 128)        
        # Second FC Layer
        self.fc1_to_fc2_r = tnn.Linear(64, 1)       # 1 output for binary classification
        self.fc1_to_fc2_c = tnn.Linear(128, 5)      # 5 outputs  for five business categories

    def forward(self, input, length):
        # Bidirectional LSTM - Shared layer
        x = tnn.utils.rnn.pack_padded_sequence(input, length, batch_first=True, enforce_sorted=False)
        _, (x, _) = self.input_to_lstm(x)                  
        x = self.lstm_dropout(x)                   
        # Ratings - two FC layers
        x_r = F.relu(self.lstm_to_fc1_r(x[-2]))         
        ratingOutput = self.fc1_to_fc2_r(x_r).squeeze(0).squeeze(-1)   
        # Category - two FC layers
        x_c = F.relu(self.lstm_to_fc1_c(x[-2]))         
        categoryOutput = self.fc1_to_fc2_c(x_c).squeeze(0).squeeze(-1)
        # Outputs for both models
        return ratingOutput, categoryOutput

class loss(tnn.Module):
    """
    Class for creating the loss function.  The labels and outputs from your
    network will be passed to the forward method during training.
    """
    def __init__(self):
        super(loss, self).__init__()
        self.loss_rating = tnn.BCEWithLogitsLoss()     # better for binary categories
        self.loss_category = tnn.CrossEntropyLoss()    # better for multiple categories

    def forward(self, ratingOutput, categoryOutput, ratingTarget, categoryTarget):
        ratingL = self.loss_rating(ratingOutput, ratingTarget.float())  
        categoryL = self.loss_category(categoryOutput, categoryTarget) 
        total_loss = 0.3*ratingL + 0.7*categoryL       # Weighted total loss is used since the model tends to converge quicker towards ratings (as it is binary)
        return total_loss

net = network()
lossFunc = loss()

################################################################################
################## The following determines training options ###################
################################################################################

trainValSplit = 0.8  # 80/20 training/validation data split
batchSize = 128      # changed from 32 to 128
epochs = 7           # changed from 10
optimiser = toptim.Adam(net.parameters(), lr=0.01) # Replaced SGD with Adam
