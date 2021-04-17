# kuzu.py
# ZZEN9444, CSE, UNSW

from __future__ import print_function
import torch
import torch.nn as nn
import torch.nn.functional as F

class NetLin(nn.Module):
    # linear function followed by log_softmax
    def __init__(self):
        super(NetLin, self).__init__()
        self.linear = nn.Linear(28 * 28,10) #28x28 inputs, 10 outputs

    def forward(self, x):
        x = x.view(-1, 28 * 28)
        output = self.linear(x) #linear
        output = F.log_softmax(output, dim=1) #logsoftmax output 
        return output

class NetFull(nn.Module):
    # two fully connected tanh layers followed by log softmax
    def __init__(self):
        super(NetFull, self).__init__()
        self.in_to_hid = nn.Linear(28 * 28,600)  # 28x28 inputs to hiddens
        self.hid_to_out = nn.Linear(600, 10) # hiddens to 10 outputs

    def forward(self, x):
        x = x.view(-1, 28 * 28)
        hid_sum = self.in_to_hid(x)
        hidden = torch.tanh(hid_sum) # tanh hidden 1st layer
        out_sum = self.hid_to_out(hidden)
        output = F.log_softmax(out_sum, dim=1) # logsoftmax output 2nd layer
        return output

class NetConv(nn.Module):
    # two convolutional layers and one fully connected layer,
    # all using relu, followed by log_softmax
    def __init__(self):
        super(NetConv, self).__init__()
        self.conv1 = nn.Conv2d(1, 80, 20, padding=3)  # 1st convolution layer (1 input, 80 output, 20 kernal, 3 padding)
        self.conv2 = nn.Conv2d(80, 20, 5, padding=3)  # 2nd convolution layer (80 input, 20 output, 5 kernal, 3 padding)
        self.fc1   = nn.Linear(5780, 159)    # 3rd fully connected layer (5780 input, 159 output)
        self.fc2   = nn.Linear(159, 10)      # 4th output layer (159 input, 10 output)

    def forward(self, x):
        out = F.relu(self.conv1(x))     # relu activation for convolation 1st layer
        out = F.relu(self.conv2(out))   # relu activation for convolation 2nd layer
        out = out.view(out.size(0), -1)
        out = F.relu(self.fc1(out))     # relu activation for fully connected 3rd layer
        out = F.log_softmax(self.fc2(out), dim=1) # logsoftmax output 4th layer
        return out
