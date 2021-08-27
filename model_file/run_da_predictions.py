import os
import numpy as np
import time
from torch.utils.data import TensorDataset
from torch.autograd import Variable
import torch
import torch.nn as nn
import torch.nn.functional as ff
import argparse
from sklearn.preprocessing import MinMaxScaler

class encoder_RNN(nn.Module):
  """
  DA-RNN encoder module
  """
  def __init__(self, features, hidden_size, T, num_layers = 1, output_size = 1):
    super(encoder_RNN, self).__init__()

    self.lstm = nn.LSTM(features, hidden_size, num_layers)
    self.lstm.flatten_parameters()
    self.lin = nn.Linear(2 * hidden_size + T - 1, output_size)
    self.T = T
    self.features = features
    self.hidden = hidden_size

  def forward(self, x):
    """
    Computes forward pass
    """

    curr_hidden = Variable(x.data.new(1, x.size(0), self.hidden).zero_())
    curr_cell_state = Variable(x.data.new(1, x.size(0), self.hidden).zero_())
    x_atn = Variable(x.data.new(x.size(0), self.T - 1, self.features).zero_()) # input with attention weights applied
    x_enc = Variable(x.data.new(x.size(0), self.T - 1, self.hidden).zero_()) # encoded hidden state

    for t in range(self.T - 1): 
      e = torch.cat((curr_hidden.repeat(self.features, 1, 1).permute(1, 0, 2), 
        curr_cell_state.repeat(self.features, 1, 1).permute(1, 0, 2), 
        x.permute(0, 2, 1)), dim = 2) # find e^{k}_{t}
      a = ff.softmax(self.lin(e.view(-1, self.hidden * 2 + self.T - 1)).view(-1, self.features), dim = 1) # ensure attention weights sum to 1
      x_atn_tmp = torch.mul(a, x[:, t, :]) # driving series extracted according to attention weights
      _, state = self.lstm(x_atn_tmp.unsqueeze(0), (curr_hidden, curr_cell_state)) # hidden state update through LSTM unit
      curr_hidden = state[0]
      curr_cell_state = state[1]

      x_atn[:, t, :] = x_atn_tmp
      x_enc[:, t, :] = curr_hidden

    return x_atn, x_enc



class decoder_RNN(nn.Module):
  """
  DA-RNN decoder module
  """
  def __init__(self, dec_hidden, enc_hidden, T):
    super(decoder_RNN, self).__init__()

    self.lstm = nn.LSTM(1, dec_hidden)
    self.lstm.flatten_parameters()
    self.lin_in = nn.Sequential(nn.Linear(2 * dec_hidden + enc_hidden, enc_hidden),
                             nn.Tanh(), 
                             nn.Linear(enc_hidden, 1))
    self.lin_upd = nn.Linear(enc_hidden + 1, 1)
    self.lin_upd.weight.data.normal_()
    self.lin_out = nn.Linear(dec_hidden + enc_hidden, 1)
    self.T = T
    self.hidden = dec_hidden
    self.enc_hidden = enc_hidden


  def forward(self, x, y):
    """
    Computes forward pass
    """

    curr_hidden = Variable(x.data.new(1, x.size(0), self.hidden).zero_())
    curr_context = Variable(x.data.new(1, x.size(0), self.hidden).zero_())

    for t in range(self.T - 1): 
      l = torch.cat((curr_hidden.repeat(self.T - 1, 1, 1).permute(1, 0, 2), 
        curr_context.repeat(self.T - 1, 1, 1).permute(1, 0, 2), 
        x), dim = 2) # find l^{i}_{t}      
      b = ff.softmax(self.lin_in(l.view(-1, 2 * self.hidden + self.enc_hidden)).view(-1, self.T - 1), dim = 1) # attention weights that sum 1
      context_step = torch.bmm(b.unsqueeze(1), x)[:, 0, :]
      if t < self.T - 1: # not last
        y_new = self.lin_upd(torch.cat((context_step, y[:, t].unsqueeze(1)), dim = 1)) # target extracted with attention weights
        _, state = self.lstm(y_new.unsqueeze(0), (curr_hidden, curr_context)) # hidden state update through LSTM unit
        curr_hidden = state[0]
        curr_context = state[1]

    # last step isn't a hidden state update, it just produces the result
    prediction = self.lin_out(torch.cat((curr_hidden[0], context_step), dim = 1))

    return prediction


parser = argparse.ArgumentParser(description='Run the thing.')
parser.add_argument('time', metavar='N', type=int, nargs='+',
                   help='prediction lag, 1, 5, 10 or 21')
parser.add_argument('--cleaned', dest='cleaned', action='store_true')
parser.set_defaults(cleaned=False)

args = parser.parse_args()
N = args.time[0]

print("loading raw data for predicting " + str(N) + " days")

if (args.cleaned):
  print("using cleaned series")
  raw_file = "../input_files/input_vol" + str(N) + "d_clean.csv"
  enc_state_file = "enc_state_" + str(N) + "d_clean.pkl"
  dec_state_file = "dec_state_" + str(N) + "d_clean.pkl"
  answer_file = "da_results_" + str(N) + "d_clean.csv"
else:
  raw_file = "../input_files/input_vol" + str(N) + "d.csv"
  dec_state_file = "dec_state_" + str(N) + "d.pkl"
  enc_state_file = "enc_state_" + str(N) + "d.pkl"
  answer_file = "da_results_" + str(N) + "d.csv"


raw_data = np.loadtxt(open(raw_file, "rb"), delimiter = ",", skiprows = 1)

# Test: (nrows - 60) ~ end (last 60 days of sample)

test_start = len(raw_data) - 60

raw_x_test = raw_data[:, :-1]
raw_y_test = raw_data[:, -1]

print("Running predictions from index " + str(test_start) + " to end of file")

with open(answer_file, "w") as file:
  file.write("pred\n")
  for i in range(test_start, len(raw_data)):

    # Values for the last N days are needed 
    # to predict the current date, since the 
    # target series uses lagged values the 
    # real values until D-N aren't available 
    # for decoder input, meaning each prediction will
    # carry into it N predictions

    print("Running prediction for index " + str(i))

    j = N - 1
    curr_index = i - N 
    available_y = raw_y_test[:curr_index, ]

    while j > 0:
      x_test = raw_x_test[:curr_index, ]
      encoder = encoder_RNN(8, 32, 10).double()
      decoder = decoder_RNN(32, 32, 10).double()
      encoder.load_state_dict(torch.load(enc_state_file))
      decoder.load_state_dict(torch.load(dec_state_file))
      encoder = encoder.eval()
      decoder = decoder.eval()
      tsteps = int(x_test.shape[0] * 0.7)
      y = np.zeros(x_test.shape[0] - tsteps)

      k = 0
      while (k < len(y)):
        idxs = np.array(range(len(y)))[k:(k + 128)]
        x = np.zeros((len(idxs), 9, x_test.shape[1]))
        yh = np.zeros((len(idxs), 9))

        for l in range(len(idxs)):
          x[l, :, :] = x_test[range(idxs[l] + tsteps - 10, idxs[l] + tsteps - 1), :]
          yh[l, :] = available_y[range(idxs[l] + tsteps - 10, idxs[l] + tsteps - 1)]

        yh = Variable(torch.from_numpy(yh))
        _, encoded = encoder(Variable(torch.from_numpy(x)))
        y[k:(k + 128)] = decoder(encoded, yh).data.numpy()[:, 0]
        k += 128

      curr_index += 1      
      available_y = np.append(available_y, np.expand_dims(y[-1], axis = 1), axis=0)
      j -= 1


    x_test = raw_x_test[:(i + 1), ]
    y_test = available_y
    
    encoder = encoder_RNN(8, 32, 10).double()
    decoder = decoder_RNN(32, 32, 10).double()
    encoder.load_state_dict(torch.load(enc_state_file))
    decoder.load_state_dict(torch.load(dec_state_file))
    encoder = encoder.eval()
    decoder = decoder.eval()

    tsteps = int(x_test.shape[0] * 0.7)
    y = np.zeros(x_test.shape[0] - tsteps)

    k = 0

    while (k < len(y)):
      idxs = np.array(range(len(y)))[k:(k + 128)]
      x = np.zeros((len(idxs), 9, x_test.shape[1]))
      yh = np.zeros((len(idxs), 9))

      for j in range(len(idxs)):
        x[j, :, :] = x_test[range(idxs[j] + tsteps - 10, idxs[j] + tsteps - 1), :]
        yh[j, :] = y_test[range(idxs[j] + tsteps - 10, idxs[j] + tsteps - 1)]

      yh = Variable(torch.from_numpy(yh))
      _, encoded = encoder(Variable(torch.from_numpy(x)))
      y[k:(k + 128)] = decoder(encoded, yh).data.numpy()[:, 0]
      k += 128

    file.write("{}\n".format(y[-1]))


