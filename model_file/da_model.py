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



def train_model(encoder, decoder, train_x, train_y, T, features,
 enc_state_path = "encoder_state.pkl", dec_state_path = "decoder_state.pkl", epochs = 500):
  """
  Train a DA-RNN model
  """

  best_loss = float("inf")
  criterion = nn.SmoothL1Loss()
  optimizer_enc = torch.optim.Adam(encoder.parameters(), lr = 1e-2)
  optimizer_dec = torch.optim.Adam(decoder.parameters(), lr = 1e-2)
  tsteps = int(train_x.shape[0] * 0.7)

  for epoch in range(epochs):

    idxs = np.array(range(tsteps - T))

    idx = 0
    losses = []

    while (idx < tsteps):
      train_idxs = idxs[idx:(idx + 128)]
      tx = np.zeros((len(train_idxs), T - 1, features))
      ty = np.zeros((len(train_idxs), T - 1))

      for i in range(len(train_idxs)):
        tx[i, :, :] = train_x[train_idxs[i]:(train_idxs[i] + T - 1), :]
        ty[i, :] = train_y[train_idxs[i]:(train_idxs[i] + T - 1)]

      tx_tensor = Variable(torch.from_numpy(tx)).cuda()
      ty_tensor = Variable(torch.from_numpy(ty)).cuda()
      target_y = Variable(torch.from_numpy(train_y[train_idxs + T])).cuda()
      target_y = target_y.view(-1, 1)

      weighted, encoded = encoder(tx_tensor)
      pred = decoder(encoded, ty_tensor)
      loss = criterion(pred, target_y)
      losses.append(float(loss))

      optimizer_enc.zero_grad()
      optimizer_dec.zero_grad()
      loss.backward()
      optimizer_enc.step()
      optimizer_dec.step()
      idx += 128

    if (np.mean(losses) < best_loss):
      msg = "\ntrain_loss = {:.8f} | epoch = {:.1f}\n".format(np.mean(losses), float(epoch))
      torch.save(encoder.state_dict(), enc_state_path)
      torch.save(decoder.state_dict(), dec_state_path)
      best_loss = np.mean(losses)
      print(msg, end="")


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
else:
  raw_file = "../input_files/input_vol" + str(N) + "d.csv"
  dec_state_file = "dec_state_" + str(N) + "d.pkl"
  enc_state_file = "enc_state_" + str(N) + "d.pkl"


raw_data = np.loadtxt(open(raw_file, "rb"), delimiter = ",", skiprows = 1)

# Test: (nrows - 60) ~ end (last 60 days of sample)
# Train: 1 ~ (nrows - 60)  (the rest of sample)

test_start = len(raw_data) - 60 

raw_x = raw_data[:(test_start - 1), :-1]
raw_y = raw_data[:(test_start - 1), -1]

print(" Raw x shape")
print(" " + str(raw_x.shape))

print(" Raw y shape")
print(" " + str(raw_y.shape))

train_x = torch.from_numpy(raw_x.reshape(-1, 1, 8)).cuda()
train_y = torch.from_numpy(raw_y.reshape(-1, 1, 1)).cuda()

encoder = encoder_RNN(8, 32, 10).double().cuda()
decoder = decoder_RNN(32, 32, 10).double().cuda()
print("starting training ")
train_model(encoder, decoder, raw_x, raw_y, 10, 8, enc_state_file, dec_state_file)

del encoder
del decoder
torch.cuda.empty_cache()
