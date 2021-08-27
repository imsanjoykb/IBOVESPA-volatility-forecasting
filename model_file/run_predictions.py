import os
import numpy as np
import time
from torch.utils.data import TensorDataset
import torch
import torch.nn as nn
import argparse
from sklearn.preprocessing import MinMaxScaler


class LSTM(nn.Module):
  """
  Recurrent LSTM network
  """
  def __init__(self, features, hidden_size, num_layers = 2, output_size = 1):
    super(LSTM, self).__init__()

    self.lstm = nn.LSTM(features, hidden_size, num_layers)
    self.lin = nn.Linear(hidden_size, output_size)

  def forward(self, x):
    """
    Computes forward pass
    """
    x, states = self.lstm(x) 
    seq_len, batch, hidden = x.shape
    x = x.view(seq_len * batch, hidden)
    x = self.lin(x)
    x = x.view(seq_len, batch, -1)
    return x

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
  state_file = "lstm_state_" + str(N) + "d_clean"
  answer_file = "results_" + str(N) + "d_clean.csv"
else:
  raw_file = "../input_files/input_vol" + str(N) + "d.csv"
  state_file = "lstm_state_" + str(N) + "d"
  answer_file = "results_" + str(N) + "d.csv"


raw_data = np.loadtxt(open(raw_file, "rb"), delimiter = ",", skiprows = 1)

# Test: (nrows - 30) ~ end (last 30 days of sample)
# Validation: (nrows - 160) ~ (nrows - 30) (around 6 months of validation)
# Train: 1 ~ (nrows - 160)  (the rest of sample)

test_start = len(raw_data) - 60

raw_x_test = raw_data[:, :-1]

print("Running predictions from index " + str(test_start) + " to end of file")

with open(answer_file, "w") as file:
  file.write("pred\n")
  for i in range(test_start, len(raw_data)):
    x_test = raw_x_test[:(i + 1), ]
    test_x = torch.from_numpy(x_test.reshape(-1, 1, 8))
    model = LSTM(8, 16).double()
    model.load_state_dict(torch.load(state_file + "_2411.pkl"))
    model = model.eval()
    pred_test = model(test_x).view(-1).data.numpy()
    file.write("{}\n".format(pred_test[-1]))


