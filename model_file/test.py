import os
import numpy as np
import argparse



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
else:
  raw_file = "../input_files/input_vol" + str(N) + "d.csv"


raw_data = np.loadtxt(open(raw_file, "rb"), delimiter = ",", skiprows = 1)

# Test: (nrows - 30) ~ end (last 30 days of sample)
# Validation: (nrows - 160) ~ (nrows - 30) (around 6 months of validation)
# Train: 1 ~ (nrows - 160)  (the rest of sample)

test_start = len(raw_data) - 30
validation_start = len(raw_data) - 160

print(test_start)
print(validation_start)
print(raw_data[test_start, :])
print(raw_data[validation_start, :])

raw_x = raw_data[:(validation_start - 1), :-1]
raw_y = raw_data[:(validation_start - 1), -1]

raw_x_valid = raw_data[validation_start:(test_start - 1), :-1]
raw_y_valid = raw_data[validation_start:(test_start - 1), -1]

print(" Raw x shape")
print(" " + str(raw_x.shape))

print(" Raw y shape")
print(" " + str(raw_y.shape))

print(" Raw x valid shape")
print(" " + str(raw_x_valid.shape))

print(" Raw y valid shape")
print(" " + str(raw_y_valid.shape))

