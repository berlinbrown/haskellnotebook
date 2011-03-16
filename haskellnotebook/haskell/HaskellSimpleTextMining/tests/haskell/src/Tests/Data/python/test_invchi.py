"""
 Test invchi
"""

import math

example_data = [
    (4.3, 6),
    (2.2, 60),
    (60, 2.2),
    (0.3, 4),
    (32.123, 20),
    (12.4, 5),
    (0.04, 3),
    (0, 0),
    (1, 1)
]

def invchi(chi, df):
    m = chi / 2.0
    sum = term = math.exp(-m)
    for i in range(1, df//2):
        term *= m / i
        sum += term
    return min(sum, 1.0)

if __name__  == '__main__':
    print "Running InvChi Test"
    for chi, df in example_data:
        print "[ chi=%s df=%s ] res=%s" % (chi, df, invchi(chi, df))
    print "Done"
