__author__ = 'ccfinch'

########
#
# Q 1
#
#######
from math import pow
def triangular_matrix_space_calculator(N):
  # First pass: every possible items once (10000000)
  # Second pass: every possible frequent pairs (N square divided by 2)
  return (10000000 + (pow(N, 2) * (pow(N, 2) - 1)) / 2) * 4

def triple_method_space_calculator(M):
  # First pass: every possible items once (10000000)
  # Second pass: 3 times the number of pairs (M because we count only pairs containing a frequent item)
  return (10000000 + (3 * M)) * 4

sets_to_test = [
  {'n': 60000, 'm': 200000000, 's': 7200000000 },
  {'n': 10000, 'm':  50000000, 's':  600000000 },
  {'n': 20000, 'm':  60000000, 's': 1000000000 },
  {'n': 10000, 'm':  40000000, 's':  200000000 },
  ]

for index, set_to_test in enumerate(sets_to_test):
  triangular = triangular_matrix_space_calculator(set_to_test['n'])
  triple = triple_method_space_calculator(set_to_test['m'])

  # We keep only the best method in terms of memory
  best = min(triangular, triple)

  # We accept 10% margin
  if set_to_test['s'] * 0.9 <= best <= set_to_test['s'] * 1.1:
      print("{0}: N={1}; M={2}; S={3}".format(index + 1, set_to_test['n'], set_to_test['m'], set_to_test['s']))

########
#
# Q 2
#
#######



def generate_pairs(items):
    d = {}
    for i in items:
        for j in items:
            if i != j and items.index(j) > items.index(i):
                d[i,j] = d.get((i,j),0)
    return d

def generate_dict(items):
    d = {}
    for a in items:
        d[a] = 0
    return(d)

def count_items(d, basket):
    for l in basket:
        for i in l:
            d[i] += 1
    return(d)

def double_count(d, items):
  for (x,y) in d.keys():
    for l in items:
      if x in l and y in l:
        d[(x,y)] += 1
  return(d)


items = ['B','C','P','M','J']

b_l = [
    ['B','P'],
    ['C','M'],
    ['B','C','J'],
    ['P','M'],
    ['B','C','M'],
    ['M','J'],
    ['P','J'],
    ['B','C','M','J']
]


pairs = generate_pairs(items)
singels = generate_dict(items)

singels = count_items(singels, b_l)


pp = double_count(pairs, b_l)

print(pp)

######
#
# Q 3
#
######


