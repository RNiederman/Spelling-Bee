import re
import pandas as pd
from urllib.request import urlopen


omit = 'ear-o'
include = 'i'
answer = 'SP??L'

o = omit.upper()
o = re.sub("[^A-Z]", "", o)

i = include.upper()
i = re.sub("[^A-Z]", "", i)

a = answer.upper()
a = re.sub("[^A-Z]", "?", a)

word_url = 'https://norvig.com/ngrams/enable1.txt'
# word_url = 'http://www-personal.umich.edu/~jlahcer/wordlist'
# word_url = 'http://www.mieliestronk.com/corncob_caps.txt'

h = urlopen(word_url)
w = h.read().decode('utf-8').upper().split()
w = list(filter(lambda word: len(word) == 5, w))

if len(o) > 0:
    patt = ".*[" + o + "].*"
    r = re.compile(patt)
    omit_list = set(filter(r.match, w))
    w = set(w).difference(set(omit_list))

if len(i) > 0:
    patt = ".*[" + i + "].*"
    r = re.compile(patt)
    w = set(filter(r.match, w))

if a != "?????":
    patt = a.replace("?", "[A-Z]{1}")
    r = re.compile(patt)
    w = list(filter(r.match, w))


def scrabble_score(word):
    d = {"A": 1, "B": 3, "C": 3,  "D": 2, "E": 1, "F": 4, "G": 2,
         "H": 4, "I": 1, "J": 8,  "K": 5, "L": 1, "M": 3, "N": 1,
         "O": 1, "P": 3, "Q": 10, "R": 1, "S": 1, "T": 1, "U": 1,
         "V": 4, "W": 4,  "X": 8, "Y": 4, "Z": 10}

    s = 0
    for letter in word.upper():
        s += d[letter]
    return s


scrab = list(map(scrabble_score, w))
df_raw = {'Word': w, 'Score': scrab}
wordle = pd.DataFrame(df_raw)
wordle = wordle.sort_values(['Score', 'Word'], ascending=(True, False))
print(wordle.to_string())
