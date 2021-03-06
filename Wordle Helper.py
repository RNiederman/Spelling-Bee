import re
import string
import pandas as pd
import sys
from urllib.request import urlopen

###############################################################################
omit = ''  # These are the Black Letters
include = ''  # These are the Yellow Letters
answer = ' ? ? ? ? ? '  # These are the Green Letters

rws = 40  # The number of rows to display
u = 'https://norvig.com/ngrams/enable1.txt'
###############################################################################

word_length = 5
a = ''.join(answer.split())
a = a[:word_length].upper()
a = re.sub("[^A-Z]", "?", a)
if len(a) != word_length:
    sys.exit("Need " + str(word_length) +
             " Letters in the Answer Key")

o = omit.upper()
o = re.sub("[^A-Z]", "", o)

i = include.upper()
i = re.sub("[^A-Z]", "", i)

f = urlopen(u)
w = f.read().decode('utf-8').upper().split()
w = list(map(lambda word: word.upper(), w))
w = list(map(lambda word: word.translate(
    str.maketrans('', '', string.punctuation)), w))
w = list(filter(lambda word: len(word) == word_length, w))

for lttr in o:
    w = [word for word in w if lttr not in word]
    # w = list(filter(lambda wordset: lttr not in wordset, w))

for lttr in i:
    w = [word for word in w if lttr in word]
    # w = list(filter(lambda wordset: lttr in wordset, w))

if a != '?????':
    patt = a.replace("?", "[A-Z]{1}")
    r = re.compile(patt)
    w = list(filter(r.match, w))


def unique_letters2(word):
    x = set(word.upper())
    y = set(a)
    z = set(i)
    q = x.difference(y).difference(z)
    return len(q)


def vowel_count(word):
    vwels = "AEIOU"
    st = set(word.upper())
    vc = 0
    for v in vwels:
        vc += v in st
    return vc


def scrabble_score(word):
    d = {'A': 1, 'B': 3, 'C': 3,  'D': 2, 'E': 1, 'F': 4, 'G': 2,
         'H': 4, 'I': 1, 'J': 8,  'K': 5, 'L': 1, 'M': 3, 'N': 1,
         'O': 1, 'P': 3, 'Q': 10, 'R': 1, 'S': 1, 'T': 1, 'U': 1,
         'V': 4, 'W': 4,  'X': 8, 'Y': 4, 'Z': 10}

    uword = word.upper()
    s = 0
    for letter in uword:
        s += d.get(letter, 0)
    return s


uniq = list(map(unique_letters2, w))
vowels = list(map(vowel_count, w))
scrab = list(map(scrabble_score, w))
df_raw = {'Word': w, 'NewLttrs': uniq, 'Vowels': vowels, 'Score': scrab}
wordle = pd.DataFrame(df_raw)
wordle = wordle.sort_values(['NewLttrs', 'Vowels', 'Score', 'Word'],
                            ascending=(False, False, True, False))
print(wordle.head(rws))
