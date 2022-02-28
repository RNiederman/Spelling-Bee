import re
import string
import pandas as pd
from english_words import english_words_set

###############################################################################
omit = 'spar tild un'  # These are the Black Letters
include = 'oc'  # These are the Yellow Letters
answer = ' ? ? ? ? e '  # These are the Green Letters
###############################################################################

o = omit.upper()
o = re.sub("[^A-Z]", "", o)

i = include.upper()
i = re.sub("[^A-Z]", "", i)

a = ''.join(answer.split())
a = a[:5].upper()
a = re.sub("[^A-Z]", "?", a)

w = list(english_words_set)
w = list(map(lambda word: word.upper(), w))
w = list(map(lambda word: word.translate(
    str.maketrans('', '', string.punctuation)), w))
w = list(filter(lambda word: len(word) == 5, w))

if len(o) > 0:
    patt = ".*[" + o + "].*"
    r = re.compile(patt)
    omit_list = set(filter(r.match, w))
    w = list(set(w).difference(set(omit_list)))

for lttr in i:
    w = list(filter(lambda wordset: lttr in wordset, w))

if a != '?????':
    patt = a.replace("?", "[A-Z]{1}")
    r = re.compile(patt)
    w = list(filter(r.match, w))


def scrabble_score(word):
    d = {'A': 1, 'B': 3, 'C': 3,  'D': 2, 'E': 1, 'F': 4, 'G': 2,
         'H': 4, 'I': 1, 'J': 8,  'K': 5, 'L': 1, 'M': 3, 'N': 1,
         'O': 1, 'P': 3, 'Q': 10, 'R': 1, 'S': 1, 'T': 1, 'U': 1,
         'V': 4, 'W': 4,  'X': 8, 'Y': 4, 'Z': 10}

    uword = word.upper()
    s = 0
    for letter in uword:
        q = int(d.get(letter) or 0)
        s += q
    return s


def unique_letters2(word):
    x = set(word.upper())
    y = set(a)
    z = set(i)
    q = x.difference(y).difference(z)
    return len(q)


scrab = list(map(scrabble_score, w))
uniq = list(map(unique_letters2, w))
df_raw = {'Word': w, 'NewLttrs': uniq, 'Score': scrab}
wordle = pd.DataFrame(df_raw)
wordle = wordle.sort_values(['NewLttrs', 'Score', 'Word'],
                            ascending=(False, True, False))
print(wordle.to_string())
