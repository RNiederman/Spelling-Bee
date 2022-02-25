import re
from urllib.request import urlopen


omit = 'spear chunk plot'
include = 'd'
answer = '?i???'

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

if len(omit) > 0:
    patt = ".*[" + o + "].*"
    r = re.compile(patt)
    omit_list = set(filter(r.match, w))
    w = set(w).difference(set(omit_list))

if len(include) > 0:
    patt = ".*[" + i + "].*"
    r = re.compile(patt)
    w = set(filter(r.match, w))

if a != "?????":
    patt = a.replace("?", "[A-Z]{1}")
    r = re.compile(patt)
    w = set(filter(r.match, w))

print(sorted(w))
