import pandas as pd
from urllib.request import urlopen


answer = '~~~~~'
omit = ''
include = ''


word_url = 'https://norvig.com/ngrams/enable1.txt'
# word_url = 'http://www-personal.umich.edu/~jlahcer/wordlist'
# word_url = 'http://www.mieliestronk.com/corncob_caps.txt'

f = urlopen(word_url)
w = f.read().decode('utf-8').upper().split()
w = list(filter(lambda word: len(word) == 5, w))
wdf = pd.DataFrame(w)
wdf.columns = ['Word']

o_set = set(omit.upper())
for o in o_set:
    wdf = wdf[~wdf['Word'].str.contains(o)]

i_set = set(include.upper())
for i in i_set:
    wdf = wdf[wdf['Word'].str.contains(i)]

regex_patt = answer.upper().replace('~', '.*')
series = wdf['Word']
idex = list(series.str.contains(regex_patt))

wdf2 = wdf[idex]
wordle_words = list(wdf2['Word'])

print(wordle_words)
