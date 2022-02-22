import pandas as pd
import string as st
from urllib.request import urlopen


sb_letters = ['F', 'U', 'N', 'L', 'I', 'B', 'E']
# Center Letter Should Always be 1st
word_url = 'https://norvig.com/ngrams/enable1.txt'

# word_url = 'http://www-personal.umich.edu/~jlahcer/wordlist'
# word_url = 'http://www.mieliestronk.com/corncob_caps.txt'


def unique_letters(word):
    a = set(word)
    b = len(a)
    return b


def spelling_bee_points(wrd):
    uniq_lttrs = unique_letters(wrd)
    word_len = len(wrd)
    pts = 1 if word_len == 4 else word_len
    long_adder = (uniq_lttrs == 7) * 7
    pts = pts + long_adder
    return pts


def build_word_list(url):
    f = urlopen(url)
    w = f.read().decode('utf-8').upper().split()
    w = list(filter(lambda word: len(word) >= 4, w))
    w = list(filter(lambda word: "S" not in (word), w))
    hc = list(map(len, w))
    ul = list(map(unique_letters, w))
    p = list(map(spelling_bee_points, w))
    dict = {'Word': w, 'Length': hc, 'Unique Letters': ul, 'Score': p}
    df1 = pd.DataFrame(dict)
    df2 = df1[(df1['Unique Letters']) <= 7]
    df2 = df2.sort_values(['Score', 'Word'], ascending=(False, True))
    return df2


def get_words(letters, mwl):
    center_letter = letters[0]
    hc = mwl
    hc = hc[hc['Word'].str.contains(center_letter)]
    alphabet = set(st.ascii_uppercase)
    invalid_letters = alphabet.difference(set(letters))
    for bad_lttr in invalid_letters:
        hc = hc[~hc['Word'].str.contains(bad_lttr)]
    cols = [0, 3]
    df = hc[hc.columns[cols]]
    return df


word_list = build_word_list(word_url)
sb_soln = get_words(sb_letters, word_list)

print(sb_soln)
