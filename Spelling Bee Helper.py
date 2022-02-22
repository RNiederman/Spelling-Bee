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
    pts = word_len if word_len > 4 else 1
    long_adder = (uniq_lttrs == 7) * 7
    pts = pts + long_adder
    return pts


def build_word_list(url):
    f = urlopen(url)
    w = f.read().decode('utf-8').upper().split()
    w = list(filter(lambda word: len(word) >= 4, w))
    w = list(filter(lambda word: "S" not in (word), w))
    ul = list(map(unique_letters, w))
    p = list(map(spelling_bee_points, w))
    d = {'Word': w, 'Unique Letters': ul, 'Score': p}
    df = pd.DataFrame(d)
    df = df[(df['Unique Letters']) <= 7]
    df = df.sort_values(['Score', 'Word'], ascending=(False, True))
    return df


def sb_helper(letters, mwl):
    center = letters[0]
    hc = mwl[mwl['Word'].str.contains(center)]
    alphabet = set(st.ascii_uppercase)
    bad_letters = alphabet.difference(set(letters))
    for lttr in bad_letters:
        hc = hc[~hc['Word'].str.contains(lttr)]
    cols = [0, 2]
    hc = hc[hc.columns[cols]]
    return hc


word_list = build_word_list(word_url)
sb_soln = sb_helper(sb_letters, word_list)

print(sb_soln)
