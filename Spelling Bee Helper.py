import pandas as pd
import string as st
import re
import sys
from urllib.request import urlopen


sb_letters = 'debtjoc'
# Center Letter Should Always be 1st
letters_reqd = 7

word_url = 'https://norvig.com/ngrams/enable1.txt'
# word_url = 'http://www-personal.umich.edu/~jlahcer/wordlist'
# word_url = 'http://www.mieliestronk.com/corncob_caps.txt'


def unique_letters(word):
    a = set(word)
    b = len(a)
    return b


def spelling_bee_points(wrd):
    ul = unique_letters(wrd)
    wl = len(wrd)
    pts = wl if wl > 4 else 1
    panagram = (ul == 7) * 7
    pts = pts + panagram
    return pts


def build_word_list(url):
    f = urlopen(url)
    w = f.read().decode('utf-8').upper().split()
    w = list(filter(lambda word: len(word) >= 4, w))
    w = list(filter(lambda word: "S" not in (word), w))
    u = list(map(unique_letters, w))
    p = list(map(spelling_bee_points, w))
    d = {'Word': w, 'Unique Letters': u, 'Score': p}
    df = pd.DataFrame(d)
    df = df[(df['Unique Letters']) <= 7]
    df = df.sort_values(['Score', 'Word'], ascending=(False, True))
    return df


def sb_helper(letters, mwl):
    ltrs = letters.upper()
    ltrs = re.findall("[A-Z]", ltrs)
    center = ltrs[0]
    hc = mwl[mwl['Word'].str.contains(center)]
    alphabet = set(st.ascii_uppercase)
    bad_letters = alphabet.difference(set(ltrs))
    for bd_lttr in bad_letters:
        hc = hc[~hc['Word'].str.contains(bd_lttr)]
    cols = [0, 2]
    hc = hc[hc.columns[cols]]
    return hc


reg_ex = re.compile('[^A-Za-z]')
tester = reg_ex.sub('', sb_letters)
bail_out = unique_letters(tester) != letters_reqd
if bail_out:
    sys.exit("Wrong Amount of Letters")
word_list = build_word_list(word_url)
sb_soln = sb_helper(sb_letters, word_list)

print(sb_soln)
