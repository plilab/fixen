import os
import sys
import csv

in_dir = sys.argv[1]
out_file = sys.argv[2]
curr_dir = os.getcwd()
os.chdir(in_dir)

s = [i for i in os.listdir() if i[:5] == "CEval"]

print(s)

res = [["filename", "LoC", "|Facts|"]]

for n in s:
    with open(n) as f:
        ls = f.readlines()
    snd_last = ls[-2]
    num = snd_last[11:]
    num = num.split(" ")
    loc = int(num[0])
    wp_line = ls.index("withPrioritiesTest =\n")
    num_facts = len(ls) - wp_line - 1
    res.append([n[:-3], loc, num_facts])

os.chdir(curr_dir)
with open(out_file, "w+") as f:
    w = csv.writer(f)
    w.writerows(res)
