import json
import os
import sys
import csv

in_dir = sys.argv[1]
out_file = sys.argv[2]
curr_dir = os.getcwd()
os.chdir(in_dir)

s = [i for i in os.listdir() if i[-4:] == "json"]

res = [["filename", "|V|", "|E|"]]

for n in s:
    with open(n) as f:
        gph = json.loads(f.read())
    num_edge = 0
    for v in gph.values():
        num_edge += len(v.values())
    num_v = len(list(gph.keys()))
    res.append([n[:-5], num_v, num_edge])
os.chdir(curr_dir)
with open(out_file, "w+") as f:
    w = csv.writer(f)
    w.writerows(res)
