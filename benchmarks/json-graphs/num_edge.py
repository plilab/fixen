import json
import os

s = [i for i in os.listdir() if i[-4:] == "json"]

for n in s:
    with open(n) as f:
        gph = json.loads(f.read())
    num_edge = 0
    for v in gph.values():
        num_edge += len(v.values())
    print(n, num_edge)
