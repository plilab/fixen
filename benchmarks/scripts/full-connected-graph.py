#!/bin/python
# This program generates a fully connected digraph.

from random import randint
import json

d = {}

NUM_VERTICES = 1000

for i in range(NUM_VERTICES):
    edges = {}
    for j in range(NUM_VERTICES):
        if i == j:
            continue
        edges[str(j)] = {"weight": randint(1, 1000)}
    d[str(i)] = edges


print(json.dumps(d))
