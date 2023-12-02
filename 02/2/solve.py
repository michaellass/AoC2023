#!/usr/bin/env python3

from functools import reduce, partial

MAP = {"red": 0, "green": 1, "blue": 2}


def parseDraw(s):
    elems = s.split(",")
    count = [0, 0, 0]
    for elem in elems:
        words = elem.strip().split(" ")
        count[MAP[words[1]]] = int(words[0])
    return count


def elementwise(op, l1, l2):
    return [op(a, b) for a, b in zip(l1, l2)]


games = []
with open("input", "r") as fh:
    for line in fh.readlines():
        games.append(line.split(":")[1])

sum = 0
for game in games:
    draws = game.split(";")
    minimum = reduce(
        partial(elementwise, max),
        [parseDraw(d) for d in draws],
    )
    sum += minimum[0] * minimum[1] * minimum[2]

print(sum)
