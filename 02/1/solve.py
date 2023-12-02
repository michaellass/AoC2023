#!/usr/bin/env python3

MAP = {"red": 0, "green": 1, "blue": 2}


def parseDraw(s):
    elems = s.split(",")
    count = [0, 0, 0]
    for elem in elems:
        words = elem.strip().split(" ")
        count[MAP[words[1]]] = int(words[0])
    return count


def isValid(counts):
    return (
        counts[MAP["red"]] <= 12
        and counts[MAP["green"]] <= 13
        and counts[MAP["blue"]] <= 14
    )


games = []
with open("input", "r") as fh:
    for line in fh.readlines():
        games.append(line.split(":")[1])

sum = 0
for id, game in enumerate(games):
    draws = game.split(";")
    valid = all([isValid(parseDraw(d)) for d in draws])
    if valid:
        sum += id + 1

print(sum)
