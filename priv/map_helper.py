#!/usr/bin/env python

import json
import sys

filename = sys.argv[1]

scaleFactor = 10
spaceChar = ' '
rectChar = '#'

print("loading maps from: "+filename)

def printWorlds(maps):
    for m in maps:
        for line in m:
            print("".join(line))

with open(filename, 'r') as mapfile:
    renderedMaps = []
    maps = json.loads(mapfile.read())
    mapsArray = maps["worlds"]
    for world in mapsArray:
        renderedMap = []
        rects = world["rects"]
        name = world["name"]
        worldWidth = world["width"] / scaleFactor
        worldHeight = world["height"] / scaleFactor
        for y in range(worldHeight):
            renderedMap.append([])
            for x in range(worldWidth):
                renderedMap[y].append(spaceChar)

        for rect in rects:
            rx = rect["x"] / scaleFactor
            ry = rect["y"] / scaleFactor
            rWidth = rect["width"] / scaleFactor
            rHeight = rect["height"] / scaleFactor
            for y in range(ry, ry + rHeight):
                for x in range(rx, rx + rWidth):
                    renderedMap[y][x] = rectChar
        
        renderedMaps.append(renderedMap)

    print("Printing world: "+name)
    printWorlds(renderedMaps)
    print("")


