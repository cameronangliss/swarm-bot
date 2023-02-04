import json
import matplotlib.pyplot as plt
import os
import sys

os.environ["R_HOME"] = "C:/Program Files/R/R-4.2.0"
os.environ["PATH"] = "C:/Program Files/R/R-4.2.0/bin/x64" + \
    ";" + os.environ["PATH"]


def mkPlotFiles():
    if recordType == "default":
        plotFits()
        plotLegMotions()
        plotBotPath()
    elif recordType == "evo":
        plotFits()
    elif recordType == "max":
        plotLegMotions()
        plotBotPath()


def plotFits():
    xs = list(range(len(maxFits)))
    maxPlot = plt.plot(xs, maxFits, color="black",
                       linewidth=1.2, label="MaxFits")
    bestPlot = plt.plot(
        xs, bestFits, color="limegreen", linewidth=1.2, label="BestFits"
    )
    avgPlot = plt.plot(xs, avgFits, color="dodgerblue",
                       linewidth=1.2, label="AvgFits")
    plt.xlabel("Generation")
    plt.ylabel("Fitness (mm)")
    plt.title("Bot Evolution\nBest Fitness = " +
              str(round(maxFits[-1])) + " mm")
    plt.legend()
    if recordType == "default":
        endLabel = "_currentBotEvolution.png"
    elif recordType == "evo":
        endLabel = "_gen" + botGen + "botEvolution.png"
    fileName = "plots/" + name + endLabel
    open(fileName, "w")
    plt.savefig(fileName)
    plt.close("all")


def plotLegMotions():
    colors = [
        "crimson",
        "darkorange",
        "limegreen",
        "dodgerblue",
        "mediumorchid",
        "dimgrey",
    ]
    for i in range(0, len(legMoves)):
        legMovesX = legMoves[i][0][:101]
        legMovesY = legMoves[i][1][:101]
        plt.plot(range(101), legMovesX, color=colors[i], linewidth=1.2)
        plt.plot(
            range(101), legMovesY, color=colors[i], linestyle="dashed", linewidth=1.2
        )
    plt.xlabel("Iteration")
    plt.ylabel("Position (mm)")
    plt.title(legTitle)
    if recordType == "default":
        endLabel = "_currentBestBotMotions.png"
    elif recordType == "max":
        endLabel = "_gen" + botGen + "maxBotMotions.png"
    fileName = "plots/" + name + endLabel
    open(fileName, "w")
    plt.savefig(fileName)
    plt.close("all")


def plotBotPath():
    xs = hexPath[0]
    ys = hexPath[1]
    xMin, xMax, yMin, yMax = min(xs), max(xs), min(ys), max(ys)
    xRange, yRange = xMax - xMin, yMax - yMin
    yMid = (yMin + yMax) / 2
    plt.plot(xs, ys, color="black", linewidth=1.2)
    if xRange > yRange:
        plt.axis([xMin, xMax, yMid - xRange / 2, yMid + xRange / 2])
    else:
        plt.axis([xMin, xMax + yRange - xRange, yMin, yMax])
    plt.xlabel("Forward Distance (mm)")
    plt.ylabel("Lateral Distance (mm)")
    plt.title(pathTitle)
    if recordType == "default":
        endLabel = "_currentBestBotPath.png"
    elif recordType == "max":
        endLabel = "_gen" + botGen + "maxBotPath.png"
    fileName = "plots/" + name + endLabel
    open(fileName, "w")
    plt.savefig(fileName)
    plt.close("all")


name = sys.argv[1]
recordType = sys.argv[2]
botGen = sys.argv[3]
dataFile = open("datafile.txt", "r")
maxFits = json.loads(dataFile.readline()[:-1])
bestFits = json.loads(dataFile.readline()[:-1])
avgFits = json.loads(dataFile.readline()[:-1])
legMoves = json.loads(dataFile.readline()[:-1])
hexPath = json.loads(dataFile.readline())
if recordType == "default":
    legTitle = (
        "Current BestBot's Leg Motions\nFitness = " +
        str(round(bestFits[-1])) + " mm"
    )
    pathTitle = (
        "Current BestBot's Path on Floor\nFitness = " +
        str(round(bestFits[-1])) + " mm"
    )
elif recordType == "max":
    legTitle = (
        "MaxBot's Leg Motions at Generation "
        + botGen
        + "\nFitness = "
        + str(round(maxFits[-1]))
        + " mm"
    )
    pathTitle = (
        "MaxBot's Path on Floor at Generation "
        + botGen
        + "\nFitness = "
        + str(round(maxFits[-1]))
        + " mm"
    )
mkPlotFiles()
