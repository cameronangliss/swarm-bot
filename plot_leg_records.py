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
        plotLegMotion()
    elif recordType == "evo":
        plotFits()
    elif recordType == "max":
        plotLegMotion()


def plotFits():
    xs = list(range(len(bestFits)))
    bestPlot = plt.plot(
        xs, bestFits, color="limegreen", linewidth=1.2, label="BestFits"
    )
    avgPlot = plt.plot(xs, avgFits, color="dodgerblue",
                       linewidth=1.2, label="AvgFits")
    plt.xlabel("Generation")
    plt.ylabel("Fitness (mm)")
    plt.title("Leg Evolution\nBest Fitness = " +
              str(round(bestFits[-1])) + " mm")
    plt.legend()
    if recordType == "default":
        endLabel = "_currentLegEvolution.png"
    elif recordType == "evo":
        endLabel = "_gen" + legGen + "legEvolution.png"
    fileName = "plots/" + name + endLabel
    open(fileName, "w")
    plt.savefig(fileName)
    plt.close("all")


def plotLegMotion():
    legMovesX = positions[0][:101]
    legMovesY = positions[1][:101]
    plt.plot(legMovesX, legMovesY, color="dimgrey", linewidth=1.2)
    plt.xlabel("Horizontal Position (mm)")
    plt.ylabel("Vertical Position (mm)")
    plt.title(legTitle)
    if recordType == "default":
        endLabel = "_currentBestLegMotion.png"
    elif recordType == "max":
        endLabel = "_gen" + legGen + "bestLegMotion.png"
    fileName = "plots/" + name + endLabel
    open(fileName, "w")
    plt.savefig(fileName)
    plt.close("all")


name = sys.argv[1]
recordType = sys.argv[2]
legGen = sys.argv[3]
dataFile = open("datafile.txt", "r")
bestFits = json.loads(dataFile.readline()[:-1])
avgFits = json.loads(dataFile.readline()[:-1])
positions = json.loads(dataFile.readline())
if recordType == "default":
    legTitle = (
        "Current BestLeg's Motion\nFitness = " +
        str(round(bestFits[-1])) + " mm"
    )
elif recordType == "max":
    legTitle = (
        "MaxLeg's Motion at Generation "
        + legGen
        + "\nFitness = "
        + str(round(bestFits[-1]))
        + " mm"
    )
mkPlotFiles()
