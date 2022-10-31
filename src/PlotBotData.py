import matplotlib.pyplot as plt
import os

os.environ["R_HOME"] = "C:/Program Files/R/R-4.2.0"
os.environ["PATH"] = "C:/Program Files/R/R-4.2.0/bin/x64" + ";" + os.environ["PATH"]
import rpy2.robjects as robjects
import sys


def mkPlotFiles():
    if recordType == "default":
        plotFits()
        plotLegMotions()
        plotBotPath()
    elif recordType == "raw" or recordType == "smooth":
        plotFits()
    elif recordType == "ref" or recordType == "max":
        plotLegMotions()
        plotBotPath()


def plotFits():
    xs = list(range(len(maxFits)))
    maxPlot = plt.plot(xs, maxFits, color="black", linewidth=1.2, label="MaxFits")
    bestPlot = plt.plot(
        xs, bestFits, color="limegreen", linewidth=1.2, label="BestFits"
    )
    avgPlot = plt.plot(xs, avgFits, color="dodgerblue", linewidth=1.2, label="AvgFits")
    plt.xlabel("Generation")
    plt.ylabel("Fitness (mm)")
    plt.title("Bot Evolution\nBest Fitness = " + str(round(maxFits[-1])) + " mm")
    plt.legend()
    if recordType == "default":
        endLabel = "_currentBotEvolution.png"
    elif recordType == "raw":
        endLabel = "_gen" + botGen + "rawBotEvolution.png"
    elif recordType == "smooth":
        endLabel = "_gen" + botGen + "smoothBotEvolution.png"
    fileName = ".stack-work\\plots\\" + name + endLabel
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
    elif recordType == "best":
        endLabel = "_gen" + botGen + "bestBotMotions.png"
    elif recordType == "max":
        endLabel = "_gen" + botGen + "maxBotMotions.png"
    fileName = ".stack-work\\plots\\" + name + endLabel
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
    elif recordType == "best":
        endLabel = "_gen" + botGen + "bestBotPath.png"
    elif recordType == "max":
        endLabel = "_gen" + botGen + "maxBotPath.png"
    fileName = ".stack-work\\plots\\" + name + endLabel
    open(fileName, "w")
    plt.savefig(fileName)
    plt.close("all")


def interpLstStr(lstStr):
    if isFlatLst(lstStr):
        return [float(s) for s in lstStr[1:-1].split(",")]
    else:
        posStart = lstStr[1:].find("[") + 1
        substr = lstStr[posStart:]
        posEnd = posStart + findEndBracket(substr)
        if posStart == 1 and posEnd + 2 == len(lstStr):
            return [interpLstStr(lstStr[posStart : posEnd + 1])]
        elif posStart == 1:
            return [interpLstStr(lstStr[posStart : posEnd + 1])] + interpLstStr(
                "[" + lstStr[posEnd + 2 :]
            )
        elif posEnd + 2 == len(lstStr):
            return interpLstStr(lstStr[: posStart - 1] + "]") + [
                interpLstStr(lstStr[posStart : posEnd + 1])
            ]
        else:
            return (
                interpLstStr(lstStr[: posStart - 1] + "]")
                + [interpLstStr(lstStr[posStart : posEnd + 1])]
                + interpLstStr("[" + lstStr[posEnd + 2 :])
            )


def findEndBracket(lstStr):
    c1 = 0
    c2 = 0
    for letter in lstStr:
        if letter == "[":
            c2 = c2 + 1
        elif letter == "]":
            c2 = c2 - 1
        else:
            pass
        if c2 == 0:
            return c1
        c1 = c1 + 1
    return -1


def isFlatLst(lstStr):
    if lstStr[1:].find("[") == -1:
        return True
    else:
        return False


name = sys.argv[1]
recordType = sys.argv[2]
botGen = sys.argv[3]
dataFile = open(".stack-work\\datafile.txt", "r")
maxFits = interpLstStr(dataFile.readline()[:-1])
bestFits = interpLstStr(dataFile.readline()[:-1])
avgFits = interpLstStr(dataFile.readline()[:-1])
legMoves = interpLstStr(dataFile.readline()[:-1])
hexPath = interpLstStr(dataFile.readline())
if recordType == "default":
    legTitle = (
        "Current BestBot's Leg Motions\nFitness = " + str(round(bestFits[-1])) + " mm"
    )
    pathTitle = (
        "Current BestBot's Path on Floor\nFitness = " + str(round(bestFits[-1])) + " mm"
    )
elif recordType == "best":
    legTitle = (
        "BestBot's Leg Motions at Generation "
        + botGen
        + "\nFitness = "
        + str(round(bestFits[-1]))
        + " mm"
    )
    pathTitle = (
        "BestBot's Path on Floor at Generation "
        + botGen
        + "\nFitness = "
        + str(round(bestFits[-1]))
        + " mm"
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
if recordType == "smooth":
    lists = []
    for lst in [bestFits, avgFits, bestFits]:
        r_smooth_spline = robjects.r["smooth.spline"]
        r_x = robjects.FloatVector(range(len(lst)))
        r_y = robjects.FloatVector(lst)
        kwargs = {"x": r_x, "y": r_y, "lambda": 1e-3}
        spline = r_smooth_spline(**kwargs)
        newLst = robjects.r["predict"](spline, r_x).rx2("y")
        lists += [newLst]
    bestFits = lists[0]
    avgFits = lists[1]
    refFits = lists[2]
mkPlotFiles()
