import sys
import rpy2.robjects as robjects
import matplotlib.pyplot as plt
import os

os.environ["R_HOME"] = "C:/Program Files/R/R-4.2.0"
os.environ["PATH"] = "C:/Program Files/R/R-4.2.0/bin/x64" + \
    ";" + os.environ["PATH"]


def mkPlotFiles():
    plotFits()
    plotMotionSpace()


def plotFits():
    xs = range(len(bestFits))
    bestPlot = plt.plot(xs, bestFits, color="limegreen", label="Best")
    avgPlot = plt.plot(xs, avgFits, color="dodgerblue", label="Average")
    plt.xlabel("Generation")
    plt.ylabel("Fitness")
    plt.title("Leg Evolution")
    plt.legend()
    if isFinal:
        fileName = "plots/" + name + "_finalLegEvolution.png"
    else:
        fileName = "plots/" + name + "_legEvolution.png"
    open(fileName, "w")
    plt.savefig(fileName)
    plt.close("all")


def plotMotionSpace():
    xMin, xMax, yMin, yMax = min(xLst), max(xLst), min(yLst), max(yLst)
    xRange, yRange = xMax - xMin, yMax - yMin
    yMid = (yMin + yMax) / 2
    plt.plot(xLst, yLst, color="dimgrey")
    plt.axis([-5, xMax + 5, -xRange / 20, 19 / 20 * xRange])
    plt.xlabel("X Position (mm)")
    plt.ylabel("Y Position (mm)")
    plt.title("Best Leg Motion\nFitness = " + str(bestFit) + " mm")
    if isFinal:
        fileName = "plots/" + name + "_finalLegMotion.png"
    else:
        fileName = "plots/" + name + "_legMotion.png"
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
            return [interpLstStr(lstStr[posStart: posEnd + 1])]
        elif posStart == 1:
            return [interpLstStr(lstStr[posStart: posEnd + 1])] + interpLstStr(
                "[" + lstStr[posEnd + 2:]
            )
        elif posEnd + 2 == len(lstStr):
            return interpLstStr(lstStr[: posStart - 1] + "]") + [
                interpLstStr(lstStr[posStart: posEnd + 1])
            ]
        else:
            return (
                interpLstStr(lstStr[: posStart - 1] + "]")
                + [interpLstStr(lstStr[posStart: posEnd + 1])]
                + interpLstStr("[" + lstStr[posEnd + 2:])
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
isFinal = bool(sys.argv[2])
dataFile = open("datafile.txt", "r")
bestFits = interpLstStr(dataFile.readline()[:-1])
bestFit = round(bestFits[-1])
avgFits = interpLstStr(dataFile.readline()[:-1])
xLst = interpLstStr(dataFile.readline()[:-1])
yLst = interpLstStr(dataFile.readline())
if isFinal:
    r_smooth_spline = robjects.r["smooth.spline"]
    r_x = robjects.FloatVector(range(len(avgFits)))
    r_y = robjects.FloatVector(avgFits)
    kwargs = {"x": r_x, "y": r_y, "lambda": 1e-3}
    spline = r_smooth_spline(**kwargs)
    avgFits = robjects.r["predict"](spline, r_x).rx2("y")
mkPlotFiles()
