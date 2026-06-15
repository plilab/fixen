from pathlib import Path
import matplotlib.pyplot as plt
import matplotlib.font_manager as font_manager
import matplotlib as mpl
import numpy as np
import csv
import os

################################################################################
# 1. First, get the time statistics
################################################################################
cwd = os.getcwd()
os.chdir("benchmarks/results")
files = os.listdir()
target_file_candidates = sorted([f for f in files if "pareto.csv" in f])
if not target_file_candidates:
    raise FileNotFoundError("Cannot find statistics for shortest paths!")
time_file = target_file_candidates[-1]

################################################################################
# Read the files
################################################################################
with open(time_file) as f:
    time_data = list(csv.reader(f))

with open("pareto-graph-stats.csv") as f:
    graph_stats_csv = list(csv.reader(f))

################################################################################
# make the graph stats as something to query
################################################################################
graph_stats = {}
for n, v, e in graph_stats_csv[1:]:
    name, number = n.split("-")
    number = int(number)
    if name not in graph_stats:
        graph_stats[name] = {}
    graph_stats[name][number] = int(e)


################################################################################
# Getting the points in the plot
################################################################################
plots = {}

for i in time_data[1:]:
    suite, t = i[0], i[1]
    graph, program = suite.split("/")
    graph_name, graph_number = graph.split("-")
    graph_number = int(graph_number)
    e = graph_stats[graph_name][graph_number]
    if graph_name not in plots:
        plots[graph_name] = {}
    if program not in plots[graph_name]:
        plots[graph_name][program] = []
    plots[graph_name][program].append((e, float(t)))

for g in plots:
    for p in plots[g]:
        plots[g][p] = sorted(plots[g][p])

################################################################################
# fonts for chart
################################################################################
lin_biolinum = Path(
    mpl.get_data_path(), "/usr/share/fonts/ttf-linux-libertine/LinBiolinum_Rah.ttf"
)

lin_libertine = font_manager.FontProperties(
    fname="/usr/share/fonts/ttf-linux-libertine/LinLibertine_Rah.ttf"
)
lin_libertine.set_math_fontfamily("cm")

################################################################################
# chart
################################################################################

# Go back to the main dir
os.chdir(cwd)

# reference curves
# x1 = np.linspace(1, 100_000, 10000)
# references = {}
# for g in plots:
#     references[g] = {}
#
# references["CTR"][1] = ((x1 / 23_500) ** 2.15, r"$O(|E|^{2.15})$")
# references["CTR"][2] = ((x1 / 65_500) ** 1.95, r"$O(|E|^{1.95})$")
# references["W"][1] = (
#     (x1 / 110_500) ** 1.44 + ((x1 / 28_500) ** 2.5),
#     r"$O(|E|^{2.5})$",
# )
# references["W"][2] = (
#     (x1 / 250_500) ** 1.3 + ((x1 / 70_500) ** 2.5),
#     r"$O(|E|^{2.5})$",
# )

for g in plots:
    # print(g)
    # if g != "W":
    #     continue
    d = plots[g]
    hand = d["Hand"]
    no_priorities = d["Fixen (No Priorities)"]
    with_priorities = d["Fixen (With Priorities)"]
    # (5, 3) is the size of the plot.
    fig, ax = plt.subplots(figsize=(4.2, 1.8))
    # use log scale for both axes
    ax.set_xscale("log")
    ax.set_yscale("log")
    # axes labels
    ax.set_xlabel("Number of Edges", font=lin_biolinum)
    ax.set_ylabel("Time (s)", font=lin_biolinum)

    # Plot the points
    ax.plot(
        [i[0] for i in hand],
        [i[1] for i in hand],
        "-o",
        label="Hand",
        color="#8839ef",
        markersize=3,
    )
    ax.plot(
        [i[0] for i in no_priorities],
        [i[1] for i in no_priorities],
        "-s",
        label="Fixen (No Priorities)",
        color="#a13c3c",
        markersize=3,
    )
    ax.plot(
        [i[0] for i in with_priorities],
        [i[1] for i in with_priorities],
        "-x",
        label="Fixen (With Priorities)",
        color="#179299",
        markersize=4,
    )

    # y1, label1 = references[g][1]
    # y2, label2 = references[g][2]

    # y1 = x1 / 8_000_000 + x1 / 10_000  # linear
    # y1 = (x1 / 40_000) ** 3.5 + (x1 / 50_000) ** 2  # quadratic
    # y3 = x1**1.53 / 50_000_000  # x^1.53
    # y4 = x1**1.26 / 45_000_000  # x^1.26
    # ax.plot(x1, y1, "-", label=label1, color="#aa8888", linewidth=1)
    # ax.plot(x1, y2, "--", label=label2, color="#8888aa", linewidth=1)
    # ax.plot(
    #     x1,
    #     y2,
    #     "--",
    #     label=r"$O(|E|^2)$",
    #     color="#8888aa",
    #     linewidth=1,
    # )
    # ax.plot(
    #     x1,
    #     y3,
    #     ":",
    #     label=r"$O(|E|^{1.53})$",
    #     color="#a13c3c",
    #     linewidth=1,
    # )
    # ax.plot(
    #     x1,
    #     y4,
    #     "-.",
    #     label=r"$O(|E|^{1.26})$",
    #     color="#8839ef",
    #     linewidth=1,
    # )

    plt.grid()

    # Axis limits
    plt.xlim(800, 100_000)
    plt.ylim((0.0001, 100))

    # Set axis tick fonts
    for label in ax.get_xticklabels():
        label.set_fontproperties(lin_libertine)
    for label in ax.get_yticklabels():
        label.set_fontproperties(lin_libertine)

    # Legend
    plt.legend(loc="upper left", prop=lin_libertine)
    # plt.show()
    # exit(0)
    # if g != "CTR":
    #     break

    # Write the chart
    plt.savefig(f"benchmarks/charts/pareto-{g}.svg", bbox_inches="tight", pad_inches=0)


################################################################################
# table
################################################################################
table_data = [
    [i[0].split("-")[0], i[1], i[2]]
    for i in sorted(graph_stats_csv[1:], key=lambda x: x[0])
    if i[0].split("-")[1] == "7"
]

tab = [
    "\\begin{tabular}{rcc}",
    "\\textbf{\\textsf{Graph}} & $|V|$ & $|E|$ \\\\",
    "\\midrule",
]

for graph, num_v, num_e in table_data:
    tab.append(f"{graph} & {int(num_v)} & {int(num_e)} \\\\")

tab.append("\\end{tabular}")

# Write the table
with open("benchmarks/charts/pareto-table.tex", "w+") as f:
    f.write("\n".join(tab))

################################################################################
# statistics commands
################################################################################
# get the statistics
np_stats = []
wp_stats = []
for graph in plots:
    for i in range(7):
        np_stats.append(
            plots[graph]["Fixen (No Priorities)"][i][1] / plots[graph]["Hand"][i][1]
        )
        wp_stats.append(
            plots[graph]["Fixen (With Priorities)"][i][1] / plots[graph]["Hand"][i][1]
        )

np_avg = round(sum(np_stats) / len(np_stats), 1)
np_max = round(max(np_stats), 1)
wp_avg = round(sum(wp_stats) / len(wp_stats), 1)
wp_max = round(max(wp_stats), 1)
speedup_avg = round(np_avg / wp_avg, 1)
speedup_max = round(max(np_stats[i] / wp_stats[i] for i in range(len(np_stats))), 1)

# Write the commands
stats = f"""\\def\\paretonoprioritiesavg{{{np_avg}\\times}}
\\def\\paretowithprioritiesavg{{{wp_avg}\\times}}
\\def\\paretonoprioritiesmax{{{np_max}\\times}}
\\def\\paretowithprioritiesmax{{{wp_max}\\times}}
\\def\\paretospeedupavg{{{speedup_avg}\\times}}
\\def\\paretospeedupmax{{{speedup_max}\\times}}
"""

with open("benchmarks/charts/pareto-statistics.tex", "w+") as f:
    f.write(stats)
