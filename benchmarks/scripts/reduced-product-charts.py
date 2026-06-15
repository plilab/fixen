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
target_file_candidates = sorted([f for f in files if "reduced-product.csv" in f])
if not target_file_candidates:
    raise FileNotFoundError("Cannot find statistics for reduced product!")
time_file = target_file_candidates[-1]

################################################################################
# Read the files
################################################################################
with open(time_file) as f:
    time_data = list(csv.reader(f))

with open("ceval-stats.csv") as f:
    graph_stats_csv = list(csv.reader(f))

################################################################################
# make the graph stats as something to query
################################################################################
graph_stats = {}
for n, v, e in graph_stats_csv[1:]:
    graph_stats[n[5:]] = int(v)

################################################################################
# Getting the points in the plot
################################################################################
hand = []
no_priorities = []
with_priorities = []

for i in time_data[1:]:
    suite, t = i[0], i[1]
    graph, program = suite.split("/")
    graph = graph.split("(")
    graph = graph[1]
    graph = graph.split(")")
    graph = graph[0]
    e = graph_stats[graph]
    if program == "Hand":
        hand.append((e, float(t)))
    elif program == "Fixen (No Priorities)":
        no_priorities.append((e, float(t)))
    elif program == "Fixen (With Priorities)":
        with_priorities.append((e, float(t)))

hand = sorted(hand)
no_priorities = sorted(no_priorities)
with_priorities = sorted(with_priorities)

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

# (5, 3) is the size of the plot.
fig, ax = plt.subplots(figsize=(6, 2))
# use log scale for both axes
# ax.set_xscale("log")
# ax.set_yscale("log")
# axes labels
ax.set_xlabel("SLoC", font=lin_biolinum)
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

# reference curves
# x1 = np.linspace(1, 2_000_000, 10000)
#
# y1 = x1 / 8_000_000  # linear
# y2 = x1**2 / 800_000_000  # quadratic
# y3 = x1**1.53 / 50_000_000  # x^1.53
# y4 = x1**1.26 / 45_000_000  # x^1.26
# ax.plot(x1, y1, "-", label="$O(|E|)$", color="#aa8888", linewidth=1)
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
plt.xlim(0, 12_000)
plt.ylim((0.000, 0.07))

# Set axis tick fonts
for label in ax.get_xticklabels():
    label.set_fontproperties(lin_libertine)
for label in ax.get_yticklabels():
    label.set_fontproperties(lin_libertine)

# Legend
plt.legend(loc="upper left", prop=lin_libertine)

# Go back to the main dir
os.chdir(cwd)

# plt.show()

# Write the chart
plt.savefig(f"benchmarks/charts/reduced-product.svg", bbox_inches="tight", pad_inches=0)

################################################################################
# table
################################################################################
table_data = sorted(graph_stats_csv[1:], key=lambda x: int(x[1]))

tab = [
    "\\begin{tabular}{rcc}",
    "\\textbf{\\textsf{Program}} & $\\textbf{\\textsf{SLoC}}$ & $\\textbf{\\textsf{\\# of Facts}}$ \\\\",
    "\\midrule",
]

for graph, num_v, num_e in table_data:
    tab.append(f"{graph} & {int(num_v)} & {int(num_e)} \\\\")

tab.append("\\end{tabular}")

# Write the table
with open("benchmarks/charts/reduced-product-table.tex", "w+") as f:
    f.write("\n".join(tab))

################################################################################
# statistics commands
################################################################################
# get the statistics
np_stats = []
wp_stats = []
for i in range(len(hand)):
    h = hand[i][1]
    n = no_priorities[i][1]
    w = with_priorities[i][1]
    np_stats.append(n / h)
    wp_stats.append(w / h)

np_avg = round(sum(np_stats) / len(np_stats), 1)
np_max = round(max(np_stats), 1)
wp_avg = round(sum(wp_stats) / len(wp_stats), 1)
wp_max = round(max(wp_stats), 1)
speedup_avg = round(wp_avg / np_avg, 1)
speedup_max = round(
    max(with_priorities[i][1] / no_priorities[i][1] for i in range(len(hand))), 1
)

# Write the commands
stats = f"""\\def\\reducedproductnoprioritiesavg{{{np_avg}\\times}}
\\def\\reducedproductwithprioritiesavg{{{wp_avg}\\times}}
\\def\\reducedproductnoprioritiesmax{{{np_max}\\times}}
\\def\\reducedproductwithprioritiesmax{{{wp_max}\\times}}
\\def\\reducedproductslowdownavg{{{speedup_avg}\\times}}
\\def\\reducedproductslowdownmax{{{speedup_max}\\times}}
"""

with open("benchmarks/charts/reduced-product-statistics.tex", "w+") as f:
    f.write(stats)
