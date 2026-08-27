# -*- coding: utf-8 -*-
"""סקירת קומה: קירות + סימון הממ"דים + סרגל קנה מידה.
   python floor_overview.py out.png X2
"""
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
import ezdxf, sys, math, re

out = sys.argv[1]
target = sys.argv[2]

BS = chr(92)
RE_U = re.compile(BS + BS + r'U\+([0-9A-Fa-f]{4})')
RE_FMT = re.compile(BS + BS + r'[a-zA-Z][^;]*;')
def clean(t):
    t = RE_U.sub(lambda m: chr(int(m.group(1), 16)), t)
    t = t.replace(BS + 'P', ' ')
    t = RE_FMT.sub('', t)
    return ' '.join(t.replace('{', '').replace('}', '').split())

doc = ezdxf.readfile("P-001.dxf")
msp = doc.modelspace()
ins = [e for e in msp.query("INSERT") if e.dxf.name == target][0]

segs, marks = [], []
KEYS = ("ממ", "מרחב", "מוגן", "מקלט")

def emit(e):
    t = e.dxftype()
    lay = e.dxf.layer.upper()
    if t in ("TEXT", "MTEXT"):
        s = clean(e.dxf.text if t == "TEXT" else e.text)
        if any(k in s for k in KEYS):
            p = e.dxf.insert
            marks.append((p.x, p.y))
        return
    if "WALL" not in lay:
        return
    if t == "LINE":
        segs.append(((e.dxf.start.x, e.dxf.start.y), (e.dxf.end.x, e.dxf.end.y)))
    elif t == "LWPOLYLINE":
        pts = [(p[0], p[1]) for p in e.get_points()]
        if e.closed and len(pts) > 2:
            pts = pts + [pts[0]]
        for i in range(len(pts) - 1):
            segs.append((pts[i], pts[i + 1]))
    elif t == "ARC":
        c = e.dxf.center; r = e.dxf.radius
        a0, a1 = math.radians(e.dxf.start_angle), math.radians(e.dxf.end_angle)
        if a1 < a0: a1 += 2 * math.pi
        n = max(6, int((a1 - a0) / 0.15))
        pts = [(c.x + r * math.cos(a0 + (a1 - a0) * i / n),
                c.y + r * math.sin(a0 + (a1 - a0) * i / n)) for i in range(n + 1)]
        for i in range(n):
            segs.append((pts[i], pts[i + 1]))

def walk(c, d=0):
    for e in c:
        if e.dxftype() == "INSERT" and d < 3:
            try: walk(e.virtual_entities(), d + 1)
            except Exception: pass
        else:
            emit(e)
walk([ins])

xs = [p[0] for s in segs for p in s]
ys = [p[1] for s in segs for p in s]
x0, x1 = min(xs), max(xs)
y0, y1 = min(ys), max(ys)
print("plan %s extents: X %.0f..%.0f (%.0f cm)  Y %.0f..%.0f (%.0f cm)"
      % (target, x0, x1, x1 - x0, y0, y1, y1 - y0))
print("mamad label points:", len(marks))

fig = plt.figure(figsize=(18, 14), dpi=120)
ax = fig.add_axes([0.02, 0.02, 0.96, 0.96])
fig.patch.set_facecolor("white"); ax.set_facecolor("white")
for (a, b) in segs:
    ax.add_line(Line2D([a[0], b[0]], [a[1], b[1]], color="#222222", linewidth=0.7))

for i, (mx, my) in enumerate(sorted(marks, key=lambda p: (-p[1], p[0]))):
    ax.plot([mx], [my], marker="o", ms=26, mfc="none", mec="#d00000", mew=2.5)
    ax.annotate(chr(65 + i), (mx, my), color="#d00000",
                fontsize=17, weight="bold", ha="center", va="center")

# סרגל 5 מטר
bx, by = x0 + 60, y0 + 60
ax.add_line(Line2D([bx, bx + 500], [by, by], color="blue", linewidth=3))
ax.annotate("5.00 m  (500 units)", (bx, by + 40), color="blue", fontsize=13)

ax.set_xlim(x0 - 100, x1 + 100); ax.set_ylim(y0 - 100, y1 + 100)
ax.set_aspect("equal"); ax.set_axis_off()
fig.savefig(out, facecolor="white")
print("saved", out)
