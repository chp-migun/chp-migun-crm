# -*- coding: utf-8 -*-
"""זום על ממ"ד עם רשת 50 ס"מ וקואורדינטות, לאיתור פינות מדויק.
   python zoom_mamad.py out.png cx cy half
"""
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
import ezdxf, sys, math

out = sys.argv[1]
cx, cy, half = float(sys.argv[2]), float(sys.argv[3]), float(sys.argv[4])
doc = ezdxf.readfile("P-001.dxf")
msp = doc.modelspace()

segs = []
def emit(e):
    lay = e.dxf.layer.upper()
    t = e.dxftype()
    if not any(k in lay for k in ("WALL", "DOOR", "GLAZ", "WIND")):
        return
    if t == "LINE":
        segs.append((((e.dxf.start.x, e.dxf.start.y), (e.dxf.end.x, e.dxf.end.y)), lay))
    elif t == "LWPOLYLINE":
        pts = [(p[0], p[1]) for p in e.get_points()]
        if e.closed and len(pts) > 2:
            pts = pts + [pts[0]]
        for i in range(len(pts) - 1):
            segs.append(((pts[i], pts[i + 1]), lay))
    elif t == "ARC":
        c = e.dxf.center; r = e.dxf.radius
        a0, a1 = math.radians(e.dxf.start_angle), math.radians(e.dxf.end_angle)
        if a1 < a0: a1 += 2 * math.pi
        n = max(8, int((a1 - a0) / 0.1))
        pts = [(c.x + r * math.cos(a0 + (a1 - a0) * i / n),
                c.y + r * math.sin(a0 + (a1 - a0) * i / n)) for i in range(n + 1)]
        for i in range(n):
            segs.append(((pts[i], pts[i + 1]), lay))

def walk(c, d=0):
    for e in c:
        if e.dxftype() == "INSERT" and d < 3:
            try: walk(e.virtual_entities(), d + 1)
            except Exception: pass
        else: emit(e)
walk(msp)

x0, x1, y0, y1 = cx - half, cx + half, cy - half, cy + half
def inwin(s):
    (a, b) = s
    return not (max(a[0], b[0]) < x0 or min(a[0], b[0]) > x1 or
                max(a[1], b[1]) < y0 or min(a[1], b[1]) > y1)
segs = [(s, l) for (s, l) in segs if inwin(s)]
print("segments:", len(segs))

fig = plt.figure(figsize=(15, 15), dpi=130)
ax = fig.add_axes([0.06, 0.06, 0.92, 0.92])
fig.patch.set_facecolor("white"); ax.set_facecolor("white")

g = 50.0
gx = math.floor(x0 / g) * g
while gx <= x1:
    major = abs(gx % 500) < 1e-6
    ax.add_line(Line2D([gx, gx], [y0, y1], color="#8ab4f8" if major else "#e3ecfb",
                       linewidth=1.0 if major else 0.6, zorder=0))
    gx += g
gy = math.floor(y0 / g) * g
while gy <= y1:
    major = abs(gy % 500) < 1e-6
    ax.add_line(Line2D([x0, x1], [gy, gy], color="#8ab4f8" if major else "#e3ecfb",
                       linewidth=1.0 if major else 0.6, zorder=0))
    gy += g

for (a, b), lay in segs:
    col = "#c00000" if ("DOOR" in lay or "GLAZ" in lay or "WIND" in lay) else "#111111"
    ax.add_line(Line2D([a[0], b[0]], [a[1], b[1]], color=col, linewidth=1.4, zorder=2))

ax.set_xlim(x0, x1); ax.set_ylim(y0, y1); ax.set_aspect("equal")
ax.set_xticks([t for t in range(int(math.ceil(x0 / 100) * 100), int(x1) + 1, 100)])
ax.set_yticks([t for t in range(int(math.ceil(y0 / 100) * 100), int(y1) + 1, 100)])
ax.tick_params(labelsize=8)
ax.grid(False)
ax.set_title("grid 50 cm  |  heavy line = 5 m  |  red = door/window layers", fontsize=11)
fig.savefig(out, facecolor="white")
print("saved", out)
