# -*- coding: utf-8 -*-
"""רינדור נקי: קירות בלבד, בלי הצללות ובלי טקסט.
   python render_walls.py out.png cx cy half [layer-substr ...]
"""
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
from matplotlib.patches import Polygon
import ezdxf, sys, math
from ezdxf.math import Vec3

out = sys.argv[1]
cx, cy, half = float(sys.argv[2]), float(sys.argv[3]), float(sys.argv[4])
subs = [s.upper() for s in sys.argv[5:]] or ["WALL"]

doc = ezdxf.readfile("P-001.dxf")
msp = doc.modelspace()

x0, x1 = cx - half, cx + half
y0, y1 = cy - half, cy + half

def want(layer):
    L = layer.upper()
    return any(s in L for s in subs)

segs = []
arcs = []

def emit(e):
    t = e.dxftype()
    if not want(e.dxf.layer):
        return
    if t == "LINE":
        segs.append(((e.dxf.start.x, e.dxf.start.y), (e.dxf.end.x, e.dxf.end.y)))
    elif t == "LWPOLYLINE":
        pts = [(p[0], p[1]) for p in e.get_points()]
        closed = e.closed
        for i in range(len(pts) - 1):
            segs.append((pts[i], pts[i + 1]))
        if closed and len(pts) > 2:
            segs.append((pts[-1], pts[0]))
    elif t == "ARC":
        c = e.dxf.center
        r = e.dxf.radius
        a0, a1 = math.radians(e.dxf.start_angle), math.radians(e.dxf.end_angle)
        if a1 < a0:
            a1 += 2 * math.pi
        n = max(6, int((a1 - a0) / 0.12))
        pts = [(c.x + r * math.cos(a0 + (a1 - a0) * i / n),
                c.y + r * math.sin(a0 + (a1 - a0) * i / n)) for i in range(n + 1)]
        for i in range(n):
            segs.append((pts[i], pts[i + 1]))
    elif t == "CIRCLE":
        c = e.dxf.center; r = e.dxf.radius
        pts = [(c.x + r * math.cos(2 * math.pi * i / 40),
                c.y + r * math.sin(2 * math.pi * i / 40)) for i in range(41)]
        for i in range(40):
            segs.append((pts[i], pts[i + 1]))

def walk(container, depth=0):
    for e in container:
        if e.dxftype() == "INSERT" and depth < 3:
            try:
                walk(e.virtual_entities(), depth + 1)
            except Exception:
                pass
        else:
            emit(e)

walk(msp)

# רק מה שנמצא בחלון
def inwin(s):
    (ax, ay), (bx, by) = s
    return not (max(ax, bx) < x0 or min(ax, bx) > x1 or
                max(ay, by) < y0 or min(ay, by) > y1)

segs = [s for s in segs if inwin(s)]
print("segments in window:", len(segs))

fig = plt.figure(figsize=(16, 16), dpi=120)
ax = fig.add_axes([0, 0, 1, 1])
fig.patch.set_facecolor("white"); ax.set_facecolor("white")
for (a, b) in segs:
    ax.add_line(Line2D([a[0], b[0]], [a[1], b[1]], color="black", linewidth=0.8))
ax.set_xlim(x0, x1); ax.set_ylim(y0, y1)
ax.set_aspect("equal"); ax.set_axis_off()
fig.savefig(out, facecolor="white")
print("saved", out)
