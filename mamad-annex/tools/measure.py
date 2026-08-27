# -*- coding: utf-8 -*-
"""מדידת עוביי קירות סביב נקודה: מאתר זוגות קווים מקבילים וסמוכים.
   python measure.py cx cy half
"""
import ezdxf, sys, math, collections

cx, cy, half = float(sys.argv[1]), float(sys.argv[2]), float(sys.argv[3])
doc = ezdxf.readfile("P-001.dxf")
msp = doc.modelspace()

segs = []
def emit(e):
    if "WALL" not in e.dxf.layer.upper():
        return
    t = e.dxftype()
    if t == "LINE":
        segs.append(((e.dxf.start.x, e.dxf.start.y), (e.dxf.end.x, e.dxf.end.y)))
    elif t == "LWPOLYLINE":
        pts = [(p[0], p[1]) for p in e.get_points()]
        if e.closed and len(pts) > 2:
            pts = pts + [pts[0]]
        for i in range(len(pts) - 1):
            segs.append((pts[i], pts[i + 1]))

def walk(c, d=0):
    for e in c:
        if e.dxftype() == "INSERT" and d < 3:
            try:
                walk(e.virtual_entities(), d + 1)
            except Exception:
                pass
        else:
            emit(e)
walk(msp)

x0, x1, y0, y1 = cx - half, cx + half, cy - half, cy + half
def inwin(s):
    (ax, ay), (bx, by) = s
    return not (max(ax, bx) < x0 or min(ax, bx) > x1 or
                max(ay, by) < y0 or min(ay, by) > y1)
segs = [s for s in segs if inwin(s)]

# רק קווים אורתוגונליים ארוכים
H, V = [], []
for (a, b) in segs:
    dx, dy = b[0] - a[0], b[1] - a[1]
    L = math.hypot(dx, dy)
    if L < 40:
        continue
    if abs(dy) < 1.0:
        H.append((round(a[1], 1), min(a[0], b[0]), max(a[0], b[0]), L))
    elif abs(dx) < 1.0:
        V.append((round(a[0], 1), min(a[1], b[1]), max(a[1], b[1]), L))

print("orthogonal wall lines in window:  H=%d  V=%d" % (len(H), len(V)))

def pairs(group, label):
    c = collections.Counter()
    for i in range(len(group)):
        for j in range(i + 1, len(group)):
            p, q = group[i], group[j]
            d = abs(p[0] - q[0])
            if 8 <= d <= 60:
                # חפיפה לאורך הקיר
                ov = min(p[2], q[2]) - max(p[1], q[1])
                if ov > 80:
                    c[round(d)] += 1
    print("\n%s wall thicknesses (units), by frequency:" % label)
    for k, v in c.most_common(14):
        print("   %3d units  x%d" % (k, v))
    return c

ch = pairs(H, "horizontal")
cv = pairs(V, "vertical")
tot = ch + cv
print("\ncombined top values:", [k for k, _ in tot.most_common(8)])
