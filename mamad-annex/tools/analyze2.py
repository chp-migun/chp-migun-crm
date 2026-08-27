# -*- coding: utf-8 -*-
"""מיפוי מבנה הקובץ: מודל מול פריסות, INSERT-ים והקנה מידה שלהם."""
import ezdxf, io, re, collections

BS = chr(92)
RE_U   = re.compile(BS + BS + r'U\+([0-9A-Fa-f]{4})')
RE_FMT = re.compile(BS + BS + r'[a-zA-Z][^;]*;')
def clean(t):
    t = RE_U.sub(lambda m: chr(int(m.group(1), 16)), t)
    t = t.replace(BS + 'P', ' ').replace(BS + '~', ' ')
    t = RE_FMT.sub('', t)
    return ' '.join(t.replace('{', '').replace('}', '').split())

doc = ezdxf.readfile("P-001.dxf")
out = []
def w(s):
    out.append(str(s)); print(s)

w("=== LAYOUTS ===")
for name in doc.layout_names():
    lay = doc.layouts.get(name)
    n = len(list(lay))
    w("  %-30s entities=%d" % (name, n))

msp = doc.modelspace()
w("\n=== MODELSPACE entity types ===")
c = collections.Counter(e.dxftype() for e in msp)
for k, v in c.most_common():
    w("  %-14s %d" % (k, v))

w("\n=== MODELSPACE inserts ===")
ins = collections.Counter()
for e in msp.query("INSERT"):
    ins[(e.dxf.name, round(e.dxf.xscale, 4))] += 1
for (nm, sc), v in ins.most_common(25):
    w("  x%-4d scale=%-10s %s" % (v, sc, nm[:55]))

w("\n=== VIEWPORTS (paper space) ===")
for name in doc.layout_names():
    if name.lower() == "model":
        continue
    lay = doc.layouts.get(name)
    for vp in lay.query("VIEWPORT"):
        h = vp.dxf.get("view_height", 0)
        hh = vp.dxf.get("height", 0)
        if h:
            w("  [%s] view_height=%.2f  vp_height=%.2f  ratio=%.4f  center=(%.1f,%.1f)"
              % (name, h, hh, (hh / h if h else 0),
                 vp.dxf.view_center_point.x, vp.dxf.view_center_point.y))

w("\n=== biggest block definitions ===")
sizes = []
for b in doc.blocks:
    if b.name.startswith(("*", "_")):
        continue
    sizes.append((len(list(b)), b.name))
sizes.sort(reverse=True)
for n, nm in sizes[:15]:
    w("  %6d  %s" % (n, nm[:60]))

io.open("struct.txt", "w", encoding="utf-8").write("\n".join(out))
