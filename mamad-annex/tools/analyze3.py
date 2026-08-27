# -*- coding: utf-8 -*-
"""לכל תוכנית קומה (X1..X11): מה שמה, איפה היא, וכמה ממ"דים יש בה."""
import ezdxf, io, re, collections
from ezdxf.math import Vec3

BS = chr(92)
RE_U   = re.compile(BS + BS + r'U\+([0-9A-Fa-f]{4})')
RE_FMT = re.compile(BS + BS + r'[a-zA-Z][^;]*;')
def clean(t):
    t = RE_U.sub(lambda m: chr(int(m.group(1), 16)), t)
    t = t.replace(BS + 'P', ' ').replace(BS + '~', ' ')
    t = RE_FMT.sub('', t)
    return ' '.join(t.replace('{', '').replace('}', '').split())

doc = ezdxf.readfile("P-001.dxf")
msp = doc.modelspace()
out = []
def w(s):
    out.append(str(s)); print(s)

# ---- 1. הכיתובים במודל (כותרות התוכניות) ----
w("=== modelspace texts ===")
for e in msp.query("MTEXT TEXT"):
    t = clean(e.dxf.text if e.dxftype() == "TEXT" else e.text)
    if t:
        p = e.dxf.insert
        w("  (%9.0f,%9.0f)  %s" % (p.x, p.y, t[:60]))

# ---- 2. כל insert של תוכנית קומה ----
w("\n=== floor-plan inserts ===")
plans = []
for e in msp.query("INSERT"):
    if re.fullmatch(r"X\d+", e.dxf.name):
        p = e.dxf.insert
        plans.append((e.dxf.name, e))
        w("  %-5s at (%9.0f,%9.0f)  scale=%.3f rot=%.1f"
          % (e.dxf.name, p.x, p.y, e.dxf.xscale, e.dxf.rotation))

# ---- 3. ממ"דים בתוך כל תוכנית ----
KEYS = ("ממ", "מרחב", "מוגן", "מקלט")
w("\n=== protected spaces per plan ===")
summary = []
for name, ins in plans:
    hits = []
    try:
        ents = list(ins.virtual_entities())
    except Exception as ex:
        w("  %s: virtual_entities failed: %s" % (name, ex))
        continue
    for ve in ents:
        if ve.dxftype() in ("TEXT", "MTEXT"):
            t = clean(ve.dxf.text if ve.dxftype() == "TEXT" else ve.text)
            if any(k in t for k in KEYS):
                p = ve.dxf.insert
                hits.append((round(p.x), round(p.y), t))
        elif ve.dxftype() == "INSERT":
            try:
                for ve2 in ve.virtual_entities():
                    if ve2.dxftype() in ("TEXT", "MTEXT"):
                        t = clean(ve2.dxf.text if ve2.dxftype() == "TEXT" else ve2.text)
                        if any(k in t for k in KEYS):
                            p = ve2.dxf.insert
                            hits.append((round(p.x), round(p.y), t))
            except Exception:
                pass
    xs = [h[0] for h in hits]
    ys = [h[1] for h in hits]
    w("  %-5s  entities=%-6d  matches=%d" % (name, len(ents), len(hits)))
    for h in sorted(hits, key=lambda r: (-r[1], r[0])):
        w("        (%8d,%9d)  %s" % h)
    summary.append((name, len(hits), xs, ys))

io.open("plans.txt", "w", encoding="utf-8").write("\n".join(out))
print("\nwritten to plans.txt")
