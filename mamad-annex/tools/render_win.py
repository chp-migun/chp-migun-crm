# -*- coding: utf-8 -*-
"""רינדור חלון מתוך התוכנית. שימוש:
   python render_win.py out.png            → כל התוכנית
   python render_win.py out.png x0 y0 x1 y1 → חלון
"""
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import ezdxf, sys
from ezdxf.addons.drawing import RenderContext, Frontend
from ezdxf.addons.drawing.matplotlib import MatplotlibBackend
from ezdxf.addons.drawing.config import Configuration

doc = ezdxf.readfile("P-001.dxf")
msp = doc.modelspace()
out = sys.argv[1]

fig = plt.figure(figsize=(24, 16), dpi=110)
ax = fig.add_axes([0, 0, 1, 1])
fig.patch.set_facecolor("white")
ax.set_facecolor("white")
ctx = RenderContext(doc)
backend = MatplotlibBackend(ax)
Frontend(ctx, backend, config=Configuration(min_lineweight=8)).draw_layout(
    msp, finalize=False)

if len(sys.argv) >= 6:
    x0, y0, x1, y1 = (float(v) for v in sys.argv[2:6])
    ax.set_xlim(x0, x1)
    ax.set_ylim(y0, y1)
    ax.set_aspect("equal")
else:
    ax.autoscale_view()
    ax.set_aspect("equal")
ax.set_axis_off()
fig.savefig(out, facecolor="white")
print("saved", out, ax.get_xlim(), ax.get_ylim())
