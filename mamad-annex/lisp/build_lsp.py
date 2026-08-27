#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
build_lsp.py — בונה את migun.lsp מתוך migun.lsp.src

למה זה נחוץ: אוטוקאד קורא קובצי .lsp בקידוד ANSI של המערכת, לא UTF-8.
  * מחרוזות שהולכות לישויות שרטוט — נעטפות במקור ב-(hs "...") ומומרות
    כאן ל-\\U+XXXX escapes (ASCII נקי, עובד בכל התקנה).
  * הודעות מסך (princ/getpoint וכו') — מומרות לבייטים של Windows-1255,
    שעובדים בחלונות עברי. גרש/גרשיים עבריים מוחלפים ב-ASCII.
  * בדיקת איזון סוגריים בסיסית לפני כתיבה.

שימוש:  python3 build_lsp.py
"""
import io, re, sys, os, hashlib

SRC = os.path.join(os.path.dirname(__file__), 'migun.lsp.src')
DST = os.path.join(os.path.dirname(__file__), 'migun.lsp')

def to_u_escapes(m):
    inner = m.group(1)
    out = []
    i = 0
    while i < len(inner):
        c = inner[i]
        if c == '\\' and i+1 < len(inner):        # escape קיים — עובר כמו שהוא
            out.append(inner[i:i+2]); i += 2; continue
        o = ord(c)
        if c == '״': out.append('\\"')
        elif c == '׳': out.append("'")
        elif o > 126: out.append('\\U+%04X' % o)
        else: out.append(c)
        i += 1
    return '"' + ''.join(out) + '"'

def main():
    s = io.open(SRC, encoding='utf-8').read()

    # חותמת בנייה: 8 תווים מתוך SHA-256 של המקור.
    # דטרמיניסטית — אותו מקור נותן אותה חותמת, כך שאין diff מיותר בגיט,
    # ובכל זאת אפשר לוודא באוטוקאד שנטענה הגרסה הנכונה.
    if '@@BUILD@@' not in s:
        sys.exit('build stamp placeholder @@BUILD@@ missing from src')
    stamp = hashlib.sha256(s.encode('utf-8')).hexdigest()[:8]
    s = s.replace('@@BUILD@@', stamp)

    # (hs "...") → מחרוזת עם \U+ escapes
    s = re.sub(r'\(hs\s+"((?:[^"\\]|\\.)*)"\)', to_u_escapes, s)

    # נירמול גרשיים עבריים בשאר הטקסט
    s = s.replace('״', '\\"').replace('׳', "'").replace('־', '-')

    # הערות עלולות להכיל סוגריים — בדיקה מדויקת יותר: הסרתן קודם
    nc = re.sub(r';[^\n]*', '', s)
    bal = 0; instr = False; esc = False
    for ch in nc:
        if instr:
            if esc: esc = False
            elif ch == '\\': esc = True
            elif ch == '"': instr = False
            continue
        if ch == '"': instr = True
        elif ch == '(': bal += 1
        elif ch == ')': bal -= 1
    if bal != 0:
        sys.exit(f'unbalanced parens: {bal:+d}')

    data = s.encode('cp1255', errors='replace')
    io.open(DST, 'wb').write(data)
    n_u = s.count('\\U+')
    print(f'OK: {DST}  ({len(data):,} bytes, {n_u} \\U+ escapes, parens balanced)')
    print(f'    build stamp: {stamp}   <-- MMDTEST must print this')

if __name__ == '__main__':
    main()
