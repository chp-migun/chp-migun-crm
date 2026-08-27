;;; ==================================================================
;;;  MIGUN.LSP — כלי נספחי מיגון ליועץ מיגון
;;;  קובץ זה נוצר מתוך migun.lsp.src על ידי build_lsp.py — אין לערוך ידנית.
;;;
;;;  פקודות:
;;;    MMD      סימון מרחב מוגן בתוכנית האדריכלית (מדידה בקליקים)
;;;    MMDDRAW  יצירת נספח: תוכניות + חתכים + טבלה מרכזת + הערות
;;;    MMDLIST  רשימת המרחבים שנאספו
;;;    MMDDEL   מחיקת מרחב מהרשימה
;;;    MMDCLR   ניקוי הרשימה כולה
;;;    MMDTEST  בדיקה עצמית ללא קליקים — מצייר נספח הדגמה + דוח תקינות
;;;    MMDWIPE  מחיקת כל מה שהכלי צייר
;;;
;;;  הנתונים נשמרים בתוך קובץ ה-DWG (named object dictionary),
;;;  כך שאפשר לסגור ולהמשיך אחר כך.
;;; ==================================================================

(vl-load-com)

;;; ---------- הגדרות ----------
;;; חותמת בנייה — build_lsp.py מחליף את 28b903b1 בחתימה של migun.lsp.src.
;;; אותו מקור ? אותה חותמת (אין "רעש" בגיט). אם MMDTEST מדפיס חותמת
;;; שונה מזו שבמקור — אוטוקאד טען קובץ ישן.
(setq *MG-BUILD* "28b903b1")
(setq *MG-PREFIX* "MIGUN-")
(setq *MG-FONT*   "arial.ttf")
(setq *MG-UNITS*  1.0)   ; 1.0 = השרטוט בס"מ ; 0.01 = השרטוט במטרים ; 10.0 = מ"מ
(setq *MG-FILL*   7.0)   ; עובי מילוי/ריצוף מעל רצפת הבטון (ס"מ) — ברירת מחדל

;;; שכבות: (שם צבע סוג-קו)
(setq *MG-LAYERS* '(("WALL" 7 "Continuous") ("HATCH" 8 "Continuous")
                    ("DOOR" 3 "Continuous") ("WINDOW" 4 "Continuous")
                    ("VENT" 6 "Continuous") ("DIM" 2 "Continuous")
                    ("TEXT" 7 "Continuous") ("TABLE" 7 "Continuous")
                    ("SECTION" 1 "Continuous") ("HIDDEN" 5 "HIDDEN2")
                    ("MARK" 6 "DASHED2")))

;;; ---------- עזרי בסיס ----------
(defun mg:lay (nm / e) (strcat *MG-PREFIX* nm))

(defun mg:mklayers ( / rec nm)
  (foreach rec *MG-LAYERS*
    (setq nm (strcat *MG-PREFIX* (car rec)))
    (if (not (tblsearch "LAYER" nm))
      (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord")
                     '(100 . "AcDbLayerTableRecord")
                     (cons 2 nm) '(70 . 0) (cons 62 (cadr rec))
                     (cons 6 (mg:lt (caddr rec))))))))

(defun mg:lt (nm)
  (if (not (tblsearch "LTYPE" nm))
    (foreach f '("acadiso.lin" "acad.lin")
      (if (not (tblsearch "LTYPE" nm))
        (vl-catch-all-apply 'vl-cmdf (list "_.-LINETYPE" "_Load" nm f "")))))
  (if (tblsearch "LTYPE" nm) nm "Continuous"))

(defun mg:style ( / nm)
  (setq nm (strcat *MG-PREFIX* "TXT"))
  (if (not (tblsearch "STYLE" nm))
    (entmake (list '(0 . "STYLE") '(100 . "AcDbSymbolTableRecord")
                   '(100 . "AcDbTextStyleTableRecord")
                   (cons 2 nm) '(70 . 0) '(40 . 0.0) '(41 . 1.0)
                   '(50 . 0.0) '(71 . 0) '(42 . 2.5)
                   (cons 3 *MG-FONT*) '(4 . ""))))
  nm)

;;; יוצרי ישויות — entmake בלבד
(defun mg:line (ly p1 p2)
  (entmake (list '(0 . "LINE") '(100 . "AcDbEntity") (cons 8 (mg:lay ly))
                 '(100 . "AcDbLine") (cons 10 p1) (cons 11 p2))))

(defun mg:pline (ly pts closed / d)
  (setq d (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 (mg:lay ly))
                '(100 . "AcDbPolyline") (cons 90 (length pts))
                (cons 70 (if closed 1 0))))
  (foreach p pts (setq d (append d (list (cons 10 (list (car p) (cadr p)))))))
  (entmake d))

(defun mg:rect (ly x1 y1 x2 y2)
  (mg:pline ly (list (list x1 y1) (list x2 y1) (list x2 y2) (list x1 y2)) T))

(defun mg:circle (ly c r)
  (entmake (list '(0 . "CIRCLE") '(100 . "AcDbEntity") (cons 8 (mg:lay ly))
                 '(100 . "AcDbCircle") (cons 10 c) (cons 40 r))))

(defun mg:arc (ly c r a0 a1)
  (entmake (list '(0 . "ARC") '(100 . "AcDbEntity") (cons 8 (mg:lay ly))
                 '(100 . "AcDbCircle") (cons 10 c) (cons 40 r)
                 '(100 . "AcDbArc") (cons 50 a0) (cons 51 a1))))

;;; טקסט: ha: 0=שמאל 1=מרכז 2=ימין ; va: 0=בסיס 1=תחתית 2=אמצע 3=עליון
(defun mg:text (ly p h s ha va / d)
  (setq d (list '(0 . "TEXT") '(100 . "AcDbEntity") (cons 8 (mg:lay ly))
                '(100 . "AcDbText") (cons 10 p) (cons 40 h) (cons 1 s)
                '(50 . 0.0) (cons 7 (strcat *MG-PREFIX* "TXT"))
                (cons 72 ha) (cons 11 p) '(100 . "AcDbText") (cons 73 va)))
  (entmake d))

;;; hatch אלכסוני 45° בין שני אלכסונים — קווים מפורשים
(defun mg:hatch (ly x1 y1 x2 y2 sp / step c xa xb)
  (if (and (> x2 x1) (> y2 y1))
    (progn
      (setq step (* sp (sqrt 2.0)))
      (setq c (* step (float (fix (/ (- y1 x2) step)))))
      (while (<= c (- y2 x1))
        (setq xa (max x1 (- y1 c)) xb (min x2 (- y2 c)))
        (if (> (- xb xa) 1e-6)
          (mg:line ly (list xa (+ xa c)) (list xb (+ xb c))))
        (setq c (+ c step))))))

(defun mg:hrect (lyr lyh x1 y1 x2 y2 sp hatch?)
  (mg:rect lyr x1 y1 x2 y2)
  (if hatch? (mg:hatch lyh x1 y1 x2 y2 sp)))

;;; מידה ליניארית אמיתית
(defun mg:dim (h? p1 p2 pd)
  (setvar "CLAYER" (mg:lay "DIM"))
  (vl-catch-all-apply 'vl-cmdf
    (list "_.DIMLINEAR" p1 p2 (if h? "_H" "_V") pd)))

;;; וקטורים
(defun v+ (a b) (mapcar '+ a b))
(defun v- (a b) (mapcar '- a b))
(defun v* (a s) (mapcar '(lambda (x) (* x s)) a))
(defun vdot (a b) (apply '+ (mapcar '* a b)))
(defun vlen (a) (sqrt (vdot a a)))
(defun vnorm (a) (v* a (/ 1.0 (max 1e-12 (vlen a)))))
(defun mg:2d (p) (list (car p) (cadr p)))

;;; ---------- שמירה בקובץ ה-DWG ----------
(defun mg:save ()
  (vlax-ldata-put "MIGUN-ANNEX" "spaces" *MMD-LIST*))
(defun mg:load ()
  (setq *MMD-LIST* (cond ((vlax-ldata-get "MIGUN-ANNEX" "spaces")) (nil))))
(mg:load)

;;; ==================================================================
;;;  MMD — איסוף מרחב מוגן מהתוכנית האדריכלית
;;; ==================================================================
;;; מבנה רשומה:
;;; (id L W H (tN tE tS tW) tCeil tFloor
;;;  ((type side off w h sill hinge) ...))
;;;  type: "door" | "win" | "vent" ; side: "N"|"E"|"S"|"W"
;;; ==================================================================

(defun mg:localize (p org u v)
  (list (vdot (v- (mg:2d p) org) u) (vdot (v- (mg:2d p) org) v)))

;;; זיהוי הפאה הקרובה לנקודה מקומית
(defun mg:nearside (lp L W / x y ds best)
  (setq x (car lp) y (cadr lp))
  (setq ds (list (list (abs y) "S") (list (abs (- W y)) "N")
                 (list (abs x) "W") (list (abs (- L x)) "E")))
  (setq best (car ds))
  (foreach d (cdr ds) (if (< (car d) (car best)) (setq best d)))
  (cadr best))

;;; מיקום לאורך הקיר (מרחק מראשית הצלע)
(defun mg:alongwall (lp side)
  (if (or (= side "S") (= side "N")) (car lp) (cadr lp)))

(defun mg:getreal-def (msg def / r)
  (setq r (getreal (strcat "\n" msg " <" (rtos def 2 1) ">: ")))
  (if r r def))

(defun mg:getthick (msg base def / p r)
  ;; מקבל מספר מוקלד או מרחק שנמדד בקליק מנקודת הבסיס
  (setq r (getdist base (strcat "\n" msg " <" (rtos def 2 1) ">: ")))
  (if r r def))

(defun c:MMD ( / *error* osm id p1 p2 p3 org u v d3 L W tmp H
                 tN tE tS tW tC tF fl ops go typ p q lp lq side off
                 opw hgt sill hinge cen dia zc mid rec n dtyp)
  (defun *error* (m)
    (if osm (setvar "OSMODE" osm))
    (if (and m (not (wcmatch (strcase m) "*BREAK*,*CANCEL*,*QUIT*")))
      (princ (strcat "\nMMD: " m)))
    (princ))

  (mg:mklayers) (mg:style)
  (if (null *MMD-LIST*) (mg:load))

  ;; מזהה
  (setq n (1+ (length *MMD-LIST*)))
  (setq id (getstring T (strcat "\nשם המרחב <" "MM-" (itoa n) ">: ")))
  (if (= id "") (setq id (strcat "MM-" (itoa n))))

  ;; שלוש נקורות: פינה, כיוון, פינה נגדית
  (setq osm (getvar "OSMODE"))
  (setvar "OSMODE" 33)  ; endpoint + intersection — כאן דווקא רוצים הצמדה
  (setq p1 (getpoint "\nפינה פנימית ראשונה של המרחב: "))
  (if (null p1) (exit))
  (setq p2 (getpoint p1 "\nנקודה על הפאה הפנימית לאורך (קובעת את כיוון ה-אורך): "))
  (if (null p2) (exit))
  (setq p3 (getpoint p1 "\nהפינה הפנימית הנגדית: "))
  (if (null p3) (exit))

  (setq org (mg:2d p1))
  (setq u (vnorm (v- (mg:2d p2) org)))
  (setq v (list (- (cadr u)) (car u)))           ; ניצב שמאלה
  (setq d3 (mg:localize p3 org u v))
  ;; נירמול: L,W חיוביים — הפיכת צירים במקרה הצורך
  (if (< (car d3) 0) (setq u (v* u -1.0)))
  (setq v (list (- (cadr u)) (car u)))
  (if (< (vdot (v- (mg:2d p3) org) v) 0) (setq v (v* v -1.0)))
  (setq d3 (mg:localize p3 org u v))
  (setq L (car d3) W (cadr d3))
  (princ (strcat "\nנמדד: " (rtos L 2 1) " x " (rtos W 2 1)
                 " (שטח " (rtos (/ (* L W) 10000.0) 2 2) " מ\"ר)"))

  ;; עוביים — קליק על הפאה החיצונית או הקלדה
  (setvar "OSMODE" 512)  ; nearest — לקליק על פאה חיצונית
  (setq mid (v+ org (v+ (v* u (* 0.5 L)) (v* v W))))
  (setq tN (mg:getthick "עובי קיר צפון (הפאה הרחוקה מהפינה הראשונה)" mid 25.0))
  (setq mid (v+ org (v* v (* 0.5 W))))
  (setq tW (mg:getthick "עובי קיר מערב" mid 25.0))
  (setq mid (v+ org (v* u (* 0.5 L))))
  (setq tS (mg:getthick "עובי קיר דרום (קיר הפינה הראשונה)" mid 25.0))
  (setq mid (v+ org (v+ (v* u L) (v* v (* 0.5 W)))))
  (setq tE (mg:getthick "עובי קיר מזרח" mid 25.0))
  (setvar "OSMODE" 0)

  (setq H  (mg:getreal-def "גובה פנים נטו (ס\"מ)" 250.0))
  (setq tC (mg:getreal-def "עובי תקרה" 20.0))
  (setq tF (mg:getreal-def "עובי רצפת בטון" 15.0))
  (setq fl (mg:getreal-def "עובי מילוי/ריצוף מעל הבטון" *MG-FILL*))

  ;; פתחים
  (setq ops '() go T)
  (setvar "OSMODE" 33)
  (while go
    (initget "Delet Halon Tzinor Enter")
    (setq typ (getkword "\nהוסף פתח [דלת(D)/חלון(H)/צינור(T)] או Enter לסיום: "))
    (cond
      ((or (null typ) (= typ "Enter")) (setq go nil))

      ((= typ "Delet")
       (setq p (getpoint "\nמשקוף ראשון על הפאה הפנימית: "))
       (setq q (getpoint p "\nמשקוף שני: "))
       (if (and p q)
         (progn
           (setq lp (mg:localize p org u v) lq (mg:localize q org u v))
           (setq side (mg:nearside lp L W))
           (setq off (min (mg:alongwall lp side) (mg:alongwall lq side)))
           (setq opw (abs (- (mg:alongwall lp side) (mg:alongwall lq side))))
           (setq hgt (mg:getreal-def "גובה הדלת" 200.0))
           (initget "1 2 3 4 5 7 0")
           (princ "\n  סוגי דלת: 1=דירתית 2=דלית-צירית 3=דלית-נגררת")
           (princ "\n            4=ורסיסים-משופרת 5=מוסדית/קומתית 7=רסיסים-25מ\"מ 0=אחר")
           (setq dtyp (cond ((getkword "\nסוג דלת [1/2/3/4/5/7/0] <1>: ")) ("1")))
           (setq hinge (getpoint "\nקליק ליד משקוף הציר: "))
           (setq hinge
             (if hinge
               (if (< (abs (- (mg:alongwall (mg:localize hinge org u v) side) off))
                      (abs (- (mg:alongwall (mg:localize hinge org u v) side) (+ off opw))))
                 "A" "B") "A"))
           (setq ops (cons (list "door" side off opw hgt 0.0 hinge dtyp) ops))
           (mg:check-door opw hgt dtyp)
           (princ (strcat "\nדלת " (rtos opw 2 0) "/" (rtos hgt 2 0)
                          " בקיר " side " במרחק " (rtos off 2 0))))))

      ((= typ "Halon")
       (setq p (getpoint "\nצד ראשון של פתח החלון: "))
       (setq q (getpoint p "\nצד שני: "))
       (if (and p q)
         (progn
           (setq lp (mg:localize p org u v) lq (mg:localize q org u v))
           (setq side (mg:nearside lp L W))
           (setq off (min (mg:alongwall lp side) (mg:alongwall lq side)))
           (setq opw (abs (- (mg:alongwall lp side) (mg:alongwall lq side))))
           (setq hgt (mg:getreal-def "גובה החלון" 100.0))
           (setq sill (mg:getreal-def "גובה אדן מהרצפה" 100.0))
           (setq ops (cons (list "win" side off opw hgt sill "A") ops))
           (princ (strcat "\nחלון " (rtos opw 2 0) "/" (rtos hgt 2 0)
                          " בקיר " side)))))

      ((= typ "Tzinor")
       (setq cen (getpoint "\nמרכז הצינור על הקיר: "))
       (if cen
         (progn
           (setq dia (mg:getreal-def "קוטר (ס\"מ)" 20.0))
           (setq zc (mg:getreal-def "גובה מרכז מהרצפה" 190.0))
           (setq lp (mg:localize cen org u v))
           (setq side (mg:nearside lp L W))
           (setq off (- (mg:alongwall lp side) (/ dia 2.0)))
           (setq ops (cons (list "vent" side off dia dia zc "A") ops))
           (mg:check-vent side off dia zc ops L W)
           (princ (strcat "\nצינור " (rtos dia 2 0) " בקיר " side)))))))
  (setvar "OSMODE" osm)

  ;; סימון על גבי התוכנית — שכבת MARK
  (mg:mark org u v L W id)

  ;; fl נוסף בסוף בכוונה — רשומות שנשמרו לפני שהשדה קיים עדיין נטענות.
  (setq rec (list id L W H (list tN tE tS tW) tC tF (reverse ops) fl))
  (setq *MMD-LIST* (append *MMD-LIST* (list rec)))
  (mg:save)
  (princ (strcat "\n=== " id " נוסף. סה\"כ " (itoa (length *MMD-LIST*))
                 " מרחבים. MMDDRAW ליצירת הנספח. ==="))
  (princ))

;;; מסגרת סימון על התוכנית האדריכלית
(defun mg:mark (org u v L W id / c)
  (mg:pline "MARK"
    (list org (v+ org (v* u L)) (v+ org (v+ (v* u L) (v* v W))) (v+ org (v* v W))) T)
  (setq c (v+ org (v+ (v* u (* 0.5 L)) (v* v (* 0.5 W)))))
  (mg:text "MARK" c (* 0.12 (min L W)) id 1 2))

;;; ==================================================================
;;;  בדיקות רכות — התראות בלבד, לעולם לא חוסמות
;;;  מקורות: תשובות המשתמש 26.08.2026; הנחיית ענף הנדסה 10.06.2019;
;;;  טבלת דלתות ת"י 4422
;;; ==================================================================

(defun mg:warn (msg) (princ (strcat "\n  !! " msg)))

(defun mg:check-door (w hgt dtyp)
  ;; רוחבים תקניים לפי סוג הדלת
  (cond
    ((member dtyp '("1" "2" "3" "4"))
     (if (not (member (fix (+ w 0.5)) '(70 80)))
       (mg:warn (strcat "רוחב דלת דירתית " (rtos w 2 0)
                        " — התקניים הם 70/80 ס\"מ"))))
    ((= dtyp "5")
     (if (not (member (fix (+ w 0.5)) '(85 90 100)))
       (mg:warn (strcat "רוחב דלת מוסדית/קומתית " (rtos w 2 0)
                        " — התקניים הם 85/90/100 ס\"מ"))))
    ((= dtyp "7")
     (if (or (< w 70) (> w 100))
       (mg:warn "דלת רסיסים 25 מ\"מ: רוחב 70-100 ס\"מ"))))
  (if (< hgt 200)
    (mg:warn (strcat "גובה דלת " (rtos hgt 2 0) " — מתחת ל-200 ס\"מ"))))

;;; בדיקת צינור לפי טבלת ההנחיה 2019 (ממ"ד): כניסה MIN 150 מהרצפה,
;;; יציאה MIN 190; מרחק מקיר ניצב 25-40 MIN; אסור מעל דלת/חלון;
;;; 60 ס"מ בין צינורות; 25-45 ס"מ מדופן פתח
(defun mg:check-vent (side off dia zc ops L W / span gap o mind)
  (setq span (if (or (= side "S") (= side "N")) L W))
  (setq gap (min off (- span off dia)))
  (if (< gap 25)
    (mg:warn (strcat "צינור במרחק " (rtos gap 2 0)
                     " ס\"מ מקיר ניצב — טבלת ההנחיה דורשת 25-40 לפחות")))
  (if (< zc 150)
    (mg:warn (strcat "מרכז הצינור בגובה " (rtos zc 2 0)
                     " — כניסת אוויר מינימום 150, יציאה 190")))
  ;; מרחק מפתחים אחרים על אותו קיר + איסור מעל פתח
  (foreach o (cdr ops)   ; ops כולל את הצינור עצמו בראש
    (if (= (cadr o) side)
      (progn
        (setq mind (if (= (car o) "vent") 60 25))
        (setq gap (cond ((> off (+ (caddr o) (cadddr o))) (- off (caddr o) (cadddr o)))
                        ((> (caddr o) (+ off dia)) (- (caddr o) off dia))
                        (T -1)))
        (cond
          ((< gap 0)
           (if (/= (car o) "vent")
             (mg:warn (strcat "הצינור מעל/בתוך פתח " (car o)
                              " — אסור מעל דלת או חלון"))))
          ((< gap mind)
           (mg:warn (strcat "מרחק " (rtos gap 2 0) " ס\"מ מ-" (car o)
                            " — נדרש " (itoa mind) " לפחות"))))))))

;;; ==================================================================
;;;  MMDLIB — טעינת ספריית הבלוקים (migun-lib.dwg ליד קובץ ה-LISP)
;;; ==================================================================
(defun mg:libpath ( / base)
  (setq base (findfile "migun-lib.dwg"))
  (if (null base)
    (setq base (findfile (strcat (vl-filename-directory
      (cond ((findfile "migun.lsp")) (""))) "\\migun-lib.dwg"))))
  base)

(defun c:MMDLIB ( / p e0)
  (setq p (mg:libpath))
  (cond
    ((null p)
     (princ "\nלא נמצא migun-lib.dwg. פתח את migun-lib.dxf באוטוקאד ושמור אותו")
     (princ "\nכ-migun-lib.dwg באותה תיקייה שבה נמצא migun.lsp."))
    ((tblsearch "BLOCK" "Prat petach chalon")
     (princ "\nספריית הבלוקים כבר טעונה בשרטוט."))
    (T
     (setq e0 (entlast))
     (vl-catch-all-apply 'vl-cmdf (list "_.-INSERT" p '(0 0 0) 1 1 0))
     ;; מחיקת ההוספה עצמה — ההגדרות המקוננות נשארות בטבלת הבלוקים
     (if (not (eq e0 (entlast))) (entdel (entlast)))
     (princ "\nהספרייה נטענה — הבלוקים זמינים כעת בשרטוט.")))
  (princ))

;;; ==================================================================
;;;  MMDDETAILS — הוספת פרטי פתחים לכל מרחב שנאסף
;;;  משתמש בבלוקים של המשרד: "Prat petach chalon" / "Prat petach bdelet"
;;; ==================================================================
(defun mg:insblock (nm pt scl / )
  (if (tblsearch "BLOCK" nm)
    (progn
      (entmake (list '(0 . "INSERT") '(100 . "AcDbEntity")
                     (cons 8 (mg:lay "TABLE"))
                     '(100 . "AcDbBlockReference")
                     (cons 2 nm) (cons 10 pt)
                     (cons 41 scl) (cons 42 scl) (cons 43 scl)))
      T)
    (progn (mg:warn (strcat "הבלוק \"" nm "\" אינו בשרטוט — הרץ MMDLIB")) nil)))

(defun c:MMDDETAILS ( / p0 x y r o names gap scl)
  (if (null *MMD-LIST*) (mg:load))
  (if (null *MMD-LIST*)
    (progn (princ "\nאין מרחבים — הרץ MMD קודם.") (exit)))
  (setq p0 (getpoint "\nנקודת בסיס לשורת הפרטים: "))
  (if (null p0) (exit))
  (setq scl (getreal "\nקנה מידה להוספת הפרטים <1.0>: "))
  (if (null scl) (setq scl 1.0))
  (setq x (car p0) y (cadr p0) gap 550.0)
  (foreach r *MMD-LIST*
    (foreach o (nth 7 r)
      (cond
        ((= (car o) "win")
         (if (mg:insblock "Prat petach chalon" (list x y 0.0) scl)
           (setq x (- x (* gap scl)))))
        ((= (car o) "door")
         (if (mg:insblock "Prat petach bdelet" (list x y 0.0) scl)
           (setq x (- x (* gap scl))))))))
  (princ "\nהפרטים הוזרקו. ערוך מידות וזיון לפי הפרויקט.")
  (princ))

;;; ==================================================================
;;;  MMDDRAW — יצירת הנספח
;;; ==================================================================

(defun mg:op-z (op)  ; טווח גבהים של פתח: (z0 z1)
  (cond ((= (car op) "win")  (list (nth 5 op) (+ (nth 5 op) (nth 4 op))))
        ((= (car op) "vent") (list (- (nth 5 op) (/ (nth 4 op) 2.0))
                                   (+ (nth 5 op) (/ (nth 4 op) 2.0))))
        (T (list 0.0 (nth 4 op)))))

;;; פיצול צלע לקטעים מלאים סביב פתחיה. מחזיר רשימת (a b) לאורך הצלע
(defun mg:wallsegs (lo hi ops side / xs cur segs o a b)
  (setq xs '())
  (foreach o ops
    (if (= (cadr o) side)
      (setq xs (cons (list (caddr o) (+ (caddr o) (cadddr o))) xs))))
  (setq xs (vl-sort xs '(lambda (a b) (< (car a) (car b)))))
  (setq cur lo segs '())
  (foreach o xs
    (setq a (car o) b (cadr o))
    (if (> a cur) (setq segs (cons (list cur a) segs)))
    (setq cur (max cur b)))
  (if (> hi cur) (setq segs (cons (list cur hi) segs)))
  (reverse segs))

;;; ציור תוכנית של מרחב אחד; ox,oy = ראשית הפנים
(defun mg:plan (rec ox oy th hatch? / id L W H tt tN tE tS tW tC tF ops
                s a b f side off opw hgt hinge sh dir lw px py cx cy dia
                pts d1 d2 d3)
  (setq id (nth 0 rec) L (nth 1 rec) W (nth 2 rec) H (nth 3 rec)
        tt (nth 4 rec) tC (nth 5 rec) tF (nth 6 rec) ops (nth 7 rec))
  (setq tN (nth 0 tt) tE (nth 1 tt) tS (nth 2 tt) tW (nth 3 tt))

  ;; קירות מפוצלים סביב פתחים — ארבע צלעות
  (foreach s (mg:wallsegs (- tW) (+ L tE) ops "S")
    (mg:hrect "WALL" "HATCH" (+ ox (car s)) (- oy tS) (+ ox (cadr s)) oy 8.0 hatch?))
  (foreach s (mg:wallsegs (- tW) (+ L tE) ops "N")
    (mg:hrect "WALL" "HATCH" (+ ox (car s)) (+ oy W) (+ ox (cadr s)) (+ oy W tN) 8.0 hatch?))
  (foreach s (mg:wallsegs 0.0 W ops "W")
    (mg:hrect "WALL" "HATCH" (- ox tW) (+ oy (car s)) ox (+ oy (cadr s)) 8.0 hatch?))
  (foreach s (mg:wallsegs 0.0 W ops "E")
    (mg:hrect "WALL" "HATCH" (+ ox L) (+ oy (car s)) (+ ox L tE) (+ oy (cadr s)) 8.0 hatch?))

  ;; פתחים
  (foreach f ops
    (setq side (cadr f) off (caddr f) opw (cadddr f) hgt (nth 4 f) hinge (nth 6 f))
    ;; מסגרת מקומית של הצלע: פאה פנימית in?out
    (cond
      ((= side "S") (setq px (+ ox off) py oy       dir '(0 -1) lw tS))
      ((= side "N") (setq px (+ ox off) py (+ oy W) dir '(0 1)  lw tN))
      ((= side "W") (setq px ox py (+ oy off)       dir '(-1 0) lw tW))
      ((= side "E") (setq px (+ ox L) py (+ oy off) dir '(1 0)  lw tE)))
    (setq d1 (if (or (= side "S") (= side "N")) '(1 0) '(0 1)))  ; לאורך הקיר

    (cond
      ((= (car f) "door") (mg:dr-door px py d1 dir lw opw hinge))
      ((= (car f) "win")  (mg:dr-win  px py d1 dir lw opw))
      (T                  (mg:dr-vent px py d1 dir lw opw))))

  ;; כיתוב מרכזי: שם, שטח, נפח — בתוך המרחב עצמו
  (mg:text "TEXT" (list (+ ox (/ L 2.0)) (+ oy (/ W 2.0) (* th 1.9))) (* th 1.5) id 1 0)
  (mg:text "TEXT" (list (+ ox (/ L 2.0)) (+ oy (/ W 2.0) (* th 0.1))) th
           (strcat (rtos (mg:area rec) 2 2) " \U+05DE\"\U+05E8") 1 0)
  (mg:text "TEXT" (list (+ ox (/ L 2.0)) (+ oy (/ W 2.0) (* th -1.6))) th
           (strcat (rtos (mg:vol rec) 2 2) " \U+05DE\"\U+05E7") 1 0)

  ;; מידות + בלוק נתונים צמוד לתוכנית (במקום טבלה מרכזת)
  (mg:dims rec ox oy)
  (mg:info rec ox oy th))

;;; שטח נטו במ"ר ונפח נטו במ"ק — הקלט בס"מ
(defun mg:area (rec) (/ (* (nth 1 rec) (nth 2 rec)) 10000.0))
(defun mg:vol  (rec) (/ (* (nth 1 rec) (nth 2 rec) (nth 3 rec)) 1000000.0))

;;; עובי המילוי/ריצוף. נשמר בסוף הרשומה, כך שרשומות ישנות (בלי השדה)
;;; עדיין נטענות — nth מחזיר nil ואז נופלים לברירת המחדל.
(defun mg:fill (rec / v) (if (setq v (nth 8 rec)) v *MG-FILL*))

;;; בלוק נתונים מתחת לתוכנית של אותו מרחב.
;;; המשתמש ביקש שלא תהיה טבלה מרכזת — כל האינפורמציה על הממ"ד עצמו.
(defun mg:info (rec ox oy th / tt y sw d1 o lin)
  (setq tt (nth 4 rec))
  (setq d1 (* 2.5 (getvar "DIMTXT") (getvar "DIMSCALE")))
  ;; הבלוק חייב לשבת מתחת לכל מה שבולט כלפי מטה: שרשרת המידות
  ;; החיצונית, הכוללת, וקשת הדלת הדרומית (שרדיוסה כרוחב הדלת).
  (setq sw 0.0)
  (foreach o (nth 7 rec)
    (if (and (= (car o) "door") (= (cadr o) "S"))
      (setq sw (max sw (cadddr o)))))
  (setq y (- oy (nth 2 tt) (max (* 3.0 d1) (+ sw (* 2.0 th))) (* 2.0 th)))
  (defun lin (s) (mg:text "TEXT" (list ox y) (* th 0.9) s 0 0)
                 (setq y (- y (* th 1.7))))
  (lin (strcat "\U+05E4\U+05E0\U+05D9\U+05DD " (rtos (nth 1 rec) 2 0) "/" (rtos (nth 2 rec) 2 0)
               " \U+05D2\U+05D5\U+05D1\U+05D4 " (rtos (nth 3 rec) 2 0)))
  (lin (strcat "\U+05E7\U+05D9\U+05E8\U+05D5\U+05EA \U+05E6/\U+05DE\U+05D6/\U+05D3/\U+05DE\U+05E2 "
               (rtos (nth 0 tt) 2 0) "/" (rtos (nth 1 tt) 2 0) "/"
               (rtos (nth 2 tt) 2 0) "/" (rtos (nth 3 tt) 2 0)))
  (lin (strcat "\U+05EA\U+05E7\U+05E8\U+05D4 " (rtos (nth 5 rec) 2 0)
               " \U+05E8\U+05E6\U+05E4\U+05D4 " (rtos (nth 6 rec) 2 0)
               " \U+05DE\U+05D9\U+05DC\U+05D5\U+05D9 " (rtos (mg:fill rec) 2 0)))
  (lin (strcat "\U+05D3\U+05DC\U+05EA " (mg:opdesc rec "door")
               "  \U+05D7\U+05DC\U+05D5\U+05DF " (mg:opdesc rec "win")))
  y)

;;; דלת: כנף + קשת. hinge "A" = בקצה off, "B" = בקצה off+w. תמיד החוצה.
(defun mg:dr-door (px py d1 dir lw w hinge / hx hy ux uy leaf a0 a1 tx ty)
  ;; קו סף על הפאה החיצונית
  (mg:line "DOOR" (list px py)
           (list (+ px (* (car d1) w)) (+ py (* (cadr d1) w))))
  ;; נקודת ציר על הפאה החיצונית
  (if (= hinge "A")
    (setq hx (+ px (* (car dir) lw)) hy (+ py (* (cadr dir) lw)) ux (car d1) uy (cadr d1))
    (setq hx (+ px (* (car d1) w) (* (car dir) lw))
          hy (+ py (* (cadr d1) w) (* (cadr dir) lw))
          ux (- (car d1)) uy (- (cadr d1))))
  ;; כנף בזווית 90 החוצה
  (setq leaf 5.0)
  (mg:pline "DOOR"
    (list (list hx hy)
          (list (+ hx (* (car dir) w)) (+ hy (* (cadr dir) w)))
          (list (+ hx (* (car dir) w) (* ux leaf)) (+ hy (* (cadr dir) w) (* uy leaf)))
          (list (+ hx (* ux leaf)) (+ hy (* uy leaf)))) T)
  ;; קשת מהכנף אל הקיר
  (setq a0 (atan (cadr dir) (car dir)))
  (setq a1 (atan uy ux))
  (if (> (rem (+ (- a1 a0) (* 4 pi)) (* 2 pi)) pi)
    (mg:arc "DOOR" (list hx hy) w a1 a0)
    (mg:arc "DOOR" (list hx hy) w a0 a1)))

;;; חלון: שני קווי הפאות + שני קווים פנימיים
(defun mg:dr-win (px py d1 dir lw w / k p1 p2)
  (foreach k '(0.0 0.35 0.65 1.0)
    (setq p1 (list (+ px (* (car dir) lw k)) (+ py (* (cadr dir) lw k))))
    (setq p2 (list (+ (car p1) (* (car d1) w)) (+ (cadr p1) (* (cadr d1) w))))
    (mg:line "WINDOW" p1 p2)))

;;; צינור: קווי פאות + עיגול
(defun mg:dr-vent (px py d1 dir lw w / p1 p2 c)
  (foreach k '(0.0 1.0)
    (setq p1 (list (+ px (* (car dir) lw k)) (+ py (* (cadr dir) lw k))))
    (setq p2 (list (+ (car p1) (* (car d1) w)) (+ (cadr p1) (* (cadr d1) w))))
    (mg:line "VENT" p1 p2))
  (setq c (list (+ px (* (car d1) w 0.5) (* (car dir) lw 0.5))
                (+ py (* (cadr d1) w 0.5) (* (cadr dir) lw 0.5))))
  (mg:circle "VENT" c (* w 0.5)))

;;; מידות לתוכנית.
;;; המשתמש ביקש (27.08.2026) שמידות הפנים יופיעו **בתוך** המרחב, ושלכל
;;; קיר תהיה מידת עובי משלו. לכן:
;;;   פנימי  — אורך ורוחב נטו, קווי המידה בתוך החדר
;;;   חיצוני — שרשרת הפתחים בצלע הדרומית + כוללת חוץ
;;;   עוביים — מידה קצרה לכל אחד מארבעת הקירות
(defun mg:dims (rec ox oy / L W tt tN tE tS tW ops xs prev x d1 d2 ins)
  (setq L (nth 1 rec) W (nth 2 rec) tt (nth 4 rec) ops (nth 7 rec))
  (setq tN (nth 0 tt) tE (nth 1 tt) tS (nth 2 tt) tW (nth 3 tt))
  (setq d1 (* 2.5 (getvar "DIMTXT") (getvar "DIMSCALE")) d2 (* 2.0 d1))
  ;; מרחק קו המידה הפנימי מפאת הקיר — לא יותר מרבע החדר, שלא ייצא החוצה
  (setq ins (min d1 (* 0.25 (min L W))))

  ;; ---- מידות פנים, בתוך החדר ----
  (mg:dim T   (list ox oy) (list (+ ox L) oy)
              (list (+ ox (/ L 2.0)) (+ oy ins)))
  (mg:dim nil (list ox oy) (list ox (+ oy W))
              (list (+ ox ins) (+ oy (/ W 2.0))))

  ;; ---- שרשרת דרום: פינות + פתחי הצלע ----
  (setq xs (list 0.0 L))
  (foreach o ops
    (if (= (cadr o) "S")
      (setq xs (append xs (list (caddr o) (+ (caddr o) (cadddr o)))))))
  (setq xs (vl-sort xs '<))
  (setq prev (car xs))
  (foreach x (cdr xs)
    (if (> (- x prev) 0.01)
      (mg:dim T (list (+ ox prev) (- oy tS)) (list (+ ox x) (- oy tS))
                (list (+ ox (/ (+ prev x) 2.0)) (- oy tS d1))))
    (setq prev x))
  ;; כוללת חוץ
  (mg:dim T (list (- ox tW) (- oy tS)) (list (+ ox L tE) (- oy tS))
            (list (+ ox (/ L 2.0)) (- oy tS d2)))
  (mg:dim nil (list (- ox tW) (- oy tS)) (list (- ox tW) (+ oy W tN))
              (list (- ox tW d2) (+ oy (/ W 2.0))))

  ;; ---- עובי כל קיר בנפרד ----
  ;; דרום ומזרח: קו המידה מוסט הצידה כדי לא להתנגש בשרשרת ובכוללת
  (mg:dim nil (list (+ ox (* 0.25 L)) (- oy tS)) (list (+ ox (* 0.25 L)) oy)
              (list (+ ox (* 0.25 L)) (- oy (/ tS 2.0))))
  (mg:dim nil (list (+ ox (* 0.75 L)) (+ oy W)) (list (+ ox (* 0.75 L)) (+ oy W tN))
              (list (+ ox (* 0.75 L)) (+ oy W (/ tN 2.0))))
  (mg:dim T   (list (- ox tW) (+ oy (* 0.75 W))) (list ox (+ oy (* 0.75 W)))
              (list (- ox (/ tW 2.0)) (+ oy (* 0.75 W))))
  (mg:dim T   (list (+ ox L) (+ oy (* 0.25 W))) (list (+ ox L tE) (+ oy (* 0.25 W)))
              (list (+ ox L (/ tE 2.0)) (+ oy (* 0.25 W)))))

;;; חתך של מרחב אחד: רצפה, תקרה, קירות מזרח/מערב, פתחי צפון/דרום מקווקווים
;;; שרשרת המידות של החתך — לפי בקשת המשתמש, 27.08.2026:
;;; רצפת בטון · מילוי · מהריצוף עד אדן החלון · גובה החלון · מראש החלון
;;; עד פני התקרה · תקרת בטון. הרמות נגזרות מהפתחים עצמם, כך שכל פתח
;;; נוסף (צינור אוורור למשל) מקבל את המידות שלו אוטומטית.
(defun mg:secdims (rec ox oy / H tt tE tW tC tF fl xr xl xo n zs prev z d1 d2 o zz)
  (setq H (nth 3 rec) tt (nth 4 rec) tC (nth 5 rec) tF (nth 6 rec)
        fl (mg:fill rec))
  (setq tE (nth 1 tt) tW (nth 3 tt))
  (setq d1 (* 2.5 (getvar "DIMTXT") (getvar "DIMSCALE")) d2 (* 2.0 d1))
  (setq xr (- ox tW))                  ; פאה שמאלית — שרשראות הפתחים
  (setq xl (+ ox (nth 1 rec) tE))      ; פאה ימנית — מבנה הרצפה והצנרת

  ;; שרשרת נפרדת לכל פתח, במרחק הולך וגדל מהקיר.
  ;; למה לא שרשרת אחת: ראש דלת בגובה 200 וראש חלון בגובה 205 יוצרים
  ;; קטע של 5 ס"מ, והטקסט נערם על שכנו. לאחד את הרמות היה מזייף מידה
  ;; בשרטוט טכני — ולכן כל פתח מקבל שרשרת משלו, וכולן נקראות.
  (setq n 0)
  (foreach o (nth 7 rec)
    (if (/= (car o) "vent")
      (progn
        (setq zz (mg:op-z o))
        (setq zs (vl-sort (list 0.0 (car zz) (cadr zz) H) '<))
        (setq xo (- xr (* d1 (1+ n))))
        (setq prev (car zs))
        (foreach z (cdr zs)
          (if (> (- z prev) 0.01)
            (mg:dim nil (list xr (+ oy prev)) (list xr (+ oy z))
                        (list xo (+ oy (/ (+ prev z) 2.0)))))
          (setq prev z))
        (setq n (1+ n)))))
  ;; אין פתחים בכלל — לפחות גובה הפנים
  (if (= n 0)
    (progn (mg:dim nil (list xr oy) (list xr (+ oy H))
                       (list (- xr d1) (+ oy (/ H 2.0))))
           (setq n 1)))

  ;; תקרת בטון — על השרשרת הראשונה
  (mg:dim nil (list xr (+ oy H)) (list xr (+ oy H tC))
              (list (- xr d1) (+ oy H (/ tC 2.0))))
  ;; גובה פנים נטו וגובה כולל — מעבר לכל השרשראות
  (mg:dim nil (list xr oy) (list xr (+ oy H))
              (list (- xr (* d1 (+ n 1))) (+ oy (/ H 2.0))))
  (mg:dim nil (list xr (- oy fl tF)) (list xr (+ oy H tC))
              (list (- xr (* d1 (+ n 2))) (+ oy (/ H 2.0))))

  ;; ---- צד ימין: רצפת בטון, מילוי, וגובה ציר כל צינור ----
  (mg:dim nil (list xl (- oy fl tF)) (list xl (- oy fl))
              (list (+ xl d1) (- oy fl (/ tF 2.0))))
  (if (> fl 0.01)
    (mg:dim nil (list xl (- oy fl)) (list xl oy)
                (list (+ xl d2) (- oy (/ fl 2.0)))))
  (foreach o (nth 7 rec)
    (if (= (car o) "vent")
      (mg:dim nil (list xl oy) (list xl (+ oy (nth 5 o)))
                  (list (+ xl d1) (+ oy (/ (nth 5 o) 2.0))))))
  ;; אופקי: פנים + עובי שני הקירות
  (mg:dim T (list ox (+ oy H tC)) (list (+ ox (nth 1 rec)) (+ oy H tC))
            (list (+ ox (/ (nth 1 rec) 2.0)) (+ oy H tC d1)))
  (mg:dim T (list (- ox tW) (+ oy H tC)) (list ox (+ oy H tC))
            (list (- ox (/ tW 2.0)) (+ oy H tC d2)))
  (mg:dim T (list (+ ox (nth 1 rec)) (+ oy H tC))
            (list (+ ox (nth 1 rec) tE) (+ oy H tC))
            (list (+ ox (nth 1 rec) (/ tE 2.0)) (+ oy H tC d2))))

(defun mg:section (rec ox oy th hatch? / id L W H tt tE tW tC tF fl ops s z f)
  (setq id (nth 0 rec) L (nth 1 rec) H (nth 3 rec)
        tt (nth 4 rec) tC (nth 5 rec) tF (nth 6 rec) ops (nth 7 rec))
  (setq tE (nth 1 tt) tW (nth 3 tt) fl (mg:fill rec))
  ;; רצפה: מילוי/ריצוף מעל, רצפת בטון מתחתיו. oy = פני הריצוף הגמור.
  (mg:hrect "SECTION" "HATCH" (- ox tW) (- oy fl tF) (+ ox L tE) (- oy fl) 8.0 hatch?)
  (mg:rect  "SECTION" (- ox tW) (- oy fl) (+ ox L tE) oy)
  (mg:hrect "SECTION" "HATCH" (- ox tW) (+ oy H) (+ ox L tE) (+ oy H tC) 8.0 hatch?)
  ;; קירות צד עם פתחיהם
  (foreach s (mg:vsegs rec "W") 
    (mg:hrect "SECTION" "HATCH" (- ox tW) (+ oy (car s)) ox (+ oy (cadr s)) 8.0 hatch?))
  (foreach s (mg:vsegs rec "E")
    (mg:hrect "SECTION" "HATCH" (+ ox L) (+ oy (car s)) (+ ox L tE) (+ oy (cadr s)) 8.0 hatch?))
  ;; פתחי הקירות הניצבים — קו נסתר
  (foreach f (nth 7 rec)
    (if (or (= (cadr f) "N") (= (cadr f) "S"))
      (progn
        (setq z (mg:op-z f))
        (mg:rect "HIDDEN" (+ ox (caddr f)) (+ oy (car z))
                          (+ ox (caddr f) (cadddr f)) (+ oy (cadr z))))))
  ;; מידות
  (mg:secdims rec ox oy)
  ;; מפלס
  (mg:pline "SECTION" (list (list (+ ox (* 0.5 L)) oy)
                            (list (+ ox (* 0.5 L) -6.0) (- oy 10.0))
                            (list (+ ox (* 0.5 L) 6.0) (- oy 10.0))) T)
  (mg:text "TEXT" (list (+ ox (* 0.5 L) 10.0) (+ oy (* th 0.3))) th "%%p0.00" 0 0)
  (mg:text "TEXT" (list (+ ox (/ L 2.0)) (- oy fl tF (* th 2.5))) (* th 1.2)
           (strcat "\U+05D7\U+05EA\U+05DA " id) 1 0))

;;; קטעי קיר אנכיים בחתך (פתחי אותה צלע)
(defun mg:vsegs (rec side / H ops zs cur segs o z)
  (setq H (nth 3 rec) ops (nth 7 rec))
  (setq zs '())
  (foreach o ops
    (if (= (cadr o) side) (setq zs (cons (mg:op-z o) zs))))
  (setq zs (vl-sort zs '(lambda (a b) (< (car a) (car b)))))
  (setq cur 0.0 segs '())
  (foreach z zs
    (if (> (car z) cur) (setq segs (cons (list cur (car z)) segs)))
    (setq cur (max cur (cadr z))))
  (if (> H cur) (setq segs (cons (list cur H) segs)))
  (reverse segs))

;;; טבלה מרכזת
(defun mg:table (ox oy th / rh cw cols x y r tt vals c i totw)
  (setq rh (* th 2.0))
  (setq cols (list (list "\U+05DE\U+05E1'" 14.0) (list "\U+05DE\U+05D9\U+05D3\U+05D5\U+05EA \U+05E4\U+05E0\U+05D9\U+05DD" 26.0)
                   (list "\U+05E9\U+05D8\U+05D7 \U+05DE\"\U+05E8" 16.0) (list "\U+05D2\U+05D5\U+05D1\U+05D4" 12.0)
                   (list "\U+05E7\U+05D9\U+05E8\U+05D5\U+05EA \U+05E6/\U+05DE\U+05D6/\U+05D3/\U+05DE\U+05E2" 30.0)
                   (list "\U+05EA\U+05E7\U+05E8\U+05D4/\U+05E8\U+05E6\U+05E4\U+05D4" 20.0) (list "\U+05D3\U+05DC\U+05EA" 16.0)
                   (list "\U+05D7\U+05DC\U+05D5\U+05DF" 16.0)))
  (setq cols (mapcar '(lambda (c) (list (car c) (* (cadr c) th 0.7))) cols))
  (setq totw (apply '+ (mapcar 'cadr cols)))
  ;; כותרת
  (mg:text "TEXT" (list (- ox (/ totw 2.0)) (+ oy th)) (* th 1.3)
           "\U+05D8\U+05D1\U+05DC\U+05EA \U+05DE\U+05E8\U+05D7\U+05D1\U+05D9\U+05DD \U+05DE\U+05D5\U+05D2\U+05E0\U+05D9\U+05DD" 1 0)
  ;; שורת כותרות (מימין לשמאל: הטור הראשון בימין)
  (setq y oy i 0)
  (mg:trow ox y cols rh (mapcar 'car cols))
  (setq y (- y rh))
  (foreach r *MMD-LIST*
    (setq tt (nth 4 r))
    (setq vals (list
      (nth 0 r)
      (strcat (rtos (nth 1 r) 2 0) "x" (rtos (nth 2 r) 2 0))
      (rtos (/ (* (nth 1 r) (nth 2 r)) 10000.0) 2 2)
      (rtos (nth 3 r) 2 0)
      (strcat (rtos (nth 0 tt) 2 0) "/" (rtos (nth 1 tt) 2 0) "/"
              (rtos (nth 2 tt) 2 0) "/" (rtos (nth 3 tt) 2 0))
      (strcat (rtos (nth 5 r) 2 0) "/" (rtos (nth 6 r) 2 0))
      (mg:opdesc r "door") (mg:opdesc r "win")))
    (mg:trow ox y cols rh vals)
    (setq y (- y rh)))
  (- oy y))

(defun mg:opdesc (rec typ / o out)
  (setq out "-")
  (foreach o (nth 7 rec)
    (if (= (car o) typ)
      (setq out (strcat (rtos (cadddr o) 2 0) "/" (rtos (nth 4 o) 2 0)))))
  out)

;;; שורת טבלה: תאים מימין לשמאל
(defun mg:trow (ox y cols rh vals / x c v th)
  (setq th (* 0.4 rh))
  (setq x ox)
  (mapcar
    '(lambda (c v)
       (mg:rect "TABLE" (- x (cadr c)) (- y rh) x y)
       (mg:text "TEXT" (list (- x (/ (cadr c) 2.0)) (- y (* rh 0.68))) th v 1 0)
       (setq x (- x (cadr c))))
    cols vals)
  nil)

;;; הערות
(defun mg:notes (ox oy th / i y n)
  (mg:text "TEXT" (list ox (+ oy th)) (* th 1.3) "\U+05D4\U+05E2\U+05E8\U+05D5\U+05EA \U+05DB\U+05DC\U+05DC\U+05D9\U+05D5\U+05EA" 2 0)
  (setq i 1 y (- oy (* th 1.2)))
  (foreach n *MG-NOTES*
    (mg:text "TEXT" (list ox y) (* th 0.85) (strcat (itoa i) ". " n) 2 0)
    (setq y (- y (* th 1.9)) i (1+ i)))
  (- oy y))

(setq *MG-NOTES* (list
  "\U+05D4\U+05D1\U+05D9\U+05E6\U+05D5\U+05E2 \U+05D1\U+05D4\U+05EA\U+05D0\U+05DD \U+05DC\U+05EA\U+05E7\U+05E0\U+05D5\U+05EA \U+05D4\U+05D4\U+05EA\U+05D2\U+05D5\U+05E0\U+05E0\U+05D5\U+05EA \U+05D4\U+05D0\U+05D6\U+05E8\U+05D7\U+05D9\U+05EA (\U+05DE\U+05E4\U+05E8\U+05D8\U+05D9\U+05DD \U+05DC\U+05D1\U+05E0\U+05D9\U+05D9\U+05EA \U+05DE\U+05E7\U+05DC\U+05D8\U+05D9\U+05DD) \U+05D5\U+05DC\U+05D4\U+05E0\U+05D7\U+05D9\U+05D5\U+05EA \U+05E4\U+05D9\U+05E7\U+05D5\U+05D3 \U+05D4\U+05E2\U+05D5\U+05E8\U+05E3 \U+05D4\U+05EA\U+05E7\U+05E4\U+05D5\U+05EA \U+05DC\U+05DE\U+05D5\U+05E2\U+05D3 \U+05D4\U+05D4\U+05D9\U+05EA\U+05E8."
  "\U+05DB\U+05DC \U+05D4\U+05E7\U+05D9\U+05E8\U+05D5\U+05EA, \U+05D4\U+05EA\U+05E7\U+05E8\U+05D4 \U+05D5\U+05D4\U+05E8\U+05E6\U+05E4\U+05D4 \U+05E9\U+05DC \U+05D4\U+05DE\U+05E8\U+05D7\U+05D1 \U+05D4\U+05DE\U+05D5\U+05D2\U+05DF \U+05DE\U+05D1\U+05D8\U+05D5\U+05DF \U+05DE\U+05D6\U+05D5\U+05D9\U+05DF \U+05D9\U+05E6\U+05D5\U+05E7 \U+05D1\U+05D0\U+05EA\U+05E8."
  "\U+05DE\U+05E2\U+05E8\U+05DB\U+05D5\U+05EA \U+05D4\U+05D0\U+05D5\U+05D5\U+05E8\U+05D5\U+05E8 \U+05D5\U+05D4\U+05E1\U+05D9\U+05E0\U+05D5\U+05DF \U+05D9\U+05D5\U+05EA\U+05E7\U+05E0\U+05D5 \U+05D1\U+05D4\U+05EA\U+05D0\U+05DD \U+05DC\U+05D3\U+05E8\U+05D9\U+05E9\U+05D5\U+05EA \U+05EA\"\U+05D9 4570."
  "\U+05D0\U+05D9\U+05DF \U+05DC\U+05D4\U+05E2\U+05D1\U+05D9\U+05E8 \U+05D3\U+05E8\U+05DA \U+05E7\U+05D9\U+05E8\U+05D5\U+05EA \U+05D4\U+05DE\U+05E8\U+05D7\U+05D1 \U+05D4\U+05DE\U+05D5\U+05D2\U+05DF \U+05E6\U+05E0\U+05E8\U+05EA \U+05E9\U+05D0\U+05D9\U+05E0\U+05D4 \U+05DE\U+05E9\U+05DE\U+05E9\U+05EA \U+05D0\U+05EA \U+05E6\U+05D5\U+05E8\U+05DB\U+05D9 \U+05D4\U+05DE\U+05E8\U+05D7\U+05D1 \U+05D4\U+05DE\U+05D5\U+05D2\U+05DF."
  "\U+05D9\U+05D5\U+05EA\U+05E7\U+05DF \U+05E6\U+05D9\U+05E0\U+05D5\U+05E8 \U+05DE\U+05E2\U+05D1\U+05E8 \U+05DB\U+05D1\U+05D9\U+05DC\U+05D4 \U+05DE\U+05D4\U+05DE\U+05DE\"\U+05D3 \U+05D0\U+05DC \U+05D4\U+05D3\U+05D9\U+05E8\U+05D4 \U+05D1\U+05D4\U+05EA\U+05D0\U+05DD \U+05DC\U+05D4\U+05E0\U+05D7\U+05D9\U+05D5\U+05EA \U+05E4\U+05E7\U+05E2\"\U+05E8."
  "\U+05D3\U+05DC\U+05EA \U+05D4\U+05D4\U+05D3\U+05E3 \U+05D5\U+05D7\U+05DC\U+05D5\U+05DF \U+05D4\U+05D4\U+05D3\U+05E3 \U+05D9\U+05D4\U+05D9\U+05D5 \U+05D1\U+05E2\U+05DC\U+05D9 \U+05EA\U+05D5 \U+05EA\U+05E7\U+05DF \U+05D5\U+05D0\U+05D9\U+05E9\U+05D5\U+05E8 \U+05E4\U+05D9\U+05E7\U+05D5\U+05D3 \U+05D4\U+05E2\U+05D5\U+05E8\U+05E3."
  "\U+05D3\U+05DC\U+05EA \U+05D4\U+05D4\U+05D3\U+05E3 \U+05E0\U+05E4\U+05EA\U+05D7\U+05EA \U+05D4\U+05D7\U+05D5\U+05E6\U+05D4."
  "\U+05D4\U+05DE\U+05D9\U+05D3\U+05D5\U+05EA \U+05D1\U+05EA\U+05D5\U+05DB\U+05E0\U+05D9\U+05EA \U+05D4\U+05DF \U+05DE\U+05D9\U+05D3\U+05D5\U+05EA \U+05E4\U+05E0\U+05D9\U+05DD \U+05E0\U+05D8\U+05D5 \U+05DC\U+05D0\U+05D7\U+05E8 \U+05D2\U+05DE\U+05E8."
  "\U+05D2\U+05D5\U+05D1\U+05D4 \U+05E4\U+05E0\U+05D9\U+05DD \U+05E0\U+05D8\U+05D5 250 \U+05E1\"\U+05DE \U+05DC\U+05E4\U+05D7\U+05D5\U+05EA \U+05D0\U+05D7\U+05E8\U+05D9 \U+05E8\U+05D9\U+05E6\U+05D5\U+05E3."
  "\U+05DB\U+05DC \U+05E1\U+05D8\U+05D9\U+05D9\U+05D4 \U+05DE\U+05D4\U+05DE\U+05E1\U+05D5\U+05DE\U+05DF \U+05D1\U+05E0\U+05E1\U+05E4\U+05D7 \U+05D6\U+05D4 \U+05D8\U+05E2\U+05D5\U+05E0\U+05D4 \U+05D0\U+05D9\U+05E9\U+05D5\U+05E8 \U+05DE\U+05D5\U+05E7\U+05D3\U+05DD \U+05E9\U+05DC \U+05D9\U+05D5\U+05E2\U+05E5 \U+05D4\U+05DE\U+05D9\U+05D2\U+05D5\U+05DF."))

;;; ---------- MMDDRAW ----------
(defun mg:sysvars () '("CMDECHO" "OSMODE" "BLIPMODE" "CLAYER" "TEXTSTYLE"
    "DIMSCALE" "DIMTXT" "DIMASZ" "DIMDEC" "DIMLFAC" "DIMTAD"
    "DIMTIH" "DIMTOH" "DIMEXE" "DIMEXO" "DIMGAP" "DIMBLK" "DIMSAH"
    "DIMATFIT" "DIMTMOVE" "DIMTOFL"))

(defun mg:setup (scl)
  (setvar "CMDECHO" 0) (setvar "OSMODE" 0) (setvar "BLIPMODE" 0)
  (setvar "TEXTSTYLE" (strcat *MG-PREFIX* "TXT"))
  (setvar "DIMSCALE" scl) (setvar "DIMTXT" 0.25)
  (setvar "DIMASZ" 0.25) (setvar "DIMEXE" 0.12) (setvar "DIMEXO" 0.1)
  (setvar "DIMGAP" 0.08) (setvar "DIMDEC" 0) (setvar "DIMLFAC" 1.0)
  (setvar "DIMTAD" 1) (setvar "DIMTIH" 0) (setvar "DIMTOH" 0) (setvar "DIMSAH" 0)
  ;; שרשרת המידות בחתך מכילה קטעים קצרים מאוד (למשל 5 ס"מ בין ראש
  ;; צינור לראש חלון). בלי אלה הטקסטים נערמים זה על זה ולא ניתן לקרוא:
  ;;   DIMATFIT 3 — כשאין מקום, הטקסט והחצים יוצאים החוצה
  ;;   DIMTMOVE 1 — טקסט שיצא מקבל קו מוביל, כדי שיהיה ברור לאיזו מידה
  ;;   DIMTOFL  1 — קו המידה נמשך בין קווי העזר גם כשהטקסט בחוץ
  (setvar "DIMATFIT" 3) (setvar "DIMTMOVE" 1) (setvar "DIMTOFL" 1)
  (vl-catch-all-apply 'setvar (list "DIMBLK" "_ARCHTICK")))

;;; ציור כל הנספח. p0 = פינה ימנית-תחתונה, scl = מכנה קנה המידה
(defun mg:drawall (p0 scl / x0 y0 th r gap hatch? maxw secy tw py miny)
  (mg:mklayers) (mg:style) (mg:setup scl)
  (setq th (* 0.25 scl))          ; טקסט 2.5 מ"מ בנייר
  (setq hatch? T  gap (* 1.2 scl))

  ;; שורת תוכניות — מנקודת הבסיס שמאלה
  (setq x0 (car p0) y0 (cadr p0) maxw 0.0)
  (foreach r *MMD-LIST*
    (setq tw (+ (nth 1 r) (nth 1 (nth 4 r)) (nth 3 (nth 4 r))))
    (setq x0 (- x0 tw (* 0.6 gap)))
    ;; mg:plan מחזירה את התחתית בפועל (סוף בלוק הנתונים) — שורת החתכים
    ;; תיפול מתחתיה במקום מרחק קבוע שמתנגש כשהדלת רחבה.
    (setq py (mg:plan r x0 (+ y0 (* 1.6 gap)) th hatch?))
    (setq miny (if miny (min miny py) py))
    (mg:text "TEXT" (list (+ x0 (/ (nth 1 r) 2.0))
                          (+ y0 (* 1.6 gap) (nth 2 r) (nth 0 (nth 4 r)) (* 0.35 gap)))
             (* th 1.3)
             (strcat "\U+05EA\U+05D5\U+05DB\U+05E0\U+05D9\U+05EA " (nth 0 r) " \U+05E7\U+05E0\"\U+05DE 1:" (rtos scl 2 0)) 1 0)
    (setq maxw (max maxw (+ (nth 2 r) (nth 0 (nth 4 r)) (nth 2 (nth 4 r)))))
    (setq x0 (- x0 (* 1.4 gap))))

  ;; שורת חתכים מתחת
  ;; החתכים מתחת לתחתית התוכניות, בתוספת מרווח למידות שמעל התקרה.
  (setq x0 (car p0) secy (- (if miny miny y0) (* 1.6 gap)))
  (foreach r *MMD-LIST*
    (setq tw (+ (nth 1 r) (nth 1 (nth 4 r)) (nth 3 (nth 4 r))))
    (setq x0 (- x0 tw (* 0.6 gap)))
    (mg:section r x0 (- secy (nth 3 r)) th hatch?)
    (setq x0 (- x0 (* 1.4 gap))))

  ;; הערות בלבד מימין לנקודת הבסיס.
  ;; אין טבלה מרכזת — לפי החלטת המשתמש 27.08.2026 הנתונים מופיעים על
  ;; כל ממ"ד בתוכנית שלו (mg:info). mg:table נשמרה בקוד למקרה שיידרש.
  (mg:notes (+ (car p0) (* 14.0 gap)) y0 th))

(defun c:MMDDRAW ( / *error* sv vals p0 scl)
  (setq sv (mg:sysvars) vals (mapcar 'getvar sv))
  (defun *error* (m)
    (mapcar '(lambda (n v) (vl-catch-all-apply 'setvar (list n v))) sv vals)
    (if (and m (not (wcmatch (strcase m) "*BREAK*,*CANCEL*,*QUIT*")))
      (princ (strcat "\nMMDDRAW: " m)))
    (princ))

  (if (null *MMD-LIST*) (mg:load))
  (if (null *MMD-LIST*)
    (progn (princ "\nאין מרחבים ברשימה — הפעל MMD קודם.") (exit)))

  (setq p0 (getpoint "\nנקודת בסיס לנספח (פינה ימנית-תחתונה): "))
  (if (null p0) (exit))
  (setq scl (getreal "\nקנה מידה 1:<50>: "))
  (if (null scl) (setq scl 50.0))

  (mg:drawall p0 scl)

  (mapcar '(lambda (n v) (vl-catch-all-apply 'setvar (list n v))) sv vals)
  (princ (strcat "\n=== הנספח נוצר: " (itoa (length *MMD-LIST*)) " מרחבים ==="))
  (princ))

;;; ==================================================================
;;;  MMDTEST — בדיקה עצמית, אפס קליקים
;;;  מצייר נספח הדגמה ומדפיס דוח תקינות דו-לשוני.
;;;  הדוח באנגלית בכוונה: אם העברית משובשת והאנגלית קריאה,
;;;  הבעיה היא בקידוד ולא בכלי.
;;; ==================================================================
(defun c:MMDTEST ( / *error* sv vals saved n0 n1 ok units fp r1)
  (setq sv (mg:sysvars) vals (mapcar 'getvar sv))
  (defun *error* (m)
    (mapcar '(lambda (n v) (vl-catch-all-apply 'setvar (list n v))) sv vals)
    (princ (strcat "\n[FAIL] MMDTEST: " (if m m "?")))
    (princ))

  (princ "\n")
  (princ "\n============================================================")
  (princ "\n  MIGUN SELF-TEST  /  בדיקה עצמית")
  (princ "\n============================================================")
  (princ (strcat "\n  AutoCAD    : " (getvar "ACADVER")))
  (princ (strcat "\n  Build      : " *MG-BUILD*
                 "   <-- must match migun.lsp.src / חייב להתאים למקור"))
  (princ (strcat "\n  Found at   : "
                 (if (setq fp (findfile "migun.lsp")) fp "(not on search path)")))
  (setq units (getvar "INSUNITS"))
  (princ (strcat "\n  INSUNITS   : " (itoa units) "  "
                 (cond ((= units 4) "= CENTIMETERS  <-- expected / כצפוי")
                       ((= units 5) "= MILLIMETERS  <-- see note below")
                       ((= units 6) "= METERS       <-- see note below")
                       ((= units 0) "= UNITLESS (probably fine / כנראה תקין)")
                       (T "= other"))))
  (princ (strcat "\n  Drawing    : " (getvar "DWGNAME")))

  ;; שמירת רשימה קיימת והחלפתה בנתוני הדגמה
  (setq saved *MMD-LIST*)
  ;; TEST-1 נושא עובי מילוי מפורש; TEST-2 בכוונה **בלי** השדה — כדי
  ;; שהטסט יריץ גם את מסלול הרשומה הישנה ויוודא שהיא עדיין נטענת.
  (setq *MMD-LIST* (list
    ;; (id L W H (tN tE tS tW) tCeil tFloor ops [fill])
    (list "TEST-1" 300.0 260.0 250.0 (list 25.0 25.0 20.0 25.0) 20.0 15.0
      (list (list "door" "S"  40.0  80.0 200.0   0.0 "A" "1")
            (list "win"  "N" 100.0 100.0 100.0 105.0 "A")
            (list "vent" "N" 240.0  20.0  20.0 190.0 "A"))
      8.0)
    (list "TEST-2" 340.0 280.0 250.0 (list 30.0 25.0 25.0 25.0) 20.0 15.0
      (list (list "door" "W"  60.0  80.0 200.0   0.0 "B" "1")
            (list "win"  "E" 120.0 100.0 100.0 105.0 "A")))))

  (setq n0 (if (entlast) 1 0))
  (princ "\n  Drawing demo annex at 0,0 ... / מצייר נספח הדגמה")
  (mg:drawall '(0.0 0.0 0.0) 50.0)

  ;; בדיקות
  (princ "\n------------------------------------------------------------")
  (setq ok T)
  (foreach nm '("WALL" "DOOR" "WINDOW" "VENT" "DIM" "TEXT" "TABLE" "SECTION")
    (if (tblsearch "LAYER" (strcat *MG-PREFIX* nm))
      (princ (strcat "\n  [ok]   layer " *MG-PREFIX* nm))
      (progn (setq ok nil)
             (princ (strcat "\n  [FAIL] layer " *MG-PREFIX* nm " missing")))))
  (if (tblsearch "STYLE" (strcat *MG-PREFIX* "TXT"))
    (princ (strcat "\n  [ok]   text style " *MG-PREFIX* "TXT (" *MG-FONT* ")"))
    (progn (setq ok nil) (princ "\n  [FAIL] text style missing")))

  ;; שטח ונפח — TEST-1 הוא 300x260x250 ס"מ ? 7.80 מ"ר ו-19.50 מ"ק
  (setq r1 (car *MMD-LIST*))
  (if (< (abs (- (mg:area r1) 7.8)) 0.005)
    (princ "\n  [ok]   area   7.80 m2")
    (progn (setq ok nil)
           (princ (strcat "\n  [FAIL] area = " (rtos (mg:area r1) 2 3)
                          " expected 7.800"))))
  (if (< (abs (- (mg:vol r1) 19.5)) 0.005)
    (princ "\n  [ok]   volume 19.50 m3")
    (progn (setq ok nil)
           (princ (strcat "\n  [FAIL] volume = " (rtos (mg:vol r1) 2 3)
                          " expected 19.500"))))
  ;; רשומה בלי שדה מילוי חייבת ליפול לברירת המחדל, לא ל-nil
  (if (equal (mg:fill (cadr *MMD-LIST*)) *MG-FILL* 0.001)
    (princ "\n  [ok]   legacy record falls back to default fill")
    (progn (setq ok nil) (princ "\n  [FAIL] legacy record fill lookup")))
  (if (equal (mg:fill r1) 8.0 0.001)
    (princ "\n  [ok]   explicit fill read from record")
    (progn (setq ok nil) (princ "\n  [FAIL] explicit fill lookup")))

  ;; חזרה למצב הקודם
  (setq *MMD-LIST* saved)
  (mapcar '(lambda (n v) (vl-catch-all-apply 'setvar (list n v))) sv vals)

  (vl-catch-all-apply 'vl-cmdf (list "_.ZOOM" "_E"))

  (princ "\n------------------------------------------------------------")
  (if ok
    (progn
      (princ "\n  RESULT: PASS")
      (princ "\n  You should now SEE on screen:")
      (princ "\n    - 2 protected-space plans, hatched walls, door arc, window")
      (princ "\n    - 2 sections below them")
      (princ "\n    - a summary table + numbered notes on the right")
      (princ "\n  אם אתה רואה את כל אלה — הכלי עובד.")
      (princ "\n  Hebrew above garbled but English readable? tell me - encoding only.")
      (princ "\n  To erase the demo:  MMDWIPE"))
    (princ "\n  RESULT: FAIL - send me this whole text (F2 to copy)"))
  (princ "\n============================================================")
  (princ))

;;; ==================================================================
;;;  MMDWIPE — מחיקת כל מה שהכלי צייר (שכבות MIGUN-*)
;;; ==================================================================
(defun c:MMDWIPE ( / ss i n)
  (setq n 0)
  (foreach rec *MG-LAYERS*
    (if (setq ss (ssget "_X" (list (cons 8 (strcat *MG-PREFIX* (car rec))))))
      (repeat (setq i (sslength ss))
        (entdel (ssname ss (setq i (1- i))))
        (setq n (1+ n)))))
  (princ (strcat "\nנמחקו " (itoa n) " ישויות / erased " (itoa n) " entities."))
  (princ))

;;; ---------- עזרי ניהול ----------
(defun c:MMDLIST ( / r)
  (if (null *MMD-LIST*) (mg:load))
  (princ (strcat "\n" (itoa (length *MMD-LIST*)) " מרחבים:"))
  (foreach r *MMD-LIST*
    (princ (strcat "\n  " (nth 0 r) "  " (rtos (nth 1 r) 2 0) "x" (rtos (nth 2 r) 2 0)
                   "  (" (rtos (/ (* (nth 1 r) (nth 2 r)) 10000.0) 2 2) " מ\"ר)  "
                   (itoa (length (nth 7 r))) " פתחים")))
  (princ))

(defun c:MMDDEL ( / id)
  (if (null *MMD-LIST*) (mg:load))
  (setq id (getstring T "\nשם המרחב למחיקה: "))
  (setq *MMD-LIST* (vl-remove-if '(lambda (r) (= (car r) id)) *MMD-LIST*))
  (mg:save)
  (princ (strcat "\nנותרו " (itoa (length *MMD-LIST*)) " מרחבים."))
  (princ))

(defun c:MMDCLR ()
  (setq *MMD-LIST* '())
  (mg:save)
  (princ "\nהרשימה נוקתה.")
  (princ))

(princ (strcat "\n=== MIGUN.LSP נטען   (build " *MG-BUILD* ") ==="))
(princ "\n  >>> חדש כאן? הקלד  MMDTEST  לבדיקה עצמית ללא קליקים <<<")
(princ "\n  MMD — סימון מרחב מוגן | MMDDRAW — יצירת נספח | MMDDETAILS — פרטי פתחים")
(princ "\n  MMDLIB — ספריית בלוקים | MMDTEST — בדיקה | MMDWIPE — מחיקת מה שצוייר")
(princ "\n  MMDLIST | MMDDEL | MMDCLR")
(princ)
