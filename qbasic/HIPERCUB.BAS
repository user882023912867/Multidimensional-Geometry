SCREEN 12
DIM cv(16, 16)
v1 = 1: v2 = 2
CONST pi = 3.141592654#
an = 90: r = 100
IN = pi / 180
rad = pi / 180
ce = ATN(1 / SQR(2))
'ce = pi / 4
ci = 15'incremento del centro
ana = pi / 4
anb = pi / 4
anc = pi / 4
n = 1
'data del cubo por punto cuadrante 8, alfa=xy,beta=yz,gamma=zx,radio
rad = pi / 180: RA = 100
FOR n = 1 TO 16: READ x, y, z, U: cv(n, 1) = x * RA: cv(n, 2) = y * RA: cv(n, 3) = z * RA: cv(n, 4) = U * RA: NEXT n
an = 90:  r = 100
cx = 180: cy = 180
DATA 1,1,1,1
DATA 1,1,1,-1
DATA 1,1,-1,1
DATA 1,1,-1,-1
DATA 1,-1,1,1
DATA 1,-1,1,-1
DATA 1,-1,-1,1
DATA 1,-1,-1,-1

DATA -1,1,1,1
DATA -1,1,1,-1
DATA -1,1,-1,1
DATA -1,1,-1,-1

DATA -1,-1,1,1
DATA -1,-1,1,-1
DATA -1,-1,-1,1
DATA -1,-1,-1,-1


10
i$ = INKEY$
IF i$ = "q" THEN ana = ana + IN: GOTO an1
IF i$ = "w" THEN ana = ana - IN: GOTO an1
IF i$ = "a" THEN anb = anb + IN: GOTO an2
IF i$ = "s" THEN anb = anb - IN: GOTO an2
IF i$ = "z" THEN anc = anc + IN: GOTO an3
IF i$ = "x" THEN anc = anc - IN: GOTO an3
IF i$ = "e" THEN ann = ann + IN: GOTO an4
IF i$ = "r" THEN ann = ann - IN: GOTO an4
IF i$ = "d" THEN ane = ane + IN: GOTO an5
IF i$ = "f" THEN ane = ane - IN: GOTO an5
IF i$ = "c" THEN anf = anf + IN: GOTO an6
IF i$ = "v" THEN anf = anf - IN: GOTO an6

IF i$ = "t" THEN cx = cx + ci
IF i$ = "y" THEN cx = cx - ci
IF i$ = "g" THEN cy = cy + ci
IF i$ = "h" THEN cy = cy - ci

IF i$ = " " THEN LOCATE 20, 1: PRINT "vertice "; n; "x="; VX(n); "y="; VY(n); "angulo"; an
IF i$ = "+" THEN r = r + 1: 'IF n = 9 THEN n = 1
IF i$ = "-" THEN r = r - 1: 'IF n = 0 THEN n = 8


IF i$ = "b" THEN IN = IN + pi / 180: ' IF IN = pi / 2 THEN IN = pi / 180
IF i$ = "n" THEN IN = IN - pi / 180: ' IF IN = 0 THEN IN = pi / 2
IF i$ <> "" THEN LOCATE 20, 1: PRINT "Angulos "; INT(ana / rad); INT(anb / rad); INT(anc / rad); "inc "; IN * 180 / pi
IF i$ = "<" THEN FOR a = 1 TO 16: LOCATE (cv(a, v2) + cy) / 16, (cv(a, v1) + cx) / 8: PRINT a: NEXT a: ' GOTO gr
IF i$ = "p" THEN FOR a = 1 TO 16: LOCATE 21, 1 + a * 4: PRINT a; : LOCATE 22, 1 + a * 4: PRINT cv(a, 1); : LOCATE 23, 1 + a * 4: PRINT cv(a, 2): LOCATE 24, 1 + a * 4: PRINT cv(a, 3): LOCATE 25, 1 + a * 4: PRINT cv(a, 4): NEXT a: i$ = ""
IF i$ = "u" THEN INPUT "coordenadas x,y del 1 al 4 no repetidas"; v1, v2
IF i$ = "o" THEN INPUT "coordenadas x,y,z perpendicular a E(z,x,y)"; x1, y1, z1, el

GOTO 10

an1:
p1 = 1: p2 = 2
FOR a = 1 TO 16
IF M = 1 THEN GOTO m1
cv(a, 5) = ATN((cv(a, p2) / cv(a, p1)))
cv(a, 0) = SQR(cv(a, p1) ^ 2 + cv(a, p2) ^ 2)
IF SGN(cv(a, p1)) = -1 THEN cv(a, 6) = pi ELSE cv(a, 6) = 0
ana = 0
m1:
cv(a, p2) = SIN(cv(a, 6) + cv(a, 5) + ana) * (cv(a, 0))
cv(a, p1) = COS(cv(a, 6) + cv(a, 5) + ana) * (cv(a, 0))
NEXT a:
M = 1
GOTO GR:
an2:
p1 = 2: p2 = 3
FOR a = 1 TO 16
IF M = 2 THEN GOTO m2
cv(a, 5) = ATN((cv(a, p2) / cv(a, p1)))
cv(a, 0) = SQR(cv(a, p1) ^ 2 + cv(a, p2) ^ 2)
IF SGN(cv(a, p1)) = -1 THEN cv(a, 6) = pi ELSE cv(a, 6) = 0
anb = 0
m2:
cv(a, p2) = SIN(cv(a, 6) + cv(a, 5) + anb) * (cv(a, 0))
cv(a, p1) = COS(cv(a, 6) + cv(a, 5) + anb) * (cv(a, 0))
NEXT a:
M = 2
GOTO GR:
an3:
p1 = 3: p2 = 1
FOR a = 1 TO 16
IF M = 3 THEN GOTO m3
cv(a, 5) = ATN((cv(a, p2) / cv(a, p1)))
cv(a, 0) = SQR(cv(a, p1) ^ 2 + cv(a, p2) ^ 2)
IF SGN(cv(a, p1)) = -1 THEN cv(a, 6) = pi ELSE cv(a, 6) = 0
anc = 0
m3:
cv(a, p2) = SIN(cv(a, 6) + cv(a, 5) + anc) * (cv(a, 0))
cv(a, p1) = COS(cv(a, 6) + cv(a, 5) + anc) * (cv(a, 0))
NEXT a:
M = 3
GOTO GR:
an4:
p1 = 3: p2 = 4
FOR a = 1 TO 16
IF M = 4 THEN GOTO m4
cv(a, 5) = ATN((cv(a, p2) / cv(a, p1)))
cv(a, 0) = SQR(cv(a, p1) ^ 2 + cv(a, p2) ^ 2)
IF SGN(cv(a, p1)) = -1 THEN cv(a, 6) = pi ELSE cv(a, 6) = 0
ann = 0
m4:
cv(a, p2) = SIN(cv(a, 6) + cv(a, 5) + ann) * (cv(a, 0))
cv(a, p1) = COS(cv(a, 6) + cv(a, 5) + ann) * (cv(a, 0))
NEXT a:
M = 4
GOTO GR:
an5:
p1 = 4: p2 = 1
FOR a = 1 TO 16
IF M = 5 THEN GOTO m5
cv(a, 5) = ATN((cv(a, p2) / cv(a, p1)))
cv(a, 0) = SQR(cv(a, p1) ^ 2 + cv(a, p2) ^ 2)
IF SGN(cv(a, p1)) = -1 THEN cv(a, 6) = pi ELSE cv(a, 6) = 0
ane = 0
m5:
cv(a, p2) = SIN(cv(a, 6) + cv(a, 5) + ane) * (cv(a, 0))
cv(a, p1) = COS(cv(a, 6) + cv(a, 5) + ane) * (cv(a, 0))
NEXT a:
M = 5
GOTO GR:
an6:
p1 = 2: p2 = 4
FOR a = 1 TO 16
IF M = 6 THEN GOTO m6
cv(a, 5) = ATN((cv(a, p2) / cv(a, p1)))
cv(a, 0) = SQR(cv(a, p1) ^ 2 + cv(a, p2) ^ 2)
IF SGN(cv(a, p1)) = -1 THEN cv(a, 6) = pi ELSE cv(a, 6) = 0
anf = 0
m6:
cv(a, p2) = SIN(cv(a, 6) + cv(a, 5) + anf) * (cv(a, 0))
cv(a, p1) = COS(cv(a, 6) + cv(a, 5) + anf) * (cv(a, 0))
NEXT a:
M = 6
GOTO GR:






GR:
CLS
'LINE (cx + cv(1, v1) + 1, cv(1, v2) + 1 + cy)-(cx + cv(9, v1) + 1, cv(9, v2) + 1 + cy), 10

'GOTO 10

'GOTO 10
LINE (cx + cv(7, v1), cv(7, v2) + cy)-(cx + cv(8, v1), cv(8, v2) + cy), 6
LINE (cx + cv(1, v1), cv(1, v2) + cy)-(cx + cv(2, v1), cv(2, v2) + cy), 6
LINE (cx + cv(13, v1), cv(13, v2) + cy)-(cx + cv(14, v1), cv(14, v2) + cy), 6
LINE (cx + cv(11, v1), cv(11, v2) + cy)-(cx + cv(12, v1), cv(12, v2) + cy), 6

'GOTO 10
LINE (cx + cv(10, v1), cv(10, v2) + cy)-(cx + cv(14, v1), cv(14, v2) + cy), 6
LINE (cx + cv(10, v1), cv(10, v2) + cy)-(cx + cv(2, v1), cv(2, v2) + cy), 6
LINE (cx + cv(10, v1), cv(10, v2) + cy)-(cx + cv(12, v1), cv(12, v2) + cy), 6
LINE (cx + cv(10, v1), cv(10, v2) + cy)-(cx + cv(9, v1), cv(9, v2) + cy), 6
LINE (cx + cv(16, v1), cv(16, v2) + cy)-(cx + cv(12, v1), cv(12, v2) + cy), 6
LINE (cx + cv(16, v1), cv(16, v2) + cy)-(cx + cv(14, v1), cv(14, v2) + cy), 6
LINE (cx + cv(16, v1), cv(16, v2) + cy)-(cx + cv(8, v1), cv(8, v2) + cy), 6
LINE (cx + cv(16, v1), cv(16, v2) + cy)-(cx + cv(15, v1), cv(15, v2) + cy), 6
LINE (cx + cv(6, v1), cv(6, v2) + cy)-(cx + cv(14, v1), cv(14, v2) + cy), 6
LINE (cx + cv(6, v1), cv(6, v2) + cy)-(cx + cv(2, v1), cv(2, v2) + cy), 6
LINE (cx + cv(6, v1), cv(6, v2) + cy)-(cx + cv(8, v1), cv(8, v2) + cy), 6
LINE (cx + cv(6, v1), cv(6, v2) + cy)-(cx + cv(5, v1), cv(5, v2) + cy), 6
LINE (cx + cv(4, v1), cv(4, v2) + cy)-(cx + cv(2, v1), cv(2, v2) + cy), 6
LINE (cx + cv(4, v1), cv(4, v2) + cy)-(cx + cv(12, v1), cv(12, v2) + cy), 6
LINE (cx + cv(4, v1), cv(4, v2) + cy)-(cx + cv(8, v1), cv(8, v2) + cy), 6
LINE (cx + cv(4, v1), cv(4, v2) + cy)-(cx + cv(3, v1), cv(3, v2) + cy), 6
'LINE (cx + cv(,v1), cv(,v2) + cy)-(cx + cv(,v1), cv(,v2) + cy), 2
LINE (cx + cv(7, v1), cv(7, v2) + cy)-(cx + cv(3, v1), cv(3, v2) + cy), 2
LINE (cx + cv(7, v1), cv(7, v2) + cy)-(cx + cv(15, v1), cv(15, v2) + cy), 2
LINE (cx + cv(11, v1), cv(11, v2) + cy)-(cx + cv(3, v1), cv(3, v2) + cy), 2
LINE (cx + cv(11, v1), cv(11, v2) + cy)-(cx + cv(15, v1), cv(15, v2) + cy), 2
'GOTO 10
LINE (cx + cv(1, v1), cv(1, v2) + cy)-(cx + cv(3, v1), cv(3, v2) + cy), 2
LINE (cx + cv(13, v1), cv(13, v2) + cy)-(cx + cv(15, v1), cv(15, v2) + cy), 2
LINE (cx + cv(11, v1), cv(11, v2) + cy)-(cx + cv(9, v1), cv(9, v2) + cy), 2
LINE (cx + cv(7, v1), cv(7, v2) + cy)-(cx + cv(5, v1), cv(5, v2) + cy), 2
LINE (cx + cv(1, v1), cv(1, v2) + cy)-(cx + cv(9, v1), cv(9, v2) + cy), 10
LINE (cx + cv(1, v1), cv(1, v2) + cy)-(cx, cy), 10
LINE (cx + cv(1, v1), cv(1, v2) + cy)-(cx + cv(5, v1), cv(5, v2) + cy), 10
LINE (cx + cv(13, v1), cv(13, v2) + cy)-(cx + cv(9, v1), cv(9, v2) + cy), 10
LINE (cx + cv(13, v1), cv(13, v2) + cy)-(cx + cv(5, v1), cv(5, v2) + cy), 10

GOTO 10