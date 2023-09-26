'     program BSTAC4

'  BASIC version 7.3 - 28 March 2001
'  compiler QuickBASIC version 4.5

'  maxr  - no. of parameters to be refined (maxpr-maxco)
'  maxp  - no. of total points
'  maxco - no. of constraints
'  maxs  - no. of species
'  maxc  - no. of components
'  maxt  - no. of titrations
'  maxpr - no. of total parameters to be refined
'  maxsp - no. of special parameters (titration parameters)

DefDbl A-H, O-Z: DefInt I-N
Option Base 1

DECLARE SUB CCCS ()
DECLARE SUB CCFR ()
DECLARE SUB CHECK ()
DECLARE SUB CONSTR ()
DECLARE SUB DINS ()
DECLARE SUB DINSM ()
DECLARE SUB DOUS ()
DECLARE SUB FACT ()
DECLARE SUB FUNV ()
DECLARE SUB HJA (EO, RTJC, IVAL#, JA#, JB#)
DECLARE SUB INVERT ()
DECLARE SUB SNTPS ()
DECLARE SUB STANS (IPRST)
DECLARE SUB WCURV ()
DECLARE SUB GRAPH ()
DECLARE SUB FILPOUT ()

Common Shared DT()
Common Shared IKA()
Common Shared TOMM(), TT(), XX()
Common Shared ADC(), LOK()
Common Shared JQRM()
Common Shared C(), U(), WA(), A(), X(), DXM()
Common Shared DIV()
Common Shared BIV()
Common Shared PARF(), PARAS()
Common Shared KCONS()
Common Shared AST$()
Common Shared KSP(), RADIB()
Common Shared CV()
Common Shared MAXP, IPP, MAXT, MAXR, MAXC, MAXS
Common Shared MAXPR, KVSP, MAXCO, NCONS, MAXSP, MAXPT
Common Shared MAXRC, MAXCS, IBM#, MAXPG
Common Shared JP, K, SIGV, SIGE, ISP, NKAM
Common Shared INDKW, KW#
Common Shared c0M, c1M, d0M, d1M, e0M, e1M
Common Shared NITER, UF, NSUB
Common Shared NIV, NFILE$, OFILE$, SUM
Common Shared TOL, IFIL
Common Shared TIME1
   
Common Shared DLNAM, IP, IPI, NGAU, EPSIT, SUMP, JCK, JACK
Common Shared CX(), HX(), ZAST(), DFI(), DI(), DIR()
Common Shared DI2(), BETA(), CI(), TCR(), JCON(), EPSR()
Common Shared DX(), DIAG(), DM(), SF(), DF(), BF()
Common Shared RELAC, NF, IFAIL, JTC, NCICL, JFAIL, ACCM, AL10, MBET, ACM1

Common Shared NMBE, NK, ICD, CG(), DG(), EG(), JQR(), TITRE!(), EMF!()

Common Shared JTP(), NTC
Common Shared TITLE$
Common Shared MAXIT, MODE, WESP, PERC
Common Shared KEMIC$(), TEMP, PHI, PHF, NKM, NCD
Common Shared IREF#, AT, BT, c0, c1, d0, d1, e0, e1, ZZ(), KCD()
Common Shared KLOG#(), IB#(), KIY(), KEYC(), KEYD(), KEYE()
Common Shared NKAP(), IKAP()
Common Shared TITL$(), JPC(), MBE(), TOTMM(), ADDC(), COI(), CTI(), IT#()
Common Shared VINIT(), SIGMV(), EZERO(), SIGME(), JAM#(), JBM#(), RTJ()
Common Shared SIGMAP!()

Common Shared KAP#(), JPOT(), SD(), PARI(), JPOTM(), JCD(), PARAM()
Common Shared CDI$(), JCOL(), JTYP(), JCUR(), JREA(), NAMTYP$()
Common Shared B(), JS(), JSP(), POT(), ISPER(), IVA#()
Common Shared FIB(), RIB(), FIT(), RIT()
Common Shared Z2(), RES(), FREEC!(), PCX(), ZT()
Common Shared SUBR$(), STER$(), WS!(), PCS(), IPERC()
Common Shared SIGM, SRM, NKV, IW, CGE, DGE, EGE, NSP, NPAR, EO, RTJC
Common Shared IVAL#, JA#, JB#

Common Shared XGX!(), YY!(), YGE!(), TC$, TIT$
Common Shared TS, ISC, IGR, NX

Dim Shared SUBR$(15), STER$(15)
Dim Shared AST$(4), NAMTYP$(5), KCD(6), JCD(6), CDI$(6), TABIVA(21)
Dim BM(2), ISCC(4)

SUBR$(1) = "main"
SUBR$(2) = "CCCS"
SUBR$(3) = "CCFR"
SUBR$(4) = "CHECK"
SUBR$(5) = "CONSTR"
SUBR$(6) = "DINS"
SUBR$(7) = "DOUS"
SUBR$(8) = "FACT"
SUBR$(9) = "FUNV"
SUBR$(10) = "INVERT"
SUBR$(11) = "SNTPS"
SUBR$(12) = "STANS"
SUBR$(13) = "WCURV"
SUBR$(14) = "HJA"
SUBR$(15) = "DINSM"

25 Restore
Close

On Error GoTo ERRORPROC

IFIL = 1
Print
NSUB = 1
STER$(1) = SUBR$(1)
Line Input "data INPUT filename  ->"; NFILE$

'  NFILE = name of INPUT data file

If NFILE$ = "" Then End
Open NFILE$ For Input As #1
IFIL = 2
MAXSP = 2000
ReDim Shared JCOL(MAXSP)
NSUB = NSUB + 1
STER$(NSUB) = SUBR$(15)
Call DINSM 'subroutine
NSUB = NSUB - 1
Erase JCOL
26 Beep
Line Input "OUTPUT device/filename  ->"; OFILE$

'  OFILE = name of OUTPUT device/filename

If OFILE$ = NFILE$ Then Print "FILE ALREADY OPEN ": GoTo 26
If OFILE$ = "" Then OFILE$ = "SCRN:": GoTo 27
If UCase$(OFILE$) = "PRN" GoTo 27
IFIL = 3
Open OFILE$ For Input As #2
Close #2
Print "FILE EXISTS, OVERWRITE? (Y/N)"
YN$ = Input$(1)
If UCase$(YN$) <> "Y" GoTo 26
27 Open OFILE$ For Output As #2
29 Line Input "OUTPUT filename with refined parameters -> "; OPFILE$
IOPF = 0
If OPFILE$ <> "" Then
    If OPFILE$ = NFILE$ Then Print "FILE ALREADY OPEN ": GoTo 29
    If OPFILE$ = OFILE$ Then Print "FILE ALREADY OPEN ": GoTo 29
    If UCase$(OPFILE$) = "PRN" GoTo 28
    IFIL = 4
    Open OPFILE$ For Input As #4
    Close #4
    Print "FILE EXISTS, OVERWRITE? (Y/N)"
    YN$ = Input$(1)
    If UCase$(YN$) <> "Y" GoTo 29
    28 Open OPFILE$ For Output As #4
    IOPF = 1
End If

'  Procedure to give correct dimension to variables

30 Close #1

MAXP = IPP: MAXT = NTC: MAXR = NPAR: MAXC = NMBE: MAXS = NK
MAXPR = KVSP: MAXCO = NCONS: MAXSP = NSP: MAXPT = MAXP
If MAXR > MAXC Then MAXRC = MAXR Else MAXRC = MAXC
If MAXC < MAXS Then MAXCS = MAXS Else MAXCS = MAXC
If MAXCO = 0 Then MAXCO = 1
If MAXSP = 0 Then MAXSP = 1
If ICD > 0 Then
    MAXTI = MAXT
    MAXSI = MAXS
Else
    MAXTI = 1
    MAXSI = 1
End If

9 ReDim Shared JTP(MAXT), JPC(MAXT), VINIT(MAXT), EZERO(MAXT), RTJ(MAXT)
ReDim Shared SIGMV(MAXT), SIGME(MAXT)
ReDim Shared JAM#(MAXT), JBM#(MAXT), MBE(MAXT)
ReDim Shared TITL$(MAXT)
ReDim Shared ISPER(MAXT)
ReDim Shared JPOT(MAXS), KIY(MAXS), BETA(MAXS), POT(MAXS), CI(MAXS), JS(MAXS)
ReDim Shared KEYC(MAXS), KEYD(MAXS), KEYE(MAXS)
ReDim Shared JSP(MAXS), PCS(MAXS), DT(MAXS), JPOTM(MAXS)
ReDim Shared KAP#(MAXS), NKAP(MAXS), IKAP(MAXS, MAXS), IKA(MAXS)
ReDim Shared KLOG#(MAXS)
ReDim Shared KEMIC$(MAXC), HX(MAXC), CX(MAXC), TOMM(MAXC), TT(MAXC)
ReDim Shared XX(MAXC), ADC(MAXC), PCX(MAXC), TCR(MAXC), LOK(MAXC + 4)
ReDim Shared JQRM(MAXC), DM(MAXC, MAXC), DIAG(MAXC)
ReDim Shared JCON(MAXC)
ReDim Shared SD(MAXR), C(MAXR), U(MAXR), WA(MAXR), A(MAXR), X(MAXR)
ReDim Shared B(MAXR, MAXR), DXM(MAXR)
ReDim Shared DX(MAXRC), SF(MAXRC), DF(MAXRC), DIV(MAXRC)
ReDim Shared BIV(MAXRC, MAXRC), BF(MAXRC, MAXRC)
ReDim Shared EPSR(MAXCS)
ReDim Shared JTYP(MAXSP), JCUR(MAXSP), JREA(MAXSP), JCOL(MAXSP)
ReDim Shared PARF(MAXPR), PARI(MAXPR), PARAM(MAXPR), PARAS(MAXPR)
ReDim Shared KCONS(MAXCO, 2)
ReDim Shared JQR(MAXS, MAXC)
ReDim Shared TOTMM(MAXT, MAXC), ADDC(MAXT, MAXC)
ReDim Shared IT#(MAXTI), FIT(MAXTI), RIT(MAXTI)
ReDim Shared COI(MAXT), CTI(MAXT)
ReDim Shared KSP(MAXS), ZAST(MAXS), ZT(MAXS), IB#(MAXSI)
ReDim Shared FIB(MAXSI), RIB(MAXSI), DI(MAXSI), DIR(MAXSI), DFI(MAXSI)
ReDim Shared CG(MAXSI), DG(MAXSI), EG(MAXSI), DI2(MAXSI), RADIB(MAXSI)

ReDim Shared ZZ(MAXC), Z2(MAXC)

ReDim Shared EMF!(MAXP), RES(MAXP), WS!(MAXP), TITRE!(MAXP)
ReDim Shared FREEC!(MAXP, MAXC)
ReDim Shared SIGMAP!(MAXP)
ReDim Shared CV(MAXP), IVA#(MAXP)
ReDim Shared XGX!(MAXPG), YY!(MAXPG), YGE!(MAXPG)
ReDim Shared IPERC(MAXP, MAXS)

Open NFILE$ For Input As #1
If OFILE$ = "SCRN:" Then
    ISCR = 0
Else
    ISCR = 1: Open "SCRN:" For Output As #3
End If

Dim AVAILABLE As String
   
Const VGA = 12
Const MCGA = 13
Const EGA256 = 9
Const EGA64 = 8
Const MONO = 10
Const HERC = 3
Const CGA = 2
   
Data "CO","EO","JA","JB","SL","PA","c0","c1","d0","d1","e0","e1"
Data 0,.01,.05,.1,.15,.25,.5,.75,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7
ACCM = 1D-175
RELAC = .0001
DLNAM = Log(ACCM)
ACM1 = 1D-75
TOL = .0001
EPSIT = 1E-12
AL10 = Log(10#)
BROW$ = " "
For I = 1 To 78
    BROW$ = BROW$ + " "
Next I
For I = 1 To 5: Read NAMTYP$(I): Next I
Read PA$
For I = 1 To 6: Read CDI$(I): Next I
For I = 1 To 21: Read TABIVA(I): Next I
NSUB = NSUB + 1
STER$(NSUB) = SUBR$(6)
Call DINS 'subroutine
NSUB = NSUB - 1
Close #1
Locate 25, 1: Print "press ESCAPE to quit after next cycle"
If WESP = 0! Then IW = 0 Else IW = 1
NP = JTP(NTC)
33 SW = 0!
For I = 1 To NP: SW = SW + WS!(I): Next I
FW = NP / SW
For I = 1 To NP: WS!(I) = WS!(I) * FW: Next I
If ICD = 0 Then IVAL# = 1
45 Print #2, ""
Print #2, "Table 2."
Print #2, ""
If IW = 2 Then
    Print #2, "weights=1/DE^"; WESP
    If ISCR = 1 Then Print #3, "weights=1/DE^"; WESP
End If
JACK = 3
If IW = 2 GoTo 71
c0 = c0M
c1 = c1M
d0 = d0M
d1 = d1M
e0 = e0M
e1 = e1M
71 Print #2, "initial time "; Time$
TIME1 = Timer
If ISCR = 1 Then Print #3, "time "; Time$
ISP = 2
For I = 1 To KVSP: PARAS(I) = PARAM(I): Next I
NSUB = NSUB + 1
STER$(NSUB) = SUBR$(11)
Call SNTPS 'subroutine
NSUB = NSUB - 1
For I = 1 To KVSP
    PARAM(I) = PARAS(I)
    PARF(I) = PARAM(I)
    If IW < 2 Then PARI(I) = PARAM(I)
Next I
Q = 0!
QC = 1!
NCICL = 0
IFAIL = 1
130 For J = 1 To NPAR: C(J) = 0!
    For K = J To NPAR: B(J, K) = 0!: Next K
Next J
NSUB = NSUB + 1
STER$(NSUB) = SUBR$(9)
Call FUNV 'subroutine
NSUB = NSUB - 1
SS = UF
If IFAIL <> 1 GoTo 530
For J = 1 To NPAR: U(J) = B(J, J): Next J

' Entry point for subsidiary loop. Q is the MARQUARDT parameter (M.P.)
' and U stores the diagonal elements of the normal equations matrix

160 For J = 1 To NPAR
    DX(J) = C(J)
    SF(J) = DX(J)
    DF(J) = SD(J)
    B(J, J) = U(J) * (Q + 1)
    For I = 1 To NPAR: BF(J, I) = B(J, I): Next I
Next J
NF = NPAR
JFAIL = 4
NSUB = NSUB + 1
STER$(NSUB) = SUBR$(8)
Call FACT 'subroutine
NSUB = NSUB - 1
For I = 1 To NPAR
    DX(I) = SF(I)
    SD(I) = DF(I)
    For J = 1 To NPAR: B(I, J) = BF(I, J): Next J
Next I
If PERC = 0! GoTo 162
For I = 1 To NPAR
    If DX(I) < -PERC Then DX(I) = -PERC
    If DX(I) > PERC Then DX(I) = PERC
Next I
162 If IFAIL = 4 GoTo 530

' DX now holds the parameter shifts. VW will be the dot product
' between shift and gradient vectors. It should be positive

VW = 0!
For J = 1 To NPAR: VW = VW + C(J) * DX(J): Next J
If VW <= 0! Then IFAIL = 5: GoTo 530
If Q = 0! GoTo 210
For J = 1 To NPAR
    X(J) = C(J)
    SF(J) = X(J)
    DF(J) = SD(J)
    B(J, J) = U(J)
    For I = 1 To NPAR: BF(J, I) = B(J, I): Next I
Next J
NF = NPAR
JFAIL = 4
NSUB = NSUB + 1
STER$(NSUB) = SUBR$(8)
Call FACT 'subroutine
NSUB = NSUB - 1
For I = 1 To NPAR
    X(I) = SF(I)
    SD(I) = DF(I)
    For J = 1 To NPAR: B(I, J) = BF(I, J): Next J
Next I
If IFAIL = 4 GoTo 530
210 For I = 1 To NPAR
    DIV(I) = SD(I)
    For J = 1 To NPAR: BIV(I, J) = B(I, J): Next J
Next I
NIV = NPAR
NSUB = NSUB + 1
STER$(NSUB) = SUBR$(10)
Call INVERT 'subroutine
NSUB = NSUB - 1
For I = 1 To NPAR
    SD(I) = DIV(I)
    For J = 1 To NPAR: B(I, J) = BIV(I, J): Next J
Next I
SIGMA = Sqr(SS / (NP - NPAR))
SIGM = SIGMA / Sqr(FW)
For I = 1 To KVSP
    If I <= NKV Then K = I Else J = I - NKV: K = JCOL(J)
    PARAM(I) = PARF(I) * (DX(K) + 1)
    SD(K) = Sqr(Abs(B(K, K))) * SIGMA
Next I
KRASH = 1
DQ = 0!
For J = 1 To NPAR: DQ = DQ + DX(J) * DX(J) * U(J): Next J
DQ = VW + Q * DQ

' DQ is a prediction of the reduction in the sum of squares
' based on a linear hypothesis. N.B. when Q=0. DQ=VW

If -DQ > TOL Then IFAIL = 3: GoTo 530
260 ISP = 1
For I = 1 To KVSP: PARAS(I) = PARAM(I): Next I
NSUB = NSUB + 1
STER$(NSUB) = SUBR$(11)
Call SNTPS 'subroutine
NSUB = NSUB - 1
For I = 1 To KVSP: PARAM(I) = PARAS(I): Next I
JACK = 2
For I = 1 To NPAR: DXM(I) = DX(I): Next I
NSUB = NSUB + 1
STER$(NSUB) = SUBR$(9)
Call FUNV 'subroutine
NSUB = NSUB - 1
For I = 1 To NPAR: DX(I) = DXM(I): Next I
SSP = UF
JACK = 1
If IFAIL = 1 GoTo 310

' The calculation of free concentrations has failed, presumably
' because of the presence of negative beta(s). The parameter shifts are
' adjusted so as to reduce the offending parameter by a factor of 10

KRASH = KRASH + 1
IFAIL = 1
FRACT = 1
For K = 1 To NPAR
    If FRACT * DX(K) > -1 GoTo 280
    FRACT = -.9 / DX(K)
    For J = 1 To NPAR: DX(J) = FRACT * DX(J): Next J
280 Next K
For K = 1 To KVSP
    If K <= NKV Then J = K Else I = K - NKV: J = JCOL(I)
    PARAM(K) = PARF(K) * (DX(J) + 1)
Next K
If KRASH < 10 GoTo 260
IFAIL = 9
GoTo 530
310 DS = SS - SSP
IEXIT = 0

' DS is the difference between the old and new sum of squares
' Now test for convergence with two criteria. (1) shifts should be
' less than 10 percent of the standard deviation on the parameters

For J = 1 To NPAR
    If Abs(DX(J) / SD(J)) > .1 Then IEXIT = 1
Next J

' To avoid premature convergence when s.d. S are large 2nd criterion

If Abs(DS / SS) > TOL Then IEXIT = 1
If IEXIT = 0 GoTo 360
If DS >= .25 * DQ GoTo 350

' The reduction in sum of squares is less than 1/4 of the predicted
' value so the M.P. is increased by a factor between 2 and 10

YZ = .5
Z = VW + VW - DS
If Z > 0! Then YZ = VW / Z
If YZ > .5 Then YZ = .5
If YZ < .1 Then YZ = .1
If Q <> 0! GoTo 340
YZ = YZ + YZ
TR = 0!
For J = 1 To NPAR: TR = TR + B(J, J) * U(J): Next J
Q = 1! / TR
QC = Q

' QC is the cut-off value of the M.P. below which it will be set to zero

340 Q = Q / YZ
GoTo 360
350 If DS <= .75 * DQ GoTo 360

' The reduction in the sum of squares is more than 3/4 of the
' predicted value so the M.P. is halved or set to zero
' thus for 1/4 < reduction < 3/4 the M.P. is left unchanged

Q = Q * .5
If Q < QC Then Q = 0!
360 NCICL = NCICL + 1

Print #2, ""
Print #2, "time "; Time$
Print #2, Using " iteration ###     sigma= ###.#####"; NCICL; SIGM
If ISCR = 1 Then
    SR = 0!
    For I = 1 To IPP: SR = SR + Abs(RES(I)): Next I
    SRM = SR / IPP
    Print #3, ""
    Print #3, "time "; Time$
    Print #3, Using " iteration ###     sigma= ###.#####"; NCICL; SIGM;
    Print #3, Using "    mean dev.=#####.####"; SRM
End If
If Q = 0! GoTo 370
Print #2, "MARQUARDT parameter for next iteration";
Print #2, Using "  #.####^^^^"; Q
If ISCR = 1 Then
    Print #3, "MARQUARDT parameter for next iteration";
    Print #3, Using "  #.####^^^^"; Q
End If
370 Print #2, "      parameter     old value    ";
Print #2, "rel.shift  new value      rel.error": Print #2, ""
If ISCR = 1 Then
    Print #3, "      parameter     old value    ";
    Print #3, "rel.shift  new value      rel.error"
End If
K = 0
KNEG = 0
For J = 1 To NK
    JOLD = JPOT(J)
    If KIY(J) >= 1 Then
        K = K + 1
        420 If PARAM(K) <= 0! Then IFAIL = 9: KNEG = J
        Z = Log(Abs(PARAM(K)))
        If Z >= 0! GoTo 430
        PARAM(K) = PARAM(K) * 10!
        JPOT(J) = JPOT(J) - 1
        GoTo 420
        430 If Z < AL10 GoTo 440
        PARAM(K) = PARAM(K) / 10!
        JPOT(J) = JPOT(J) + 1
        GoTo 420
        440 PARFL = Log(Abs(PARF(K))) / AL10 + JOLD
        PARAML = Log(Abs(PARAM(K))) / AL10 + JPOT(J)
        DXK = (PARAML - PARFL) / PARFL
        PARFK = Exp(PARFL * AL10)
        PARAMK = Exp(PARAML * AL10)
        If PARF(K) <= 0! Then PARFK = -PARFK
        If PARAM(K) <= 0! Then PARAMK = -PARAMK
        Print #2, Using "     log K##  ####.#####"; J; PARFL;
        Print #2, Using "      ####.##### ####.#####"; DX(K); PARAML;
        If ISCR = 1 Then
            Print #3, Using "     log K##  ####.#####"; J; PARFL;
            Print #3, Using "      ####.##### ####.#####"; DX(K); PARAML;
        End If
        If KNEG = J Then
            Print #2, " *";
            If ISCR = 1 Then Print #3, " *";
        Else
            Print #2, "  ";
            If ISCR = 1 Then Print #3, "  ";
        End If
        Print #2, Using "    ####.#####"; SD(K)
        If ISCR = 1 Then Print #3, Using "    ####.#####"; SD(K)
    End If
    If KEYC(J) = 1 Then
        K = K + 1
        Print #2, Using "         C##  ####.#####"; J; PARF(K);
        Print #2, Using "       ###.##### ####.#####"; DX(K); PARAM(K);
        Print #2, Using "       ###.#####"; SD(K)
        If ISCR = 1 Then
            Print #3, Using "         C##  ####.#####"; J; PARF(K);
            Print #3, Using "       ###.##### ####.#####"; DX(K); PARAM(K);
            Print #3, Using "       ###.#####"; SD(K)
        End If
    End If
    If KEYD(J) = 1 Then
        K = K + 1
        Print #2, Using "         D##  ####.#####"; J; PARF(K);
        Print #2, Using "       ###.##### ####.#####"; DX(K); PARAM(K);
        Print #2, Using "       ###.#####"; SD(K)
        If ISCR = 1 Then
            Print #3, Using "         D##  ####.#####"; J; PARF(K);
            Print #3, Using "       ###.##### ####.#####"; DX(K); PARAM(K);
            Print #3, Using "       ###.#####"; SD(K)
        End If
    End If
    If KEYE(J) = 1 Then
        K = K + 1
        Print #2, Using "         E##  ####.#####"; J; PARF(K);
        Print #2, Using "       ###.##### ####.#####"; DX(K); PARAM(K);
        Print #2, Using "       ###.#####"; SD(K)
        If ISCR = 1 Then
            Print #3, Using "         E##  ####.#####"; J; PARF(K);
            Print #3, Using "       ###.##### ####.#####"; DX(K); PARAM(K);
            Print #3, Using "       ###.#####"; SD(K)
        End If
    End If
Next J
If NCD = 0 GoTo 480
For I = 1 To NCD
    K = K + 1
    J = JCD(I)
    Print #2, Using "      \\      ####.#####   "; CDI$(J); PARF(K);
    Print #2, Using " ######.##### ####.#####    "; DX(K); PARAM(K);
    Print #2, Using " #####.#####"; SD(K)
    If ISCR = 1 Then
        Print #3, Using "      \\      ####.#####   "; CDI$(J); PARF(K);
        Print #3, Using " ######.##### ####.#####    "; DX(K); PARAM(K);
        Print #3, Using " #####.#####"; SD(K)
    End If
Next I
480 If K = NPAR GoTo 510
For J = K + 1 To KVSP
    JPAR = J - NKV
    I = JCOL(JPAR)
    KIND = JTYP(JPAR)
    JTC = JCUR(JPAR)
    Print #2, Using " curve ## \\"; JTC; NAMTYP$(KIND);
    If KIND = 1 Then Print #2, Using "#"; JREA(JPAR); Else Print #2, " ";
    Print #2, Using " ####.#####      ####.#####"; PARF(J); DX(I);
    Print #2, Using " ####.#####      ####.#####"; PARAM(J); SD(I)
Next J
510 For J = 1 To KVSP: PARF(J) = PARAM(J): Next J
If IEXIT = 0 GoTo 530
If InKey$ = Chr$(27) Then
    Beep
    Locate 25, 1
    Print "                                            "
    Close
    End
End If
If NCICL > MAXIT GoTo 525
ISP = 1
For I = 1 To KVSP: PARAS(I) = PARF(I): Next I
NSUB = NSUB + 1
STER$(NSUB) = SUBR$(11)
Call SNTPS 'subroutine
NSUB = NSUB - 1
For I = 1 To KVSP: PARF(I) = PARAS(I): Next I
GoTo 130
525 IFAIL = 2
530 Locate 25, 1
Print #2, BROW$
NSUB = NSUB + 1
STER$(NSUB) = SUBR$(4)
Call CHECK 'subroutine
NSUB = NSUB - 1
If IFAIL = 9 GoTo 570
If IFAIL > 3 GoTo 25
SR = 0!
For I = 1 To NP: SR = SR + Abs(RES(I)): Next I
SRM = SR / NP
NSUB = NSUB + 1
STER$(NSUB) = SUBR$(7)
Call DOUS 'subroutine
NSUB = NSUB - 1
If IND = 2 GoTo 570
If ICD = 0 GoTo 560
Print #2, ""
Print #2, "Table 5."
Print #2, ""
Print #2, "": Print #2, "log(BETA) = f(ionic strength)"
Print #2, ""
IMAX! = 0
For I = 1 To NP
    If IVA#(I) > IMAX! Then IMAX! = IVA#(I)
Next I
IMAX! = IMAX! + .2
For J = 1 To 21
    If IMAX! < TABIVA(J) Then LASTJ = J: GoTo 535
Next J
535 Print #2, "  I   ";
For J = 1 To NK
    If J > 1 And (J Mod 9) = 1 Then Print #2, "": Print #2, Tab(7);
    Print #2, Using "logB##  "; J;
Next J
Print #2, ""
For J = 1 To LASTJ
    IVAL# = TABIVA(J)
    RADI = Sqr(IVAL#)
    Print #2, Using "#.##"; IVAL#;
    For L = 1 To NK
        DI(L) = IVAL# - IB#(L)
        DI2(L) = IVAL# * IVAL# - IB#(L) * IB#(L)
        DIR(L) = IVAL# * RADI - IB#(L) * RADIB(L)
        DFI(L) = AT * RADI / (1 + BT * RADI) - AT * RADIB(L) / (1 + BT * RADIB(L))
        W = Log(BETA(L)) / AL10
        W = W - (ZAST(L) * DFI(L) - CG(L) * DI(L) - DG(L) * DIR(L) - EG(L) * DI2(L))
        If L > 1 And (L Mod 9) = 1 Then Print #2, "": Print #2, "    ";
        Print #2, Using "####.###"; W;
    Next L
    Print #2, ""
Next J
560 If IND = 2 GoTo 570
NSUB = NSUB + 1
STER$(NSUB) = SUBR$(12)
IPRST = 1
Call STANS(IPRST) 'subroutine
NSUB = NSUB - 1
If IW = 1 Then IW = 2: GoTo 33
GoTo 36
570 Print #2, Using " beta ## negative"; KNEG;
Print #2, " - this model cannot be refined"
GoTo 25
36 If IOPF = 1 Then
    For I = 1 To NK
        KLOG#(I) = Log(KAP#(I)) / AL10 + JPOT(I)
    Next I
    Open NFILE$ For Input As #1
    Call FILPOUT
    Close #1
End If
Beep
Locate 25, 1
Print #2, BROW$
If ISCR = 1 Then
    Locate 25, 1
    Print #3, BROW$
End If
Locate 25, 1
35 Line Input "GRAPHICS (Y/N)? ", GR$
If GR$ <> "Y" And GR$ <> "y" Then
    Locate 25, 1: Print "                                          "
    GoTo 25
End If
IGR = 1

On Error GoTo 0

BM(1) = EGA256
BM(2) = CGA
ISCC(1) = 4
ISCC(2) = 2
HH = 0
BESTMODE = HERC
ISC = 1
    
On Error GoTo VIDEOERR
Screen BESTMODE
On Error GoTo EGAERR
TS = BESTMODE
Screen 0
On Error GoTo 0
   
IP = 1
For IGRA = 1 To NTC
    TIT$ = TITL$(IGRA)
    NCET = MBE(IGRA)
    IP1 = JTP(IGRA)
    IPER = JPC(IGRA)
    TC$ = Left$(KEMIC$(IPER), 4)
    NX = IP1 - IP + 1
    J = 0
    For I = IP To IP1
        J = J + 1
        XGX!(J) = TITRE!(I)
        YGE!(J) = EMF!(I)
        YY!(J) = EMF!(I) - RES(I)
    Next I
    IP = IP1 + 1
    Call GRAPH 'module GRAMOD
    If IGR = 0 GoTo 35
Next IGRA
GoTo 35
End

VIDEOERR:
HH = HH + 1
BESTMODE = BM(HH)
ISC = ISCC(HH)
If HH > 2 Then
    Print "Sorry graphics not available."
    Input " Push Enter", PP
End If
Resume

' Trap to detect 64K EGA

EGAERR:
BESTMODE = EGA64
AVAILABLE = "12789"
ISC = 4
Resume Next

ERRORPROC:

Select Case Err
    Case 4
        Print "Out of data"
        Resume 25
    Case 5
        GoSub patherr
        Print "Illegal function call between label "; Erl;
        Print " and subsequent label"
        Resume 25
    Case 6
        GoSub patherr
        Print "Overflow between label "; Erl; " and subsequent label"
        Resume 25
    Case 7
        GoSub patherr
        Print "Out of memory"
        Print "Please recover memory in your RAM!"
        Print "Remove TSR program from memory or load TSR in high memory."
        Resume 25
    Case 9
        Print "memory overflow"
        End
    Case 11
        GoSub patherr
        Print "Division by zero between label "; Erl; " and subsequent label"
        Resume 25
    Case 25
        Print "Device fault - Hit any key (ESC for exit)"
        Beep
        A$ = Input$(1)
        If A$ = Chr$(27) Then End
        Resume
    Case 27
        Print "Out of paper"
        Print "Insert the paper, then hit any key to continue"
        A$ = Input$(1)
        Resume
    Case 52
        Print "Bad file number"
        If IFIL = 1 Then Resume 25 Else Resume 26
    Case 53
        If IFIL = 3 Then Close #2: Resume 27
        If IFIL = 4 Then Close #4: Resume 28
        Print "File not found"
        If IFIL = 1 Then Resume 25 Else Resume 26
    Case 55
        Print "File already open"
        Resume 26
    Case 57
        Print "Device I/O error"
        If IFIL = 1 Then Resume 25 Else Resume 26
    Case 61
        Print "Disk full"
        End
    Case 62
        Print "Input past end"
        Resume 25
    Case 64
        Print "Bad file name"
        If IFIL = 1 Then Resume 25 Else Resume 26
    Case 68
        Print "Device unavailable"
        Resume 25
    Case 70
        Print "Disk write protect"
        Print "Remove protection, then hit any key to continue"
        A$ = Input$(1)
        Resume
    Case 71
        If UCase$(OFILE$) <> "PRN" GoTo 5
        Print "Printer not ready - active printer!": GoTo 6
        5 Print "Disk not ready - Insert floppy disk!"
        6 Print "Then hit any key to continue"
        A$ = Input$(1)
        Resume
    Case 72
        Print "Disk media error"
        End
    Case 75
        Print "Path/file access error"
        If IFIL = 1 Then Resume 25 Else Resume 26
    Case 76
        Print "Path not found"
        If IFIL = 1 Then Resume 25 Else Resume 26
    Case Else
        GoSub patherr
        Print "Code Error"; Err; " between label "; Erl;
        Print #2, " and subsequent label"
        End
End Select

patherr:
   
Print #2, ""
Print #2, "main";
For IER = 2 To NSUB: Print #2, " -> " + STER$(IER);: Next IER
Print #2, ""
Print #2, "error in "; STER$(NSUB)
Return

'
Sub CHECK

    ' prints the title and checks for failure

    TIME2 = Timer
    DTIME = TIME2 - TIME1
    HOUR = Int(DTIME / 3600)
    DT1 = DTIME - HOUR * 3600
    MIN = Int(DT1 / 60)
    SEC = DT1 - MIN * 60
    Print #2, ""
    Print #2, "final time   "; Time$
    Print #2, Using "net time     ##:##:##.##"; HOUR; MIN; SEC
    Print #2, ""
    Print #2, "Table 3."
    Print #2, ""
    Print #2, Using " #### iterations"; NCICL
    On IFAIL GOTO 622, 624, 626, 628, 630, 632, 632, 634, 636
    622 Print #2, " refinement terminated successfully": Exit Sub
    624 Print #2, Using "#### iterations (maximum) completed"; MAXIT: Exit Sub
    626 Print #2, " predicted improvement negative in MAIN program"
    Print #2, " convergence probably attained": Exit Sub
    628 Print #2, " normal equations singular in MAIN program"
    Print #2, " parameters cannot be refined": Exit Sub
    630 Print #2, " shift vector points uphill in MAIN program"
    Print #2, " parameters cannot be refined": Exit Sub
    632 Print #2, " free concentrations cannot be calculated"
    Print #2, " there may be a gross inconsistency in the data": Exit Sub
    636 Print #2, " free concentrations cannot be calculated"
    Print #2, " but refinement may have converged": IFAIL = 2
634 End Sub

'
Sub CONSTR

    ' sets up the constraints and allocates to each dangerous parameter
    ' the appropriate column in the design matrix. Constrained parameters
    ' are associated with the same column there

    K = NKV
    NCONS = 0
    For I = 1 To NSP
        JI = JCOL(I)
        If JI > 0 GoTo 648
        K = K + 1
        JCOL(I) = K
        If JI = -100 GoTo 648
        IP1 = I + 1
        For J = IP1 To NSP
            If JCOL(J) = JI Then
                NCONS = NCONS + 1
                If JTYP(I) <> JTYP(J) Then
                    Print #2, " wrong constraint setting between special parameters";
                    Print #2, Using "### and ###"; J; I: Stop
                End If
                KCONS(NCONS, 1) = J
                KCONS(NCONS, 2) = I
                JCOL(J) = JCOL(I)
            End If
        Next J
    648 Next I
    If NCONS = 0 Then Exit Sub

    ' the equality constraints on the dangerous parameters are written out

    Print #2, "": Print #2, Using "### constraints"; NCONS
    Print #2, "   the relative shifts of the";
    Print #2, " following parameters are constrained to be equal"
    For K = 1 To NCONS
        I = KCONS(K, 1)
        J = KCONS(K, 2)
        JTI = JTYP(I)
        JCI = JCUR(I)
        JCJ = JCUR(J)
        If JTI = 1 Then JRI = JREA(I): JRJ = JREA(J)
        Print #2, Using "    \\"; NAMTYP$(JTI);
        If JTI = 1 Then Print #2, Using "#"; JRI; Else Print #2, " ";
        Print #2, Using " curve### = \\"; JCI; NAMTYP$(JTI);
        If JTI = 1 Then Print #2, Using "#"; JRJ; Else Print #2, " ";
        Print #2, Using " curve###"; JCJ
    Next K
End Sub

'
Sub DINS

    ' this subroutine collects the titration curve data

    ReDim KEYMM(200)

    650 Line Input #1, TITLE$

    '  TITLE$: title of the job

    Print #2, Chr$(12)
    Print #2, "program BSTAC4"
    Print #2, "BASIC version 7.3 - 282001"
    Print #2, "INPUT filename: "; NFILE$
    Print #2, "OUTPUT device/filename: "; OFILE$
    Print #2, ""
    Print #2, "day "; Date$
    Print #2, ""
    Print #2, TITLE$
    Print #2, ""
    Input #1, MAXIT, NMBE, NKM, MODE, ICD, WESP, PERC

    '  MAXIT = maximum no. of iterations cycles
    '  NMBE = no. of components
    '  NKM = no. of species
    '  MODE = 1: weights=1 (unit weight for each experimental point)
    '       = 0: weights=1/SIGMA**2
    '       = 2: weights=1/SIGP**2 (SIGP given by the user)
    '  ICD = 0: the constant medium method is used in the job
    '      = 1: the titrations in the job are at different ionic strenghts
    '      = 2: as ICD=1 with E0=f(CGE,DGE,EGE) , reads values CGE,DGE,EGE
    '  WESP = weights=1/DE^WESP
    '  PERC =limit of the shifts % (PERC=0 no limit for shifts)

    For I = 1 To NMBE: Input #1, KEMIC$(I): Next I

    '  KEMIC$ = name of mass balance equation

    Input #1, TEMP, PHI, PHF

    '  TEMP = temperature in degrees cent.
    '  PHI,PHF - pH range to be considered in the job
    '            (if PHI=PHF=0 all the points are considered)

    Print #2, Using "Temperature  ###.## deg.cent."; TEMP
    If MODE = 0 Then
        Print #2, "weights   1/sig2"
    Else
        Print #2, "weights   1"
    End If
    JPH = 0
    If PHI + PHF = 0! GoTo 652
    JPH = 1
    Print #2, Using "range pH  ##.##"; PHI;
    Print #2, "-";
    Print #2, Using "##.##"; PHF
    652 RTF = (TEMP + 273.15) / 11.6048
    DT25 = TEMP - 25!
    NCD = 0
    If ICD > 0 Then
        Input #1, IREF#, AT, BT, c0M, c1M, d0M, d1M, e0M, e1M
        For K = 1 To 6: Input #1, KCD(K): Next K

        '  IREF = reference ionic strength
        '         (if IREF<0, then IB for each formation constant must be given)
        '  AT,BT = A and B parameters of Debye-Huckel type equation
        '          (if AT=BT=0 then program fixes AT=0.5, BT=1.5)
        '  c0M,c1M,d0M,d1M,e0M,e1M = parameters for the ionic strength dependence of
        '                    formation constants [text eqn.(9a)]
        '  KCD(1) = 0: c0 constant
        '         = 1: c0 refined
        '  KCD(2) = 0: c1 constant
        '         = 1: c1 refined
        '  KCD(3) = 0: d0 constant
        '         = 1: d0 refined
        '  KCD(4) = 0: d1 constant
        '         = 1: d1 refined
        '  KCD(5) = 0: e0 constant
        '         = 1: e0 refined
        '  KCD(6) = 0: e1 constant
        '         = 1: e1 refined

    End If
    For K = 1 To NMBE: Input #1, ZZ(K): Next K

    '  ZZ = charge of components
   
    If ICD > 0 Then
        If ICD = 2 Then
            Input #1, CGE, DGE, EGE

            '  CGE,DGE,EGE: E0=f(CGE,DGE,EGE)

        Else
            CGE = .335 - .0023 * DT25
            DGE = -.109 + .0018 * DT25
            EGE = 0!
        End If
        If AT = 0! And BT = 0! Then AT = .5: BT = 1.5
        If c0M = 0! And c1M = 0! And d1M = 0! Then
            c0M = .11
            c1M = .2 - .0006 * DT25
            d1M = -.075 + .0007 * DT25
        End If
        For K = 1 To 6
            If KCD(K) = 1 Then NCD = NCD + 1: JCD(NCD) = K
        Next K
        Print #2, ""
        If IREF# < 0! GoTo 653
        Print #2, Using "Reference ionic strength  ####.###"; IREF#
        Print #2, ""
        653 Print #2, "Ionic strength parameters"
        Print #2, Using "A= #.####   B= #.####"; AT; BT
        Print #2, Using "c0=###.##   c1=###.##"; c0M; c1M
        Print #2, Using "d0=###.##   d1=###.##"; d0M; d1M
        Print #2, Using "e0=###.##   e1=###.##"; e0M; e1M
        Print #2, Using "C(E0)=##.###  D(E0)=##.###  E(E0)=##.###"; CGE; DGE; EGE
        Print #2, ""
    End If
    For K = 1 To NMBE
        Print #2, "reactant "; K; " - "; KEMIC$(K); " (";
        Z2(K) = ZZ(K) ^ 2
        IZ = ZZ(K)
        IZZ = Abs(IZ)
        If IZ <> ZZ(K) Then
            Print #2, Using "##.##"; Abs(ZZ(K));
        Else
            If IZZ <> 1 Then Print #2, Using "##"; IZZ;
        End If
        If IZ > 0 Then Print #2, "+";
        If IZ < 0 Then Print #2, "-";
        Print #2, ")"
    Next K
    Print #2, ""
    Print #2, "       logK";
    If ICD > 0 Then Print #2, "        Iref     C       D       E";
    Print #2, "     Z*     P*  species";
    If NKAM = 1 Then Print #2, "     +K?" Else Print #2, ""
    NKV = NCD
    I = 0
    For I1 = 1 To NKM
        Input #1, KLOG#

        '  KLOG: log(formation constant)

        If ICD > 0 Then
            If IREF# < 0! Then Input #1, IBM# Else IBM# = IREF#
            Input #1, CGM, DGM, EGM
        End If

        '  IBM: reference ionic strength (to be given only if IREF<0 and ICD>0)
        '  CGM,DGM,EGM: parameters of DEBYE-HUCKEL equation
        '               [text eqn.(9b)] (only if ICD>0)
        '               (if program reads c0M,c1M,d0M,d1M,e0M,e1M set CGM=DGM=EGM=0)
     
        657 For K = 1 To NMBE: Input #1, JQRM(K): Next K

        '  JQRM: stoichiometric coefficients
     
        Input #1, KEYM
        If ICD > 0 Then Input #1, KEYCM, KEYDM, KEYEM

        '  KEYMX: index for refinement
        '       KEYM = 0: parameter K constant
        '               1: refines K
        '              -1: species ignored in the model
        '       KEYCM = 0: parameter C constant
        '               1: refines C
        '       KEYDM = 0: parameter D constant
        '               1: refines D
        '       KEYEM = 0: parameter E constant
        '               1: refines E
     
        Input #1, NKA
        For J = 1 To NKA: Input #1, IKA(J): Next J

        '  NKA: no. of K to be added for obtaining BETA
        '  IKA: indexes of K to be added for obtaining BETA

        If KEYM = -1 GoTo 667
        I = I + 1
        KIY(I) = KEYM
        KEYC(I) = KEYCM
        KEYD(I) = KEYDM
        KEYE(I) = KEYEM
        For J = 1 To 4: AST$(J) = " ": Next J
        J = KLOG#
        JPOT(I) = J
        W1 = KLOG# - J
        KAP#(I) = Exp(W1 * AL10)
        JPOTM(I) = J
        If KEYM = 1 Then AST$(1) = "*"
        If KEYCM = 1 Then AST$(2) = "*"
        If KEYDM = 1 Then AST$(3) = "*"
        If KEYEM = 1 Then AST$(4) = "*"
        69 Print #2, Using " ##  ###.###\\ "; I; KLOG#; AST$(1);
        ZAST(I) = 0!
        KSP(I) = 0
        XZ = 0!
        XZ2 = 0!
        KS = -1
        For K = 1 To NMBE
            JQ = JQRM(K)
            KS = KS + JQ
            XZ = XZ + JQ * ZZ(K)
            XZ2 = XZ2 + JQ * Z2(K)
        Next K
        KSP(I) = KS
        ZAST(I) = XZ2 - XZ * XZ
        ZT(I) = XZ
        If ICD > 0 Then
            CG(I) = CGM
            DG(I) = DGM
            EG(I) = EGM
            RADIB(I) = Sqr(IBM#)
            FIB(I) = AT * RADIB(I) / (1! + BT * RADIB(I))
            RIB(I) = IBM# * RADIB(I)
            IB#(I) = IBM#
            If CGM = 0! And DGM = 0! And EGM = 0! Then
                CG(I) = c0M * KSP(I) + c1M * ZAST(I)
                DG(I) = d0M * KSP(I) + d1M * ZAST(I)
                EG(I) = e0M * KSP(I) + e1M * ZAST(I)
                If JQRM(NMBE) < 0 Then CG(I) = CG(I) + .015 * JQRM(NMBE)
            End If
            Print #2, Using " ###.### "; IBM#;
            Print #2, Using "###.##\\"; CG(I); AST$(2); DG(I); AST$(3); EG(I); AST$(4);
        End If
        IZAS = ZAST(I)
        If IZAS = ZAST(I) Then
            Print #2, Using " ###    ### "; IZAS; KSP(I);
        Else
            Print #2, Using " ###.## ### "; ZAST(I); KSP(I);
        End If
        Print #2, " ";
        For K = 1 To NMBE
            JJ = JQRM(K)
            JQR(I, K) = JJ
            If JJ = 0 GoTo 661
            If JJ > 0 Then
                SPE = Len(KEMIC$(K))
                C$ = Left$(KEMIC$(K), SPE)
            Else
                C$ = "OH"
                JJ = -JJ
            End If
            Print #2, "("; C$; ")";
            If JJ > 1 Then
                STC$ = Str$(JJ)
                LSTC = Len(STC$) - 1
                IX$ = Right$(STC$, LSTC)
                Print #2, IX$;
            End If
        661 Next K
        If NKA > 0 Then
            Print #2, " ";
            For J = 1 To NKA
                IKAP(I, J) = IKA(J)
                IK = IKA(J)
                If IK < 10 Then
                    Print #2, Using "+K#"; IK;
                Else
                    Print #2, Using "+K##"; IK;
                End If
            Next J
        End If
        Print #2, ""
        NKAP(I) = NKA
        NKV = NKV + KEYM + KEYCM + KEYDM + KEYEM
    667 Next I1
    NK = I
    Print #2, " * value to be refined"
    For J = 1 To NK
        BETA(J) = KAP#(J) * Exp(JPOT(J) * AL10)
        NKA = NKAP(J)
        If NKA = 0 GoTo 669
        For K = 1 To NKA
            IKA = IKAP(J, K)
            BETA(J) = BETA(J) * KAP#(IKA) * Exp(JPOT(IKA) * AL10)
        Next K
    669 Next J
    NPAR = NKV
    NSPC = NMBE + 4
    For I = 1 To NK
        For J = 1 To NMBE - 1
            If JQR(I, J) <> 0 GoTo 21
        Next J
        If JQR(I, NMBE) = -1 Then
            INDKW = I
            KW# = BETA(I)
            GoTo 12
        End If
    21 Next I
    INDKW = 0
    KW# = 0!
    12 NSP = 0
    NCONS = 0
    NTC = 0
    IP = 0
    IPP = 0
    NC0 = NMBE
    NTABR = 45
    Print #2, ""
    Print #2, "Table 1."
    Print #2, ""
    Print #2, "   TITRATION       no. of";
    Print #2, "       VO     s(V)    SLOPE    CO(I) ";
    NTAB = NTABR + 12
    For I = 1 To NMBE
        NTAB = NTAB + 10
        If NTAB <= 120 GoTo 665
        Print #2, ""
        NTAB = NTABR + 22
        If NC0 = NMBE Then NC0 = I
        665 Print #2, Tab(NTAB); "CO";
    Next I
    Print #2, ""
    NTAB = NTABR + 12
    Print #2, "no."; Tab(20); "points";
    Print #2, "       EO     s(E)     ja      CT(I) ";
    For I = 1 To NMBE
        NTAB = NTAB + 10
        If NTAB > 120 Then
            Print #2, ""
            NTAB = NTABR + 22
        End If
        Print #2, Tab(NTAB); "CT";
    Next I
    Print #2, ""
    Print #2, Tab(29); "electrode";
    If ICD > 0 Then Print #2, Tab(41); "IT";
    Print #2, Tab(49); "jb";
    Print #2, Tab(NTABR + 22);
    NTAB = NTABR + 12
    For I = 1 To NMBE
        NTAB = NTAB + 10
        If NTAB > 120 Then
            Print #2, ""
            NTAB = NTABR + 22
            Print #2, Tab(NTAB);
        End If
        Print #2, Using "\        \"; KEMIC$(I);
    Next I
    Print #2, ""

    ' begin reading in a new titration curve

    662 If EOF(1) GoTo 702
    Line Input #1, NAMET$

    '  NAMET$: title of the titration

    Input #1, JP, MBET

    '  JP: percentages (final table) are calculated with respect to the
    '      component JP
    '  MBET: the e.m.f. (or pX=-log[X]) read in for the titration refers to the
    '        component MBET

    664 If MBET = 0 Then MBET = NMBE
    NTC = NTC + 1
    JPC(NTC) = JP
    MBE(NTC) = MBET
    TITL$(NTC) = NAMET$
    For I = 1 To NMBE
        Input #1, TOTMM(NTC, I), ADDC(NTC, I), LOK(I)
    Next I

    '  TOTMM = initial concentration in the titration vessel (mol/L)
    '  ADDC = concentration in the titrant (mol/L)
    '  LOK(I) = 0: TOTMM constant
    '         = 1: TOTMM refined

    Input #1, COI, CTI, IREFT#

    '  COI: concentration of ionic species which does not take place in
    '       complexation reactions (reaction vessel)
    '  CTI: idem in the titrant solution
    '  IREFT: initial value of ionic strength (not used for ICD=0)

    ' the total mol/L and burette concentrations have now been read in
    ' next the initial volume and its error and then the electrode data

    Input #1, VO, SIGMAV

    '  VO: initial volume (cm**3)
    '  SIGMAV: estimated standard deviation in the titrant volume

    Input #1, EO, SIGMAE, JA#, JB#, SLOPE
    Input #1, LOK(NMBE + 1), LOK(NMBE + 2), LOK(NMBE + 3), LOK(NMBE + 4)

    '  EO: standard potential of the electrode in mV (EO=0 if reads pH)
    '  SIGMAE: estimated standard deviation in e.m.f. (EO<>0) or pH (EO=0)
    '  JA: linear coefficient of acid junction potential
    '  JB: linear coefficient of basic junction potential
    '  SLOPE: slope
    '         (if SLOPE=0 the program calculates SLOPE=Nernstian value)
    '  LOK = 0: EO (JA,JB,SLOPE) constant
    '      = 1: EO (JA,JB,SLOPE) refined

    IVAL# = IREFT#
    EZERO(NTC) = EO
    VINIT(NTC) = VO
    JAM#(NTC) = JA#
    JBM#(NTC) = JB#
    SIGMV(NTC) = SIGMAV
    SIGME(NTC) = SIGMAE
    If SLOPE = 0! Then SLOPE = RTF * AL10

    ' the pointers JCUR, JTYP, JREA and JCOL are set up from the LOK
    ' values to index the dangerous parameters for refinement.
    ' JTYP is 1 for total mol/L, 2 for standard electrode potential,
    ' 3 for JA, 4 for JB, 5 for SLOPE. JREA stores the numbers corresponding
    ' to the reagents and JCUR its titration curve. JCOL temporarely stores
    ' LOK-101 if LOK is different from zero. The values of JCOL
    ' will be adjusted in subroutine CONSTR to deal with any constraints

    For I = 1 To NSPC
        If LOK(I) = 0 GoTo 668
        NSP = NSP + 1
        JCUR(NSP) = NTC
        JTYP(NSP) = 1
        JREA(NSP) = I
        If I > NMBE Then JTYP(NSP) = I - NMBE + 1
        JCOL(NSP) = LOK(I) - 101
    668 Next I
    RTJ(NTC) = SLOPE
    SLOP = SLOPE / AL10
    If EO = 0! Then SIGMAE = SIGMAE * SLOPE
    COI(NTC) = COI
    CTI(NTC) = CTI
    If ICD > 0 Then
        IT#(NTC) = IVAL#
        FIT(NTC) = AT * Sqr(IVAL#) / (1! + BT * Sqr(IVAL#))
        RIT(NTC) = IVAL# * Sqr(IVAL#)
    End If
    SIGV = SIGMAV * SIGMAV
    SIGE = SIGMAE * SIGMAE

    ' read in a new titration point

    684 Input #1, TITV, EMFC
    If MODE = 2 Then Input #1, SIGP
    Input #1, IND

    '  TITV: titrant volume
    '  EMFC: potential (or pH, if EO=0)
    '  SIGP: sigma for W=1/sigp^2
    '  IND = 0: normal
    '      = 1: for the last point of the titration

    If TITV = 0 GoTo 694
    If EO <> 0! Then
        PH = (EO - EMFC) / SLOPE
    Else
        PH = EMFC
    End If
    If JPH = 0 GoTo 683
    If PH < PHI Or PH > PHF GoTo 694
    683 IP = IP + 1
    If IP <= MAXP GoTo 688
    Print #2, " the number of data points is "; IP; ">"; MAXP
    GoTo 720
    688 TITRE!(IP) = TITV
    If MODE = 2 Then WS!(IP) = 1! / (SIGP * SIGP): SIGMAP!(IP) = SIGP
    If ICD = 0 GoTo 690
    VTV = VO + TITV
    CV(IP) = (COI * VO + CTI * TITV) / VTV
    SAZ = 0!
    For J = 1 To NMBE
        CCAN = (TOTMM(NTC, J) * VO + ADDC(NTC, J) * TITV) / VTV
        SAZ = SAZ + CCAN * Z2(J)
    Next J
    IVA#(IP) = (CV(IP) + SAZ) * .5
    690 If EO <> 0! Then
        EMF!(IP) = EMFC
    Else
        EMF!(IP) = EO - EMFC * SLOPE
    End If
    For J = 1 To NMBE: FREEC!(IP, J) = 0!: Next J
    694 If IND = 0 GoTo 684

    ' a complete curve has been read in

    K = IPP + 1
    NPC = IP - IPP
    If MODE = 2 GoTo 698
    If MODE = 1 Then
        For I = K To IP: WS!(I) = 1: Next I
        GoTo 698
    End If

    ' the weights matrix is calculated and placed in WS

    NSUB = NSUB + 1
    STER$(NSUB) = SUBR$(13)
    Call WCURV 'subroutine
    NSUB = NSUB - 1
    698 Print #2, Using "#### \                      \"; NTC; NAMET$;
    Print #2, Using "####.#   #.###   ###.## "; VO; SIGMAV; SLOPE;
    Print #2, Using "###.######"; COI;
    NTAB = NTABR + 12
    For I = 1 To NMBE
        NTAB = NTAB + 10
        If NTAB > 120 Then
            Print #2, ""
            NTAB = NTABR + 19
            Print #2, Tab(NTAB);
        End If
        Print #2, Using "###.######"; TOTMM(NTC, I);
    Next I
    Print #2, ""
    Print #2, Tab(20); Using "####      "; NPC;
    Print #2, Using "####.#   #.###  ####.##"; EO; SIGMAE; JA#;
    Print #2, Using " ###.######"; CTI;
    NTAB = NTABR + 12
    For I = 1 To NMBE
        NTAB = NTAB + 10
        If NTAB > 120 Then
            Print #2, ""
            NTAB = NTABR + 19
            Print #2, Tab(NTAB);
        End If
        Print #2, Using "###.######"; ADDC(NTC, I);
    Next I
    Print #2, ""
    Print #2, Tab(33); KEMIC$(MBET);
    If ICD > 0 Then Print #2, Tab(38); Using "##.##"; IVAL#;
    Print #2, Tab(45); Using " ####.##"; JB#
    700 IPP = IP
    JTP(NTC) = IP
    GoTo 662
    702 Print #2, ""
    Print #2, " total no. of points = ";
    Print #2, Using "####"; IP
    Print #2, ""

    ' a complete set of data has now been read in

    If NCD = 0 GoTo 714
    Print #2, "ionic strength parameters to be refined:"
    For I = 1 To NCD
        J = JCD(I)
        On J GOTO 704, 706, 708, 710, 709, 711
        704 Print #2, Using "c0= ##.####"; c0M: GoTo 712
        706 Print #2, Using "c1= ##.####"; c1M: GoTo 712
        708 Print #2, Using "d0= ##.####"; d0M: GoTo 712
        710 Print #2, Using "d1= ##.####"; d1M: GoTo 712
        709 Print #2, Using "e0= ##.####"; e0M: GoTo 712
        711 Print #2, Using "e1= ##.####"; e1M
    712 Next I
    Print #2, ""
    714 If NSP = 0 GoTo 718
    Print #2, Using "### titration parameters to be refined"; NSP
    Print #2, "   curve           value"
    For I = 1 To NSP
        JT = JTYP(I)
        JC = JCUR(I)
        If JT = 1 GoTo 715
        If JT = 2 Then VALUE = EZERO(JC): GoTo 716
        If JT = 3 Then VALUE = JAM#(JC): GoTo 716
        If JT = 4 Then VALUE = JBM#(JC) Else VALUE = RTJ(JC)
        GoTo 716
        715 JR = JREA(I)
        VALUE = TOTMM(JC, JR)
        716 Print #2, Using "     ##   \\"; JC; NAMTYP$(JT);
        If JT = 1 Then Print #2, Using "#"; JR; Else Print #2, " ";
        Print #2, Using "######.#####"; VALUE
    Next I
    NSUB = NSUB + 1
    STER$(NSUB) = SUBR$(5)
    Call CONSTR 'subroutine
    NSUB = NSUB - 1
    718 KVSP = NKV + NSP
    NPAR = KVSP - NCONS
    Exit Sub
    720 Print #2, "therefore run abandoned before refinement"
    Stop
End Sub

'
Sub DINSM

    ' this subroutine reads data for calculating dimensions

    3 AL10 = Log(10#)
    Line Input #1, TITLE$
    Print TITLE$
    Input #1, MAXIT, NMBE, NKM, MODE, ICD, WESP, PERC
    If MAXIT < 0 Then Print "MAXIT ="; MAXIT; " must be >= 0": End
    If NMBE <= 0 Then Print "NMBE ="; NMBE; " must be > 0": End
    If NKM <= 0 Then Print "NKM ="; NKM; " must be > 0": End
    If (MODE < 0) Or (MODE > 2) Then
        Print "MODE ="; MODE; "; must be (0,1,2)": End
    End If
    If (ICD < 0) Or (ICD > 2) Then
        Print "ICD ="; ICD; " must be (0,1,2)": End
    End If
    For I = 1 To NMBE: Line Input #1, KEM$: Next I
    Input #1, TEMP, PHI, PHF
    If PHI > PHF Then
        15 Print , "PHI>PHF"; PHI; PHF: End
    End If
    If PHI > PHF Then Print "PHI > PHF "; PHI; PHF: End
    If PHI + PHF = 0! Then JPH = 0 Else JPH = 1
    RTF = (TEMP + 273.15) / 11.6048
    NCD = 0
    If ICD > 0 Then
        Input #1, IREF#, AT, BT, c0M, c1M, d0M, d1M, e0M, e1M
        For K = 1 To 6
            Input #1, KCD!
            If (KCD! < 0) Or (KCD! > 1) Or Int(KCD!) <> KCD! Then
                4 Print "KCD ="; KCD!; " must be (0,1)": End
            End If
            If KCD! = 1 Then NCD = NCD + 1
        Next K
    End If
    For K = 1 To NMBE: Input #1, ZZ: Next K
    If ICD = 2 Then Input #1, CGE, DGE, EGE
    NKV = NCD
    NKAM = 0
    I = 0
    For I1 = 1 To NKM
        Input #1, KLOG#
        If ICD > 0 Then
            If IREF# < 0! Then Input #1, IBM#
            Input #1, CGM, DGM, EGM
        End If
        For K = 1 To NMBE
            Input #1, JQRM!
            If Int(JQRM!) <> JQRM! Then
                10 Print , "JQRM="; JQRM!; " must be integer": End
            End If
        Next K
        Input #1, KEYM
        If ICD > 0 Then Input #1, KEYCM, KEYDM, KEYEM
        Input #1, NKA
        If NKA > 0 Then
            For J = 1 To NKA: Input #1, IKAP: Next J
            NKAM = 1
        End If
        If (KEYM < -1) Or (KEYM > 1) Then
            7 Print "KEYM ="; KEYM; " must be (-1,0,1)": End
        End If
        If KEYM >= 0 Then
            I = I + 1
            NKV = NKV + KEYM + KEYCM + KEYDM + KEYEM
        End If
        Print
    Next I1
    NK = I
    NPAR = NKV
    NSPC = NMBE + 4
    NSP = 0
    NCONS = 0
    NTC = 0
    IP = 0
    IPP = 0
    MAXPG = 0
    1662 If EOF(1) GoTo 1702
    Line Input #1, NAMET$
    Print NAMET$
    Input #1, JP, MBET
    Print JP; MBET
    If (JP < 1) Or (JP > NMBE) Then
        Print "JP ="; JP; " must be (1.."; NMBE; ")": End
    End If
    If (MBET < 0) Or (MBET > NMBE) Then
        Print "MBET ="; MBET; " must be (1.."; NMBE; ")": End
    End If
    NTC = NTC + 1
    For I = 1 To NMBE
        Input #1, TOTMM, ADDC, LOK
        Print TOTMM; ADDC; LOK
        If LOK < 0 Then
            Print "LOK ="; LOK; " must be >= 0": End
        End If
        If LOK > 0 Then NSP = NSP + 1: JCOL(NSP) = LOK - 101
    Next I
    Input #1, COI, CTI, IVAL#
    Print COI; CTI; IVAL#
    Input #1, VO, SIGMAV
    Print VO; SIGMAV
    Input #1, EO, SIGMAE, JA#, JB#, SLOPE
    Print EO; SIGMAE; JA#; JB#; SLOPE;
    For I = 1 To 4
        Input #1, LOK
        Print LOK;
        If LOK < 0 Then
            Print "LOK ="; LOK; " must be >=0": End
        End If
        If LOK > 0 Then NSP = NSP + 1: JCOL(NSP) = LOK - 101
    Next I
    Print
    If SLOPE = 0! Then SLOPE = RTF * AL10
    1684 Input #1, TITV, EMFC
    Print TITV; EMFC;
    If MODE = 2 Then Input #1, SIGP: Print SIGP;
    Input #1, IND
    Print IND
    If (IND < 0) Or (IND > 1) Then
        Print "IND ="; IND; " must be (0,1)"
        End
    End If
    If TITV = 0 GoTo 1694
    If JPH = 0 GoTo 1695
    If EO = 0! Then PH = EMFC Else PH = (EO - EMFC) / SLOPE
    If PH < PHI Or PH > PHF GoTo 1694
    1695 IP = IP + 1
    1694 If IND = 0 GoTo 1684
    K = IPP + 1
    NPC = IP - IPP
    IPP = IP
    If MAXPG < NPC Then MAXPG = NPC
    GoTo 1662
    1702 If NSP = 0 GoTo 1718
    K = NKV
    For I = 1 To NSP
        JI = JCOL(I)
        If JI > 0 GoTo 1648
        K = K + 1
        JCOL(I) = K
        If JI = -100 GoTo 1648
        IP1 = I + 1
        For J = IP1 To NSP
            If JCOL(J) <> JI GoTo 1646
            NCONS = NCONS + 1
            JCOL(J) = JCOL(I)
        1646 Next J
    1648 Next I
    1718 KVSP = NKV + NSP
    NPAR = KVSP - NCONS
End Sub

'
Sub FUNV

    ' the core routine of the main minimisation

    UF = 0!
    IPF = 0

    ' beginning of loop for each titration curve. Extract the relevant
    ' information from the global storage variables

    For JTC = 1 To NTC
        IPI = IPF + 1
        IPF = JTP(JTC)
        MBET = MBE(JTC)
        VO = VINIT(JTC)
        For J = 1 To NMBE
            TOMM(J) = TOTMM(JTC, J)
            ADC(J) = ADDC(JTC, J)
            CX(J) = 0!
        Next J
        RTJC = RTJ(JTC) / AL10
        EO = EZERO(JTC)
        JA# = JAM#(JTC)
        JB# = JBM#(JTC)
        IVAL# = 1#
        If ICD > 0 Then IVAL# = IVA#(IPI)
        If JA# = 0! GoTo 771
        If MBET = NMBE GoTo 783
        JAB# = JA# * CX(NMBE) + JB# * KW# / CX(NMBE)
        CX(MBET) = Exp((EMF!(IPI) - EO - JAB#) / RTJC)
        GoTo 773
        783 IP = IPI
        NSUB = NSUB + 1
        STER$(NSUB) = SUBR$(14)
        Call HJA(EO, RTJC, IVAL#, JA#, JB#) 'subroutine
        NSUB = NSUB - 1
        GoTo 773
        771 CX(MBET) = Exp((EMF!(IPI) - EO) / RTJC)
        773 For K = 1 To NK: POT(K) = AL10 * JPOT(K): Next K

        ' beginning of the loop through all points in a curve

        For IP = IPI To IPF
            If ICD = 0 GoTo 777
            IVAL# = IVA#(IP)
            RADI = Sqr(IVAL#)
            FI = AT * RADI / (1! + BT * RADI)
            For K = 1 To NK
                DFI(K) = FI - FIB(K)
                DI(K) = IVAL# - IB#(K)
                DI2(K) = IVAL# * IVAL# - IB#(K) * IB#(K)
                DIR(K) = IVAL# * RADI - RIB(K)
            Next K
            '        IF NCICL > 1 OR IW = 2 THEN
            DFIE = FI - FIT(JTC)
            DIE = IVAL# - IT#(JTC)
            DIE2 = IVAL# * IVAL# - IT#(JTC) * IT#(JTC)
            DIRE = IVAL# * RADI - RIT(JTC)
            CDE = CGE * DIE + DGE * DIRE + EGE * DIE2
            EO = EZERO(JTC) - RTJC * AL10 * (DFIE * Z2(MBET) - CDE)
            '        END IF
            777 If JA# = 0! GoTo 775
            If MBET = NMBE GoTo 785
            CTE = FREEC!(IP, NMBE)
            CX(MBET) = Exp((EMF!(IP) - EO - JA# * CTE - JB# * KW# / CTE) / RTJC)
            GoTo 772
            785 NSUB = NSUB + 1
            STER$(NSUB) = SUBR$(14)
            Call HJA(EO, RTJC, IVAL#, JA#, JB#) 'subroutine
            NSUB = NSUB - 1
            GoTo 772
            775 CX(MBET) = Exp((EMF!(IP) - EO) / RTJC)
            772 VOL = VO + TITRE!(IP)
            I = 0
            For J = 1 To NMBE
                TT(J) = (TOMM(J) * VO + TITRE!(IP) * ADC(J)) / VOL
                If Abs(TT(J)) > .0000000001# Then
                    I = I + 1
                    JCON(I) = J
                End If
            333 Next J
            NGAU = I
            If JACK = 3 GoTo 781

            ' this is true only on cycle 1. Then estimates of free concentration
            ' are taken as the values at the previous point. On subsequent
            ' cycles the estimates are taken as the value at previous cycle

            For J = 1 To NMBE
                CX(J) = FREEC!(IP, J)
            Next J
            781 For I = 1 To NMBE: TCR(I) = TT(I): Next I
            For I = 1 To NK: EPSR(I) = DT(I): Next I
            NSUB = NSUB + 1
            STER$(NSUB) = SUBR$(3)
            Call CCFR 'subroutine
            NSUB = NSUB - 1
            For I = 1 To NMBE: TT(I) = TCR(I): Next I
            For I = 1 To NK: DT(I) = EPSR(I): Next I
            If ICD = 0 GoTo 774
            If JACK = 2 GoTo 774
            SCZ = 0!
            SAZ = 0!
            For J = 1 To NMBE: SAZ = SAZ + CX(J) * Z2(J): Next J
            For J = 1 To NK: SCZ = SCZ + CI(J) * ZT(J) ^ 2: Next J
            IVAL# = (SCZ + SAZ + CV(IP)) * .5
            IVA#(IP) = IVAL#
            774 If IFAIL = 1 GoTo 776

            ' calculation of free concentrations has failed almost certainly
            ' due to there being parameter(s) having a wrong sign, e.g.
            ' negative formation constants

            If IFAIL <> 6 Then Exit Sub
            Print #2, Using "free concns calculation failed at point####"; IP
            Exit Sub
            776 For J = 1 To NMBE: FREEC!(IP, J) = CX(J): Next J

            ' the free concentrations are now stored in FREEC
            ' the weighted residuals on e.m.f. will be stored in RES

            DY = EMF!(IP) - EO - RTJC * Log(CX(MBET))
            If CX(NMBE) > 0! Then
                DY = DY - JA# * CX(NMBE) / IVAL# - JB# * KW# / CX(NMBE)
            End If
            S = DY
            RES(IP) = S
            UF = UF + WS!(IP) * S * S

            ' UF now holds the sum of weighted squared residuals

            If JACK = 2 GoTo 802

            ' with JACK=1 or 3 the jacobian will be calculated and stored in array A
            ' the weighted jacobian will be in array WA

            For J = 1 To NPAR: A(J) = 0!: Next J
            For I = 1 To NGAU
                DIV(I) = DX(I)
                For J = 1 To NGAU: BIV(I, J) = DM(I, J): Next J
            Next I
            NIV = NGAU
            NSUB = NSUB + 1
            STER$(NSUB) = SUBR$(10)
            Call INVERT 'subroutine
            NSUB = NSUB - 1
            For I = 1 To NGAU
                DX(I) = DIV(I)
                For J = 1 To NGAU: DM(I, J) = BIV(I, J): Next J
            Next I
            For J = 1 To NGAU
                J1 = JCON(J)
                If J1 = MBET Then MBT = J
                For K = J To NGAU: DM(J, K) = DM(K, J): Next K
            Next J
            If NKV = 0 GoTo 794
            IK = 0
            For K = 1 To NK
                For J = 1 To NGAU
                    J1 = JCON(J)
                    XX(J) = -JQR(K, J1) * CI(K)
                Next J
                DER = 0!
                For J = 1 To NGAU: DER = DER + DM(MBT, J) * XX(J): Next J
                DER = DER * RTJC
                If KIY(K) = 1 Then
                    IK = IK + 1
                    ' dE/dK
                    DERR = DER
                    For KK = 1 To NK
                        If KK = K GoTo 1777
                        NKA = NKAP(KK)
                        If NKA = 0 GoTo 1777
                        For JJ = 1 To NKA
                            If IKAP(KK, JJ) <> K GoTo 1776
                            For J = 1 To NGAU
                                J1 = JCON(J)
                                XX(J) = -JQR(KK, J1) * CI(KK)
                            Next J
                            DR = 0!
                            For J = 1 To NGAU: DR = DR + DM(MBT, J) * XX(J): Next J
                            DR = DR * RTJC
                            DERR = DERR + DR
                        1776 Next JJ
                    1777 Next KK
                    A(IK) = DERR
                End If
                If KEYC(K) = 1 Then
                    IK = IK + 1
                    ' dE/dCG
                    A(IK) = DER * DI(K) * CG(K) * AL10
                End If
                If KEYD(K) = 1 Then
                    IK = IK + 1
                    ' dE/dDG
                    A(IK) = DER * DIR(K) * DG(K) * AL10
                End If
                If KEYE(K) = 1 Then
                    IK = IK + 1
                    ' dE/dEG
                    A(IK) = DER * DI2(K) * EG(K) * AL10
                End If
            Next K
            If NCD = 0 GoTo 794
            SCD1I = 0!
            SCD2I = 0!
            SCD1IR = 0!
            SCD2IR = 0!
            SCD1I2 = 0!
            SCD2I2 = 0!
            For K = 1 To NK
                For J = 1 To NGAU
                    J1 = JCON(J)
                    XX(J) = -JQR(K, J1) * CI(K)
                Next J
                DER = 0!
                For J = 1 To NGAU: DER = DER + DM(MBT, J) * XX(J): Next J
                DER = DER * RTJC
                DER1 = DER * KSP(K)
                DER2 = DER * ZAST(K)
                SCD1I = SCD1I + DER1 * DI(K)
                SCD2I = SCD2I + DER2 * DI(K)
                SCD1IR = SCD1IR + DER1 * DIR(K)
                SCD2IR = SCD2IR + DER2 * DIR(K)
                SCD1I2 = SCD1I2 + DER1 * DI2(K)
                SCD2I2 = SCD2I2 + DER2 * DI2(K)
            Next K
            For K = 1 To NCD
                J = JCD(K)
                IK = IK + 1
                On J GOTO 784, 786, 788, 790, 791, 793
                ' dE/dc0
                784 A(IK) = SCD1I * c0 * AL10: GoTo 792
                ' dE/dc1
                786 A(IK) = SCD2I * c1 * AL10: GoTo 792
                ' dE/dd0
                788 A(IK) = SCD1IR * d0 * AL10: GoTo 792
                ' dE/dd1
                790 A(IK) = SCD2IR * d1 * AL10: GoTo 792
                ' dE/de0
                791 A(IK) = SCD1I2 * e0 * AL10: GoTo 792
                ' dE/de1
                793 A(IK) = SCD2I2 * e1 * AL10
            792 Next K
            794 If NSP = 0 GoTo 800
            For JP = 1 To NSP
                If JCUR(JP) <> JTC GoTo 798
                IK = JCOL(JP)
                KIND = JTYP(JP)
                ' dE/dEO
                If KIND = 2 Then A(IK) = A(IK) + EO: GoTo 798
                ' dE/dJA
                If KIND = 3 Then A(IK) = A(IK) + JA# * CX(NMBE) / IVAL#: GoTo 798
                ' dE/dJB
                If KIND = 4 Then A(IK) = A(IK) + JB# * KW# / CX(NMBE): GoTo 798
                ' dE/dSLOPE
                If KIND = 5 Then A(IK) = A(IK) + RTJC * Log(CX(MBET)): GoTo 798
                ' dE/dTOTAL
                J = JREA(JP)
                For JJ = 1 To NGAU
                    J1 = JCON(JJ)
                    If J1 = J GoTo 20
                Next JJ
                20 A(IK) = A(IK) + RTJC * DM(MBT, JJ) * TOMM(J) * VO / VOL
            798 Next JP
            800 For I = 1 To NPAR: WA(I) = A(I) * WS!(IP): Next I

            ' accumulate the normal equations matrix in B and the gradient vector
            ' in array C

            For J = 1 To NPAR
                WAJ = WA(J)
                C(J) = C(J) + WAJ * RES(IP)
                For K = J To NPAR: B(J, K) = B(J, K) + WAJ * A(K): Next K
            Next J
        802 Next IP
    Next JTC
End Sub

'
Sub HJA (EO, RTJC, IVAL#, JA#, JB#)

    ' calculates iteratively H value

    PH0 = (EO - EMF!(IP)) / RTJC / AL10
    ITER = 0
    H = Exp(-PH0 * AL10)
    PSI = PH0
    COST = JA# / IVAL# / RTJC / AL10
    COST1 = JB# * KW# / RTJC / AL10
    1001 PH1 = PSI + COST * H + COST1 / H
    ITER = ITER + 1
    If Abs(PH1) > 75 Then CX(NMBE) = H: Exit Sub
    H = Exp(-PH1 * AL10)
    If Abs(PH0 - PH1) < .0001 Then CX(NMBE) = H: Exit Sub
    If ITER < 200 Then PH0 = PH1: GoTo 1001
    Print #2, "max iter tit,ip,ja,ph0,ph1"; JTC; IP; JA#; PH0; PH1
End Sub

'
Sub INVERT

    ' completes the inversion of the matrix B, which has been factorised by
    ' FACT. On entry the strict lower triangle holds the Choleski factor,
    ' and D holds the diagonal Choleski factors. On exit the lower triangle
    ' of B holds the inverse of the original B matrix. The diagonal elements
    ' of the original are overwritten
    '                        -1
    ' forward substitution LL  =E

    If NIV = 1 Then BIV(1, 1) = 1 / DIV(1): Exit Sub
    For JCOL = 1 To NIV
        BIV(JCOL, JCOL) = 1
        K1 = JCOL + 1
        If K1 <= NIV Then
            For JROW = K1 To NIV
                Z = -BIV(JROW, JCOL)
                K2 = JROW - 1
                If K1 <= K2 Then
                    For K = K1 To K2: Z = Z - BIV(JROW, K) * BIV(K, JCOL): Next K
                End If
                BIV(JROW, JCOL) = Z
            Next JROW
        End If
    Next JCOL
    '                                             T -1 -1  -1
    ' complete the inversion by multiplying out (L )  D  (L  )

    For JCOL = 1 To NIV
        For JROW = JCOL To NIV
            Z = 0!
            For K = JROW To NIV: Z = Z + BIV(K, JCOL) * BIV(K, JROW) / DIV(K): Next K
            BIV(JROW, JCOL) = Z
        Next JROW
    Next JCOL
End Sub

'
Sub SNTPS

    ' if ISP=1 converts parameters to special names after checking if any are zero
    ' if ISP=2 converts special names to parameters for use in main refinement

    K = 0
    For IK = 1 To NK
        If KIY(IK) = 1 Then
            K = K + 1
            If ISP = 2 Then PARAS(K) = KAP#(IK) Else KAP#(IK) = PARAS(K)
            If Abs(KAP#(IK)) <= ACCM Then
                Print #2, Using "K### is zero - refinement abandoned"; IK
                Stop
            End If
        End If
        If KEYC(IK) = 1 Then
            K = K + 1
            If ISP = 1 Then CG(IK) = PARAS(K) Else PARAS(K) = CG(IK)
        End If
        If KEYD(IK) = 1 Then
            K = K + 1
            If ISP = 1 Then DG(IK) = PARAS(K) Else PARAS(K) = DG(IK)
        End If
        If KEYE(IK) = 1 Then
            K = K + 1
            If ISP = 1 Then EG(IK) = PARAS(K) Else PARAS(K) = EG(IK)
        End If
    Next IK
    For I = 1 To NK
        BETA(I) = KAP#(I) * Exp(JPOT(I) * AL10)
        NKA = NKAP(I)
        If NKA > 0 Then
            For J = 1 To NKA
                IKA = IKAP(I, J)
                BETA(I) = BETA(I) * KAP#(IKA) * Exp(JPOT(IKA) * AL10)
            Next J
        End If
    Next I
    If INDKW > 0 Then KW# = BETA(INDKW)
    If NCD = 0 GoTo 856
    KC = 0
    KD = 0
    KE = 0
    For I = 1 To NCD
        J = JCD(I)
        K = K + 1
        If J < 3 Then KC = 1: GoTo 833
        If J < 5 Then KD = 1 Else KE = 1
        833 If ISP = 2 GoTo 844
        On J GOTO 834, 836, 840, 842, 841, 843
        834 c0 = PARAS(K): GoTo 854
        836 c1 = PARAS(K): GoTo 854
        840 d0 = PARAS(K): GoTo 854
        842 d1 = PARAS(K): GoTo 854
        841 e0 = PARAS(K): GoTo 854
        843 e1 = PARAS(K): GoTo 854
        844 On J GOTO 846, 848, 850, 852, 851, 853
        846 PARAS(K) = c0: GoTo 854
        848 PARAS(K) = c1: GoTo 854
        850 PARAS(K) = d0: GoTo 854
        852 PARAS(K) = d1: GoTo 854
        851 PARAS(K) = e0: GoTo 854
        853 PARAS(K) = e1
    854 Next I
    For I = 1 To NK
        If KC = 1 Then
            CG(I) = KSP(I) * c0 + ZAST(I) * c1
            If JQR(I, NMBE) < 0 Then CG(I) = CG(I) + .015 * JQR(I, NMBE)
        End If
        If KD = 1 Then DG(I) = KSP(I) * d0 + ZAST(I) * d1
        If KE = 1 Then EG(I) = KSP(I) * e0 + ZAST(I) * e1
    Next I
    856 If NSP = 0 Then Exit Sub
    For K = 1 To NSP
        L = NKV + K
        JR = JREA(K)
        JC = JCUR(K)
        JT = JTYP(K)
        If ISP = 2 GoTo 864
        On JT GOTO 858, 860, 862, 861, 863
        858 TOTMM(JC, JR) = PARAS(L): GoTo 872
        860 EZERO(JC) = PARAS(L): GoTo 872
        862 JAM#(JC) = PARAS(L): GoTo 872
        861 JBM#(JC) = PARAS(L): GoTo 872
        863 RTJ(JC) = PARAS(L): GoTo 872
        864 On JT GOTO 866, 868, 870, 873, 871
        866 PARAS(L) = TOTMM(JC, JR): GoTo 872
        868 PARAS(L) = EZERO(JC): GoTo 872
        870 PARAS(L) = JAM#(JC): GoTo 872
        873 PARAS(L) = JBM#(JC): GoTo 872
        871 PARAS(L) = RTJ(JC)
        872 If Abs(PARAS(L)) > ACCM GoTo 874
        Print #2, Using "special parameter## (curve## type## "; K; JC; JT;
        Print #2, Using "reactant##) is zero"; JR
        Print #2, " refinement abandoned": Stop
    874 Next K
End Sub

'
Sub WCURV

    ' calculates the weights

    910 For JP = K To IP
        JM1 = JP - 1
        J1 = JP + 1
        If JP = K GoTo 912
        If JP = IP GoTo 914
        DER = (EMF!(JP) - EMF!(JM1)) / (TITRE!(JP) - TITRE!(JM1))
        DER = (DER + (EMF!(J1) - EMF!(JP)) / (TITRE!(J1) - TITRE!(JP))) * .5
        GoTo 916
        912 DER = (EMF!(J1) - EMF!(JP)) / (TITRE!(J1) - TITRE!(JP))
        GoTo 916
        914 DER = (EMF!(JP) - EMF!(JM1)) / (TITRE!(JP) - TITRE!(JM1))
        916 WS!(JP) = 1! / (DER * DER * SIGV + SIGE)
    Next JP
End Sub

Sub FILPOUT

    ' routine for output file with refined parameters

    ReDim JQRM(NMBE), IKA(NKM), CGM(NKM), DGM(NKM), EGM(NKM), IBM#(NKM)

    For I = 1 To 2
        Line Input #1, TITLE$
        Print #4, TITLE$
    Next I
    For I = 1 To NMBE + 1
        Line Input #1, TITLE$
        Print #4, TITLE$
    Next I
    If ICD > 0 Then
        Input #1, TITLE$
        Print #4, IREF#; AT; BT;
        If NCD = 0 Then
            Print #4, c0; c1; d0; d1; e0; e1;
        Else
            Print #4, Using "###.### "; c0; c1; d0; d1; e0; e1;
        End If
        For I = 1 To 6: Print #4, KCD(I);: Next I
        Print #4, ""
    End If
    Input #1, TITLE$
    For I = 1 To NMBE: Print #4, ZZ(I);: Next I: Print #4, ""
    10 If ICD = 2 Then
        Input #1, TITLE$
        Print #4, TITLE$
    End If
    I = 0
    For I1 = 1 To NKM
        Input #1, KLOG#
        If ICD > 0 Then
            If IREF# < 0! Then Input #1, IBM#(I1)
            Input #1, CGM(I1), DGM(I1), EGM(I1)
        End If
        For K = 1 To NMBE: Input #1, JQRM(K): Next K
        Input #1, KEYM
        If ICD > 0 Then Input #1, KEYCM, KEYDM, KEYEM
        Input #1, NKA
        For J = 1 To NKA: Input #1, IKA(J): Next J
        If KEYM = -1 Then
            Print #4, KLOG#;
            If ICD > 0 Then
                If IREF# < 0! Then Print #4, IBM#(I1);
                Print #4, CGM(I1); DGM(I1); EGM(I1);
            End If
        Else
            I = I + 1
            Print #4, Using "####.##### "; KLOG#(I);
            If ICD > 0 Then
                If IREF# < 0! Then Print #4, IB#(I);
                Print #4, Using "####.##### "; CG(I); DG(I); EG(I);
            End If
        End If
        For K = 1 To NMBE: Print #4, JQRM(K);: Next K
        Print #4, KEYM;
        If ICD > 0 Then Print #4, KEYCM; KEYDM; KEYEM;
        Print #4, NKA;
        For J = 1 To NKA: Print #4, IKA(J);: Next J
        Print #4, ""
    Next I1
    For ITC = 1 To NTC
        Line Input #1, NAMET$
        Print #4, NAMET$
        Line Input #1, NAMET$
        Print #4, NAMET$
        For I = 1 To NMBE
            Input #1, TOTMM, ADDC, LOK
            If LOK = 0 Then
                Print #4, TOTMM;
            Else
                Print #4, Using "####.###### "; CO(ITC, I);
            End If
            Print #4, ADDC; "0"
        Next I
        Input #1, COI, CTI, IVAL#
        Print #4, COI; CTI; IVAL#
        Input #1, VO, SIGMAV
        Print #4, VO; SIGMAV
        Input #1, EO, SIGMAE, JA#, JB#, SLOPE, LOK1, LOK2, LOK3, LOK4
        If LOK1 = 0 Then Print #4, EO; Else Print #4, Using "####.### "; EZERO(ITC);
        Print #4, SIGMAE;
        If LOK2 = 0 Then Print #4, JA#; Else Print #4, Using "####.###  "; JAM#(ITC);
        If LOK3 = 0 Then Print #4, JB#; Else Print #4, Using "####.###  "; JBM#(ITC);
        If LOK4 = 0 Then Print #4, SLOPE; Else Print #4, Using "####.### "; SL(ITC);
        Print #4, " 0 0 0 0"
        111 Input #1, TITV, EMFC
        If MODE = 2 Then Input #1, SIGP
        Input #1, IND
        Print #4, TITV; EMFC;
        If JW = 2 Then Print #4, SIGP;
        Print #4, IND
        If IND = 0 GoTo 111
    Next ITC
End Sub


Sub CCCS

    ' converts the free concentrations, CX, to species concentrations, CI
    ' the formation constants are stored as mantissa (BETA) and exponent
    ' (POT) thus allowing constants and concentrations to be negative

    For I = 1 To NMBE
        If CX(I) > 0! Then HX(I) = Log(CX(I))
    Next I
    For K = 1 To NK
        W = 0!
        If ICD > 0 Then
            CDE = CG(K) * DI(K) + DG(K) * DIR(K) + EG(K) * DI2(K)
            W = W - (ZAST(K) * DFI(K) - CDE) * AL10
        End If
        For I = 1 To NMBE
            If CX(I) = 0! Then
                If JQR(K, I) <> 0 Then W = 0!: GoTo 10
            Else
                W = W + HX(I) * JQR(K, I)
            End If
        Next I
        If W > -DLNAM Then W = -DLNAM
        If W < DLNAM Then W = DLNAM
        W = Exp(W)
        10 CI(K) = W * BETA(K)
    Next K
End Sub

'
Sub CCFR

    ' calculation of the free concentrations (CX) by NEWTON's method,
    ' using residuals on the total concentration of all reactants (TCR).
    ' Initial estimated are supplied by FUNV

    ITMAX = 100
    NITER = 0
    If IP > IPI GoTo 612
    For I = 1 To NMBE
        If I <> MBET Then CX(I) = Abs(TCR(I) * .01)
    Next I
    NSUB = NSUB + 1
    Call CCCS 'subroutine
    NSUB = NSUB - 1
    For I = 1 To NGAU
        I1 = JCON(I)
        W = CX(I1)
        For K = 1 To NK: W = W + CI(K) * JQR(K, I1): Next K
        RR = Abs(TCR(I1) / W) ^ .5
        CX(I1) = CX(I1) * RR
    Next I

    ' beginning of iteration cycle

    612 NITER = NITER + 1
    JCK = 0
    614 NSUB = NSUB + 1
    Call CCCS 'subroutine
    NSUB = NSUB - 1

    ' the concentration of each species is now in CI

    SUM = 0!: SUMT = 0!
    For I = 1 To NGAU
        I1 = JCON(I)
        W = TCR(I1) - CX(I1)
        For K = 1 To NK
            W = W - CI(K) * JQR(K, I1)
        Next K
        EPSR(I) = W
        SUM = SUM + W * W
        SUMT = SUMT + (W / TCR(I1)) ^ 2
    Next I

    ' SUM is now the sum of squared residuals EPSR

    If NITER = 1 GoTo 618
    If SUMT <= EPSIT GoTo 616
    If SUM <= SUMP Or JCK = 1 GoTo 618

    ' divergence has occurred. The shifts are successively halved

    For I = 1 To NGAU
        I1 = JCON(I)
        DX(I) = .5 * DX(I)
        CX(I1) = CX(I1) - DX(I)
    Next I
    JCK = JCK + 1
    GoTo 614

    ' refinement converged. DIAG is needed for inversion of DM in FUNV

    616 For I = 1 To NGAU: DX(I) = DIAG(I): Next I
    Exit Sub
    618 SUMP = SUM * (RELAC + 1)

    ' set up the jacobian in DM

    For I = 1 To NGAU
        I1 = JCON(I)
        For J = I To NGAU
            J1 = JCON(J)
            If I = J Then W = CX(I1) Else W = 0
            For K = 1 To NK: W = W + CI(K) * JQR(K, I1) * JQR(K, J1): Next K
            DM(I, J) = W
        Next J
    Next I
    For I = 1 To NGAU
        SF(I) = EPSR(I)
        DF(I) = DIAG(I)
        For J = 1 To NGAU: BF(I, J) = DM(I, J): Next J
    Next I
    NF = NGAU
    JFAIL = 6
    NSUB = NSUB + 1
    Call FACT 'subroutine
    NSUB = NSUB - 1

    ' relative shifts on the free concentrations are now in EPSR

    For I = 1 To NGAU
        EPSR(I) = SF(I)
        DIAG(I) = DF(I)
        For J = 1 To NGAU: DM(I, J) = BF(I, J): Next J
    Next I
    If IFAIL <> 1 Then Exit Sub
    FRACT = 1!
    For I = 1 To NGAU
        DX(I) = EPSR(I)
        If Abs(DX(I)) >= ACCM And FRACT * DX(I) <= -1 Then FRACT = -.9 / DX(I)
    Next I

    ' if a free concentration is going negative, divide the shifts by ten

    For I = 1 To NGAU
        I1 = JCON(I)
        DX(I) = DX(I) * CX(I1) * FRACT
        CX(I1) = CX(I1) + DX(I)
        If CX(I1) <= ACM1 Then CX(I1) = ACM1
    Next I
    If NITER < ITMAX GoTo 612
    Print #2, Using "point ####  ##.###  ####.##"; IP; TITRE!(IP); EMF!(IP);
    Print #2, Using "  tit ##  cycle ### did not converge"; JTC; NCICL
    IFAIL = 7
End Sub

'
Sub FACT
    '                                              T
    ' factorises the symmetric matrix B so that LDL =B. On entry
    ' the upper triangle is filled. On exit the upper triangle is
    ' unchanged and L is in the strict lower triangle. The equations
    '                                                   T
    ' BX=S are solved in forward (LZ=S) and backward (DL X=Z) substitutions.
    ' The (unit) diagonal elements of L are not stored.

    IFAIL = 1
    DF(1) = BF(1, 1)
    If DF(1) > 0 GoTo 762
    If DF(1) = 0 Or JFAIL <> 6 Then IFAIL = JFAIL: Exit Sub
    762 If NF = 1 Then SF(1) = SF(1) / DF(1): Exit Sub
    For K = 2 To NF: BF(K, 1) = BF(1, K) / DF(1): Next K
    For I = 2 To NF
        DF(I) = BF(I, I)
        IM1 = I - 1
        For K = 1 To IM1
            BFIK = BF(I, K)
            DF(I) = DF(I) - BFIK * BFIK * DF(K)
        Next K
        If DF(I) > 0! GoTo 764
        If DF(I) = 0! Or JFAIL <> 6 Then IFAIL = JFAIL: Exit Sub
        764 IP1 = I + 1
        If IP1 > NF GoTo 766
        For J = IP1 To NF
            Z = BF(I, J)
            For K = 1 To IM1
                BFIK = BF(I, K)
                BFJK = BF(J, K)
                Z = Z - BFIK * BFJK * DF(K)
            Next K
            BF(J, I) = Z / DF(I)
        Next J
    766 Next I

    ' forward substitution

    For J = 2 To NF
        JM1 = J - 1
        For K = 1 To JM1: SF(J) = SF(J) - BF(J, K) * SF(K): Next K
    Next J

    ' backward substitution

    SF(NF) = SF(NF) / DF(NF)
    J = NF - 1
    768 JP1 = J + 1
    SF(J) = SF(J) / DF(J)
    For K = JP1 To NF: SF(J) = SF(J) - BF(K, J) * SF(K): Next K
    J = J - 1
    If J >= 1 GoTo 768
End Sub

Sub DOUS

    ' the output routine which performs two functions
    ' it prints out the parameter values and errors, and correlation matrix
    ' it checks for any beta which is negative, if one is found IND is set =2

    Print #2, Using " sigma     =###.####";SIGM
    Print #2, " mean dev. =";
    Print #2, Using "###.###"; SRM
    Print #2, ""
    Print #2, Tab(15); "initial      refined"
    Print #2, Tab(15); "value        value        std.dev.     species"
    K = 0
    IND = 1
    If NKV = 0 GoTo 754
    For I = 1 To NK
        If KIY(I) = 1 Then
            K = K + 1
            If KAP#(I) <= 0 Then
                IND = 2
                Print #2, Using " K### negative"; I
                KAPP# = KAP#(I) * Exp(JPOT(I) * AL10)
            Else
                XL = Log(KAP#(I)) / AL10 + JPOT(I)
                SDL = SD(K) / AL10
                XLI = Log(PARI(K)) / AL10 + JPOTM(I)
                KAPP# = Exp(XL * AL10)
            End If
            SDC = Abs(SD(K) * KAPP#)
            If KAP#(I) > 0! Then
                Print #2, Using "   log K## "; I;
                Print #2, Using "###.#####    "; XLI; XL; SDL;
                For J = 1 To NMBE
                    JJ = JQR(I, J)
                    If JJ = 0 GoTo 666
                    If JJ > 0 Then
                        SPE = Len(KEMIC$(J))
                        C$ = Left$(KEMIC$(J), SPE)
                    Else
                        C$ = "OH"
                        JJ = -JJ
                    End If
                    Print #2, "("; C$; ")";
                    If JJ > 1 Then
                        STC$ = Str$(JJ)
                        LSTC = Len(STC$) - 1
                        IX$ = Right$(STC$, LSTC)
                        Print #2, IX$;
                    End If
                666 Next J
                Print #2, ""
            End If
        End If
        If KEYC(I) = 1 Then
            K = K + 1
            SDC = Abs(SD(K) * CG(I))
            Print #2, Using "       C## "; I;
            Print #2, Using "###.#####    "; PARI(K); CG(I); SDC
        End If
        If KEYD(I) = 1 Then
            K = K + 1
            SDC = Abs(SD(K) * DG(I))
            Print #2, Using "       D## "; I;
            Print #2, Using "###.#####    "; PARI(K); DG(I); SDC
        End If
        If KEYE(I) = 1 Then
            K = K + 1
            SDC = Abs(SD(K) * EG(I))
            Print #2, Using "       E## "; I;
            Print #2, Using "###.#####    "; PARI(K); EG(I); SDC
        End If
    Next I
    If NCD > 0 Then
        For I = 1 To NCD
            J = JCD(I)
            K = K + 1
            SDC = Abs(SD(K) * PARAM(K))
            Print #2, Using "     \\    "; CDI$(J);
            Print #2, Using "###.#####    "; PARI(K); PARAM(K); SDC
        Next I
    End If
    754 If NSP = 0 GoTo 756
    Print #2, ""
    Print #2, "curve             initial        refined"
    Print #2, "                  value           value       std.dev."
    For I = 1 To NSP
        L = K + I
        JC = JCOL(I)
        SDD = Abs(SD(JC) * PARAM(L))
        KIND = JTYP(I)
        JTC = JCUR(I)
        Print #2, Using "  ##    \\"; JTC; NAMTYP$(KIND);
        If KIND = 1 Then Print #2, Using "#"; JREA(I); Else Print #2, " ";
        Print #2, Using " ######.#######"; PARI(L); PARAM(L); SDD
    Next I
    756 If NPAR = 1 Or IND = 2 Then Exit Sub
    If IW = 1 Then Exit Sub
    Print #2, ""
    Print #2, "Table 4."
    Print #2, ""
    Print #2, " correlation matrix"
    For I = 2 To NPAR
        K = I - 1
        For J = 1 To K: B(I, J) = B(I, J) / Sqr(B(I, I) * B(J, J)): Next J
        Print #2, Using "###"; I;
        For J = 1 To K
            If (J > 1) And (J Mod 15) = 1 Then Print #2, "": Print #2, "   ";
            Print #2, Using "##.##"; B(I, J);
        Next J
        Print #2, ""
    Next I
    Print #2, "  ";
    K = NPAR - 1
    For I = 1 To K
        If (I > 1) And (I Mod 15) = 1 Then Print #2, "": Print #2, "  ";
        Print #2, Using "#####"; I;
    Next I
    Print #2, ""
End Sub

'
Sub STANS (IPRST)

    ' prints out final table
    PA$ = "pa"
    IPI = 1
    For JTC = 1 To NTC
        JR = JPC(JTC)
        IPF = JTP(JTC)
        NPT = IPF - IPI + 1
        MBET = MBE(JTC)
        VO = VINIT(JTC)
        TM = TOTMM(JTC, JR)
        AD = ADDC(JTC, JR)
        JA# = JAM#(JTC)
        JB# = JBM#(JTC)
        EO = EZERO(JTC)
        RTJC = RTJ(JTC) / AL10
        NS = 0
        For K = 1 To NK
            For J = 1 To NMBE
                If TOTMM(JTC, J) = 0! And ADDC(JTC, J) = 0! Then
                    If JQR(K, J) <> 0 GoTo 111
                End If
            Next J
            If JQR(K, JR) <> 0 Then
                NS = NS + 1
                JS(NS) = K
                JSP(NS) = K
            End If
            POT(K) = AL10 * JPOT(K)
        111 Next K
        ISPER(JTC) = NS
        If IW = 1 GoTo 881
        Print #2, ""
        Print #2, "Table 6."
        Print #2, ""
        If JTC = 1 Then Print #2, "W'= normalized weight"
        Print #2, Using " curve###"; JTC
        Print #2, " point  V      E     DE     W'    I      ";
        If ICD > 0 Then Print #2, " EO    ";
        For I = 1 To NMBE: Print #2, Using "\\#   "; PA$; I;: Next I:
        Print #2, ""
        881 SR = 0!
        SI = 0!
        IVAL# = 1
        For IP = IPI To IPF
            VOL = VO + TITRE!(IP)
            If ICD = 0 Then
                CVIP = (COI(JTC) * VO + CTI(JTC) * TITRE!(IP)) / VOL
                SCZ = 0!
                SAZ = 0!
                For J = 1 To NMBE
                    CX(J) = FREEC!(IP, J)
                    SAZ = SAZ + CX(J) * Z2(J)
                Next J
                Call CCCS 'subroutine
                For J = 1 To NK: SCZ = SCZ + CI(J) * ZT(J) ^ 2: Next J
                IVAL# = (SCZ + SAZ + CVIP) * .5
                GoTo 882
            End If

            IVAL# = IVA#(IP)
            RADI = Sqr(IVAL#)
            FI = AT * RADI / (1! + BT * RADI)
            For K = 1 To NK
                DFI(K) = FI - FIB(K)
                DI(K) = IVAL# - IB#(K)
                DI2(K) = IVAL# ^ 2 - IB#(K) ^ 2
                DIR(K) = IVAL# * RADI - RIB(K)
            Next K
            DFIE = FI - FIT(JTC)
            DIE = IVAL# - IT#(JTC)
            DIRE = IVAL# * RADI - RIT(JTC)
            DIE2 = IVAL# * IVAL# - IT#(JTC) * IT#(JTC)
            CDE = CGE * DIE + DGE * DIRE + EGE * DIE2
            EO = EZERO(JTC) - RTJ(JTC) * (DFIE * Z2(MBET) - CDE)
            882 SR = SR + Abs(RES(IP))
            SI = SI + IVAL#
            For K = 1 To NMBE
                CX(K) = FREEC!(IP, K)
                If CX(K) > 0! Then
                    PCX(K) = -Log(CX(K)) / AL10
                Else
                    PCX(K) = 0!
                End If
            Next K
            If JA# = 0! GoTo 891
            If MBET = NMBE GoTo 891
            JAB# = JA# * CX(NMBE) + JB# * KW# / CX(NMBE)
            CX(MBET) = Exp((EMF!(IP) - EO - JAB#) / RTJC)
            891 NSUB = NSUB + 1
            STER$(NSUB) = SUBR$(2)
            Call CCCS 'subroutine
            NSUB = NSUB - 1
            WP = WS!(IP)
            If IW <> 1 GoTo 883
            If RES(IP) = 0! Then WS!(IP) = 1!: GoTo 883
            WS!(IP) = 1! / Abs(RES(IP)) ^ WESP
            883 If IW = 1 GoTo 885
            Print #2, Using "#### ##.### ####.#"; IP; TITRE!(IP); EMF!(IP);
            Print #2, Using " ###.# ###.#"; RES(IP); WP;
            Print #2, Using "###.### "; IVAL#;
            If ICD > 0 Then Print #2, Using "####.##"; EO;
            For J = 1 To NMBE: Print #2, Using "###.##"; PCX(J);: Next J
            Print #2, ""
        885 Next IP
        If IW = 1 GoTo 888
        SRM = SR / NPT
        SIM = SI / NPT
        Print #2, Using "mean              (###.#)"; SRM;
        Print #2, Tab(30); Using " (##.###)"; SIM;
        884 Print #2, ""
        Print #2, "Table 7."
        Print #2, ""
        Print #2, Using " curve###"; JTC
        Print #2, Using " point   p\  \"; KEMIC$(MBET);
        Print #2, " percentages of species calculated"
        Print #2, Tab(15);
        Print #2, Using " with respect to \          \"; KEMIC$(JR)
        Print #2, Tab(14);
        For I = 1 To NS
            Print #2, Using "####  "; JSP(I);
        Next I
        Print #2, ""
        For IP = IPI To IPF
            VOL = VO + TITRE!(IP)
            If ICD = 0 GoTo 1882
            IVAL# = IVA#(IP)
            RADI = Sqr(IVAL#)
            FI = AT * RADI / (1! + BT * RADI)
            For K = 1 To NK
                DFI(K) = FI - FIB(K)
                DI(K) = IVAL# - IB#(K)
                DIR(K) = IVAL# * RADI - RIB(K)
            Next K
            DFIE = FI - FIT(JTC)
            DIE = IVAL# - IT#(JTC)
            DIE2 = IVAL# * IVAL# - IT#(JTC) * IT#(JTC)
            DIRE = IVAL# * RADI - RIT(JTC)
            CDE = CGE * DIE + DGE * DIRE + EGE * DIE2
            EO = EZERO(JTC) - RTJ(JTC) * (DFIE * Z2(MBET) - CDE)
            1882 For K = 1 To NMBE
                CX(K) = FREEC!(IP, K)
                If CX(K) > 0! Then
                    PCX(K) = -Log(CX(K)) / AL10
                Else
                    PCX(K) = 0!
                End If
            Next K
            If JA# = 0! GoTo 1891
            If MBET = NMBE GoTo 1891
            JAB# = JA# * CX(NMBE) + JB# * KW# / CX(NMBE)
            CX(MBET) = Exp((EMF!(IP) - EO - JAB#) / RTJC)
            1891 NSUB = NSUB + 1
            STER$(NSUB) = SUBR$(2)
            Call CCCS 'subroutine
            NSUB = NSUB - 1
            W = CX(JR)
            For K = 1 To NK: W = W + CI(K) * JQR(K, JR): Next K
            TOMMJR = W
            For J = 1 To NS
                JJ = JS(J)
                If TOMMJR = 0! Then
                    PPCS = 0!
                Else
                    PPCS = CI(JJ) * JQR(JJ, JR) / TOMMJR * 100!
                End If
                PCS(J) = PPCS
            Next J
            If IPRST = 1 Then Print #2, Using "####  ###.## "; IP; PCX(MBET);
            For J = 1 To NS
                IPCS = PCS(J)
                If IPRST = 1 Then Print #2, Using " ###.#"; PCS(J);
                IPERC(IP, J) = IPCS
            Next J
            If IPRST = 1 Then Print #2, ""
        Next IP
        888 IPI = IPF + 1
    Next JTC
End Sub

Function FNX% (X)
    FNX% = X0 + (X1 - X0) * (X - XA) / (XB - XA) + .5
End Function
Function FNY% (Y)
    FNY% = Y0 + (Y1 - Y0) * (Y - YA) / (YB - YA) + .5
End Function

'
Sub GRAPH

    ' subroutine per grafico

    Dim COLABEL(80, 24), PXY$(80, 24)
    Shared X0, X1, XA, XB, Y0, Y1, YA, YB, X, Y

    Cls
    KEY Off
    For QQ = 1 To 80
        For WW = 1 To 24
            PXY$(QQ, WW) = ""
        Next WW
    Next QQ
    IN$ = "<--": IN1$ = "   "

    On Error GoTo 0

    Screen TS
    If ISC = 2 Then PP = 8 Else PP = 10
    Line (107, 19)-(114, 26), 2, B
    Screen 0
    On ISC GOTO 10130, 10140, 10150, 10160
    10130 FX1 = 1
    FY1 = 1
    RX = 23
    RX1 = 25
    MX = 720
    MY = 348
    GoTo 10170
    10140 FX1 = 640# / 720#
    FY1 = 200# / 348#
    RX = 23
    RX1 = 25
    MX = 640
    MY = 200
    GoTo 10170
    10150 FX1 = 640# / 720#
    FY1 = 480# / 348#
    RX = 27
    RX1 = 30
    MX = 640
    MY = 480
    GoTo 10170
    10160 FX1 = 640# / 720#
    FY1 = 1
    RX = 23
    RX1 = 25
    MX = 640
    MY = 348
    10170 FX2 = 63 * FX1
    FY2 = 28 * FY1
    X0 = 60 * FX1
    Y0 = 300 * FY1
    X1 = 690 * FX1
    Y1 = 20 * FY1
    Cls
    10370 Print TIT$
    Print "min x     max x     min y     max y"
    YA = YY!(1)
    XA = XGX!(1)
    XB = XA
    YB = YA
    For I = 1 To NX
        If XGX!(I) > XB Then XB = XGX!(I)
        If XGX!(I) < XA Then XA = XGX!(I)
        If YY!(I) > YB Then YB = YY!(I)
        If YY!(I) < YA Then YA = YY!(I)
        If YGE!(I) > YB Then YB = YGE!(I)
        If YGE!(I) < YA Then YA = YGE!(I)
    Next I
    Print Using "####.##   "; XA; XB; YA; YB
    Input "Are these values to be changed (Y/N) "; A$
    If A$ = "Y" Or A$ = "y" Then Input "new values "; XA, XB, YA, YB
    XAB = XB - XA
    YAB = YB - YA
    Print
    TX$ = "v"
    TY$ = "E"
    LX = Int(XB)
    For I = 0 To 3
        If LX >= 10 ^ I GoTo 10100
        IX1 = I
        IX2 = 4 - I
        X$ = ""
        GoTo 10110
    10100 Next I
    10110 LY = -Int(-Abs(YB))
    For I = 2 To 3
        If LY >= 10 ^ I GoTo 10120
        IY1 = I
        IY2 = 4 - I
        Y$ = ""
        GoTo 10310
    10120 Next I
    10310 Screen TS
    LB1 = 0
    GoSub RLABEL 'subroutine RLABEL
    GoSub 10480 'subroutine VALXY
    GoSub 10620 'subroutine CURVE
    GoSub 10720 'subroutine PLOT
    Exit Sub

    ' subroutine VALXY

    10480 Cls
    If LB1 = 1 GoTo LB
    PXY$(2, 1) = TIT$
    LX = 2
    LY = 6
    For K = 0 To 5
        X$ = Str$(CInt((XA + XAB * K / 5) * 10 ^ IX2) / 10 ^ IX2)
        If Val(X$) >= 0 Then X$ = Right$(X$, Len(X$) - 1)
        PXY$(7 + 14 * K + LX - Len(X$), RX) = X$
        Y = YB - YAB * K / 5
        If K = 5 Then Y = YA
        If RX = 23 Then IR! = 4 Else IR! = 4.8
        Y$ = Str$(CInt(Y * 10 ^ IY2) / 10 ^ IY2)
        If Val(Y$) >= 0 Then
            Y$ = Right$(Y$, Len(Y$) - 1)
        Else
            Y$ = Mid$(Y$, 1, 6)
        End If
        L1 = LY - Len(Y$)
        If L1 <= 0 Then L1 = 1
        PXY$(L1, 2 + IR! * K) = Y$
    Next K
    PXY$(71 - LX, RX) = Left$(TX$, 5)
    PXY$(1, 4) = Left$(TY$, 5)
    LB: GoSub GLABEL 'subroutine GLABEL
    Line (X0, Y0)-(X1, Y1), , B
    For K = 1 To 9
        Line (X0 + FX2 * K, Y0)-(X0 + FX2 * K, Y0 - 5)
        Line (X0, Y0 - FY2 * K)-(X0 + 7, Y0 - FY2 * K)
    Next K
    Return

    '  subroutine CURVE

    10620 K = 0
    For I = 1 To NX
        FX = FNX%(XGX!(I))
        FY = FNY%(YY!(I))
        If FX > X1 Then FX = X1
        If FY > Y0 Then FY = Y0
        If FX < X0 Or FY < Y1 Then K = 0: GoTo 10690
        If K = 0 Then PSet (FX, FY): K = 1 Else Line -(FX, FY)
    10690 Next I
    For I = 1 To NX
        FX = FNX%(XGX!(I))
        FY = FNY%(YGE!(I))
        If FX > X1 Then FX = X1
        If FY > Y0 Then FY = Y0
        If FX >= X0 And FY >= Y1 Then Circle (FX, FY), 4
    Next I
    Return

    '  subroutine PLOT?

    10720 Locate RX1, 1: Print "                                                                         ";
    Locate RX1, 1
    ' Color 7
    Print "File, New graph , Quit (F/N/Q) ";
    111 SC$ = UCase$(InKey$)
    If SC$ = "" GoTo 111
    If SC$ = "F" Then
        Locate RX1, 1
        Print "Name of file ?                                         ";
        GoSub RNF 'subroutine RNF
    End If
    If SC$ = "F" Then Open NFS$ For Output As #6: GoTo 10770
    If SC$ = "Q" Then IGR = 0
    Cls
    Color 7
    Screen 0
    Return
    10770 FX1M = FX1
    FX2M = FX2
    FY1M = FY1
    FY2M = FY2
    X0M = X0
    X1M = X1
    Y0M = Y0
    Y1M = Y1
    RXM = RX
    RX1M = RX1
    FX1 = 640# / 720#
    FY1 = 1
    RX = 23
    RX1 = 25
    FX2 = 63 * FX1
    FY2 = 28 * FY1
    X0 = 60 * FX1
    Y0 = 300 * FY1
    X1 = 690 * FX1
    Y1 = 20 * FY1
    Print #6, "IN;IP1000,1000,9000,6000;"
    Print #6, "SC0,640,0,348;SR1,2;"
    Print #6, "SP1;PU;PA"; X0; Abs(Y0 - 348);
    Print #6, "EA"; X1; Abs(Y1 - 348);
    Print #6, "TL4,0;"
    Print #6, "PU;PA0,0;"
    Print #6, "TL100,0;"
    Print #6, "PU;"
    For QQ = 1 To 80
        For WW = 1 To 23
            If PXY$(QQ, WW) = "" GoTo NEX
            Print #6, "SP" + Str$(COLABEL(QQ, WW)) + ";"
            Print #6, "pa"; ((8 * QQ) - 9); ((Abs(CInt(13.92 * WW) - 348 - 4)));
            Print #6, "lb"; PXY$(QQ, WW);
            Print #6, Chr$(3)
        NEX: Next WW
    Next QQ
    Print #6, "SP1;"
    For K = 0 To 10
        Print #6, "PA"; (X0 + FX2 * K); Abs(Y0 - 348); "PD;"
        Print #6, "PA"; (X0 + FX2 * K); Abs((Y0 - 5) - 348); "PU";
        Y = YA + YAB * K / 10
        Print #6, "PA"; X0; Abs((Y0 - FY2 * K) - 348); "PD;"
        Print #6, "PA"; (X0 + 7); Abs((Y0 - FY2 * K) - 348); "PU";
    Next K
    K = 0
    Print #6, "SP0;"
    For I = 1 To NX
        FX = FNX%(XGX!(I))
        FY = FNY%(YY!(I))
        If FX > X1 Then FX = X1
        If FY > Y0 Then FY = Y0
        FYC = Abs(FY - 348)
        If FX < X0 Or FY < Y1 Then K = 0: GoTo 10990
        If K = 0 Then
            Print #6, "PU"; FX; FYC; "PD;": K = 1
        Else
            Print #6, "PA"; FX; FYC; "PD;"
        End If
    10990 Next I
    Print #6, "PU;"
    10992 Print #6, "SP0;"
    X0 = X0M
    Y0 = Y0M
    X1 = X1M
    Y1 = Y1M
    RX = RXM
    RX1 = RX1M
    FX1 = FX1M
    FX2 = FX2M
    FY1 = FY1M
    FY2 = FY2M
    Close #6
    GoTo 10720

    ' subroutine RNF

    RNF: NFS$ = ""
    For CX = 1 To 30
        CCC: C1$ = InKey$
        If C1$ = "" Then GoTo CCC
        If C1$ = Chr$(13) Then Return
        If C1$ < Chr$(32) Or C1$ > Chr$(127) Then GoTo CCC
        NFS$ = NFS$ + C1$
        Locate 25, 16: Print NFS$;
    Next CX
    Return

    ' subroutine GLABEL

    GLABEL: For QQ = 1 To 80
        For WW = 1 To 24
            If PXY$(QQ, WW) <> "" Then Locate WW, QQ: Print PXY$(QQ, WW)
        Next WW
    Next QQ
    Return

    ' subroutine RLABEL

    RLABEL: For QQ = 1 To 6
        For WW = 2 To 23
            PXY$(QQ, WW) = ""
        Next WW
    Next QQ
    For WW = 1 To 80: PXY$(WW, 23) = "": Next WW
    Return
End Sub