;********************************************
; Program : LSLOP
;           Line with SLOP
;           Suk-Jong Yi
;           97/10/14
;********************************************
; 지정한 slop를 가진 line을 그려준다.
;********************************************

(defun C:LSLOP()

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

  (setq oer *error* *error* seterr)

  (setq ipnt (getpoint "\nStart point: "))      ;시작점
  (setq dpnt (getpoint ipnt "\nDirection point: "))  ;방향점
  (setq dx (getreal "\nX Distance: "))          ;x 거리
  (setq slp (getreal "\nSlop(%): "))           ;Slop입력

  (if (< (- (car dpnt) (car ipnt)) 0)             ;x 부호
    (setq xsgn -1)
    (setq xsgn 1))

  (setq epnt (list (+ (car ipnt) (* dx xsgn))   ;끝점 구하기
                   (+ (cadr ipnt) (* dx slp 0.01))))

  (command "LINE" ipnt epnt "")                             ;LINE그리기

  (setq slpnt (getpoint "\nInsert point of Slop mark: "))   ;slop mark삽입점

  (f_slop slpnt xsgn slp)                                   ;slop mark

  (push-env)
  (setq *error* oer seterr nil)
  (princ)

) ;of DEFUN



;***********************************
; Program : F_SLOP
;           Function SLOP mark
;           Suk-Jong Yi
;           96/7/3
;***********************************
;노면의 slop을 표시해준다. (%)
; 넘어오는 값
;     MP : Mid Point 화살표의 중간점
;   SGNX : (화살표의 진행방향)
;     SL : SLop값 (%)
;***********************************

(defun F_SLOP(MP SGNX SL
 /
        ds      mp      sp      sgnx    sl      ent     spent
        epent   dx      sgn     tmp     dy      ang     angl
        sgnt    sltxt   getxt   lsttxt  txtlen  txtl    blsp
        blep    ap1ang  ap1     ap2ang  ap2     txtp    txtang  clr
)

  (setq ds (getvar "DIMSCALE")
        th (getvar "DIMTXT"))                ;글씨크기

  (setq ang (atan (/ sl 100.0)))
  (cond                                    ;화살표가 왼쪽으로 가는가 오른쪽으로
    ((= sgnx -1) (setq angl (- pi ang)))   ;가는가에 따라서 라인의 각도를 계산
    ((= sgnx 1) (setq angl (+ 0.0 ang)))   ;왼쪽으로 갈 경우 180+ang
  ) ;of cond                               ;오른쪽으로 갈 경우 0+ang


;기울기의 부호를 스트링으로 만든다
  (if (> sl 0.0)
    (setq sgnt "S=+")
    (setq sgnt "S=")
  )               ;양의 slop인 경우 +기호 추가

  (if (> (abs (rem sl 1)) 0)
    (setq slt (rtos sl 2 3))
    (setq slt (rtos (float sl) 2 1))
  ) ;of if
  (setq sltxt (strcat sgnt slt "%"))   ;부호 + slop + "%"
  (setq lsttxt sltxt)
  (setq txtlen (* (strlen lsttxt) th ds))                    ;전체 text의 길이
  (setq txtl (+ txtlen (* ds 2 th)))  ;text길이에다 2개의 글자를 추가한 길이

;화살표 베이스라인의 시작점과 끝점을 구한다
  (setq blsp (polar mp (- angl pi) (/ txtl 2.0)))
  (setq blep (polar mp    angl     (+ (/ txtl 2.0) (* ds th)))) ;화살표쪽을 길게
  (setq ap1ang (+ angl pi (* (dtor 15.0) sgnx -1)))
  (setq ap1 (polar blep ap1ang (* ds 2.0)))     ;화살표 사길이는 2.0mm
  (setq ap2ang (+ ap1ang (* (dtor 105.0) sgnx)))
  (setq ap2 (polar ap1 ap2ang (* (sin (dtor 15.0)) (* ds 2.0))))


  (setq txtp (polar mp (* (+ angl (/ pi 2.0)) sgnx) (* ds th)))  ;텍스트의 insert point
  (if (< sgnx 0)
    (setq txtang (- angl pi))          ;왼쪽방향일 경우 text각 구함
    (setq txtang angl)                 ;오른쪽 방향일 경우 베이스라인 각과 같다
  )
  (setq clr (getvar "CECOLOR"))
  (setvar "CECOLOR" "RED")
  (command "PLINE" blsp blep ap1 ap2 "")                      ;화살표 그리기
  (setvar "CECOLOR" clr)
  (command "TEXT" "J" "M" txtp (* ds th) (rtod txtang) lsttxt) ;텍스트 쓰기

) ;of defun
