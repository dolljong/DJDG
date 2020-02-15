;*******************************
; Program : SLOP
;           SLOP mark
;           Suk-Jong Yi
;           1995. 3. 20
;*******************************
;노면의 slop을 표시해준다. (%)
;*******************************

(defun C:SLOP(/
        ds      mp      sp      sgnx    sl      ent     spent
        epent   dx      sgn     tmp     dy      ang     angl
        sgnt    sltxt   getxt   lsttxt  txtlen  txtl    blsp
        blep    ap1ang  ap1     ap2ang  ap2     txtp    txtang  clr
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

  (push-env)                                        ;환경변수 대피

  (setq ds (getvar "DIMSCALE"))
  (setq th (getvar "DIMTXT")) 

  (setq mp (getpoint "\nPick mid-point: "))
  (setq sp (getpoint mp "\nPick side: "))
  (setq sgnx (/ (abs (- (car sp) (car mp))) (- (car sp) (car mp))))
                                                  ;중간점과 사이드 포인트의 x부호
  (initget "Referance 2Points")
  (setq sl (getreal "\nReferance/2Points <Slope>: "))     ;직접 기울기를 입력할 것인가?
  (cond
    ((= sl "Referance")                          ;아니면 엔티티를 참조할 것인가?
      (setq ent (entget (car (entsel))))
      (setq spent (cdr (assoc 10 ent)))
      (setq epent (cdr (assoc 11 ent)))
      (setq dx (- (car epent) (car spent)))
      (setq sgn (/ (abs dx) dx))
      (if (/= sgn sgnx)                           ;라인의 방향이 사용자가 선택한
        (progn                                    ;방향과 반대 방향일 경우 시작점과
          (setq tmp spent)                        ;끝점의 순서를 바꾼다
          (setq spent epent)
          (setq epent tmp)
        ) ;of progn
      ) ;of if
      (setq dx (- (car epent) (car spent)))      ;Delta X를 구함
      (setq dy (- (cadr epent) (cadr spent)))    ;Delta Y를 구함
      (setq sl (* (/ dy dx 0.01) sgnx))
    );subcond
    ((= sl "2Points")
     (setq pnt1 (getpoint "\nPick first point: ")
           pnt2 (getpoint pnt1 "\nPick second  point: "))
     (setq dx (- (car pnt2) (car pnt1))
	   dy (- (cadr pnt2) (cadr pnt1)))
     (setq sl (* (/ dy dx 0.01) sgnx))
    );subcond
    
  ) ;of if

  (setq ang (atan (/ sl 100.0)))
  (cond                                    ;화살표가 왼쪽으로 가는가 오른쪽으로
    ((= sgnx -1) (setq angl (- pi ang)))   ;가는가에 따라서 라인의 각도를 계산
    ((= sgnx 1) (setq angl (+ 0.0 ang)))   ;왼쪽으로 갈 경우 180+ang
  ) ;of cond                               ;오른쪽으로 갈 경우 0+ang


  ;기울기의 부호를 스트링으로 만든다
  (if (> sl 0.0)
    (setq sgnt "+")
    (setq sgnt "")
  )               ;양의 slop인 경우 +기호 추가

  (setq sltxt (strcat "S=" sgnt (rtos (float sl) 2 3) "%"))   ;부호 + slop + "%"
  (princ "\nEnter text:<")             ;프로그램이 구한 Text를 그래로 사용할
  (princ sltxt)                        ;것인지 아니면 사용자가 임의로 줄건지 결정
  (princ ">: ")                        ;default는 구하여진 slop
  (setq getxt (getstring))             ;사용자 text입력
  (if (= getxt "")                     ;사용자 text가 nil일 경우 default로
    (setq lsttxt sltxt)
    (setq lsttxt getxt)
  ) ;of if
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

  (pop-env)                           ;대피해둔 환경변수 복귀

  (setq *error* oer seterr nil)       ;내장에러루틴 복귀

  (princ)

) ;of defun
