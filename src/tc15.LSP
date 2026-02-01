;***********************************
; Program : TC15
;           Thickness Change 1:5
;           Suk-Jong Yi
;           96/5/9
;***********************************
; 강재의 두께가 변화 표시를 해준다.
;***********************************

(defun C:TC15(/                                     ;지역변수정의
ang     ang2        bp      bp2     bpnt        cp          dist        dist1
dist2   distc       ds      ea      entype1     entype2     len         len2
p1      p2          po      r       sa          thick       thicke      thickep
thickp  thicksp     thin    thine   thinep      thinep      thinp       thinsp
thinsp  tl          tp      wh4
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
  (setq ds (getvar "DIMSCALE")                      ;스케일값
        th (getvar "DIMTXT"))

  (setq thin (entsel "\nSelect thin plate: "))      ;얇은 판 선택
  (setq thick (entsel "\nSelect thick plate: "))    ;두꺼운 판 선택

  (setq thinp (cadr thin)                           ;얇은 판 선택점
        thine (entget (car thin))                   ;얇은 판 entity정보
        entype1 (cdr (assoc 0 thine)))              ;entity type
  (cond
    ((= entype1 "LINE")                             ;entity가 직선인 경우
      (setq thinsp (cdr (assoc 10 thine))           ;얇은 판 시작점
            thinep (cdr (assoc 11 thine)))          ;얇은 판 끝점
    )
    ((= entype1 "ARC")                              ;entity가 호인경우
      (setq r (cdr (assoc 40 thine))                ;호의 반지름
            cp (cdr (assoc 10 thine))               ;호의 중심점
            sa (cdr (assoc 50 thine))               ;호의 시작각
            ea (cdr (assoc 51 thine))               ;호의 끝각
            thinsp (polar cp sa r)                  ;얇은 판 시작점
            thinep (polar cp ea r))                 ;얇은 판 끝점
    )
  ) ;of COND
  (setq dist1 (distance thinp thinsp)               ;선택점에서 시작점까지 거리
        dist2 (distance thinp thinep))              ;선택점에서 끝점까지 거리
  (if (< dist1 dist2)                               ;선택점에서 가까운쪽을 첫점으로
    (setq p1 thinsp)
    (setq p1 thinep))

  (setq thickp (cadr thick)                         ;두꺼운 판 선택점
        thicke (entget (car thick))                 ;두꺼운 판 entity 정보
        entype2 (cdr (assoc 0 thicke)))             ;entity type
  (cond
    ((= entype2 "LINE")                             ;entity가 직선인 경우
      (setq thicksp (cdr (assoc 10 thicke))         ;얇은 판 시작점
            thickep (cdr (assoc 11 thicke)))        ;얇은 판 끝점
      (setq dist1 (distance thickp thicksp)         ;선택점에서 시작점까지 거리
            dist2 (distance thickp thickep))        ;선택점에서 끝점까지 거리
      (if (< dist1 dist2)                           ;선택점에서 가까운쪽을 둘째점으로
        (setq p2 thicksp
              po thickep)
        (setq p2 thickep
              po thicksp)
      ) ;of IF
      (setq dist (distance p1 p2))                      ;첫점에서 끝점까지 거리
      (setq distc (* dist 5.0))                         ;chamfer 거리

      (setq ang (angle po p2)                           ;첫점에서 끝점으로의 각도
            len (distance p2 po)                        ;두꺼운 판의 길이
            len2 (- len distc)                          ;두꺼운 새로운 길이
            bpnt (polar po ang len2))                   ;break point
    ) ;of LINE
    ((= entype2 "ARC")                                  ;entity가 호인경우
      (setq r (cdr (assoc 40 thicke))                   ;호의 반지름
            cp (cdr (assoc 10 thicke))                  ;호의 중심점
            sa (cdr (assoc 50 thicke))                  ;호의 시작각
            ea (cdr (assoc 51 thicke))                  ;호의 끝각
            thicksp (polar cp sa r)                     ;얇은 판 시작점
            thickep (polar cp ea r))                    ;얇은 판 끝점
      (setq dist1 (distance thickp thicksp)             ;선택점에서 시작점까지 거리
            dist2 (distance thickp thickep))            ;선택점에서 끝점까지 거리
      (if (< dist1 dist2)                               ;선택점에서 가까운쪽을 둘째점으로
        (setq p2 thicksp
              dist (distance p1 p2)                     ;첫점에서 끝점까지 거리
              distc (* dist 5.0)                        ;chamfer 거리
              bpnt (polar cp (+ sa (/ distc r)) r))     ;Break point
        (setq p2 thickep
              dist (distance p1 p2)                     ;첫점에서 끝점까지 거리
              distc (* dist 5.0)                        ;chamfer 거리
              bpnt (polar cp (- ea (/ distc r)) r))     ;Break point
      ) ;of IF
    ) ;of ARC
  ) ;of COND


  (command "BREAK" thick "F" p2 bpnt)                   ;taper구간 잘라내기
  (command "LINE" p1 bpnt "")                           ;경사선 그리기

  (setq ang (angle p1 bpnt)                             ;경사선의 각도
        tl (* ds th 5)                                 ;text길이
        bp (polar bpnt ang tl)                          ;base line끝점
        bp2 (polar bpnt ang (/ tl 2.0)))                ;base line 중간점

  (setq wh4 (which4 ang))                               ;몇사분면에 있는가?

  (cond                                                 ;1~4사분면에 있을 때
     ((= wh4 1)                                         ;1사분면에 있을 때
       (progn
         (setq ang2 ang)
         (setq tp (polar bp2 (+ ang (/ pi 2.0)) (* ds th)))
       );of PROGN
     )
     ((= wh4 2)                                        ;2사분면에 있을 때
       (progn
         (setq ang2 (- ang pi))
         (setq tp (polar bp2 (- ang (/ pi 2.0)) (* ds th)))
       ) ;of PROGN
     )
     ((= wh4 3)                                        ;3사분면에 있을 때
       (progn
         (setq ang2 (- ang pi))
         (setq tp (polar bp2 (+ ang (/ pi 2.0)) (* ds th)))
       ) ;of PROGN
     )
     ((= wh4 4)                                        ;4사분면에 있을때
       (progn
         (setq ang2 (- ang (* 2 pi)))
         (setq tp (polar bp2 (- ang (/ pi 2.0)) (* ds th)))
       ) ;of PROGN
     )
  );of cond

  (command "TEXT" "M" tp (* ds th) (rtod ang2) "1:5")      ;1:5쓰기
  (setvar "CECOLOR" "1")                                  ;빨간색으로
  (command "LINE" bpnt bp "")                               ;base line그리기
  (setvar "CECOLOR" "BYLAYER")                              ;원래 색으로

  (pop-env)
  (setq *error* oer seterr nil)
  (princ)

) ;of DEFUN
