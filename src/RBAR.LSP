;********************************
; Program : RBAR
;           Round BAR
;           Suk-Jong Yi
;           96/5/21
;********************************
; 라운드철근을 점선으로 표시
;********************************

(defun C:RBAR(/
      fp1       fp2     sslst   nss     count   rlen    fl      oldlt
      ename     lent    spnt    epnt    leng    crsp    dst1    dst2
      epnt1     spnt1   epnt2
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

  (push-env)                                                    ;환경변수 대피

  (setq fp1 (getpoint "\nStart point: "))                       ;fance라인 첫점
  (setq fp2 (getpoint fp1 "\nEnd point: "))                     ;fance라인 끝점
  (setq sslst (ssget "F" (list fp1 fp2) '((0 . "LINE"))))       ;fance로 라인만 선택
  (setq nss (sslength sslst))                                   ;라인 갯수

  (setq count 0)
  (repeat nss
    (redraw (ssname sslst count) 3)                             ;선택된 엔티티 강조
    (setq count (1+ count))                                     ;다음 엔티티로
  ) ;of repeat

  (princ nss)
  (princ " LINES found")                                        ;선택된 선수 표시

  (setq rlen (getdist "\nRound length: "))                      ;round길이
  (setq fl (list fp1 fp2))


  (if (= (tblsearch "LTYPE" "HIDDEN2") nil)                     ;hidden2가 없을 때 load
    (command "LINETYPE" "L" "HIDDEN2" "ACAD" ""))
  (setq oldlt (getvar "CELTYPE"))                               ;현재 라인타입대피
  (setvar "CELTYPE" "HIDDEN2")                                   ;라인타입을 히든으로

  (setq count 0)                                                ;첫번째 라인부터

  (repeat nss
    (setq ename (ssname sslst count))                           ;엔티티 이름
    (setq lent (entget ename))                                  ;라인엔티티 정보

    (setq spnt (cdr (assoc 10 lent))                            ;라인 시작점
          epnt (cdr (assoc 11 lent))                            ;라인 끝점
          leng (distance spnt epnt))                            ;라인의 길이

    (setq crsp (inters spnt epnt fp1 fp2)                       ;fance와의 교차점
          dst1 (distance spnt crsp)                             ;교차점과 시작점거리
          dst2 (distance epnt crsp))                            ;교차점과 끝점거리

    (if (< dst1 dst2)
      (setq epnt1 (polar epnt (angle epnt spnt) (- leng rlen))  ;break point
            spnt1 epnt                                          ;ent point
            epnt2 spnt)
      (setq epnt1 (polar spnt (angle spnt epnt) (- leng rlen))
            spnt1 spnt
            epnt2 epnt)
    ) ;of if
    (setq lent (subst (cons 10 spnt1) (assoc 10 lent) lent))    ;시작점 바꾸기
    (setq lent (subst (cons 11 epnt1) (assoc 11 lent) lent))    ;끝점 바꾸기
    (entmod lent)                                               ;엔티티 수정
    (command "LINE" epnt1 epnt2 "")                             ;점선 그리기
    (setq count (1+ count))                                     ;다음 라인으로
  ) ;of repeat

  (setvar "CELTYPE" oldlt)                                      ;원래 라인타입 복귀

  (pop-env)
  (setq *error* oer seterr nil)
  (princ)

 ) ;of defun

