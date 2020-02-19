;*************************************
; Program : SLOPL
;           SLOP Lines
;           By Suk-Jong Yi
;           1995/7/3
;*************************************
; 기울어진 면을 표시하는 선그리기
;*************************************

(defun C:SLOPL(/
            l1      l2      dnum    l1s     l1e     l2s     l2e     dst1
            dst2    temp    angs    ange    dt1     dt2     oldc    count
            dls     dle
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

  (push-env)                                                ;환경변수 대피

  (setq l1 (entget (car (entsel "\nSelect first line: "))))
  (setq l2 (entget (car (entsel "\nSelect second line: "))))
  (setq dnum (getint "\nDivide number?: "))

  (setq ln1s (cdr (assoc 10 l1)))
  (setq ln1e (cdr (assoc 11 l1)))
  (setq ln2s (cdr (assoc 10 l2)))
  (setq ln2e (cdr (assoc 11 l2)))

  (setq dst1s2s (distance ln1s ln2s))
  (setq dst1s2e (distance ln1s ln2e))
  (setq dst1e2s (distance ln1e ln2s))
  (setq dst1e2e (distance ln1e ln2e))
  
  (setq mindist (min dst1s2s dst1s2e dst1e2s dst1e2e))

  (cond
    ((<= dst1s2s mindist)
      (setq l1s ln1s
	    l1e ln1e
	    l2s ln2s
	    l2e ln2e)
    );sub cond
    ((<= dst1s2e mindist)
      (setq l1s ln1s
	    l1e ln1e
	    l2s ln2e
	    l2e ln2s)
    );sub cond
    ((<= dst1e2s mindist)
      (setq l1s ln1e
	    l1e ln1s
	    l2s ln2s
	    l2e ln2e)
    );sub cond 
    ((<= dst1e2e mindist)
      (setq l1s ln1e
	    l1e ln1s
	    l2s ln2e
	    l2e ln2s)
    );sub cond 
    
  );cond
  
;;;  (if (< dst2 dst1)
;;;    (progn
;;;      (setq temp l2s)
;;;      (setq l2s l2e)
;;;      (setq l2e temp)
;;;    ) ;of progn
;;;  ) ;of if(dst2<dst1)

  (setq angs (angle l1s l2s))          ;첫라인 시작~두째라인 시작 각
  (setq ange (angle l1e l2e))          ;첫라인 끝~두째라인 끝 각

  (setq dst1 (distance l1s l2s))
  (setq dst2 (distance l1e l2e))

  (setq dt1 (/ dst1 dnum))              ;시작점~지작점
  (setq dt2 (/ dst2 dnum))

  (setq oldc (getvar "CECOLOR"))
  (setvar "CECOLOR" "GREEN")

  (setq count 1)
  (repeat (1- dnum)
    (setq dls (polar l1s angs (* count dt1)))
    (setq dle (polar l1e ange (* count dt2)))
    (command "LINE" dls dle "")
    (setq count (1+ count))
  ) ;of repeat

  (setvar "CECOLOR" oldc)

  (pop-env)                                                 ;환경변수 복귀
  (setq *error* oer seterr nil)
  (prin1)

) ;of defun
