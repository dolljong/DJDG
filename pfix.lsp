;----------------------
; program : pfix
;           PreFix
;           Yi Suk Jong
;----------------------

(defun c:pfix()
;  (setq srctxt (entget (car (entsel "\nPick Source Text: ")))) ;원본text선택
;  (if (= (cdr (assoc 0 srctxt)) "TEXT")
   (setq ptxt (getstring "\nEnter Prefix text: "))
;      (setq srctxt (cdr (assoc 1 srctxt)))                     ;원본text내용
      (princ "\nSelct destination text: ")
      (setq ss1 (ssget))                                    ;대상text선택
      (setq num (sslength ss1))                             ;text갯수

      (princ num)
      (princ " Text Found")                             ;발견된 text갯수 표시

      (setq index 0)                                    ;초기화
      (setq cnum 0)

      (repeat num                                       ;text 갯수만큼 반복
        (setq entl (entget (ssname ss1 index)))         ;대상엔티티정보확보
        (if (= (cdr (assoc 0 entl)) "TEXT")             ;text인경우에만
          (progn
            (setq ass1 (assoc 1 entl))                ;대상text assoc데이타 확보
            (setq srctxt (strcat ptxt (cdr ass1)))     ;prefix더하기
	    (setq co (cons (car ass1) srctxt))        ;원본text assoc데이타 작성
            (setq entl1 (subst co ass1 entl))         ;새로운 entity정보작성
            (entmod entl1)                            ;새로운 text로 업데이트
            (setq cnum (1+ cnum))                     ;고쳐진 text갯수
          );progn

          (princ "\nNon-Text entity was filtered")    ;text아닌 경우
        );if
        (setq index (1+ index))                   ;다음 text로
      ) ;of repeat
;    );progn

    (princ "\Text not found")

;  );if

   (terpri)
   (princ cnum)
   (princ " Text Modified")

;  (setq *error* oer seterr nil)

   (princ)

 
);defun  
	      
