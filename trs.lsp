;****************************************
;*    trs
;*              Copye TeXT
;*              By Suk-Jong Yi
;*              99/2/9
;****************************************

(defun C:trs(
              / llist txtlst txtlst1)     ;지역변수정의

;      (setq fn (getfiled "INPUT DATA" "" "DAT" 0))      ;file name입력
      (setq fn (getstring "\nEnter file name: "))      ;file name입력
      (setq opf (open fn "r"))                          ;file open
      (if opf                                           ;file이 없는 경우
        (progn
           (setq count 1)
           (while (setq ch (read-line opf))             ;한줄을 읽는다
              (princ (chr 13))                          ;입력중 메세지 출력
              (princ count)
              (princ " Line Processing...")
;              (setq inline (data-in ch))
              (setq llist (append llist (list ch)))                   ;llist에 추가
              (setq count (1+ count))                                  ;line번호 증가
           ) ;of while
        ) ;of progn
        (princ "\nFile not found")                          ;file이 없는 경우
      ) ;of if
      (close opf)                                           ;file close
    
  (setq nline (length llist))   ;입력데이터 라인 수
  (setq nrepeat (fix (/ nline 2)))    ;반복회수 = 전체수/2
  (setq i 0)
  (repeat nrepeat
    (setq txtlst (append txtlst (list (nth (* i 2) llist))))          	;0, 2, 4
    (setq txtlst1 (append txtlst1 (list (nth (1+ (* i 2)) llist))))     ;1, 3, 5
    (setq i (1+ i))
  );repeat  

  (setq sstxt (ssget "X" '((0 . "TEXT"))))   ;모든 text 택
  
  (setq nss (sslength sstxt))			;ss갯수
  (setq index 0)

  (repeat nss					;text갯수만큼 반복
    (setq ent (entget (ssname sstxt index)))	;entity정보 축출
    (setq oldass1 (assoc 1 ent))   		;text내용축출
    (setq i 0)

    (repeat (length txtlst)			;text list만큼 반복
      (if (= (delspc (cdr oldass1))   (nth i txtlst))  	;문자비교,비교할 때 space는 지우고 비교
        (progn
          (setq newass1 (cons 1 (nth i txtlst1)))	;
          (setq entl1 (subst newass1 oldass1 ent))         ;새로운 entity정보작성
          (entmod entl1)                            ;새로운 text로 업데이트
        );prog
      );if
      (setq i (1+ i))
    ); repeat
    (setq index (1+ index))
  );repeat
);defun  

;****************************************
;*    function : DELSPC
;*              DELete SPaCe
;*              By Suk-Jong Yi
;*              03/10/30
;****************************************

(defun delspc( txt / txt1 result lstr i ch)
  (setq txt1 txt)
  (setq result "")
  (setq lstr (strlen txt1))
  (setq i 1)
  (repeat lstr
    (setq ch (substr txt1 i 1))
    (if (/= ch " ")
     (setq result (strcat result ch))
    );if		  
    (setq i (1+ i))
  );repeat
  result
);defun