;****************************************
; Program : TBREAK
;           Text BREAK
;           By Suk-Jong Yi
;           1998/10/1
;****************************************

; text를 둘로 나누어준다.

(defun C:TBREAK(/
;ent trimx txt txt1 ip nearest count subx subdist newtxt1 newtxt2

)

  (defun SETERR(s)                                  ;내장에러루틴 정의
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

;  (setq oer *error* *error* seterr)                ;내장에러루틴 가동

  (push-env)                                          ;환경변수 대피

  (setq ent (entget (car (entsel "\nSelect Text entity: ")))) ;entity선택

  (setq trimx (car (getpoint "\nPick trim point: ")))         ;trim점 입력

  (setq txt  (cdr (assoc 1 ent)))                       ;string
  (setq txtl (strlen txt))                              ;string 길이
  (setq ip  (cdr (assoc 10 ent)))                        ;text의 insert point

  (setq nearest (abs (- (car ip) trimx)))        ;가장가까운점을 인서트포인트로
;  (print nearest)

  (setq count 1)                                        ;첫글자부터

  (repeat txtl
;    (print (caadr (textbox (list (cons 1 (substr txt 1 count))))))
    (setq subent (subst (cons 1 (substr txt 1 count)) (assoc 1 ent) ent)) ;subtext entity정보만들기
    (setq subx (+ (car ip) (caadr (textbox subent))))   ;현재글자 끝x
;    (setq subx (+ (car ip) (caadr (textbox (list (cons 1 (substr txt 1 count))))))) ;현재글자 끝x
    (setq subdist (abs (- subx trimx)))                 ;삽입점~현재글자끝
    (print subdist)
    (if (<= subdist nearest)                             ;지정한 점과 가장가까운 글자 찾기
      (setq nearest subdist                             ;가장가까운 거리
            trimcount count                             ;가장가까운 글자 번호
            trimxy (list subx (cadr ip)))               ;    "      글자 끝점
    );if
;    (print (substr txt 1 count))
;    (print trimx)
    (setq count (1+ count))                         ;다음 text길이로
  );repeat

  (setq newtxt1 (subst (cons 1 (substr txt 1 trimcount)) (assoc 1 ent) ent)) ;text1변경
  (setq newtxt1 (subst (cons 72 0) (assoc 72 newtxt1) newtxt1))     ;text1 정렬방식지정

  (entmod newtxt1)                                         ;text1변경

  (setq newtxt2 (subst (cons 1 (substr txt (1+ trimcount) (- txtl trimcount)))
                  (assoc 1 ent)
                  ent))                                 ;text2의 글자내용바꾸기

  (setq newtxt2 (subst (cons 10 trimxy)
                      (assoc 10 newtxt2)
                     newtxt2))                          ;   "    삽입점 바꾸기

  (setq newtxt2 (subst (cons 72 0)
                      (assoc 72 ent)
                     newtxt2))                          ;   정렬방식 바꾸기

  (entmake newtxt2)                                      ;text2 생성

  (pop-env)                                             ;환경변수복귀

  (setq *error* oer seterr nil)                         ;내장에러루틴 복귀

);defun
