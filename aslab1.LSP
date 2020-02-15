;----------------------------------------------
; program : ASLAB1
;           draw Approach slab for Ascon
;           By Yi Suk-Jong
;           2000/6/24
;----------------------------------------------
(defun c:aslab1(
               / ipnt gsel gent gpnt ent_type v_list salblen
                 slabt hunchl bracketl parapeth paveh tempnt1
                 tempnt2 tempnt3 tempnt4 ang270 slabepnt1
                 slabepnt2 hunchepnt1 hunchepnt2 ipnt1 ipnt2
                 bracketepnt
	       )

  (setq bbracket 300
	tslab    400
	tbase    100
	bbase    100
	slop     4)
  
  (setq ipnt (getpoint "\nInsert point: "))
  (setq sidepnt (getpoint "\nPick side point: "))
  (setq len (getreal "\nLength of A-SLAB(m): ")
	len (* 1000 len))

  
  (setq ipntx (car ipnt)
	ipnty (cadr ipnt))

  (if (> ipntx  (car sidepnt))
    (setq bbracket (* bbracket -1)
	  bbase (* bbase -1)
	  len (* len -1))
  )  

  (setq p1 (list             ipntx  (+ ipnty tslab))
	p2 (list (+ ipntx bbracket) (cadr p1))
	p3 (list (+ ipntx len) (- (cadr p2) (* (abs (- len bbracket)) (* slop 0.01))))
	p4 (list (car p3) (- (cadr p3) tslab))
	p5 (list (+ ipntx bbracket) ipnty)
	p6 (list (+ (car p4) bbase) (- (cadr p4) (abs (* bbase (* slop 0.01)))))
	p7 (list (car p5) (- (cadr p5) tbase))
	p8 (list (car p6) (- (cadr p6) tbase))
  );setq 

  (command "line" ipnt p1 p2 p3 p4 "")    ;draw aslab
  (command "line" ipnt p5 p6 p8 p7 "")    ;draw base con'c
	
  
  
);defun


; -------------------------------------
; function : mk_vertlist
; LwPolyline의 vertex list를 만들어준다.
; 인수: vname  : vertext entity name
;                 (car (entsel)) 상태로 넘어와야한다.
; -------------------------------------

  (defun mk_vertlist (vname
  /  vlist count nvert tmp vert_list pt1
                     );of local variable

    (setq vlist (entget vname))                          ;엔티티 정보

    (setq count 0)                                      ;첫 vertex 찾아감
    (while (/= (car (nth count vlist)) 10)
        (setq count (+ count 1))
    )                                                   ;첫째 vertex 위치


    (setq nvert (cdr (assoc 90 vlist)))                 ;vertext수

    (setq vert_list nil)                                 ;빈 list만들기
    (setq index 0)                                      ;첫vertex부터

    (repeat nvert
      (setq tmp (nth (+ count (* index 4)) vlist))     ;(10 x y)
      (setq tmp (append tmp (list (cdr (assoc 38 vlist)))))  ;z좌표추가
      (setq pt1 (trans (cdr tmp) (cdr (assoc -1 vlist)) 1))  ;ucs좌표로 치환
      (setq vert_list (append vert_list (list pt1)))         ;vertexlist에추가
      (setq index (1+ index))                                      ;다음 vertext로
    ); repeat

     (setq vert_list (list vert_list nvert))
  ) ;of defun


; -------------------------------------
; function : mk_vertlistp
; Polyline의 vertex list를 만들어준다.
; 인수: vname  : vertext entity name
;                (car (entsel)) 상태로 넘어와야한다.
; -------------------------------------
(defun mk_vertlistp(vname
/
)

  (setq vert_list nil                             ;리스트 초기화
        count 0                                   ;절점 갯수
        nxt vname)                                ;첫 엔티티 이름

  (while (setq nxt (entnext nxt))
    (progn
      (setq ent (entget nxt))                        ;엔티티정보축출
      (if (= (cdr (assoc 0 ent)) "VERTEX")           ;절점일때만
        (setq vert_list (append vert_list (list (cdr (assoc 10 ent))))
              count (1+ count))                      ;갯수추가
      );if
    );progn
  );while

  (setq vert_list (list vert_list count))            ;결과리턴

);defun


;------------------------------------
; function : cp_line_pline
;            cross point of line and polyline
;            Yi suk jong
;            99/10/8
;------------------------------------
; 기능 : line과 pline의 교점을 찾아준다
;
; 넘어오는 값
;     pnt1 : 라인의 첫째점
;     pnt2 : 라인의 둘째점
;     plist: polyline의 point list
;
; 넘어가는 값 : 교점들을 list로 묶어서 넘김
;-----------------------------------------------

(defun cp_line_pline(pnt1 pnt2 plist
/ pnt1 pnt2 plist n z pnt12d pnt22d cplist index pp1
  )
  (setq n (length plist))               ;polyline의 point갯수
  (setq z (nth 2 (nth 0 plist)))         ;polyline의 z좌표

  (setq pnt12d (list (car pnt1) (cadr pnt1))   ;xy좌표축출
        pnt22d (list (car pnt2) (cadr pnt2)))  ;xy좌표축출

  (setq cplist nil) ;교점list비우기
  (setq index 1)

  (repeat (1- n)
    (setq pp1 (reverse (cdr (reverse (nth (1- index) plist))))  ;xy좌표축출
          pp2 (reverse (cdr (reverse (nth     index  plist))))) ;xy좌표축출
    (setq cp (inters pnt12d pnt22d pp1 pp2))        ;교차점 찾기
    (if cp
      (progn
        (setq cp (append cp (list z)))                ;z좌표추가
        (setq cplist (append cplist (list cp))) ;교차점이 존재할 때 더하기
      );progn
    );if
    (setq index (1+ index))                     ;다음구간으로
  );repeat
    (if cplist cplist)                          ;교차점이 존재하면 리턴

);defun  
