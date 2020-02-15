;-------------------------------
; program : noripf
;           Nori profile
;           by Yi Suk-Jong
;           2000/6/23
;-------------------------------
;기능: 노리의 측면도를 그려줌
;사용법: 노리의 시작점을 찍어주고 지반선을 선택해줌
(defun c:noripf(
  / steph ipnt gsel gent gpnt ent_type v_list count miny
    cy slop nextp sodanb sodanp cpnt
		)

  (setq #meter 1)
  
  (setq ipnt (getpoint "\nPick insert point: "))  ;노리시작점
  (setq gsel (entsel "\nSelect Ground: "))        ;지반선선택
  (setq gent (entget (car gsel)))                 ;지반선정보
  (setq gpnt (cdr gsel))                          ;지반선선택점

  (noripf-dialog)

  (if (= #meter 1)                        ;미터 단위 밀리미터 단위에 따라
     (setq steph 6.0 sodanb 1.0)          ;소단 높이 및 소단 폭 설정
     (setq steph 6000 sodanb 1000)
  );of if
  
  (if (> (caar gpnt) (car ipnt)) (setq sgn 1) (setq sgn -1))   ;노리진행방향부호

  (setq ent_type (cdr (assoc 0 gent)))
  (cond
    ((= ent_type "LWPOLYLINE")
      (setq v_list (car (mk_vertlist (car gsel))))               ;vertex list만들기
    );subcond
    ((= ent_type "POLYLINE")
      (setq v_list (car (mk_vertlistp (car gsel))))              ;vertex list만들기
    );subcond
    ((= ent_type "LINE")                                         ;line인 경우
      (setq v_list (list (cdr (assoc 10 gent)) (cdr (assoc 11 gent))))
    );subcond 
  );cond

  (setq count 1                                 ;최소 y값을 처음 node값으로
	miny (nth 1 (nth 0 v_list)))
  
  (repeat (1- (length v_list))                  ;최소 y값 구하기
    (setq cy (nth 1 (nth count v_list)))
    (if (<= cy miny) (setq miny cy))
    (setq count (1+ count))
  );repeat
  
  (setq slop 1.5)                               ;최초소단은 1:1.5로

  
  (setq nextp (list (+ (car ipnt) (* slop sgn steph)) ;첫재단의 끝 점
		    (- (cadr ipnt) steph)))

  (setq count 0)

  (while (and (> (cadr ipnt) miny)          ;단의 끝이 최소y값보다 작거나 지반선과 겹칠 때까지 반복
	      (= (setq cpnt (car (cp_line_pline ipnt nextp v_list))) nil))
    (setq sodanp (list (+ (car nextp) (* sodanb sgn)) (cadr nextp)))
    (command "line" ipnt nextp sodanp "" )
    (2ptext ipnt nextp (strcat "1:" (rtos slop 2 1)))
    (setq count (1+ count))
    (if (> count 0) (setq slop 1.8))
    (setq nextp (list (+ (car sodanp) (* slop sgn steph))
               	      (- (cadr sodanp) steph)))
    (setq ipnt sodanp)
  ); while
  (command "LINE" ipnt cpnt "")
  (2ptext ipnt cpnt (strcat "1:" (rtos slop 2 1)))
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

;---------------------------
; function : noripf-dialog
;            2000/6/24
;---------------------------
; 다이얼로그 박스를 통해서 noripf의 옵션을 입력받는다.
;---------------------------
(defun noripf-dialog(/  
                  dcl_id )  
  
  (setq dcl_id (load_dialog "DJDG"))                 ;ddscl.dcl load  
  (if (not (new_dialog "noripf" dcl_id)) (exit))         ;ddscl.dcl 안의 scl빨몹  
  
  (set_tile "meter" "1")
  (set_tile "milimeter" "0")
  (set_tile "wslop" "1")  

  
  (action_tile "meter" "(setq #meter 1)")               ;user 럼력 box  
  (action_tile "milimeter" "(setq #meter 0)")                     ;grid ON/OFF toggle  
  (action_tile "wslop" "(set-wslop)")                     ;grid ON/OFF toggle  
  
  (start_dialog)  
  (unload_dialog dcl_id)  
  (princ)  
) ;of defun nori-DIALOG

(defun set-wslop()
  (setq #wslop (get_tile "wslop"))
)

;-----------------------------
; function : 2ptext
;            2 point text
;            By Yi Suk-Jong
;            2000/6/23
;-----------------------------
; 두점을 밑선으로 하는 text를 만들어준다.
;  이때 텍스트의 크기는 dimscale x dimtxt 이며
;  텍스트의 위치 및 회전각은 치수선 법칙에 따라
;  만들어진다.
;-----------------------------
(defun 2ptext(p1 p2 text
	      / p1 p2 text th ang wh4 ang2 tang txtpnt oldosmode)

  (setq th (* (getvar "DIMSCALE") (getvar "DIMTXT")))
  
  (setq ang (angle p1 p2))
  
  (setq wh4 (which4 ang))                              ;몇사분면에 있는가?

  (cond                                                ;1~4사분면에 있을 때
     ((= wh4 1)
       (setq ang2 ang)
       (setq tang (+ ang (* pi 0.5)))
     )
     ((= wh4 2)
       (setq ang2 (- ang pi))
       (setq tang (- ang (* pi 0.5)))      
     )
     ((= wh4 3)
       (setq ang2 (- ang pi))
       (setq tang (- ang (* pi 0.5)))      
     )
     ((= wh4 4)
       (setq ang2 (- ang (* 2 pi)))
       (setq tang (+ ang (* pi 0.5)))      
     )
  );of cond

  (setq txtpnt (polar (mid-point p1 p2) tang (* th 0.5)))  ;text point

  (setq oldosmode (getvar "OSMODE")) (setvar "OSMODE" 0)
  (command "TEXT" "J" "C" txtpnt th (rtod ang2) text "")
  (setvar "OSMODE" oldosmode)

);defun of 2ptext
