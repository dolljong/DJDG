;************************************
; Program : CSEC
;           Cross SECtion
;           By Suk-Jong Yi
;           99/10/8
;************************************
; 지반 횡단도를 그려준다

(defun C:CSEC(
/ sp ep dst ang delta_dst blk_name
  div_n ans rot_ang count
              ); of variable

  (defun SETERR(s)                                  ;내장에러루틴 정의
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

  (setq oer *error* *error* seterr)                   ;내장에러루틴 가동

  (push-env)                                          ;환경변수 대피
  (setq v_list nil)

  (setq polylines (ssget  '((-4 . "<OR")           ; lwpolyline이나 polyline만 선택
                              (0 . "LWPOLYLINE")
                              (0 . "POLYLINE")
                           (-4 . "OR>"))))

  (setq sp (getpoint "\nPick start point: "))       ;시작점
  (setq ep (getpoint sp "\nPick end point: "))      ;끝점

  (setq n_polylines (sslength polylines))             ;선택된 polyline의 갯수

  (setq count 0)
  (setq csec_list nil)                                      ;횡단 data비우기

  (repeat n_polylines                                       ;entity갯수만큼 반복

    (setq ent_pline (ssname polylines count))               ;entity이름

    (setq ent_type (cdr (assoc 0 (entget ent_pline))))
    (cond
      ((= ent_type "LWPOLYLINE")
        (setq v_list (mk_vertlist ent_pline))               ;vertex list만들기
      );subcond
      ((= ent_type "POLYLINE")
        (setq v_list (mk_vertlistp ent_pline))              ;vertex list만들기
      );subcond
    );cond

    ;poly line에 관한 정보 얻기
    (setq nvlist (cadr v_list)                          ;vertext갯수
          v_list (car v_list))                          ;vertext list

    (setq cp_list (cp_line_pline sp ep v_list))         ;교차점 찾기

    (if (/= cp_list nil)                                 ;교점이 존재하면
      (setq csec_list (append csec_list cp_list))        ;횡단 점에 추가
    );if

    (setq count (1+ count))                             ;다음 polyline으로
  );repeat

  (setq csec_count 0)
  (repeat (length csec_list)
    (command "point" (nth csec_count csec_list))
    (setq csec_count (1+ csec_count))
  );repeat

  ;교차점에 시점에서부터의 거리 추가하기
  (setq csec_d_list nil)                        ;거리가 추가된 point list비우기
  (setq csec_count 0)
  (repeat (length csec_list)
    (setq nth_point (nth csec_count csec_list))
    (setq csec_d_list
      (append csec_d_list
        (list  (append nth_point
                  (list (distance (list (car sp) (cadr sp) 0)
                          (list (car nth_point) (cadr nth_point) 0)
                        )
                  )
               )
        )
      )
    )
    (setq csec_count (1+ csec_count))           ;다음 point로
  );repeat

;  (princ csec_d_list)
  (setq csec_d_list (sort_plist csec_d_list 3))
;  (princ csec_d_list)

  (draw_csec csec_d_list)

);defun CSEC

; 구해진 elevation점들을 이용해 횡단면도 그리기
(defun draw_csec(csec_data
 / ip first_dist first_z csec_count nth_csec current_dist current_z pnt)
  (setq ip (getpoint "\nPick Insert point: "))
  (setq first_dist (nth 3 (nth 0 csec_d_list))) ;첫번째 교점의 sp로부터 거리
  (setq first_z    (nth 2 (nth 0 csec_d_list))) ;첫번째 교점의 z값
  (command "pline" ip)

  (setq csec_count 1)
  (repeat (1- (length csec_list))
    (setq nth_csec (nth csec_count csec_d_list))
    (setq current_z    (nth 2 nth_csec)
          current_dist (nth 3 nth_csec))
    (setq pnt (list (+ (car ip) (- current_dist first_dist))
                    (+ (cadr ip) (- current_z first_z)) 0))
    (command pnt)
    (setq csec_count (1+ csec_count))
  );repeat
    (command "")
);defun draw_csec



(defun test()

;시작점과 끝점에 가장가까운 노드 찾기
  (setq spnode (near_vert v_list sp))               ;시작점과 가장가까운 노드
  (setq epnode (near_vert v_list ep))               ;끝점과 가장가까운 노드

  (if (< epnode spnode)                             ;node순서와 시종점이 방향이
    (setq v_list (reverse v_list)                   ;반대일 경우 순서바꾸어줌
          spnode (near_vert v_list sp)
          epnode (near_vert v_list ep))
  );if

  (princ "\nspnode: ") (princ spnode)
  (princ "\nepnode: ") (princ epnode)

;--- 시작점의 다음노드 찾기

  (cond
    ((= spnode 0)
      (setq spnextnode 1))                          ;최대가까운점이 0이면
    (T
      (if (< (distance (nth (1- spnode) v_list) sp) ;최대가까운 노드의 앞노드에서 에서 시작점까지 거리
             (distance (nth (1- spnode) v_list) (nth spnode v_list))) ;최대가까운 노드의 앞노드에서 최대가까운 노드까지 거리
        (setq spnextnode spnode)                    ;시작점다음노드는 가장가까운노드
        (setq spnextnode (1+ spnode))               ;시작점다음노드는      "        다음
      ) ;if
    );subcond
  );of cond

;--- 끝점 다음노드
  (cond
    ((= epnode (1- nvlist))
      (setq epnextnode (1- nvlist)))                ;최대가까운점이 마지막점이면
    (T
      (if (< (distance (nth (1- epnode) v_list) ep) ;최대가까운 노드의 앞노드에서 에서 시작점까지 거리
             (distance (nth (1- epnode) v_list) (nth epnode v_list))) ;최대가까운 노드의 앞노드에서 최대가까운 노드까지 거리
        (setq epnextnode epnode)
        (setq epnextnode (1+ epnode))
      ) ;if
    );subcond
  );of cond

  (princ "\nspnextnode: ") (princ spnextnode)
  (princ "\nepnextnode: ") (princ epnextnode)

;--- 사용자가 선택한 구간만으로 이루어진 vertix리스트만들기
  (setq nvlist (list sp ))      ;사용자가 선택한 구간으로 이루어진 vertix list
  (setq nllist (list 0 ))       ;                "                 vertext 길이 list

  (setq clen 0)                                 ;현재 길이 = 0
  (setq lastpnt sp)                             ;지난점을 시작점으로

  (setq index spnextnode)

  (while (< index epnextnode)
    (setq crntpnt (nth index v_list))                           ;현재포인트
    (setq clen (+ clen (distance lastpnt crntpnt)))             ;현재길이
    (setq nvlist (append nvlist (list crntpnt)))                ;node추가
    (setq nllist (append nllist (list clen)))                   ;누가길이추가
    (setq index (1+ index))                    ;다음노드로
    (setq lastpnt crntpnt)                     ;현재점을 전점으로
  );while

  (setq nvlist (append nvlist (list ep))                  ;사용자가 선택한 마지막 노드추가
        nllist (append nllist (list (+ clen (distance (nth (1- index) v_list) ep)))))

  (setq vlnum (length nllist)                   ;vlist의 갯수
        vllen (nth (1- vlnum) nllist))          ;pline의 총길이

  (princ "vlnum:") (princ vlnum)
  (princ "vllen:") (princ vllen)

  ;--- 노리 제원 입력하기

  (setq noridir (getpoint "\nPick Nori Direction: "))
  (if (<= (dang (angle sp noridir) (angle sp ep)) 0) ;우측으로 노리를 그릴경우
    (setq d90 (* pi 0.5))                           ;-90도
    (setq d90 (* pi -0.5))                            ;+90도
  );if

  (setq norilength (getdist "\nLength of Nori: ")) ;노리길이 입력

;  (setq norilength 1)

  (setq gap_default (* 4 (getvar "DIMSCALE")))      ;노리선간격(출력기준 4mm)
  (princ "\n노리선 간격 <") (princ gap_default)
  (setq gap_nori (getdist ">: "))                   ;노리선 간격입력
  (if (= gap_nori nil) (setq gap_nori gap_default))

;  (setq gap_nori 0.1)

  ;--- 노리 그리기
  (setq clen 0)                                     ;현재길이
  (setq ccnt 0)                                     ;현재 갯수

  (while (<= clen vllen)                            ;현재노리위치의 거리가
    (setq tmp (PointonPline nvlist nllist clen))    ;현재길이 점의 (좌표,node)
    (setq vsp (nth (1- (cadr tmp)) nvlist))          ;현재점이 있는 선분의 시작점
    (setq vep (nth (cadr tmp) nvlist))               ;          "          끝점
    (setq seang (angle vsp vep))                      ;시작점-끝점 각
    (setq p1 (car tmp))                                     ;노리선의 시작점
    (if (= (rem ccnt 5) 0)                          ;다섯개마다 한개씩 길게
      (setq tmp norilength)                     ;나머지는 짧게(반)
      (setq tmp (* 0.5 norilength))             ;나머지는 짧게(반)
    );if
    (setq p2 (polar p1 (+ seang d90) tmp))      ;노리선의 끝점
    (command "LINE" p1 p2 "")                   ;line그리기
    (setq ccnt (1+ ccnt))                       ;카운트 하나 더하기
    (setq clen (+ clen gap_nori))               ;다음nori누가거리
  );while

;  (princ "vllist") (princ vllist)

  (pop-env)                         ;환경변수 복귀

  (setq *error* oer seterr nil)     ;에러루틴 복귀

  (princ)
) ;of defun


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

;--------------------------------------
; function : near_vert
;            nearest vertix
;            by Yi Suk-Jong
;            98/8/30
;--------------------------------------
; 가장가까운  node찾기
;--------------------------------------
(defun near_vert(vlst pnt
/ nvert middist near_node index dist
)
  (setq nvert (length vlst))            ;vertix수 구하기

  (setq mindist  (distance (nth 0 vlst) pnt))       ;최단거리 = 첫점과의 거리
  (setq near_node 0)                                ;가까운전 0

  (setq index 1)                                    ;첫노드부터
  (repeat (1- nvert)
    (setq dist (distance (nth index vlst) pnt))     ;주어진점과 조사점의 거리
    (if (<= dist mindist)                            ;거리가 제일 가까우면
      (setq mindist dist                            ;최단거리 변경
            near_node index)                        ;최단node 변경
    );if
    (setq index (1+ index))                         ;다음 노드로
  ); repeat
  (setq near_node near_node)                        ;제일가까운점 전달
) ;defun

;------------------------------------
; function : PointOnPline
;            By Yi Suk-Jong
;            98/9/3
;------------------------------------
; 주어진 polyline의 시점에서
; 일정 위치에 있는 점의 좌표를 구함
;   Plist     : Polyline의 point list
;   Vllist    : Vertex 누가거리 list
;   Distfrom0 : Distance from start point
;   retuen : (list pnt node)  <-- pnt: x,y   node:다음노드번호
;-------------
(defun PointOnPline(Plist Vllist Distfrom0
 / plist Vllist distfrom0 nvertex pnt index node
                   )
  ;------ vertext갯수 구하기

  (setq nvertex (length Plist))

  (setq pnt nil)

  (setq index 1)
  (while (< index nvertex)
    (cond
      ((< Distfrom0 (nth index vllist))     ;주어진 거리가 사이에 있을 때
        (setq pnt (polar (nth (1- index) Plist)                 ;바로 전점
               (angle (nth (1- index) Plist) (nth index Plist)) ;각도
               (- Distfrom0 (nth (1- index) vllist))))          ;거리차
        (setq node index)                                       ;다음노드
        (setq index (1+ nvertex))                               ;while문 끝내기 위해 큰수 입력
      );sub cond
      ((= Distfrom0 (nth index vllist))     ;주어진 거리가 index번째 노드일때
        (setq pnt (nth index Plist))                            ;x,y좌표
        (setq node index)                                       ;노드번호는 index
        (setq index (1+ nvertex))                               ;while문 끝내기 위해 큰수 입력
      );sub cond
      ( T (setq index (1+ index)))          ;주어진 거리가 사이에 없을 때
    );cond
  );while

  (if (= pnt nil)
    nil                                     ;pnt에 아무 입력 없으면 nil return
    (list pnt node)                         ;pnt, node return
  );if

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
;/ pnt1 pnt2 plist n z pnt12d pnt22d cplist index pp1
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

;******************************************************
; Function : SORT_PLIST
;           SORT Polyline LIST
;           Yi Suk-Jong
;           1996/2/23
;******************************************************
; list를 sort해준다.
; 넘어오는 값
;     ALIST : SORT되어야할 LIST
;      AIDX : 기준이 되는 sub list (첫 sub list = 0)
; 넘겨지는 값
;             SORT된 LIST
;******************************************************

(defun SORT_plist(alist aidx
/       alist       nl       rlist       slist        count      min
        min_th      count1   c_list      c_val        aidx
)

  (setq nl (length alist))                  ;list의 갯수

  (setq slist nil)                          ;빈 sort된 list만듬
  (setq rlist alist)                        ;최대값을 축출한 나머지 list

  (setq count nl)                           ;list 갯수부터 한개씩 빼면서 반복

  (repeat nl                                        ;list갯수만큼
    (setq min (nth aidx (nth 0 rlist)))             ;첫번째 list를 작은 값으로
    (setq min_th 0)                                 ;최소값의 위치를 처음으로
    (setq count1 1)                                 ;두번째 list부터
    (repeat (1- count)
      (setq c_list (nth count1 rlist))              ;현재 list
      (setq c_val (nth aidx (nth count1 rlist)))    ;현재 값
      (if (<= c_val min)                             ;현재 값이 min보다 작을때
        (progn
          (setq min_th count1)                      ;최소값위치를 현재 위치로
          (setq min c_val)                          ;최소값을 현재 값으로
        ) ;of progn
      ) ;of if
      (setq count1 (1+ count1))                     ;다음 list로
    ) ;of repeat
    (setq slist (append slist (list (nth min_th rlist)))) ;최소값을 sort된 list에 추가
    (setq rlist (del_atom rlist min_th))            ;남은list에서 최소 list 제거
    (setq count (1- count))                         ;한개 줄여서
  ) ;of repeat
  (setq slist slist)
) ;of defun

;************************************************
; Function : LT
;           Less Then
;           Yi Suk-Jong
;           1996/2/27
;************************************************
; 인수-1이 인수-2보다 작은가를 판단해준다.
; 넘어오는 값:
;       ARG1 : 인수-1
;       ARG2 : 인수-2
; 넘어가는 값:
;       T    : 인수-1이 인수-2보다 작을 때
;       nil  : 인수-1이 인수-2보다 작지 않을때
;************************************************

(defun LT(arg1 arg2)
  (if (and (is-num arg1) (is-num arg2))
    (< (atof arg1) (atof arg2))
    (< arg1 arg2)
  ) ;of if
) ;of defun


;************************************************
; Function : DEL_ATOM
;           DELete ATOM
;           Yi Suk-Jong
;           1996/2/23
;************************************************
; list에서 특정 atom을 지운다
; 넘어오는 값
;             b_list : 축출전 list
;               anth : 축출되야할 atom의 위치
; 넘겨가는 값
;                    : 축출후 list
;************************************************

(defun DEL_ATOM(b_list anth
/       b_list      mlist       a_list      count   ;지역변수
)

  (setq nlist (length b_list))                      ;list의 갯수

  (setq a_list nil)                                 ;빈 list생성
  (setq count 0)                                    ;첫번째 list부터

  (repeat nlist                                     ;list갯수만큼 반복
    (if (/= count anth)                             ;지정된 atom이 아닌경우만
      (setq a_list (append a_list (list (nth count b_list))))   ;list에다 추가
    ) ;of if
    (setq count (1+ count))
  ) ;of repeat

  (setq a_list a_list)

) ;of defun

;************************************
; Function : IS-NUM
;            IS NUMber ?
;            By Suk-Jong Yi
;            1996/2/23
;************************************
; 문자열이 숫자인가?를 판단해준다.
;************************************

(defun IS-NUM(str
/ str strl count ch )

  (setq strl (strlen str))
  (setq count 1)
  (while (or (and (>= (setq ch (ascii (substr str count 1))) 48)
                  (<= ch 57))
             (= ch 44)
             (= ch 46)
             (and (= count 1) (= ch 43))
             (and (= count 1) (= ch 45))
         )
    (setq count (+ count 1))
  ) ;of while

  (if (= count (+ strl 1)) strl NIL)

) ;of defun

