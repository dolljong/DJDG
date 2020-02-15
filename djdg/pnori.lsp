; Program : PNORI 	:   Plan NORI
; function : mk_vertlistp:   Polyline의 vertex list를 만들어준다.
; function : near_vert   : nearest vertix
; function : PointOnPline : 주어진 polyline의 시점에서 일정 위치에 있는 점의 좌표를 구함


;************************************
; Program : PNORI
;           Plan NORI
;           By Suk-Jong Yi
;           98/8/28
;************************************
; 평면상의 노리를 그려준다. (노리선간격은 출력기준 4mm)

(defun C:PNORI(
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

  ;(setq oer *error* *error* seterr)                   ;내장에러루틴 가동

  (push-env)                                          ;환경변수 대피

;  (setq ent_pline (entget (car (entsel "\nSelect Polyline: ")))) ;polyline선택
  (setq ent_pline (car (entsel "\nSelect Polyline: "))) ;polyline선택

  (setq sp (getpoint "\nPick start point: "))
  (setq ep (getpoint sp "\nPick end point: "))

  (setq ent_type (cdr (assoc 0 (entget ent_pline))))
  (cond
    ((= ent_type "LWPOLYLINE")
      (setq v_list (mk_vertlist ent_pline))               ;vertex list만들기
    );subcond
    ((= ent_type "POLYLINE")
      (setq v_list (mk_vertlistp ent_pline))               ;vertex list만들기
    );subcond
  );cond

  ;poly line에 관한 정보 얻기
  (setq nvlist (cadr v_list)                          ;vertext갯수
        v_list (car v_list))                          ;vertext list

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

