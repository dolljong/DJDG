;*****************************************
;       CAMBER
;           draw CAMBER
;           Jong-Suk Yi
;           1996. 2. 12
;*****************************************
; 이 풀그림은 처짐도(CAMBER)를 그려준다.
; DATA 입력사항은 (절점번호, X-좌표, Y-좌표) 이다.
;  update list
; --- 03/07/14(월) 
;   - 캠버값에 factor를 줄 수 있도록 수정
;   - 캠버값의 precision을 지정할 수 있도록 수정
;   - Dimzin값을 0으로 setting후 실행

(defun C:CAMBER(
 /               gap     bdrx     bdry    yfac   rh       fcw      trg     th
                 fn      opf      nlist   comb   drw      ch       nnum    ncamb
                 ncomb   ndraw    xmax    xmin   xscale   yscale   cw      node
                 X       YC       ccount  count  cnt_comb draw_cnt
                 ncmb    maxy_lst n_node  maxy   n_case   cn       n_y
                 camby   firstx   lastx   nd     xx       bpnt     ap      b1
                 b2      cp       txtpnt  yy     oldc     cmb_cnt  c_count
                 n_count pnt      table   ntbl   tblh     maxn     tcount  node1
                 node2   tbly     tblpx   tblpy
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

;  (setq oer *error* *error* seterr)

  ;----- 각종 초기값 지정 (Camber도)
  (setq gap (getint "\nLEFT MARGIN (mm)<60>: "))    ;도각과 camber도와의 간격(여백)
  (if (= gap nil) (setq gap 60))                    ;return입력시 gap=60mm
  (setq bdrx 780)                                   ;border의 x크기
  (setq bdry 557)                                   ;border의 y크기
;  (setq yfac 100)                                   ;y-factor
;  (setq yfac 1000)                                   ;y-factor

  ;----- 각종 초기값 지정 (Camber table)
  (setq rh 7)                                     ;row height (table 줄높이)
  (setq fcw 20)                                   ;first column width
  (setq trg 2)                                    ;text right gap

  (setvar "CMDECHO" 0)                            ;command echo off
  (setvar "BLIPMODE" 0)                           ;blip mode off
  (setvar "DIMzin" 0)
  (setq th (getvar "DIMTXT"))                     ;text size 지정

  (command "ZOOM" "W" "0,0" (list bdrx bdry))       ;도각이 꽉 차게 보이기

  (setq fn (getfiled "INPUT DATA" "" "DAT" 0))    ;file name입력
  (setq opf (open fn "r"))                        ;file open

  (setq nlist nil)                                ;빈 node-list 만듬
  (setq comb nil)                                 ;빈 조합 만듬
  (setq drw nil)                                 ;빈 draw-list만듬

  ;----- data file로부터 DATA입력

  (if opf                                         ;file이 없는 경우
    (progn
       (while (and (/= (setq ch (read-line opf)) nil)         ;파일의 끝
                   (/= (strcase ch) "COMB")                   ;comb라는 단어
                   (/= (strcase ch) "DRAW")                   ;comb라는 단어
                   (/= ch ""))                                ;빈칸
            (setq nlist (append nlist (list (strloc ch))))    ;nlist에 추가
       ) ;of while
       (if (or (= ch "COMB") (= ch "comb"))                 ;comb일때
         (while (and (/= (setq ch (read-line opf)) nil)     ;comb데이타 읽기
                     (/= ch "")
                     (/= (strcase ch) "DRAW"))
            (setq comb (append comb (list ch)))
         ) ;of while
       ) ;of if
       (if (or (= ch "DRAW") (= ch "draw"))
         (while (and (/= (setq ch (read-line opf)) nil)
                     (/= ch ""))
           (setq drw (append drw (list (strloc ch))))
         ) ;of while
       ) ;of if
    ) ;of progn
    (princ "\nFile not found")                    ;file이 없는 경우
  ) ;of if
  (close opf)                                     ;file close

  ;----- 입력 line수 등 구함
  (setq nnum (length nlist))                         ;입력 line의 갯수
  (setq ncamb (- (length (nth 1 nlist)) 2))          ;camber의 수
  (setq ncomb (length comb))                          ;입력 Combination의 갯수
  (setq ndraw (length drw))                          ;draw수

  (if (= drw nil)
    (progn (princ "\nDRAW not found") (exit))) ;draw없을 때 종료

  ;----- 입력 line수등 표시해줌
  (princ nnum) (princ "-NODE / ")
  (princ ncamb) (princ "-CAMBER(S) / ")
  (princ ncomb) (princ "-COMBINATION(S) / ")
  (princ ndraw) (princ "-DRAWING(S) FOUND")

  (setq xmax (atof (nth 1 (nth (1- nnum) nlist))))  ;맨 끝점의 x값
  (setq xmin (atof (nth 1 (nth 0 nlist))))           ;첫점 X값

  (setq xscale (/ (- bdrx (* 2 gap)) xmax))          ;도각과의 x scale(계산)
;  (setq yscale (getint "\nY-Scale: "))               ;y방향 scale(입력값)

  (setq node nil)                                     ;node list 초기화
  (setq    x nil)                                     ;x값 list초기화
  (setq   YC nil)                                     ;Y값 list의 list

  ;----- node list, x값 list 만들기
  (setq count 0)
  (repeat nnum                                        ;node갯수만큼
    (setq node (append node (list (nth 0 (nth count nlist)))))
    (setq X    (append x (list (atof (nth 1 (nth count nlist))))))
    (setq count (1+ count))                           ;다음 node로
  ) ;of repeat

  ;----- 최소 column의 폭 구하기/입력받기
  (setq min_xlen (* (+ (strlen (rtos (nth 0 (reverse x)) 2 3)) 2) th))
  (princ "\nColumn width (mm)<")
  (princ min_xlen)
  (setq cw (getint ">: "))                      ;camber table 칸 넓이 입력받음
  (if (< cw min_xlen) (setq cw min_xlen))       ;최소폭보다 적으면 최소폭으로
  (setq cw (fix min_xlen))

  ;----- 캠버값 factor입력받기
  
  (setq yfac (getreal "\nEnter Factor of Value <1.0>: "))
  (if (= yfac nil) (setq yfac 1.0))
  (setq vprec (getint "\nEnter Precision <3>: "))
  (if (= vprec nil) (setq vprec 3))


  ;----- Y값 list 만들기
  (setq ccount 2)
  (repeat ncamb                                           ;camber수만큼
    (setq count 0)                                        ;첫점부터..
    (setq y nil)                                          ;y list 초기화
    (repeat nnum                                          ;마지막 점까지..
      (setq cy (atof (nth ccount (nth count nlist))))     ;y값 축출
      (setq Y (append Y (list cy)))
      (setq count (1+ count))
    ) ;of repeat
    (setq YC (append YC (list Y)))                        ;y list 묶음 만들기
    (setq ccount (1+ ccount))                             ;다음camber로
  ) ;of repeat

  ;----- combination결과 Y값 list에 추가하기
  (setq cnt_comb 0)
  (repeat ncomb
    (setq yc (append yc (list (op (nth cnt_comb comb) yc)))) ;comb를 계산하여 yc에 추가
    (setq cnt_comb (1+ cnt_comb))                             ;다음 comb로
  ) ;of repeat

  (setq n_camber (length yc))                           ;camber의 총수

  ;----- 최대 y값 및 yscale구하기
  (setq maxcy (abs (nth 0 (nth 0 yc))))                 ;최대 y값은 처음 값
  (setq count_c 0)
  (repeat n_camber                                      ;camber수 만큼
    (setq count_n 0)
    (repeat nnum                                        ;node수 만큼
      (setq cy (nth count_n (nth count_c yc)))          ;y값 축출
      (if (> (abs cy) maxcy) (setq maxcy (abs cy)))     ;최대y값찾기
      (setq count_n (1+ count_n))                       ;다음 node로
    ) ;of repeat
    (setq count_c (1+ count_c))                         ;다음 camber로
  ) ;of repeat
  (setq yscale (/ xmax 10 maxcy))                       ;y-scale = x길이의 1/10

  ;----- camber도 그리기
  (setq draw_cnt 0)
  (repeat ndraw                                  ;draw갯수만큼 반복
    (setq ncmb (length (nth draw_cnt drw)))      ;camber도에 들어갈 case수

    ;---- 수직선의 y좌표 구하기(절대값 y max)
    (setq maxy_lst nil)                             ; max-Y list 초기화
    (setq n_node 0)                                 ;첫 node부터
    (repeat nnum                                    ;case수만큼 반복
      (setq n (1- (atoi (nth 0 (nth draw_cnt drw)))))
      (setq maxy (nth n_node (nth n yc)))           ;첫번째 case가 최대값
      (setq n_case 0)                               ;둘째 case부터
      (repeat ncmb
        (setq cn (1- (atoi (nth n_case (nth draw_cnt drw))))) ;case번호
        (setq n_y (nth n_node (nth cn yc)))         ;n번재 node의 y값
        (if (> (abs n_y) (abs maxy))                ;현재 max보다 클때
          (setq maxy n_y)
        ) ;of if
        (setq n_case (1+ n_case))                  ;다음 case로
      ) ;of repeat                                 ;maxy list에 추가
      (setq maxy_lst (append maxy_lst (list maxy)))
      (setq n_node (1+ n_node))
    ) ;of repeat

    ;---- 밑줄 그리기
    (setq camby (+ 0.3 (* (/ 0.7 ndraw) (- ndraw (1+ draw_cnt)))))      ;camber도 y위치

    (setq firstx (list (+ (* xscale xmin) gap) (* bdry camby)))  ;첫 x점
    (setq lastx (list (+ (* xscale xmax) gap) (* bdry camby)))   ;마지막 x점
    (command "LINE" firstx lastx "")                 ;base line그리기

    ;---- node번호/pier/수직선그리기
    (setq count 0)                                  ;첫점부터..
    (repeat nnum                                    ;마지막 점까지..
      (setq nd (nth count node))                    ;node축출
      (setq xx (nth count x))

      (setq bpnt (list (+ (* xx xscale) gap) (* bdry camby)))  ;base point

      (if (= (substr nd 1 1) "+")               ;node에 fix단표시 있을때
        (progn
          (command "LINE" bpnt "@3<-60" "@3<180" "C")   ;fix단 표시(삼각형)
          (setq nd (substr nd 2))                   ;fix단 표시(+)제거
          (setq ap (nth 0 (reverse (nth count nlist)))) ;Abut/Pier번호 읽음
        )
        (if (= (substr nd 1 1) "-")            ;node에 move단표시 있을때
          (progn
            (command "LINE" bpnt "@3<-60" "@3<180" "C") ;move단 표시(삼각형)
            (setq b1 (list (- (+ (* xx xscale) gap) 1.5) ;밑줄 시작점
                           (- (* bdry camby) 3)))
            (setq b2 (list (+ (+ (* xx xscale) gap) 1.5) ;밑줄 끝점
                           (- (* bdry camby) 3)))
            (command "LINE" b1 b2 "")                   ;밑줄 그림
            (setq nd (substr nd 2))                 ;move단 표시(-)제거
            (setq ap (nth 0 (reverse (nth count nlist)))) ;Abut/Pier번호 읽음
          ) ;of progn
        );of if
      ) ;of if

      (if (/= ap nil)                                     ;Abut/Pier일 경우
        (progn
          (setq cp (list (car bpnt) (- (cadr bpnt) 12)))  ;원의 중심
          (command "CIRCLE" cp "4.0")                     ;원그리기
          (command "TEXT" "M" cp 3.0 0.0 ap)              ;text쓰기
          (setq ap nil)
        ) ;of progn
      ) ;of if

      (setq nd (atoi nd))                     ;node를 정수로
      (setq nd (itoa nd))                     ;node를 문자열로(공백제거)
      (setq txtpnt (list (+ (* xx xscale) gap) (- (* bdry camby) 5))) ;text pnt
      (command "TEXT" "M" txtpnt th "0" nd)     ;node 번호적기

      (setq count (1+ count))
    ) ;of repeat
                                     ;다음 node로
    ;----- 수직선그리기
    (setvar "CECOLOR" "1")
    (setq count 0)                                  ;첫점부터..
    (repeat nnum                                    ;마지막 점까지..
      (setq xx (nth count x))
      (setq bpnt (list (+ (* xx xscale) gap) (* bdry camby)))  ;base point
      (setq yy (+ (* (nth count maxy_lst) yscale -1 xscale) (* bdry camby)))
      (command "LINE" bpnt
                      (list (car bpnt) yy) "")

      (setq count (1+ count))
    ) ;of repeat                                      ;다음 node로
    (setvar "CECOLOR" "BYLAYER")

    ;---- camber그리기
    (setq oldc (getvar "CECOLOR"))                    ;현재색 대피
    (setq cmb_cnt 0)
    (repeat ncmb
      (setq c_count (1- (atoi (nth cmb_cnt (nth draw_cnt drw)))))
      (setvar "CECOLOR" (itoa (1+ c_count)))               ;색 지정
      (setq n_count 0)
      (command "PLINE")
      (repeat nnum
        (setq xx (nth n_count x))
        (setq yy (nth n_count (nth c_count yc)))
        (setq pnt (list (+ (* xx xscale) gap)                         ;camber 점
                        (+ (* yy yscale -1 xscale) (* bdry camby))))

        (command pnt)                                             ;camber선 그리기
        (setq n_count (1+ n_count))
      ) ;of repeat
      (command "")
      (setq cmb_cnt (1+ cmb_cnt))
    ) ;of repeat                                            ;다음 camber로
    (setvar "CECOLOR" oldc)                                 ;옛색 돌리기
    (setq draw_cnt (1+ draw_cnt))
  ) ;of repeat                                              ;다음 draw로

  ;------ table그리기
  (setq table (append (list x) yc))
  (setq  ncamb (1+ n_camber))
  (setq ntbl (+ (/ (* (1+ nnum) cw) (- bdrx (* gap 2))) 1))  ;table갯수
  (setq tblh (* rh (+ 2 ncamb)))                             ;table 높이

  (setq maxn (/ (- bdrx (* gap 2)) cw))         ;한줄 최대 node수

  (setq tcount 0)                               ;table count
  (repeat ntbl
    (setq node1 (1+ (* tcount maxn)))           ;시작 node
    (setq node2 (* (1+ tcount) maxn))           ;끝 node
    (if (> node2 nnum)                          ;끝 node가 마지막 node보다 크면
      (setq node2 nnum))

    (setq tbly (/ camby 2.0))                   ;camber table y위치
    (setq tblpx (- gap fcw)
          tblpy (- (* bdry tbly) (* tcount (+ tblh cw))))   ;table의 삽입위치

    (camb_table tblpx tblpy node1 node2 table)              ;table그리기

    (setq tcount (1+ tcount))
  ) ;of repeat

;  (setq *error* oer seterr nil)
  (princ)
) ;;of defun



;*******************************************************************
;     Function : STRLOC
;                get STRing LOCation
;                Jong-Suk Yi
;                1995. 2. 8
;******************************************************************
; 이 함수는 ,로 불리된 data를 나누어 한개의 list에 묶어준다.
; 이때 형변환 없이 모든 data는 문자열로 return된다.
;******************************************************************
(defun STRLOC(arg1
/             srt    strl    count    num    strt    nchr    subs
              lst    rslt
)
;(setq oer *error* *error* seterr)   ;Store AutoLISP error routine
   (setq str arg1)                              ;넘어온 문자열
   (setq strl (strlen arg1))                    ;넘어온 문자열의 길이
   (setq count 1)
   (setq num 1)
   (setq strt 1)                                ;추출시작 위치
   (setq nchr 1)                                ;추출문자 갯수
   (repeat (+ strl 1)
      (setq subs (substr str count 1))          ;문자 한개
      (if (or (= subs ",") (= subs ""))         ;현재 문자가 ,이거나 끝일때
         (progn
            (setq lst (substr str strt (- nchr 1)))    ;시작위치부터
            (if (= rslt nil)
               (setq rslt (list lst))                  ;돌림값이 비었을때
               (setq rslt (append rslt (list lst)))    ;돌림값에다 추가
            ) ;of if
            (setq nchr 0)                       ;추출갯수 다시 0으로
            (setq strt (1+ count))              ;다음 추출시작을 다음문자로
         ) ;of progn
         nil
      ) ;of if
      (setq count (1+ count))                   ;다음 문자로
      (setq num (1+ num))                       ;
      (setq nchr (1+ nchr))                     ;문자 갯수 한개 증가
   ) ;of repeat
   (setq arg1 rslt)                             ;돌림값 돌림
;(setq *error* oer seterr nil)                  ; Restore previous error handler
) ;of defun STRLOC


;****************************************************
; Function : CAMB_TABLE
;            CAMBer TABLE
;            Yi Suk-Jong
;            1996/2/14
;****************************************************
; 이 함수는 주어진 data로 솟음도 Table만들어준다
;  받는 값은
;      ax : table의 좌측상단의 x위치
;      ay : table의 좌측상단의 y위치
;  anode1 : 시작 node번호
;  anode2 : 끝 node번호
;  anlist : 입력된 data list
;****************************************************

(defun CAMB_TABLE(ax ay anode1 anode2 anlist
/                 ipnt     noden     xl      oldc     hl       y        st_pnt
                  end_pnt  ey        f_sp    f_ep     vl       nx       sp
                  ep       nn        cc      nx       ny       nxy      nd
                  ncx      nc        ncy     ncoord   nctxt    ncxy
)

  (setq noden (+ 1 (- anode2 anode1)))              ;node 갯수
  (setq xl (+ fcw (* noden cw)))                    ;x길이

  (setq oldc (getvar "CECOLOR"))                    ;현재 색 대피
  (setvar "CECOLOR" "RED")                          ;table의 색을 빨간색으로


  ;----- 수평선 그리기
  (setq hl 0)                                       ;수평선 count

  (repeat (+ ncamb 2)                               ;수평선그리기
    (setq y (- ay (* hl rh)))                       ;수평선 y값
    (setq st_pnt (list ax y))                       ;수평선 시작점
    (setq end_pnt (list (+ ax xl) y))               ;수평선 끝점
    (command "LINE" st_pnt end_pnt "")              ;수평선 그리기
    (setq hl (1+ hl))                               ;다음 수평선
  ) ;of repeat

  ;----- 수직선 그리기
  (setq ey (- ay (* (+ ncamb 1) 7)))                ;수직선의 끝점 y값
  (setq f_sp (list ax ay))                          ;첫번째 start point
  (setq f_ep (list ax ey))                          ;첫번째 end point
  (command "LINE" f_sp f_ep "")                     ;첫번째 수직선 그리기

  (setq vl 0)                                       ;수직선 count

  (repeat (1+ noden)                                ;node갯수 만큼 반복
    (setq nx (+ ax fcw (* vl cw)))                  ;n번째의 X좌표
    (setq sp (list nx ay)                           ;시작점
          ep (list nx ey))                          ;끝점
    (command "LINE" sp ep "")                       ;수직선 그리기
    (setq vl (1+ vl))                               ;다음 수직선으로
  ) ;of repeat

  (setvar "CECOLOR" oldc)                           ;옛색 되돌리기

  ;----- node/x/camber적기
  ;----- node번호 적기
  (setq nn anode1)                                  ;node count
  (setq cc 1)                                       ;column count

  (repeat noden                                     ;node갯수 만큼 반복
    (setq nx (+ ax fcw (/ cw 2.0) (* (1- cc) cw)))  ;n번째 node의 x좌표
    (setq ny (- ay (/ rh 2.0)))                     ;node의 y좌표
    (setq nxy (list nx ny))                         ;node의 xy좌표
    (setq nd (nth (1- nn) node))                ;node번호 입력값
    (if (or (= (substr nd 1 1) "-") (= (substr nd 1 1) "+"))  ;Abut/Pier인경우
      (setq nd (itoa (atoi (substr nd 2))))         ;첫문자(+/-)와 공백 제거
      (setq nd (itoa (atoi nd)))                    ;공백 제거
    ) ;of if
    (command "TEXT" "M" nxy th 0.0 nd)             ;node번호 적기

    ;----- X,camber 적기
    (setq ncx (+ ax fcw cw (- 0.0 trg) (* (1- cc) cw)))   ;n번째 좌표text의 y좌표

    (setq nc 0)                                           ;첫번째 camber부터

    (repeat (1+ n_camber)                                 ;camber수만큼 반복
      (setq ncy (- ny (* rh (1+ nc))))                    ;camber text의 y좌표
      (setq ncoord (nth (1- nn) (nth nc anlist)))   ;camber량(입력값,실수)
      (if (= nc 0)
        (setq nctxt (rtos ncoord 2 3))
;        (setq nctxt (rtos (* yfac ncoord -1) 2 3))          ;camber량 text
        (setq nctxt (rtos (* yfac ncoord -1) 2 vprec))          ;camber량 text
      ) ;of if
      (setq ncxy (list ncx ncy))                          ;text의 insert point
      (command "TEXT" "MR" ncxy th 0.0 nctxt)            ;text쓰기
      (setq nc (1+ nc))                                   ;다음 camber로
    ) ;of repeat

    (setq nn (1+ nn))                                     ;다음 node로
    (setq cc (1+ cc))                                     ;다음 column으로
  ) ;of repeat
) ;of defun


;---------------------------------------------
; Function : OP
;            OPeration
;            Yi Suk Jong
;            97/6/23
;---------------------------------------------
; 사용자가 입력한 연산로를 연산을 수행한다.
; 입력: eq  : 연산식이다. (현재 +,-만 가능)
;       lst : 연산될 값들, list로 되어있다.
; ex) (op "1+2" '((1 2) (2 3)))
;    --> (3 5)
;---------------------------------------------

(defun op(eq lst /
lst eq neq nlop lop oval count c val n cnt )
  (setq neq (strlen eq))                      ;식의 길이
  (setq nlst (length (nth 0 lst)))            ;list개수
  (setq nlop 0)                               ;전연산자의 위치
  (setq lop "+")                              ;전연산자
  (setq oval nil)                             ;전계산값

  (setq count 1)                                        ;첫번 글자부터
  (repeat (1+ neq)                                      ;마지막 글자까지
    (setq c (substr eq count 1))                        ;한글자 찝어내기
    (if (or (= c "+") (= c "-") (= count (1+ neq)))     ; +,-이거나 끝일때
      (if (= nlop 0)                                    ; 처음이면
        (setq n (atoi (substr eq (1+ nlop) (- count nlop 1)))
              oval (nth (1- n) lst)                          ;첫값들을 최종값으로
              nlop count                                ;전연산자위치
              lop c)                                    ;전연산자
        (progn                                          ;두번째부터이면
          (setq val nil)                                ;최종값을 초기화
          (setq n (atoi (substr eq (1+ nlop) (- count nlop 1))))   ;전연산자에서 현연산자까지=숫자
          (cond
            ((= lop "+")                                ;+일때 +수행
              (setq cnt 0)                              ;첫번 값부터
              (repeat nlst
                (setq val (append val (list (+ (nth cnt oval)
                                         (nth cnt (nth (1- n) lst))))))
                (setq cnt (1+ cnt))
              ) ;of repeat
              (setq oval val)
            ) ;of sub-cond
            ((= lop "-")                                ;-일때 -수행
              (setq cnt 0)
              (repeat nlst
                (setq val (append val (list (- (nth cnt oval)
                                         (nth cnt (nth (1- n) lst))))))
                (setq cnt (1+ cnt))
              ) ;of repeat
              (setq oval val)                           ;현재 값을 최종값으로
            ) ;of sub-cond
          ) ;of cond
          (setq nlop count                              ;현재 위치를 전연산자위치로
                lop c)                                  ;현재 연산자를 전 연산자로
        ) ;of progn
      ) ;of if
    ) ;of if
    (setq count (1+ count))                             ;다음 글자로
  ); of repeat
  val
) ;of defun
