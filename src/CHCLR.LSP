;***************************************************************
; Program : CHCLR
;           CHange CoLoR
;           Yi Suk-Jong
;           1995/2/22
;***************************************************************
; 도면내의 특정색을 다른 색으로 바꾸어준다.
;***************************************************************

(defun C:CHCLR(
/               fn      opf     llist       ch      lnum        oclr
                lcount  f_list  ss
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

  (setq oer *error* *error* seterr)                         ;에러루틴 가동

    (setq fn (getfiled "INPUT DATA" "" "DAT" 0))            ;file name입력
    (setq opf (open fn "r"))                                ;file open

    (setq llist nil)                                        ;빈 node-list 만듬

;*** data file로부터 DATA입력

    (if opf                                                 ;file이 있는 경우
      (progn
         (while (setq ch (read-line opf))                   ;한줄을 읽는다
            (setq llist (append llist (list (strloc ch))))  ;Line list에 추가
         ) ;of while
      ) ;of progn
      (princ "\nFile not found")                            ;file이 없는 경우
    ) ;of if
    (close opf)                                             ;file close


    (setq lnum (length llist))                              ;입력 line의 갯수

    (setq lcount 0)                                         ;첫번째 line부터

    (repeat lnum                                            ;line갯수만큼 반복
      (setq oclr (atoi (nth 0 (nth lcount llist)))          ;바꾸기전색
            nclr (nth 1 (nth lcount llist)))                ;바꾼후색
      (setq f_list (list (cons 62 oclr)))   ;filter list
      (setq ss (ssget "X" f_list))                          ;entity선택
      (if (= ss nil)
        (princ "\nNot found!")                              ;entity 없을 때
        (command "CHPROP" ss "" "C" nclr "")                ;entity 있을 때
      ) ;of if
      (setq lcount (1+ lcount))                             ;다음 line으로
    ) ;of repeat

  (setq *error* oer seterr nil)
  (princ)
) ;of defun


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

