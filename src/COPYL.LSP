;****************************************
; Program : COPYL
;           COPY to current Layer
;           By Suk-Jong Yi
;           1995/5/27
;****************************************

;다른 레이어에 있는 엔티티를 현재의 레이어로 copy한다.

(defun C:COPYL(/                    ;지역변수 정의
clayer ents nent bpnt dpnt
count cent lent oldl newl
)

(push-env)                          ;환경변수 대피

(setq clayer (getvar "CLAYER"))     ;현재 레이어 알아냄
(setq ents (ssget))                 ;카피할 엔티티 선택
(setq nent (sslength ents))         ;선택된 엔티티 갯수
(setq bpnt (getpoint "\nBase point or displacement: "))        ;베이스포인트
(setq dpnt (getpoint bpnt "\nSecond point of displacement: ")) ;둘째포인트
(setq count 0)                                                 ; coun=0
(repeat nent                                               ;엔티티갯수만큼 반복
  (setq cent (ssname ents count))             ;카피될 엔티티
  (command "COPY" cent "" bpnt dpnt)          ;카피명령 실행
  (setq lent (entget (entlast)))              ;방금 카피된 엔티티
  (setq oldl (assoc 8 lent))                  ;방금카피된 엔티티의 layer
  (setq newl (cons 8 clayer))                 ;oldlayer를 대체할 layer
  (setq lent (subst newl oldl lent))          ;옛레이어를 현 레이어로 대체
  (entmod lent)                             ;대체된 엔티티 정보로 갱신
  (setq count (+ count 1))                  ;다음 엔티티로
) ;of repeat                                ;엔티티의 갯수만큼 반복

(pop-env)                           ;환경변수 복귀

) ;of defun

