
(vl-load-com)

(defun R15plotters ()
  (cdr (vlax-safearray->list
        (vlax-variant-value
         (vlax-make-variant
          (vla-GetPlotDeviceNames
           (vla-get-ActiveLayout
            (vla-get-ActiveDocument
             (vlax-get-acad-object)
  )))))))
)
