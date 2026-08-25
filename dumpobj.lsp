(defun c:dumpobj ()
(setq ent (car (entsel "\nВыберите GCDBPOINT: ")))
(if ent
    (progn
     (setq obj (vlax-ename->vla-object ent))
     (vlax-dump-object obj t)
     (princ "\n\nНажмите F2, чтобы увидеть полный список свойств.")
    )
)
(princ)
)
