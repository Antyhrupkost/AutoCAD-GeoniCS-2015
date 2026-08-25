(defun c:off_even_label ()
(setq ss (ssget "X" '((0 . "GCDBPOINT"))))
(if ss
    (progn
     (setq i 0)
     (setq count 0)
     (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))
        
        ; Читаем номер точки
        (setq num (vlax-get-property obj 'Number))
        (if (numberp num)
         (if (= (rem num 2) 0) ; чётный номер
            (progn
             ; Скрываем подпись (высоту)
             (vlax-put-property obj 'ShowElevation 0)
             (setq count (1+ count))
            )  ))
        (setq i (1+ i)))
     (princ (strcat "\nTurned OFF labels for " (itoa count) " points with EVEN numbers.")))
    (princ "\nNo GCDBPOINTs found."))
(princ))
