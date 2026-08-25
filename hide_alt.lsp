(vl-load-com)

(defun c:hidealtcycle ( / ent obj )
  ;; Цикл продолжается, пока пользователь выбирает объекты
  (while (setq ent (entsel "\nВыберите геоточку для скрытия отметки (Выход - Пробел/Esc): "))
    (setq obj (vlax-ename->vla-object (car ent)))
    
    ;; Проверяем тип объекта
    (if (= (vla-get-objectname obj) "GcDbPoint")
      (progn
        ;; Скрываем высотную отметку
        (vlax-put-property obj 'ShowElevation 0)
        (vla-update obj)
      )
    )
  )
  (princ "\nЦиклическое скрытие завершено.")
  (princ)
)
