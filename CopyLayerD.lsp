(vl-load-com)

(defun c:CopyLayerD (/ acdoc layerName ss i ent obj copyobj minpt maxpt 
                      midPt textHeight gap mtextW
                      style1 style2 mtextObj mtextNum)
  (setq layerName "21_Здания_строения")

  ;; Создаём слой, если его нет
  (if (not (tblsearch "LAYER" layerName))
    (command "_.layer" "_make" layerName "")
  )

  (setq acdoc (vla-get-activedocument (vlax-get-acad-object)))

  ;; Выбираем все полилинии в слое "D"
  (setq ss (ssget "X" '((0 . "LWPOLYLINE,POLYLINE") (8 . "D"))))

  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))

        ;; Копируем полилинию
        (setq copyobj (vla-copy obj))
        (vla-put-layer copyobj layerName)
        (vla-put-color copyobj 7)   ; чёрный

        ;; Габаритный прямоугольник исходной полилинии
        (vla-GetBoundingBox obj 'minpt 'maxpt)
        (if (and minpt maxpt)
          (progn
            (setq textHeight 1.0)          ; высота текста
            (setq gap (* 0.5 textHeight))   ; расстояние между текстами

            ;; Координаты центра
            (setq midX (/ (+ (vlax-safearray-get-element minpt 0)
                             (vlax-safearray-get-element maxpt 0)) 2.0))
            (setq midY (/ (+ (vlax-safearray-get-element minpt 1)
                             (vlax-safearray-get-element maxpt 1)) 2.0))

            ;; Точка вставки для "Ж" (смещена влево на половину gap)
            (setq ptLeft (vlax-3d-point (list (- midX gap) midY 0.0)))
            ;; Точка вставки для "№" (смещена вправо на половину gap)
            (setq ptRight (vlax-3d-point (list (+ midX gap) midY 0.0)))

            (setq mtextW 0.0) ; однострочный MTEXT

            ;; ---- MTEXT "Ж" слева (стиль P131, чёрный) ----
            (setq mtextObj (vla-AddMText (vla-get-modelspace acdoc) ptLeft mtextW "Ж"))
            (vla-put-height mtextObj textHeight)
            (setq style1 "P131")
            (if (tblsearch "STYLE" style1)
              (vla-put-stylename mtextObj style1)
            )
            (vla-put-layer mtextObj layerName)
            (vla-put-color mtextObj 7)

            ;; ---- MTEXT "№" справа (стиль D431, КРАСНЫЙ) ----
            (setq mtextNum (vla-AddMText (vla-get-modelspace acdoc) ptRight mtextW "№"))
            (vla-put-height mtextNum textHeight)
            (setq style2 "D431")
            (if (tblsearch "STYLE" style2)
              (vla-put-stylename mtextNum style2)
            )
            (vla-put-layer mtextNum layerName)
            (vla-put-color mtextNum 1)   ; красный
          )
        )

        (setq i (1+ i))
      )
      (princ "\nЗдания_строения отрисованы")
    )
    (princ "\n?? Полилинии в слое 'D' не найдены.")
  )
  (princ)
)
