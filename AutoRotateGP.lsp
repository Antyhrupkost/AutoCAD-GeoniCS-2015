 (vl-load-com)
;; Безопасное извлечение плановых геометрических параметров смежной линии
(defun GetGPLineAngle (entLine pt2D / ptVertex dist param startParam endParam ptStart ptEnd maxParam cRad cDeg result)
  (setq result nil)
  (if (not (vl-catch-all-error-p (vl-catch-all-apply 'vlax-curve-getEndParam (list entLine))))
    (progn
      (setq ptVertex (vl-catch-all-apply 'vlax-curve-getClosestPointTo (list entLine pt2D)))
      (if (and ptVertex (not (vl-catch-all-error-p ptVertex)))
        (progn
          (setq ptVertex (list (car ptVertex) (cadr ptVertex) 0.0))
          (if (< (distance pt2D ptVertex) 1.5) ;; Ищем строго в радиусе 1.5 метров
            (progn
              (setq param (vl-catch-all-apply 'vlax-curve-getParamAtPoint (list entLine ptVertex)))
              (if (and param (not (vl-catch-all-error-p param)))
                (progn
                  (setq startParam (fix param) endParam (1+ startParam) maxParam (vlax-curve-getEndParam entLine))
                  (if (>= endParam maxParam) (setq endParam startParam startParam (1- startParam)))
                  (setq ptStart (vl-catch-all-apply 'vlax-curve-getPointAtParam (list entLine startParam)))
                  (setq ptEnd (vl-catch-all-apply 'vlax-curve-getPointAtParam (list entLine endParam)))
                  
                  ;; Обход мусорных нулевых узлов-дубликатов чертежа
                  (if (and ptStart ptEnd (not (vl-catch-all-error-p ptStart)) (not (vl-catch-all-error-p ptEnd)) (equal ptStart ptEnd 1e-3))
                    (if (< endParam maxParam) (setq endParam (1+ endParam)) (setq startParam (1- startParam)))
                  )
                  (setq ptStart (vlax-curve-getPointAtParam entLine startParam) ptEnd (vlax-curve-getPointAtParam entLine endParam))
                  (if (or (not ptStart) (not ptEnd) (equal ptStart ptEnd 1e-3))
                    (setq ptStart (vlax-curve-getPointAtParam entLine 0) ptEnd (vlax-curve-getPointAtParam entLine maxParam))
                  )
                  (if (and ptStart ptEnd (not (equal ptStart ptEnd 1e-3)))
                    (progn
                      (setq cRad (angle (list (car ptStart) (cadr ptStart) 0.0) (list (car ptEnd) (cadr ptEnd) 0.0)))
                      (setq cDeg (* cRad (/ 180.0 pi)))
                      
                      ;; КАРТОГРАФИЧЕСКИЙ ФИЛЬТР: Корректируем перевернутый угол ДО его записи в GeoniCS
                      (while (< cDeg 0.0) (setq cDeg (+ cDeg 360.0)))
                      (while (>= cDeg 360.0) (setq cDeg (- cDeg 360.0)))
                      (if (and (> cDeg 90.01) (< cDeg 270.01))
                        (progn
                          (setq cDeg (+ cDeg 180.0))
                          (setq cRad (+ cRad pi))
                        )
                      )
                      (while (< cDeg 0.0) (setq cDeg (+ cDeg 360.0)))
                      (while (>= cDeg 360.0) (setq cDeg (- cDeg 360.0)))
                      (while (< cRad 0.0) (setq cRad (+ cRad (* 2.0 pi))))
                      (while (>= cRad (* 2.0 pi)) (setq cRad (- cRad (* 2.0 pi))))
                      
                      (setq result (list cDeg cRad ptVertex))
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
  result
)

;; Главная управляющая команда плагина
(defun c:AutoRotateGP ( / textOffset ssAll ssLines ssPoints i ent obj dxfData ptGeo pt2D dataRes bestAngle bestRad savedVertex countFound countRotated countSkipped totalPoints nextAnimTime animChars animIdx entType layerName firstChar shiftX shiftY ptShifted vX vY vLen)
  (setq oldcmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  
  ;; =========================================================================
  ;; НАСТРОЙКА ОТСТУПА: расстояние от линии до текста в метрах чертежа
  (setq textOffset 0.8) 
  ;; =========================================================================
  
  (princ (strcat "\n--- Тотальный автоматический расчет GeoniCS со сдвигом " (rtos textOffset 2 2) "м ---"))
  (setq ssAll (ssget "_A"))
  (setq countFound 0 countRotated 0 countSkipped 0)
  
  (if ssAll
    (progn
      ;; Создаем в оперативной памяти изолированные списки сущностей
      (setq ssPoints nil ssLines nil i 0)
      (while (< i (sslength ssAll))
        (setq ent (ssname ssAll i) dxfData (entget ent))
        (if dxfData
          (progn
            (setq entType (strcase (cdr (assoc 0 dxfData))) layerName (cdr (assoc 8 dxfData)))
            (if (= entType "GCDBPOINT")
              (setq ssPoints (cons ent ssPoints))
              (if (member entType '("LINE" "LWPOLYLINE" "POLYLINE" "3DPOLYLINE" "GCOBJECTPLANTRASS" "GCDBCONTOUR" "GCRELIEFLINE"))
                (progn
                  (setq firstChar (ascii (substr layerName 1 1)))
                  ;; Фильтруем только целевые слои GeoniCS (начинаются на цифры 1-9)
                  (if (and (>= firstChar 49) (<= firstChar 57)) (setq ssLines (cons ent ssLines)))
                )
              )
            )
          )
        )
        (setq i (1+ i))
      )
      
      (setq totalPoints (length ssPoints))
      (princ (strcat "\n-> Извлечено из памяти: геоточек = " (itoa totalPoints) ", целевых топо-линий = " (itoa (length ssLines))))
      (setq animChars '("|" "/" "-" "\\") animIdx 0 nextAnimTime (getvar "MILLISECS"))
      
      ;; Основной цикл выравнивания
      (foreach entGeo ssPoints
        (setq dxfData (entget entGeo) countFound (1+ countFound) obj (vlax-ename->vla-object entGeo))
        (setq ptGeo (cdr (assoc 10 dxfData)) pt2D (list (car ptGeo) (cadr ptGeo) 0.0))
        (setq bestAngle nil savedVertex nil minDist 1.5)
        
        (foreach entLine ssLines
          (setq dataRes (GetGPLineAngle entLine pt2D))
          (if dataRes
            (progn
              (setq dist (distance pt2D (caddr dataRes)))
              (if (< dist minDist)
                (setq bestAngle (car dataRes) bestRad (cadr dataRes) savedVertex (caddr dataRes) minDist dist)
              )
            )
          )
        )
        
        (if (and bestAngle savedVertex)
          (progn
            ;; 1. Записываем идеальный неперевернутый угол напрямую в свойство GeoniCS (в градусах)
            (vlax-put-property obj 'RotateText bestAngle)
            
            ;; 2. ВЫЧИСЛЕНИЕ НАПРАВЛЕНИЯ ОТСТУПА ПО ГЕОДЕЗИЧЕСКОМУ РЕГЛАМЕНТУ:
            (setq vX (- (car pt2D) (car savedVertex)) vY (- (cadr pt2D) (cadr savedVertex)))
            (setq vLen (sqrt (+ (* vX vX) (* vY vY))))
            
            ;; Если точка сидит идеально на линии (зазор равен 0), гарантированно выносим её ВВЕРХ (экранный сдвиг)
            (if (< vLen 1e-4)
              (progn
                (setq shiftX 0.0)
                (setq shiftY textOffset)
              )
              ;; РЕГЛАМЕНТ: Линия сверху (vY < 0) -> уводим текст ВНИЗ. Линия снизу (vY > 0) -> уводим ВВЕРХ.
              (progn
                (setq shiftX 0.0)
                (if (> vY 0.0)
                  (setq shiftY textOffset)   ;; Сдвигаем строго вверх по оси Y экрана
                  (setq shiftY (- textOffset)) ;; Сдвигаем строго вниз по оси Y экрана
                )
              )
            )
            
            ;; Смещаем весь объект геоточки на величину нормативного зазора
            (setq ptShifted (list (+ (car ptGeo) shiftX) (+ (cadr ptGeo) shiftY) (caddr ptGeo)))
            (vla-Move obj (vlax-3d-point ptGeo) (vlax-3d-point ptShifted))
            
            ;; Принудительно заставляем GeoniCS перерисовать внутреннюю графику точки по новым правилам
            (vlax-invoke-method obj 'Update)
            (setq countRotated (1+ countRotated))
          )
          (setq countSkipped (1+ countSkipped))
        )
        
        ;; Индикатор загрузки в реальном времени
        (if (>= (getvar "MILLISECS") nextAnimTime)
          (progn
            (princ (strcat "\rГеодезический расчет " (nth animIdx animChars) " Выровнено: " (itoa countFound) " из " (itoa totalPoints)))
            (setq animIdx (1+ animIdx)) (if (>= animIdx 4) (setq animIdx 0))
            (setq nextAnimTime (+ (getvar "MILLISECS") 50))
          )
        )
      )
      
      (princ (strcat "\n============================================="
                     "\n[ГЕОДЕЗИЧЕСКАЯ ОБРАБОТКА УСПЕШНО ЗАВЕРШЕНА]:"
                     "\n-> Всего геоточек обработано: " (itoa totalPoints) 
                     "\n-> Успешно сориентировано и смещено: " (itoa countRotated) 
                     "\n-> Пропущено (истинные одиночные точки): " (itoa countSkipped)
                     "\n============================================="))
    )
    (princ "\nНа чертеже объектов не обнаружено.")
  )
  (setvar "CMDECHO" oldcmdecho)
  (princ)
)

(princ "\nФинальный плагин GeoniCS загружен без ошибок. Команда: AutoRotateGP")
(princ)
