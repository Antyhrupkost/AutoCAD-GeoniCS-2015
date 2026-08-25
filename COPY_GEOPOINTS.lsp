;; ================================================================
;;  Скрипт: Синтез оригинальных геоточек GeoniCS 2015 (Кружки)
;;  (100% фиксация графики + автонумерация точек от 1, 2, 3...)
;;  Шаг сетки: 50 м
;;  Новая команда для запуска: COPY_GEOPOINTS
;; ================================================================

(defun c:COPY_GEOPOINTS ( / ss i ent pt count
                             boundary coords poly
                             minX maxX minY maxY
                             step halfStep x y inside
                             answer point-list sample-pt sample-ent 
                             sample-z sample-layer sample-textstyle sample-desc old-osmode old-cmdecho)
  (vl-load-com)
  (setq old-cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  
  ;; Отключаем привязки на время генерации, чтобы точки не смещались к чужим узлам
  (setq old-osmode (getvar "OSMODE"))
  (setvar "OSMODE" 0)

  (princ "\n=== ЗАПУСК ПРЯМОГО DXF-СИНТЕЗА С АВТОНУМЕРАЦИЕЙ ===\n")

  ;; --- Вспомогательная функция: точка внутри полигона (алгоритм лучей) ---
  (defun point-in-polygon (pt poly / x y inside i j xi yi xj yj)
    (setq x (car pt)  y (cadr pt)
          inside nil i 0 j (1- (length poly)))
    (while (< i (length poly))
      (setq xi (car (nth i poly)) yi (cadr (nth i poly))
            xj (car (nth j poly)) yj (cadr (nth j poly)))
      (if (and (not (equal (<= yi y) (<= yj y)))
               (< x (+ (/ (* (- xj xi) (- y yi)) (- yj yi)) xi)))
        (setq inside (not inside)))
      (setq j i i (1+ i)))
    inside
  )

  ;; --- 1. Сбор координат с полилиний и блоков чертежа ---
  (setq ss (ssget "_X" '((0 . "LWPOLYLINE,INSERT"))))
  (setq point-list '())

  (defun collect-point (pt)
    (setq point-list (cons (list (car pt) (cadr pt) (if (caddr pt) (caddr pt) 0.0)) point-list))
  )

  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (entget (ssname ss i)))
        (setq objType (cdr (assoc 0 ent)))
        (cond
          ((= objType "LWPOLYLINE")
            (setq coords (vl-remove-if-not '(lambda (x) (= (car x) 10)) ent))
            (foreach item coords (collect-point (cdr item))))
          ((= objType "INSERT")
            (setq pt (cdr (assoc 10 ent)))
            ;; Защита: не берем координаты самого примера геоточки
            (if (and pt (not (= (cdr (assoc 0 ent)) "GCDBPOINT"))) 
              (collect-point pt))))
        (setq i (1+ i))))
  )

  ;; --- 2. ГЕНЕРАЦИЯ СЕТКИ ВНУТРИ КОНТУРА ---
  (princ "\nХотите добавить сетку точек внутри контура? (1 - Да, 2 - Нет): ")
  (setq answer (getstring))
  (if (or (= answer "1") (= answer "y") (= answer "Y") (= answer "д") (= answer "Д"))
    (progn
      (princ "\nВыберите замкнутую полилинию (контур съёмки): ")
      (setq boundary (car (entsel)))
      (if (and boundary
               (wcmatch (cdr (assoc 0 (entget boundary))) "LWPOLYLINE")
               (= (logand (cdr (assoc 70 (entget boundary))) 1) 1))
        (progn
          (setq coords (vl-remove-if-not '(lambda (x) (= (car x) 10)) (entget boundary)))
          (setq poly (mapcar 'cdr coords))
          (if (> (length poly) 2)
            (progn
              (setq minX (apply 'min (mapcar 'car poly))
                    maxX (apply 'max (mapcar 'car poly))
                    minY (apply 'min (mapcar 'cadr poly))
                    maxY (apply 'max (mapcar 'cadr poly)))
              (setq step 50.0 halfStep (/ step 2))
              (princ (strcat "\nГенерация сетки с шагом " (rtos step 2 2) " м..."))
              (setq x minX)
              (while (<= x maxX)
                (setq y minY)
                (while (<= y maxY)
                  (setq pt (list x y 0.0))
                  (if (point-in-polygon pt poly) (collect-point pt))
                  (setq y (+ y step)))
                (setq x (+ x step)))
              (setq x (+ minX halfStep))
              (while (<= x maxX)
                (setq y (+ minY halfStep))
                (while (<= y maxY)
                  (setq pt (list x y 0.0))
                  (if (point-in-polygon pt poly) (collect-point pt))
                  (setq y (+ y step)))
                (setq x (+ x step)))
              (princ "\nСетка сгенерирована."))
            (princ "\nОшибка: контур содержит менее 3 вершин."))
        )
        (princ "\nОшибка: выбранный объект не является замкнутой полилинией."))
    )
    (princ "\nГенерация сетки пропущена.")
  )

  ;; --- 3. Выбор эталона пользователем ---
  (if (= (length point-list) 0)
    (progn (princ "\n[Ошибка] На чертеже не найдено точек для расстановки!") (setvar "OSMODE" old-osmode) (exit))
  )
  
  (princ (strcat "\nСобрано координат для расстановки: " (itoa (length point-list))))
  (princ "\nВыберите вашу ПРАВИЛЬНУЮ геоточку GeoniCS (КРУЖОК-эталон): ")
  (setq sample-pt (car (entsel)))

  ;; --- 4. ПРЯМОЙ СИНТЕЗ ОРИГИНАЛЬНОЙ DXF-СТРУКТУРЫ С АВТОНУМЕРАЦИЕЙ ---
  (setq count 0)
  (if (and sample-pt (setq sample-ent (entget sample-pt)))
    (progn
      ;; Нативно считываем метаданные оформления из вашего идеального примера-кружка
      (setq sample-layer     (cdr (assoc 8 sample-ent)))     
      (setq sample-textstyle (cdr (assoc 7 sample-ent)))     
      (setq sample-desc      (cdr (assoc 1 sample-ent)))     
      (setq sample-z         (cadddr (assoc 10 sample-ent))) 

      (princ "\nПрямой синтез оригинальных геоточек GeoniCS...")
      (foreach pt point-list
        (if (entmake (list
                       '(0 . "GCDBPOINT")                   
                       '(100 . "AcDbEntity")
                       (cons 8 sample-layer)                
                       '(100 . "GcDbPoint")
                       '(70 . 4)                            
                       (cons 10 (list (car pt) (cadr pt) (if (> (caddr pt) 0.0) (caddr pt) sample-z))) 
                       '(90 . 327680)
                       '(40 . 0.6)                          
                       '(91 . 3)
                       (cons 11 (list (car pt) (cadr pt) (if (> (caddr pt) 0.0) (caddr pt) sample-z))) 
                       '(92 . 65538)
                       '(50 . 0.0)                          
                       (cons 93 (1+ count))                 ; Внутренний числовой номер (1, 2, 3...)
                       '(94 . -1023410174)                  
                       (cons 40 (if (> (caddr pt) 0.0) (caddr pt) sample-z)) 
                       '(94 . -1040187392)
                       (cons 1 sample-desc)                 
                       '(94 . -1023475712)
                       (cons 7 sample-textstyle)            
                       '(40 . 1.0)                          
                       '(210 0.0 0.0 1.0)
                       (cons 1 (itoa (1+ count)))           ; Имя геоточки на плане (строка "1", "2", "3"...)
                       '(94 . -1023410172)
                       '(1 . "")
                       '(1 . "")
                       '(1 . "")
                     ))
          (setq count (1+ count))
        )
      )
    )
    (princ "\nОшибка: Вы не выбрали геоточку-пример!")
  )

  ;; Возвращаем настройки экрана и объектных привязок
  (setvar "OSMODE" old-osmode)
  (setvar "CMDECHO" old-cmdecho)
  (command "_.REGEN")
  
  (princ (strcat "\n=== УСПЕХ! Программа напрямую воссоздала геоточек GeoniCS: " (itoa count) " ===\n"))
  (princ)
)

(princ "\nЗагружено! Введите COPY_GEOPOINTS для запуска.")
(princ)
