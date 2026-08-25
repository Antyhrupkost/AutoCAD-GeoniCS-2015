;; ================================================================
;;  Скрипт: Создание точек и файла для импорта геоточек
;;  + генерация сетки в шахматном порядке внутри контура
;;  + автоматическое удаление дубликатов точек (допуск 0.001 м)
;;  + удаление всех точек на слое "Point" и самого слоя после экспорта
;;  Шаг сетки: 50 м
;;  Команда: PREPARE_GEOPOINTS
;; ================================================================

(defun c:PREPARE_GEOPOINTS ( / ss i ent pt filepath file count
                              boundary coords poly
                              minX maxX minY maxY
                              step halfStep x y inside
                              answer point-list unique-points
                              ss-points layer-obj)
  (vl-load-com)
  (setvar "CMDECHO" 0)
  (princ "\n=== ЗАПУСК СКРИПТА (С ГЕНЕРАЦИЕЙ СЕТКИ, УДАЛЕНИЕМ ДУБЛИКАТОВ И ОЧИСТКОЙ) ===\n")

  ;; --- Вспомогательная функция: точка внутри полигона (алгоритм лучей) ---
  (defun point-in-polygon (pt poly / x y inside i j xi yi xj yj)
    (setq x (car pt)  y (cadr pt)
          inside nil
          i 0
          j (1- (length poly)))
    (while (< i (length poly))
      (setq xi (car (nth i poly)) yi (cadr (nth i poly))
            xj (car (nth j poly)) yj (cadr (nth j poly)))
      (if (and (not (equal (<= yi y) (<= yj y)))
               (< x (+ (/ (* (- xj xi) (- y yi)) (- yj yi)) xi)))
        (setq inside (not inside)))
      (setq j i
            i (1+ i)))
    inside
  )

  ;; --- Вспомогательная функция: удаление дубликатов (по X,Y с допуском tol) ---
  (defun unique-points (pts tol / sorted result prev)
    (if pts
      (progn
        (setq sorted (vl-sort pts
                       (function (lambda (a b)
                                   (if (equal (car a) (car b) tol)
                                     (< (cadr a) (cadr b))
                                     (< (car a) (car b))
                                   )
                                 )
                       )
              )
        )
        (setq result (list (car sorted))
              prev   (car sorted))
        (foreach p (cdr sorted)
          (if (or (null prev)
                  (not (and (equal (car p) (car prev) tol)
                            (equal (cadr p) (cadr prev) tol))))
            (progn
              (setq result (cons p result))
              (setq prev p)
            )
          )
        )
        (reverse result)
      )
      nil
    )
  )

  ;; --- 1. Создаём слой "Point" (зелёный) ---
  (if (not (tblsearch "LAYER" "Point"))
    (entmake (list (cons 0 "LAYER")
                   (cons 100 "AcDbSymbolTableRecord")
                   (cons 100 "AcDbLayerTableRecord")
                   (cons 2 "Point")
                   (cons 70 0)
                   (cons 62 3)
                   (cons 6 "Continuous")))
  )

  ;; --- 2. Выбираем ПОЛИЛИНИИ и БЛОКИ (существующие объекты) ---
  (setq ss (ssget "_X" '((0 . "LWPOLYLINE,INSERT"))))
  (if (not ss)
    (princ "\nНе найдено полилиний или блоков, но продолжим...")
  )

  ;; --- Инициализируем список для всех точек ---
  (setq point-list '())

  ;; --- Функция добавления точки в список ---
  (defun collect-point (pt)
    (setq point-list (cons pt point-list))
  )

  ;; --- 3. Обработка существующих объектов (полилинии и блоки) ---
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (entget (ssname ss i)))
        (setq objType (cdr (assoc 0 ent)))
        (cond
          ((= objType "LWPOLYLINE")
            (setq coords (vl-remove-if-not '(lambda (x) (= (car x) 10)) ent))
            (foreach item coords
              (collect-point (cdr item))
            )
          )
          ((= objType "INSERT")
            (setq pt (cdr (assoc 10 ent)))
            (if pt (collect-point pt))
          )
        )
        (setq i (1+ i))
      )
    )
  )

  ;; --- 4. Генерация сетки внутри контура (по запросу) ---
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

              (setq step 50.0)
              (setq halfStep (/ step 2))

              (princ (strcat "\nГенерация сетки с шагом " (rtos step 2 2) " м..."))

              ;; Первый ряд
              (setq x minX)
              (while (<= x maxX)
                (setq y minY)
                (while (<= y maxY)
                  (setq pt (list x y 0.0))
                  (if (point-in-polygon pt poly)
                    (collect-point pt)
                  )
                  (setq y (+ y step))
                )
                (setq x (+ x step))
              )

              ;; Второй ряд (смещённый на полшага)
              (setq x (+ minX halfStep))
              (while (<= x maxX)
                (setq y (+ minY halfStep))
                (while (<= y maxY)
                  (setq pt (list x y 0.0))
                  (if (point-in-polygon pt poly)
                    (collect-point pt)
                  )
                  (setq y (+ y step))
                )
                (setq x (+ x step))
              )

              (princ "\nСетка сгенерирована.")
            )
            (princ "\nОшибка: контур содержит менее 3 вершин.")
          )
        )
        (princ "\nОшибка: выбранный объект не является замкнутой полилинией.")
      )
    )
    (princ "\nГенерация сетки пропущена.")
  )

  ;; --- 5. Удаление дубликатов ---
  (princ (strcat "\nСобрано точек (включая дубликаты): " (itoa (length point-list))))
  (setq unique-points (unique-points point-list 0.001))
  (princ (strcat "\nУникальных точек: " (itoa (length unique-points))))

  ;; --- 6. Создание точек и запись в файл ---
  (setq filepath (strcat (getenv "USERPROFILE") "\\Desktop\\geopoints_import.txt"))
  (setq file (open filepath "w"))
  (if (not file)
    (progn
      (princ "\nОшибка: не удалось создать файл.")
      (setvar "CMDECHO" 1)
      (exit)
    )
  )

  (setq count 0)
  (foreach pt unique-points
    (entmake (list (cons 0 "POINT") (cons 8 "Point") (cons 10 pt)))
    (write-line (strcat (rtos (car pt) 2 3) " " (rtos (cadr pt) 2 3) " 0.000") file)
    (setq count (1+ count))
  )

  (close file)

  ;; --- 7. Итог по созданию ---
  (princ (strcat "\n? Всего создано обычных точек: " (itoa count) " (слой 'Point')"))
  (princ (strcat "\n? Подготовлен файл для импорта: " filepath))

  ;; --- 8. Удаление временных точек и слоя ---
  (princ "\n\nУдаление временных точек на слое 'Point'...")
  (setq ss-points (ssget "_X" '((8 . "Point"))))
  (if ss-points
    (progn
      (command "_.ERASE" ss-points "")
      (princ (strcat "Удалено точек: " (itoa (sslength ss-points))))
    )
    (princ "Точек для удаления не найдено.")
  )

  ;; Удаление слоя (с проверкой на пустоту)
  (princ "\nПопытка удалить слой 'Point'...")
  (if (tblsearch "LAYER" "Point")
    (progn
      ;; Проверяем, есть ли объекты на слое
      (if (not (ssget "_X" '((8 . "Point"))))
        (progn
          (setq layer-obj (vlax-ename->vla-object (tblobjname "LAYER" "Point")))
          (if layer-obj
            (progn
              (vla-delete layer-obj)
              (princ "\nСлой 'Point' успешно удалён.")
            )
            (princ "\nНе удалось получить объект слоя.")
          )
        )
        (princ "\nСлой 'Point' не пуст (содержит объекты), удаление отменено.")
      )
    )
    (princ "\nСлой 'Point' не существует.")
  )

  (princ "\n\n=== ДАЛЕЕ ВЫПОЛНИТЕ ИМПОРТ В GeoniCS ===")
  (princ "\n1. Откройте меню: ТОПОПЛАН > Рельеф > Геоточки > Импорт")
  (princ "\n2. Укажите путь к файлу, показанному выше.")
  (princ "\n3. В настройках формата выберите: X Y H, разделитель - пробел.")
  (princ "\n4. Нажмите ОК — все геоточки будут созданы за один раз.\n")

  (setvar "CMDECHO" 1)
  (princ)
)

(princ "\nЗагружено! Введите PREPARE_GEOPOINTS для запуска.")
(princ)
