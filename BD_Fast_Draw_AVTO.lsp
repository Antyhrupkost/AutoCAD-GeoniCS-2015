;;; ======================================================================
;;; BD_Fast_Draw_AVTO (2D) для GeoniCS 2015 – привязка по префиксу кода
;;; Фильтр: точки с кодом, начинающимся с начального кода
;;; Команда: BD_Fast_draw_AVTO
;;; ======================================================================

(vl-load-com)

;; ------------------------------------------------------------
;;  СПИСОК СТИЛЕЙ (точное совпадение кода)
;; ------------------------------------------------------------
(setq BD-*STYLES*
  '( 
     ("derzb" "22_Ограждения" "475_1" nil)      ; заборы деревянные сплошные
     ("prf"   "22_Ограждения" "474_1b" nil)     ; ограды металл. на мет. круглых столбах Н более 1 м
     ("mzb"   "22_Ограждения" "474_2b" nil)     ; Ограды металл. на мет. круглых столбах Н менее 1 м
     ("sht"   "22_Ограждения" "475_2" nil)      ; штакетник 
     ("rab"   "22_Ограждения" "476_3" nil)      ; рабица 
     ("kamn"  "32_Территория" "366" nil)        ; камни 
     ("fund"  "21_Здания_строения" "22_2" nil)  ; фундамент строящихся зданий
     ("bet"   "41_Дорожная_сеть" "189_2" nil)   ; бетон 
     ("bet1"  "41_Дорожная_сеть" "189_2" nil)   ; бетон 
     ("pgs"   "41_Дорожная_сеть" "189_2" nil)   ; пгс 
     ("gr"    "41_Дорожная_сеть" "193_2" nil)   ; грунтовка 
     ("a"     "41_Дорожная_сеть" "189_2" nil)   ; асфальт 
     ("les"   "51_Растительность" "366" nil)    ; контур леса
     ("les1"  "51_Растительность" "366" nil)    ; контур леса
     ("prsl"  "51_Растительность" "366" nil)    ; поросоль 
     ("prsl1" "51_Растительность" "366" nil)    ; поросоль 
     ("vkan"  "51_Растительность" "237_248" nil); ручьи, каналы и канавы 
     ;; ("shtrab" "22_Ограждения" "475_2" nil)  ; можно добавить отдельный стиль для подкода
   )
)

(defun BD_Get_Style_For_Code (code / entry)
  (setq entry (assoc code BD-*STYLES*))
  (if entry (list (cadr entry) (caddr entry) (cadddr entry)) nil)
)

;; ------------------------------------------------------------
;;  Команда
;; ------------------------------------------------------------
(defun c:BD_Fast_draw_AVTO ()
  (princ "\n=== BD_Fast_Draw_AVTO (2D полилиния) ===")
  (BD_Draw2D)
)

;; ----------------------------------------------------------------------
;; Вспомогательные функции
;; ----------------------------------------------------------------------
(defun BD_Is_Valid_Point (obj)
  (and (listp obj) (>= (length obj) 2) (numberp (car obj)) (numberp (cadr obj)))
)

(defun BD_Get_Code_At_Point (pt / ss ent obj code)
  (setq ss (ssget "_C"
                  (list (- (car pt) 0.01) (- (cadr pt) 0.01) 0.0)
                  (list (+ (car pt) 0.01) (+ (cadr pt) 0.01) 0.0)
                  '((0 . "GCDBPOINT"))))
  (if ss
    (progn
      (setq ent (ssname ss 0)
            obj (vlax-ename->vla-object ent))
      (if (vlax-property-available-p obj 'Code1)
        (progn
          (setq code (vlax-get-property obj 'Code1))
          (if (and code (not (eq code ""))) code nil)
        )
        nil
      )
    )
    nil
  )
)

;; ----------------------------------------------------------------------
;; Поиск ближайшей точки, у которой код начинается с filter-code (префикс)
;; ----------------------------------------------------------------------
(defun BD_Get_Real_Pos_2D (pos radius filter-code / ss i ent pt_ent obj code min_dist best_pt)
  (if (not (BD_Is_Valid_Point pos)) (setq pos (list 0.0 0.0 0.0)))
  (setq ss (ssget "_C"
                  (list (- (car pos) radius) (- (cadr pos) radius) 0.0)
                  (list (+ (car pos) radius) (+ (cadr pos) radius) 0.0)
                  '((0 . "POINT,GCDBPOINT"))))
  (if ss
    (progn
      (setq min_dist nil best_pt nil i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i)
              pt_ent (cdr (assoc 10 (entget ent))))
        (if (BD_Is_Valid_Point pt_ent)
          (progn
            (setq code nil)
            (if filter-code
              (progn
                (if (= (cdr (assoc 0 (entget ent))) "GCDBPOINT")
                  (progn
                    (setq obj (vlax-ename->vla-object ent))
                    (if (vlax-property-available-p obj 'Code1)
                      (setq code (vlax-get-property obj 'Code1))
                    )
                  )
                )
                ;; Проверяем, начинается ли код с filter-code
                (if (and code (wcmatch code (strcat "*" filter-code "*")))
                  (progn
                    (setq dist (distance (list (car pos) (cadr pos) 0.0)
                                         (list (car pt_ent) (cadr pt_ent) 0.0)))
                    (if (or (null min_dist) (< dist min_dist))
                      (setq min_dist dist best_pt pt_ent)
                    )
                  )
                )
              )
              ;; если filter-code nil – притягиваемся ко всем
              (progn
                (setq dist (distance (list (car pos) (cadr pos) 0.0)
                                     (list (car pt_ent) (cadr pt_ent) 0.0)))
                (if (or (null min_dist) (< dist min_dist))
                  (setq min_dist dist best_pt pt_ent)
                )
              )
            )
          )
        )
        (setq i (1+ i))
      )
      (if (and best_pt (<= min_dist radius))
        (list (car best_pt) (cadr best_pt) 0.0)
        pos
      )
    )
    pos
  )
)

;; ----------------------------------------------------------------------
;; Отрисовка предпросмотра
;; ----------------------------------------------------------------------
(defun BD_Draw_Preview_2D (pt_list last_pt cursor_pos / i p1 p2)
  (redraw)
  (if (> (length pt_list) 1)
    (progn
      (setq i 0)
      (repeat (1- (length pt_list))
        (setq p1 (nth i pt_list) p2 (nth (1+ i) pt_list))
        (if (and (BD_Is_Valid_Point p1) (BD_Is_Valid_Point p2))
          (grdraw (list (car p1) (cadr p1)) (list (car p2) (cadr p2)) 7 0)
        )
        (setq i (1+ i))
      )
    )
  )
  (if (and (BD_Is_Valid_Point last_pt) (BD_Is_Valid_Point cursor_pos))
    (grdraw (list (car last_pt) (cadr last_pt)) (list (car cursor_pos) (cadr cursor_pos)) 3 1)
  )
)

;; ----------------------------------------------------------------------
;; Создание полилинии со стилем
;; ----------------------------------------------------------------------
(defun BD_Create_Polyline_2D (pt_list style / layer linetype color dxf_list)
  (setq pt_list (vl-remove-if-not 'BD_Is_Valid_Point pt_list))
  (if (> (length pt_list) 1)
    (progn
      (setq dxf_list
        (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
              (cons 90 (length pt_list)) '(70 . 0)))
      (if style
        (progn
          (setq layer (nth 0 style) linetype (nth 1 style) color (nth 2 style))
          (if layer (setq dxf_list (append dxf_list (list (cons 8 layer)))))
          (if linetype (setq dxf_list (append dxf_list (list (cons 6 linetype)))))
          (if color (setq dxf_list (append dxf_list (list (cons 62 color)))))
        )
      )
      (setq dxf_list (append dxf_list (mapcar '(lambda (pt) (list 10 (car pt) (cadr pt))) pt_list)))
      (entmakex dxf_list)
    )
  )
)

;; ----------------------------------------------------------------------
;; Основная функция рисования
;; ----------------------------------------------------------------------
(defun BD_Draw2D ( / pt_start radius pt_list last_pt
                  cursor_pos done gr_code gr_data
                  filter_code style)
  (setq pt_start (getpoint "\nУкажите начальную точку: "))
  (if (null pt_start) (progn (princ "\n* Отменено *") (exit)))

  (setq filter_code (BD_Get_Code_At_Point pt_start))
  (if filter_code
    (princ (strcat "\nФильтр по префиксу кода: " filter_code " (все коды, начинающиеся с " filter_code ")"))
    (princ "\nФильтр отключён (привязка ко всем точкам)")
  )

  (setq style (BD_Get_Style_For_Code filter_code))
  (if style
    (progn
      (princ (strcat "\nПрименён стиль (точное совпадение кода): слой=" (nth 0 style)
                     (if (nth 1 style) (strcat ", тип линии=" (nth 1 style)) "")
                     (if (nth 2 style) (strcat ", цвет=" (itoa (nth 2 style))) "")))
    )
    (princ "\nСтиль для кода не найден, используются текущие настройки")
  )

  (setq radius (getdist pt_start "\nЗадайте радиус автопривязки: "))
  (if (null radius) (progn (princ "\n* Отменено *") (exit)))
  (if (<= radius 0) (setq radius 0.001))

  (setq pt_list (list pt_start)
        last_pt pt_start
        done nil)

  (princ (strcat "\nРадиус: " (rtos radius 2 2)
                 ". Наводите на точку, ЛКМ – зафиксировать, ПКМ – отменить, Enter/Esc – выход"))

  (while (not done)
    (setq gr_data (grread T 0)
          gr_code (car gr_data)
          cursor_pos (cadr gr_data))

    (cond
      ((= gr_code 5) ; движение
       (if (BD_Is_Valid_Point cursor_pos)
         (progn
           (setq cursor_pos (BD_Get_Real_Pos_2D cursor_pos radius filter_code))
           (BD_Draw_Preview_2D pt_list last_pt cursor_pos)
         )
         (BD_Draw_Preview_2D pt_list last_pt cursor_pos)
       )
      )
      ((= gr_code 3) ; ЛКМ
       (if (BD_Is_Valid_Point cursor_pos)
         (progn
           (setq cursor_pos (BD_Get_Real_Pos_2D cursor_pos radius filter_code))
           (if (and (BD_Is_Valid_Point cursor_pos)
                    (not (equal cursor_pos last_pt 0.001)))
             (progn
               (setq pt_list (append pt_list (list cursor_pos))
                     last_pt cursor_pos)
               (princ (strcat "\nТочка " (itoa (length pt_list)) ": "
                              (rtos (car cursor_pos) 2 3) ", "
                              (rtos (cadr cursor_pos) 2 3)))
               (BD_Draw_Preview_2D pt_list last_pt nil)
             )
             (setq done T) ; если привязки нет или точка та же – завершить
           )
         )
         (setq done T) ; клик вне точек – завершить
       )
      )
      ((= gr_code 25) ; ПКМ
       (if (> (length pt_list) 1)
         (progn
           (setq pt_list (reverse (cdr (reverse pt_list)))
                 last_pt (last pt_list))
           (princ "\n* Сегмент отменён *")
           (BD_Draw_Preview_2D pt_list last_pt nil)
         )
         (princ "\n* Нечего отменять *")
       )
      )
      ((= gr_code 2)
       (if (or (= cursor_pos 13) (= cursor_pos 32) (= cursor_pos 27))
         (setq done T)
       )
      )
    )
  )

  (redraw)
  (setq pt_list (vl-remove-if-not 'BD_Is_Valid_Point pt_list))
  (if (> (length pt_list) 1)
    (progn
      (BD_Create_Polyline_2D pt_list style)
      (princ (strcat "\nПостроено " (itoa (length pt_list)) " точек."))
    )
    (princ "\n* Недостаточно точек для построения *")
  )
  (princ)
)

(princ "\nЗагружена команда: BD_Fast_draw_AVTO (фильтр по префиксу кода, подтверждение ЛКМ)")
(princ)
