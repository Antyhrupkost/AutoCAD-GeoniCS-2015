(defun c:ZD ( / p1 p2 p3 p4 sel side_pt ent c_pt dcl_id dcl_file file_path 
                type_str num_str style1 style2 p_near param pt_start pt_end 
                v_edge len_edge u_edge v_in inside_dir norm_ang text_base
                scr_start scr_end p_left_corner p_right_corner u_right
                tmp_txt minpt maxpt txt_w txt_h shift_back shift_in 
                old_osmode old_error vec_x vec_y len proj_len)
  (vl-load-com)
  
  ;; Сохраняем исходные параметры для защиты интерфейса
  (setq old_osmode (getvar "OSMODE"))
  (setq old_error *error*)

  ;; Глобальный обработчик ошибок для автоматического возврата привязок
  (defun *error* (msg)
    (setvar "OSMODE" old_osmode)
    (setq *error* old_error)
    (princ (strcat "\n[ZD] Работа скрипта прервана: " msg))
    (princ)
  )

  ;; Вспомогательная функция для определения фиксированной высоты стиля
  (defun get-style-height (style_name / style_data)
    (if (setq style_data (tblsearch "STYLE" style_name))
      (cdr (assoc 40 style_data))
      0.0
    )
  )

  ;; Вспомогательная функция для создания честного MTEXT
  (defun draw-mtext-safe (str pt height ang style lay col attachment / th_val)
    (setq th_val (get-style-height style))
    (if (= th_val 0.0) (setq th_val height))
    
    (entmake (list
               '(0 . "MTEXT")
               '(100 . "AcDbEntity")
               (cons 8 lay)
               (if col (cons 62 col) '(62 . 256))
               '(100 . "AcDbMText")
               (cons 10 pt)
               (cons 40 th_val)
               (cons 71 attachment)
               (cons 1 str)
               (cons 7 style)
               (cons 11 (list (cos ang) (sin ang) 0.0))
             ))
    (entlast)
  )

  ;; Проверка и создание слоя
  (if (not (tblsearch "LAYER" "21_Здания_строения"))
    (entmake (list
               '(0 . "LAYER")
               '(100 . "AcDbSymbolTableRecord")
               '(100 . "AcDbLayerTableRecord")
               '(2 . "21_Здания_строения")
               '(70 . 0)
               '(62 . 7)
               '(6 . "Continuous")
             ))
  )

  ;; Определение стилей текста
  (setq style1 "Р131")
  (if (not (tblsearch "STYLE" style1)) (setq style1 "P131"))
  (if (not (tblsearch "STYLE" style1)) (setq style1 (getvar "TEXTSTYLE")))

  (setq style2 "D431")
  (if (not (tblsearch "STYLE" style2)) (setq style2 (getvar "TEXTSTYLE")))

  ;; 2. ЗАПРОС ТОЧЕК С ОБНУЛЕНИЕМ ВЫСОТЫ Z ДЛЯ ЗАЩИТЫ ОТ СХЛОПЫВАНИЯ
  (setq p1 (getpoint "\nУкажите первую точку (базовая линия): "))
  ;; Если точка выбрана (даже с Z координатой геоточки), принудительно зануляем Z
  (if p1 (setq p1 (list (car p1) (cadr p1) 0.0)))
  
  (if p1 
    (progn
      (setq p2 (getpoint p1 "\nУкажите вторую точку (базовая линия): "))
      (if p2 (setq p2 (list (car p2) (cadr p2) 0.0)))
    )
  )
  
  (if (and p1 p2)
    (progn
      (setq p3 (getpoint p2 "\nУкажите третью точку (ширина): "))
      (if p3 (setq p3 (list (car p3) (cadr p3) 0.0)))
    )
  )
  
  (if (and p1 p2 p3)
    (progn
      ;; Отключаем привязки на время расчетов геометрии, чтобы исключить влияние
      (setvar "OSMODE" 0)

      ;; Чистый 2D тригонометрический расчет прямоугольника
      (setq vec_x (- (car p2) (car p1))
            vec_y (- (cadr p2) (cadr p1))
            len (distance p1 p2)
      )
      
      (if (equal len 0.0 0.001) (setq len 1.0))
      
      (setq u_x (/ vec_x len)
            u_y (/ vec_y len)
            v_x (- u_y)
            v_y u_x
            proj_len (+ (* (- (car p3) (car p1)) v_x) (* (- (cadr p3) (cadr p1)) v_y))
            p4 (list (+ (car p2) (* proj_len v_x)) (+ (cadr p2) (* proj_len v_y)) 0.0)
            p3 (list (+ (car p1) (* proj_len v_x)) (+ (cadr p1) (* proj_len v_y)) 0.0)
      )

      ;; Построение контура здания
      (entmake (list
                 '(0 . "LWPOLYLINE")
                 '(100 . "AcDbEntity")
                 '(8 . "21_Здания_строения")
                 '(100 . "AcDbPolyline")
                 '(90 . 4)
                 '(70 . 1)
                 (cons 10 (list (car p1) (cadr p1)))
                 (cons 10 (list (car p2) (cadr p2)))
                 (cons 10 (list (car p4) (cadr p4)))
                 (cons 10 (list (car p3) (cadr p3)))
               ))
      (setq ent (entlast))

      ;; Геометрический центр здания
      (setq c_pt (list (/ (+ (car p1) (car p4)) 2.0) (/ (+ (cadr p1) (cadr p4)) 2.0) 0.0))
      
      ;; Возвращаем привязки для корректной работы окон чертежа
      (setvar "OSMODE" old_osmode)

      ;; 3. Создание и вызов DCL окна
      (setq file_path (vl-filename-mktemp "zd_dialog.dcl")
            dcl_file (open file_path "w"))
      (write-line "zd_dialog : dialog { title = \"Данные здания\";" dcl_file)
      (write-line "  : edit_box { label = \"Тип сооружения:\"; key = \"eb_type\"; edit_width = 30; }" dcl_file)
      (write-line "  : edit_box { label = \"Номер дома:\"; key = \"eb_num\"; edit_width = 30; }" dcl_file)
      (write-line "  ok_cancel;" dcl_file)
      (write-line "}" dcl_file)
      (close dcl_file)

      (setq dcl_id (load_dialog file_path))
      (if (not (new_dialog "zd_dialog" dcl_id)) 
        (progn (setvar "OSMODE" old_osmode) (exit))
      )
      
      (action_tile "accept" "(setq type_str (get_tile \"eb_type\") num_str (get_tile \"eb_num\")) (done_dialog)")
      (action_tile "cancel" "(setq type_str nil num_str nil) (done_dialog)")
      (start_dialog)
      (unload_dialog dcl_id)
      (vl-file-delete file_path)

      ;; 4. Запрос выбора стороны прямоугольника (Курсор-квадрат)
      (setvar "OSMODE" 0)
      (setq sel (entsel "\nУкажите вдоль какой линии расположить подпись (выберите грань): "))
      (setvar "OSMODE" old_osmode)
      
      (if (and sel ent)
        (progn
          (setvar "OSMODE" 0)

          (setq side_pt (cadr sel)
                p_near (vlax-curve-getClosestPointTo ent side_pt)
                param (vlax-curve-getParamAtPoint ent p_near)
                pt_start (vlax-curve-getPointAtParam ent (fix param))
                pt_end (vlax-curve-getPointAtParam ent (1+ (fix param)))
          )
          
          ;; Экранная система координат для поиска правого угла на мониторе
          (setq scr_start (trans pt_start 0 2)
                scr_end (trans pt_end 0 2))
          
          (if (> (car scr_end) (car scr_start))
            (setq p_right_corner pt_end   p_left_corner pt_start)
            (setq p_right_corner pt_start p_left_corner pt_end)
          )

          ;; Рассчитываем базовые векторы от правого угла
          (setq v_edge (mapcar '- p_left_corner p_right_corner)
                len_edge (distance p_right_corner p_left_corner)
                u_right (mapcar '(lambda (x) (/ (- x) len_edge)) v_edge)
                v_in (list (- (cadr u_right)) (car u_right) 0.0)
          )

          ;; Строгий вектор вовнутрь здания
          (if (minusp (+ (* (- (car c_pt) (car p_right_corner)) (car v_in)) (* (- (cadr c_pt) (cadr p_right_corner)) (cadr v_in))))
            (setq inside_dir (mapcar '- v_in))
            (setq inside_dir v_in)
          )

          ;; Угол текста (всегда направлен слева направо)
          (setq norm_ang (angle '(0.0 0.0 0.0) u_right))
          (if (and (> norm_ang (/ pi 2.0)) (<= norm_ang (* pi 1.5)))
            (setq norm_ang (rem (+ norm_ang pi) (* 2.0 pi)))
          )

          ;; --- Создание текста "Тип сооружения" ---
          (if (and type_str (/= type_str ""))
            (draw-mtext-safe type_str c_pt 1.0 norm_ang style1 "21_Здания_строения" nil 5)
          )

          ;; --- Создание текста "Номер дома" ---
          (if (and num_str (/= num_str ""))
            (progn
              ;; Замеряем физические габариты строки vla-методом
              (setq tmp_txt (draw-mtext-safe num_str '(0.0 0.0 0.0) 1.0 norm_ang style2 "21_Здания_строения" 1 5))
              (vla-getBoundingBox (vlax-ename->vla-object tmp_txt) 'minpt 'maxpt)
              (setq minpt (vlax-safearray->list minpt)
                    maxpt (vlax-safearray->list maxpt)
                    txt_w (distance (list (car minpt) (cadr minpt) 0.0) (list (car maxpt) (cadr minpt) 0.0))
                    txt_h (distance (list (car minpt) (cadr minpt) 0.0) (list (car minpt) (cadr maxpt) 0.0))
              )
              (entdel tmp_txt)

              ;; Вычисляем идеальный отступ центра
              (setq shift_back (+ (/ txt_w 2.0) 0.2))
              (setq shift_in (+ (/ txt_h 2.0) 0.2))

              ;; Финальная координата центра MTEXT номера дома
              (setq text_base (list
                                (+ (car p_right_corner) (* (- shift_back) (car u_right)) (* shift_in (car inside_dir)))
                                (+ (cadr p_right_corner) (* (- shift_back) (cadr u_right)) (* shift_in (cadr inside_dir)))
                                0.0
                              ))

              ;; Создаем начисто MTEXT (Привязка 5 - MC)
              (draw-mtext-safe num_str text_base 1.0 norm_ang style2 "21_Здания_строения" 1 5)
            )
          )
        )
      )
    )
  )
  
  ;; Сброс обработчика ошибок и штатное возвращение привязок
  (setvar "OSMODE" old_osmode)
  (setq *error* old_error)
  (princ)
)

(princ "\nСкрипт ZD успешно загружен. Для запуска введите: ZD\n")
(princ)
