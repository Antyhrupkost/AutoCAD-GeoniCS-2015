;;; Итерация 1 — Построение Красного контура строго по геоточкам (GEO_POINTS)
;;; Команда: GRANYZA_RUN_FIRST

(defun c:GRANYZA_RUN_FIRST ( / layer_name ltype_name max_dist ss i ename obj_pt cx cy pts_r
                               layers_table layer_obj old_echo old_osm sublist p_start hull
                               next_found p_curr p_next val changed p1 p2 idx_next d_edge best_pt
                               min_score target_pt d1 d2 score vertex_list poly_data poly_r obj
                               area_val_r area_ha_r)
  (vl-load-com)
  
  (setq layer_name "150_Формуляр")
  (setq ltype_name "496")
  (setq max_dist 40.0)

  (setq layers_table (vla-get-Layers (vla-get-ActiveDocument (vlax-get-acad-object))))
  (if (vl-catch-all-error-p (setq layer_obj (vl-catch-all-apply 'vla-Item (list layers_table layer_name))))
    (setq layer_obj (vla-Add layers_table layer_name))
  )
  (vla-put-Color layer_obj 1)
  (vla-put-Plottable layer_obj :vlax-false)

  (setq old_echo (getvar "CMDECHO"))
  (setq old_osm (getvar "OSMODE"))
  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 0)

  (defun sublist (lst start end / res idx)
    (setq idx 0 res nil)
    (foreach item lst
      (if (and (>= idx start) (< idx end)) (setq res (cons item res)))
      (setq idx (1+ idx))
    )
    (reverse res)
  )

  (setq ss_r (ssget "X" '((0 . "GCDBPOINT") (8 . "GEO_POINTS"))))
  (if (not ss_r)
    (progn
      (princ "\n[Ошибка] Красный контур: Точки GeoniCS (GCDBPOINT) на слое 'GEO_POINTS' не найдены!")
      (setvar "CMDECHO" old_echo) (setvar "OSMODE" old_osm) (exit)
    )
  )

  (setq i 0 pts_r nil)
  (while (< i (sslength ss_r))
    (setq ename (ssname ss_r i))
    (setq obj_pt (vlax-ename->vla-object ename))
    (setq cx (vl-catch-all-apply 'vlax-get-property (list obj_pt 'CoordX)))
    (setq cy (vl-catch-all-apply 'vlax-get-property (list obj_pt 'CoordY)))
    (if (and (not (vl-catch-all-error-p cx)) (not (vl-catch-all-error-p cy)))
      (setq pts_r (cons (list cx cy) pts_r))
    )
    (setq i (1+ i))
  )
  
  (setq pts_r (vl-sort pts_r '(lambda (a b) (if (equal (car a) (car b) 1e-3) (< (cadr a) (cadr b)) (< (car a) (car b))))))
  (setq i 0) (while (< i (1- (length pts_r))) (if (equal (nth i pts_r) (nth (1+ i) pts_r) 1e-3) (setq pts_r (vl-remove (nth (1+ i) pts_r) pts_r)) (setq i (1+ i))))

  (if (< (length pts_r) 3)
    (progn (princ "\n[Ошибка] Недостаточно уникальных плановых точек съёмки.") (setvar "CMDECHO" old_echo) (setvar "OSMODE" old_osm) (exit))
  )

  (setq p_start (car pts_r))
  (foreach pt (cdr pts_r)
    (if (or (< (car pt) (car p_start)) (and (equal (car pt) (car p_start) 1e-3) (< (cadr pt) (cadr p_start)))) (setq p_start pt))
  )
  (setq p_curr p_start hull nil next_found T)
  (while (and next_found (< (length hull) (length pts_r)))
    (setq hull (cons p_curr hull))
    (setq p_next (car pts_r))
    (if (equal p_next p_curr 1e-3) (setq p_next (cadr pts_r)))
    (foreach pt pts_r
      (if (not (equal pt p_curr 1e-3))
        (progn
          (setq val (- (* (- (cadr pt) (cadr p_curr)) (- (car p_next) (car p_curr))) (* (- (car pt) (car p_curr)) (- (cadr p_next) (cadr p_curr)))))
          (if (> val 1e-3) (setq p_next pt) (if (and (< (abs val) 1e-3) (> (distance p_curr pt) (distance p_curr p_next))) (setq p_next pt)))
        )
      )
    )
    (setq p_curr p_next) (if (equal p_curr p_start 1e-3) (setq next_found nil))
  )
  (setq hull (reverse hull))

  (setq changed T)
  (while changed
    (setq changed nil i 0)
    (while (and (not changed) (< i (length hull)))
      (setq p1 (nth i hull) idx_next (if (= i (1- (length hull))) 0 (1+ i)) p2 (nth idx_next hull) d_edge (distance p1 p2))
      (if (> d_edge max_dist)
        (progn
          (setq best_pt nil min_score 1e10)
          (foreach target_pt pts_r
            (if (not (vl-position target_pt hull))
              (progn
                (setq d1 (distance p1 target_pt) d2 (distance p2 target_pt))
                (if (and (< d1 d_edge) (< d2 d_edge))
                  (progn (setq score (+ d1 d2)) (if (< score min_score) (setq min_score score best_pt target_pt)))
                )
              )
            )
          )
          (if best_pt (progn (setq hull (append (sublist hull 0 (1+ i)) (list best_pt) (sublist hull (1+ i) (length hull)))) (setq changed T)))
        )
      )
      (setq i (1+ i))
    )
  )

  (setq vertex_list nil)
  (foreach pt hull (setq vertex_list (append vertex_list (list (cons 10 pt)))))
  (setq poly_data (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 62 1) (cons 8 layer_name) '(100 . "AcDbPolyline") (cons 90 (length hull)) '(70 . 1)) vertex_list))
  (if (tblsearch "LTYPE" ltype_name) (setq poly_data (append poly_data (list (cons 6 ltype_name)))))
  
  (if (setq poly_r (entmake poly_data))
    (progn
      (setq obj (vlax-ename->vla-object (entlast)))
      (setq area_val (if (vlax-property-available-p obj 'Area) (vla-get-Area obj) (progn (command "_.area" "_o" (entlast)) (getvar "AREA"))))
      (setq area_ha (/ area_val 10000.0))
      (setq *GEONYZA_RED_AREA* area_val)
      (setq *GEONYZA_RED_HA* area_ha)
    )
  )
  (setvar "CMDECHO" old_echo) (setvar "OSMODE" old_osm) (princ)
)
;;; Итерация 2 — Построение Зеленого контура по линиям и блокам (С защитой от дубликатов)
;;; Команда: GRANYZA

(defun c:GRANYZA ( / layer_name ltype_name max_dist ss_g i ent_data ent_type ent_layer pt pts_g
                     layers_table layer_obj old_echo old_osm sublist p_start hull 
                     next_found p_curr p_next val changed p1 p2 idx_next d_edge best_pt 
                     min_score target_pt d1 d2 score vertex_list poly_data poly_g obj 
                     area_val_g area_ha_g layer_tbl_ent sub_ent item is_inside_poly 
                     pt_outside min_d insert_idx test_p1 test_p2 ext_changed outer_pts)
  (vl-load-com)
  
  (setq *GEONYZA_RED_AREA* nil *GEONYZA_RED_HA* nil)
  (c:GRANYZA_RUN_FIRST)

  (setq layer_name "150_Формуляр")
  (setq ltype_name "496")
  (setq max_dist 40.0)

  (setq layers_table (vla-get-Layers (vla-get-ActiveDocument (vlax-get-acad-object))))
  (if (vl-catch-all-error-p (setq layer_obj (vl-catch-all-apply 'vla-Item (list layers_table layer_name))))
    (setq layer_obj (vla-Add layers_table layer_name))
  )
  (vla-put-Plottable layer_obj :vlax-false)

  (setq old_echo (getvar "CMDECHO"))
  (setq old_osm (getvar "OSMODE"))
  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 0)

  (defun sublist (lst start end / res idx)
    (setq idx 0 res nil)
    (foreach item lst
      (if (and (>= idx start) (< idx end)) (setq res (cons item res)))
      (setq idx (1+ idx))
    )
    (reverse res)
  )

  (defun is_inside_poly (p poly_pts / x y inside p1 p2 x1 y1 x2 y2 j k)
    (setq x (car p) y (cadr p) inside nil)
    (setq j (1- (length poly_pts)) k 0)
    (while (< k (length poly_pts))
      (setq p1 (nth k poly_pts) p2 (nth j poly_pts)
            x1 (car p1) y1 (cadr p1) x2 (car p2) y2 (cadr p2))
      (if (and (or (and (< y1 y) (>= y2 y)) (and (< y2 y) (>= y1 y)))
               (< (+ x1 (* (/ (- y y1) (- y2 y1)) (- x2 x1))) x))
        (setq inside (not inside))
      )
      (setq j k k (1+ k))
    )
    inside
  )

  (setq ss_g (ssget "_X" '((0 . "LINE,LWPOLYLINE,POLYLINE,3DPOLYLINE,INSERT,ARC"))))
  (if (not ss_g)
    (princ "\n[Ошибка] Зеленый контур: Линейные объекты или блоки не найдены!")
    (progn
      (setq i 0 pts_g nil)
      (while (< i (sslength ss_g))
        (setq ename (ssname ss_g i))
        (setq ent_data (entget ename))
        (setq ent_type (cdr (assoc 0 ent_data)))
        (setq ent_layer (cdr (assoc 8 ent_data)))
        
        (if (and ent_layer (setq layer_tbl_ent (tblsearch "LAYER" ent_layer)))
          (progn
            (if (and (>= (cdr (assoc 62 layer_tbl_ent)) 0) 
                     (= (logand (cdr (assoc 70 layer_tbl_ent)) 1) 0)
                     (/= ent_layer layer_name))
              (progn
                (if (vl-position ent_type '("LINE" "INSERT" "ARC"))
                  (progn
                    (setq pt (cdr (assoc 10 ent_data)))
                    (if pt (setq pts_g (cons (list (car pt) (cadr pt)) pts_g)))
                    (if (= ent_type "LINE")
                      (progn (setq pt (cdr (assoc 11 ent_data))) (if pt (setq pts_g (cons (list (car pt) (cadr pt)) pts_g))))
                    )
                  )
                )
                (if (vl-position ent_type '("LWPOLYLINE" "POLYLINE" "3DPOLYLINE"))
                  (progn
                    (if (= ent_type "LWPOLYLINE")
                      (foreach item ent_data (if (= (car item) 10) (setq pts_g (cons (list (cadr item) (caddr item)) pts_g))))
                      (progn
                        (setq sub_ent (entnext ename))
                        (while (and sub_ent (= (cdr (assoc 0 (entget sub_ent))) "VERTEX"))
                          (setq pt (cdr (assoc 10 (entget sub_ent))))
                          (if pt (setq pts_g (cons (list (car pt) (cadr pt)) pts_g)))
                          (setq sub_ent (entnext sub_ent))
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
        (setq i (1+ i))
      )
      
      (setq pts_g (vl-sort pts_g '(lambda (a b) (if (equal (car a) (car b) 1e-3) (< (cadr a) (cadr b)) (< (car a) (car b))))))
      (setq i 0) 
      (while (< i (1- (length pts_g))) 
        (if (equal (nth i pts_g) (nth (1+ i) pts_g) 1e-3) 
          (setq pts_g (vl-remove (nth (1+ i) pts_g) pts_g)) 
          (setq i (1+ i))
        )
      )

      (if (>= (length pts_g) 3)
        (progn
          (setq p_start (car pts_g))
          (foreach pt (cdr pts_g)
            (if (or (< (car pt) (car p_start)) (and (equal (car pt) (car p_start) 1e-3) (< (cadr pt) (cadr p_start)))) (setq p_start pt))
          )
          (setq p_curr p_start hull nil next_found T)
          (while (and next_found (< (length hull) (length pts_g)))
            (setq hull (cons p_curr hull))
            (setq p_next (car pts_g))
            (if (equal p_next p_curr 1e-3) (setq p_next (cadr pts_g)))
            (foreach pt pts_g
              (if (not (equal pt p_curr 1e-3))
                (progn
                  (setq val (- (* (- (cadr pt) (cadr p_curr)) (- (car p_next) (car p_curr))) (* (- (car pt) (car p_curr)) (- (cadr p_next) (cadr p_curr)))))
                  (if (> val 1e-3) (setq p_next pt) (if (and (< (abs val) 1e-3) (> (distance p_curr pt) (distance p_curr p_next))) (setq p_next pt)))
                )
              )
            )
            (setq p_curr p_next) (if (equal p_curr p_start 1e-3) (setq next_found nil))
          )
          (setq hull (reverse hull))

          (setq changed T)
          (while changed
            (setq changed nil i 0)
            (while (and (not changed) (< i (length hull)))
              (setq p1 (nth i hull) idx_next (if (= i (1- (length hull))) 0 (1+ i)) p2 (nth idx_next hull) d_edge (distance p1 p2))
              (if (> d_edge max_dist)
                (progn
                  (setq best_pt nil min_score 1e10)
                  (foreach target_pt pts_g
                    (if (not (vl-position target_pt hull))
                      (progn
                        (setq d1 (distance p1 target_pt) d2 (distance p2 target_pt))
                        (if (and (< d1 d_edge) (< d2 d_edge))
                          (progn (setq score (+ d1 d2)) (if (< score min_score) (setq min_score score best_pt target_pt)))
                        )
                      )
                    )
                  )
                  (if best_pt (progn (setq hull (append (sublist hull 0 (1+ i)) (list best_pt) (sublist hull (1+ i) (length hull)))) (setq changed T)))
                )
              )
              (setq i (1+ i))
            )
          )

          (setq outer_pts nil)
          (foreach pt_outside pts_g
            (if (and (not (vl-position pt_outside hull))
                     (not (is_inside_poly pt_outside hull)))
              (setq outer_pts (cons pt_outside outer_pts))
            )
          )
          
          (if outer_pts
            (progn
              (setq ext_changed T)
              (while ext_changed
                (setq ext_changed nil)
                (foreach pt_outside outer_pts
                  (if (not (vl-position pt_outside hull))
                    (progn
                      (setq min_d 1e10 insert_idx nil i 0)
                      (while (< i (length hull))
                        (setq test_p1 (nth i hull))
                        (setq test_p2 (nth (if (= i (1- (length hull))) 0 (1+ i)) hull))
                        (setq score (+ (distance test_p1 pt_outside) (distance test_p2 pt_outside)))
                        (if (< score min_d) (setq min_d score insert_idx i))
                        (setq i (1+ i))
                      )
                      (if (and insert_idx (< (- min_d (distance (nth insert_idx hull) (nth (if (= insert_idx (1- (length hull))) 0 (1+ insert_idx)) hull))) 60.0))
                        (progn
                          (setq hull (append (sublist hull 0 (1+ insert_idx)) (list pt_outside) (sublist hull (1+ insert_idx) (length hull))))
                          (setq ext_changed T)
                        )
                      )
                    )
                  )
                )
              )
            )
          )

          (setq vertex_list nil)
          (foreach pt hull (setq vertex_list (append vertex_list (list (cons 10 pt)))))
          (setq poly_data (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 62 3) (cons 8 layer_name) '(100 . "AcDbPolyline") (cons 90 (length hull)) '(70 . 1)) vertex_list))
          (if (tblsearch "LTYPE" ltype_name) (setq poly_data (append poly_data (list (cons 6 ltype_name)))))
          
          (if (setq poly_g (entmake poly_data))
            (progn
              (setq obj (vlax-ename->vla-object (entlast)))
              (setq area_val_g (if (vlax-property-available-p obj 'Area) (vla-get-Area obj) (progn (command "_.area" "_o" (entlast)) (getvar "AREA"))))
              (setq area_ha_g (/ area_val_g 10000.0))
            )
          )
        )
      )
    )
  )

  (if *GEONYZA_RED_AREA*
    (princ (strcat "\nКрасный контур по точкам. Площадь: " (rtos *GEONYZA_RED_AREA* 2 2) " кв. м. (" (rtos *GEONYZA_RED_HA* 2 4) " га)"))
    (princ "\nКрасный контур по точкам: Ошибка построения.")
  )
  (if area_val_g
    (princ (strcat "\nЗеленый контур по съёмке.  Площадь: " (rtos area_val_g 2 2) " кв. м. (" (rtos area_ha_g 2 4) " га)"))
    (princ "\nЗеленый контур по съёмке:  Ошибка построения.")
  )

  (setvar "CMDECHO" old_echo) (setvar "OSMODE" old_osm) (princ)
)

(setq GRANYZA_RED nil)
(defun c:GRANYZA_RED () (c:GRANYZA_RUN_FIRST))

(princ "\nСкрипт GRANYZA успешно загружен!\n")
(princ)
