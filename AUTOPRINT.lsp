(vl-load-com)
(defun c:AUTOPRINT ( / ss i ent obj minpt maxpt p1 p2 w h formatName pdfName doc path dMax dMin isFormat
                       layout winMin winMax oldBackPlot oldCmdEcho oldExpert oldFill oldText oldInsUnits oldUnits oldMeas plotObj
                       blockList blkSS blkEnt blkObj blkPos blkName atts attName numStr numVal dataList item )
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  
  (if (= (getvar "TILEMODE") 1)
    (progn
      (princ "\nОшибка: Команду AUTOPRINT нужно запускать только на вкладке ЛИСТ!")
      (exit)
    )
  )

  (vla-put-ActiveSpace doc 0)
  (if (/= (getvar "CVPORT") 1)
    (vl-cmdf "_.PSPACE")
  )

  (vla-SendCommand doc (chr 3))
  (vla-SendCommand doc (chr 3))

  (setq oldFill (getvar "FILLMODE") oldText (getvar "TEXTFILL"))
  (setvar "FILLMODE" 1)
  (setvar "TEXTFILL" 1)

  (setq oldInsUnits (getvar "INSUNITS") oldUnits (getvar "LUNITS") oldMeas (getvar "MEASUREMENT"))
  (setvar "MEASUREMENT" 1)
  (setvar "LUNITS" 2)
  (setvar "INSUNITS" 4)

  (setq oldBackPlot (getvar "BACKGROUNDPLOT") oldCmdEcho (getvar "CMDECHO") oldExpert (getvar "EXPERT"))
  (setvar "BACKGROUNDPLOT" 0)
  (setvar "CMDECHO" 0)
  (setvar "EXPERT" 5)

  (setq layout (vla-get-ActiveLayout doc))

  (setq blockList nil)
  (setq blkSS (ssget "X" '((0 . "INSERT") (66 . 1))))
  (if blkSS
    (progn
      (setq i 0)
      (repeat (sslength blkSS)
        (setq blkEnt (ssname blkSS i))
        (setq blkObj (vlax-ename->vla-object blkEnt))
        (setq blkName (vlax-get-property blkObj 'EffectiveName))
        (if (= (strcase blkName) (strcase "Nomer_Lista"))
          (progn
            (setq blkPos (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint blkObj))))
            (setq atts (vlax-invoke blkObj 'GetAttributes))
            (setq numStr "0")
            (foreach att atts
              (setq attName (vla-get-TagString att))
              (if (= (strcase attName) (strcase "№"))
                (setq numStr (vla-get-TextString att))
              )
            )
            (setq numVal (distof numStr 2))
            (if (not numVal) (setq numVal numStr))
            (setq blockList (cons (list blkPos numVal numStr) blockList))
          )
        )
        (setq i (1+ i))
      )
    )
  )

  (setq ss (ssget "X" '((0 . "LWPOLYLINE") (8 . "160_Оформление") (-4 . "&=") (70 . 1))))
  
  (if ss
    (progn
      (setq path (getvar "DWGPREFIX"))
      (setq i 0)
      (setq dataList nil)

      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))
        
        (vla-GetBoundingBox obj 'minpt 'maxpt)
        (setq p1 (vlax-safearray->list minpt) p2 (vlax-safearray->list maxpt))
        
        (setq numVal 9999)
        (setq numStr "Без_номера")
        (foreach blk blockList
          (setq bp (car blk))
          (if (and (>= (car bp) (car p1)) (<= (car bp) (car p2))
                   (>= (cadr bp) (cadr p1)) (<= (cadr bp) (cadr p2)))
            (progn
              (setq numVal (cadr blk))
              (setq numStr (caddr blk))
            )
          )
        )
        
        (setq dataList (cons (list numVal numStr p1 p2) dataList))
        (setq i (1+ i))
      )

      (setq dataList (vl-sort dataList '(lambda (e1 e2) 
        (cond 
          ((and (numberp (car e1)) (numberp (car e2))) (< (car e1) (car e2)))
          ((and (stringp (car e1)) (stringp (car e2))) (< (car e1) (car e2)))
          (t (numberp (car e1)))
        )
      )))

      (princ "\nСтарт упорядоченного пакетного экспорта планшетов...")
      (foreach item dataList
        (setq numStr (cadr item))
        (setq p1 (caddr item))
        (setq p2 (cadddr item))
        
        (setq w (abs (- (car p2) (car p1))))
        (setq h (abs (- (cadr p2) (cadr p1))))
        (if (> w h) (setq dMax w dMin h) (setq dMax h dMin w))
        
        (setq formatName nil)
        (setq isFormat nil)

        (cond
          ((and (> dMax 415) (< dMax 425) (> dMin 292) (< dMin 302))
           (setq isFormat t)
           (if (> w h) (setq formatName "ISO_full_bleed_A3_(420.00_x_297.00_MM)") (setq formatName "ISO_full_bleed_A3_(297.00_x_420.00_MM)")))
          
          ;; СЕКЦИЯ А4 С ИСПРАВЛЕННЫМ ИМЕНЕМ ДЛЯ ОБОИХ НАПРАВЛЕНИЙ
          ((and (> dMax 292) (< dMax 302) (> dMin 205) (< dMin 215))
           (setq isFormat t)
           (setq formatName "ISO_full_bleed_A4_(297.00_x_210.00_MM)"))
          
          ((and (> dMax 589) (< dMax 599) (> dMin 415) (< dMin 425))
           (setq isFormat t)
           (if (> w h) (setq formatName "ISO_full_bleed_A2_(594.00_x_420.00_MM)") (setq formatName "ISO_full_bleed_A2_(420.00_x_594.00_MM)")))
          ((and (> dMax 836) (< dMax 846) (> dMin 589) (< dMin 599))
           (setq isFormat t)
           (if (> w h) (setq formatName "ISO_full_bleed_A1_(841.00_x_594.00_MM)") (setq formatName "ISO_full_bleed_A1_(594.00_x_841.00_MM)")))
        )

        (if isFormat
          (progn
            (setq pdfName (strcat path (vl-filename-base (getvar "DWGNAME")) "_Планшет_№" numStr ".pdf"))
            (if (findfile pdfName) (vl-file-delete pdfName))

            (setq winMin (vlax-make-safearray vlax-vbDouble '(0 . 1)))
            (vlax-safearray-put-element winMin 0 (car p1))
            (vlax-safearray-put-element winMin 1 (cadr p1))
            (setq winMax (vlax-make-safearray vlax-vbDouble '(0 . 1)))
            (vlax-safearray-put-element winMax 0 (car p2))
            (vlax-safearray-put-element winMax 1 (cadr p2))

            (vla-put-ConfigName layout "DWG To PDF.pc3")
            (vla-put-CanonicalMediaName layout formatName)
            (vla-put-PaperUnits layout acMillimeters)
            
            ;; Автоповорот: ac0degrees для альбома, ac90degrees для вашей книжной А4
            (vla-put-PlotRotation layout (if (> w h) ac0degrees ac90degrees))
            
            (vla-SetWindowToPlot layout winMin winMax)
            (vla-put-PlotType layout acWindow)
            (vla-put-CenterPlot layout :vlax-true)
            (vla-put-UseStandardScale layout :vlax-false)
            (vla-SetCustomScale layout 1.0 1.0)
            (vla-put-StyleSheet layout "")
            (vla-put-PlotWithLineweights layout :vlax-true)
            (vla-put-PlotWithPlotStyles layout :vlax-true)
            (vla-put-PlotViewportsFirst layout :vlax-true)
            (vla-RefreshPlotDeviceInfo layout)

            (setq plotObj (vla-get-Plot doc))
            (vl-catch-all-apply 'vla-PlotToFile (list plotObj pdfName "DWG To PDF.pc3"))

            (if (findfile pdfName)
              (princ (strcat "\n[ФАЙЛ ЗАПИСАН] Планшет №" numStr " (Формат А4)"))
              (princ (strcat "\n[ОШИБКА] Не удалось создать файл для планшета №" numStr))
            )
          )
        )
      )
      (princ (strcat "\n\nПакетная выгрузка полностью завершена! Всего обработано рамок: " (itoa (length dataList))))
    )
    (princ "\nВнимание: Замкнутые рамки на слое '160_Оформление' не найдены.")
  )

  (setvar "MEASUREMENT" oldMeas)
  (setvar "LUNITS" oldUnits)
  (setvar "INSUNITS" oldInsUnits)
  (setvar "BACKGROUNDPLOT" oldBackPlot)
  (setvar "CMDECHO" oldCmdEcho)
  (setvar "EXPERT" oldExpert)
  (setvar "FILLMODE" oldFill)
  (setvar "TEXTFILL" oldText)
  (princ)
)
(princ "\nСкрипт AUTOPRINT (Исправлен А4 вертикальный) успешно загружен!")
(princ)
