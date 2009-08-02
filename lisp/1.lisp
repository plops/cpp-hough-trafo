(require :asdf)
(require :cells-gtk)
(defpackage :kielhorn.martin.cgtk-show
  (:use :cells :cl :cells-gtk))

(in-package :kielhorn.martin.cgtk-show)

(defmodel my-app (gtk-app)
  ()
  (:default-initargs
      :md-name :martin-top
    :width (c-in 300)
    :height (c-in 300)
    :kids (c-in nil)))

(init-gtk)
(start-win 'my-app)


(defun draw-grid ()
;  (gl:line-width .7)
  (gl:color 1 1 1 .6)
  (gl:with-primitive :lines
    (loop for i from -10 below 12 by 2 do
	  (gl:vertex i 10) (gl:vertex i -10))
    (loop for i from -10 below 12 by 2 do
	  (gl:vertex 10 i) (gl:vertex -10 i))))

(defun threed-draw (self)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (draw-grid)
  (gl:with-pushed-matrix
    (gl:translate 0 0 (zpos self))
    (gl:with-primitive :lines
      ;;(gl:line-width 2)
      (gl:color 1 0 0) (gl:vertex 5 0 0) (gl:vertex 0 0 0)
      (gl:color 0 1 0) (gl:vertex 0 5 0) (gl:vertex 0 0 0)
      (gl:color 0 0 1) (gl:vertex 0 0 5) (gl:vertex 0 0 0)))
  ;;(timeout-add 30 #'(lambda () (redraw (find-widget :threed))))
  )

(defmodel threed (gl-drawing-area)
  ((zpos :cell t :reader zpos :initarg :zpos :initform (c-in 0)))
  (:default-initargs
      :width (c-in 200)
    :height (c-in 200)
    :init #'(lambda (self)
	      (gl:enable :blend :depth-test :line-smooth)
	      (gl:blend-func :one :src-alpha)
	      (with-integrity (:change :adjust-widget-size)
		(setf (allocated-width self) (width self)
		      (allocated-height self) (height self))))
    :resize #'(lambda (self)
		(let ((w (allocated-width self))
		      (h (allocated-height self)))
		  (gl:viewport 0 0 w h)
		 (with-matrix-mode (:projection)
		   (glu:perspective 60 (/ w h) .1 200))
		 (gl:matrix-mode :modelview)
		 (gl:load-identity)
		 (glu:look-at 10 10 3
			      0 0 0
			      0 0 1)))
    :draw 'threed-draw))

(setf (kids (find-widget :martin-top))
      (list 
       (mk-vbox :fm-parent (find-widget :martin-top)
		:kids
		(kids-list? 
		 (mk-hscale :md-name :scale-zpos
			    :min -10
			    :max 10
			    :init 0
			    :step .05
			    )
		 (make-kid 'threed
			   :md-name :threed
			   :zpos (c? (progn
				       (redraw (find-widget :threed))
				       (widget-value :scale-zpos))))))))))

(progn
   (not-to-be (find-widget :threed))
   (setf (kids (find-widget :martin-top))
	 nil))
