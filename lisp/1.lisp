(require :asdf)
(require :cffi)
(require :cells-gtk)
(defpackage :kielhorn.martin.cgtk-show
  (:use :cells :cl :cells-gtk))

(in-package :kielhorn.martin.cgtk-show)

;; like www.den.rcast.u-tokyo.ac.jp/~salvi  2008-08-15
(defclass bild () ; can't call it image - clashes with something else in cgtk
  ((width :initarg :width :reader width)
   (height :initarg :height :reader height)
   (data :initarg :data :accessor data)))

(defun make-bild (width height &optional data)
  (let ((data (or data
		  (make-array (* width height)
			      :initial-element 0
			      :element-type '(unsigned-byte 8)))))
    (make-instance 'bild
		   :width width
		   :height height
		   :data data)))

(defun read-without-comment (s)
  "Read the next data from stream S, skipping any comments."
  (if (eq (peek-char nil s) #\#)
      (progn
	(read s)
	(read-without-comment s))
      (read s)))

(defun read-pgm (filename)
  "Read binary PGM file and return bild."
  (let (width height grays pos)
    (with-open-file (s filename :external-format :ascii) 
      (when (not (equal (symbol-name (read s)) "P5"))
	(error "not a binary PGM file"))
      (format nil "image~%")
      (setf width (read-without-comment s)
	    height (read-without-comment s)
	    grays (read-without-comment s)
	    pos (file-position s))
      (format nil "~a~%" (list width height)))
    (let ((data (cffi:make-shareable-byte-vector (* width height))
					;(make-array (* width height) :element-type '(unsigned-byte 8))
	    ))
      (with-open-file (s filename :element-type '(unsigned-byte 8))
	(file-position s pos)
	(read-sequence data s))
      (make-bild width height data))))

(defparameter img (read-pgm "../grid25.pgm"))

(defmodel my-app (gtk-app)
  ()
  (:default-initargs
      :md-name :martin-top
    :width (c-in 700)
    :height (c-in 450)
    :kids (c-in nil)))

(init-gtk)
(start-win 'my-app)


(defun draw-grid (&optional (w 100) (h 100))
;  (gl:line-width .7)
  (gl:color 1 1 1 .6)
  (gl:with-primitive :lines
    (loop for i from 0 below w by 20 do
	  (gl:vertex i 0) (gl:vertex i 210))
    (loop for i from 0 below h by 20 do
	  (gl:vertex 1 i) (gl:vertex 210 i))))

(defparameter *texture* nil)
(defun unload-tex ()
  (when *texture*
    (gl:delete-textures (list *texture*))
    (setf *texture* nil)))
(defun load-tex () 
  (unless *texture*
   (let ((texture (first (gl:gen-textures 1))))
     (gl:bind-texture :texture-2d texture)
     (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
     (gl:tex-parameter :texture-2d :texture-min-filter :linear)
     (cffi:with-pointer-to-vector-data (addr (data img))
       (gl:tex-image-2d :texture-2d 0 :rgba
			(width img) (height img)
			0 :luminance :unsigned-byte addr))
     (setf *texture* texture))))

(defun threed-draw (self)
  ;(unload-tex)
  (load-tex)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:with-pushed-matrix
    (gl:translate 0 (* 9 (zpos self)) 0)
    (draw-grid (width self) (height self))

    (gl:enable :texture-2d)
    (gl:bind-texture :texture-2d *texture*)
    (gl:color 1 1 1)
    (gl:scale 2 2 1)
    (gl:with-primitive :quads
      (gl:vertex 0 0) (gl:tex-coord (/ 5.809 512)
				    (/ 5.789 512))
      (gl:vertex 0 130) (gl:tex-coord (/ 2.793 512)
					(/ 463.45 512))
      (gl:vertex 120 130) (gl:tex-coord (/ 384.313 512)
				      (/ 473.237 512))
      (gl:vertex 120 0) (gl:tex-coord (/ 394.775 512)
				      (/ 13.8367 512)))
    (gl:disable :texture-2d)

    (gl:with-primitive :lines
      (gl:line-width 2)
      (gl:color 1 0 0) (gl:vertex 50 0 0) (gl:vertex 0 0 0)
      (gl:color 0 1 0) (gl:vertex 0 50 0) (gl:vertex 0 0 0)
      (gl:color 0 0 1) (gl:vertex 0 0 50) (gl:vertex 0 0 0)))
  ;;(timeout-add 30 #'(lambda () (redraw (find-widget :threed))))
  )

(defmodel threed (gl-drawing-area)
  ((zpos :cell t :reader zpos :initarg :zpos :initform (c-in 0)))
  (:default-initargs
      :width (c-in 300)
    :height (c-in 300)
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
		   (gl:ortho 0 w h 0 -1 1))
		 (gl:matrix-mode :modelview)
		 (gl:load-identity)))
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
				       (widget-value :scale-zpos))))))))
#|
(progn
   (not-to-be (find-widget :threed))
   (setf (kids (find-widget :martin-top))
	 nil))
|#
