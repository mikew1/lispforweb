(ql:quickload '(cl-who hunchentoot parenscript))

(defpackage :retro-games
  (:use :cl :cl-who :hunchentoot :parenscript))

(in-package :retro-games)

(defclass game ()                                              ; [1]
  ((name  :reader name
          :initarg :name)
   (votes :accessor votes
          :initform 0)))

(defvar many-lost-hours (make-instance 'game :name "Tetris"))  ; [2]

(defmethod vote-for (user-selected-game)                       ; [3]
  (incf (votes user-selected-game)))

(defvar *games* nil)                                           ; [4]

(defun game-from-name (name)                                   ; [5]
  (find name *games* :test #'string-equal                      ; [6]
        :key #'name))

(defun game-stored? (game-name)                                ; [7]
  (game-from-name game-name))

(defun games ()                                                ; [8]
  (sort (copy-list *games*) #'> :key #'votes))

(defun add-game (name)                                         ; [9]
  (unless (game-stored? name)
    (push (make-instance 'game :name name) *games*)))

(defmethod print-object ((object game) stream)                 ; [10]
  (print-unreadable-object (object stream :type t)
    (with-slots (name votes) object
      (format stream "name: ~s with ~d votes" name votes))))


;; [1]  no user-defined superclasses, two slots.
;;      a game's name & number of votes.
;;      reader creates read, accessor creates read and write.
;;      here, we just use general names, not concerned with collisions.
;;      usually would be game-name & game-votes.
;; [2]  defvar to define a fixed thing, many-lost-hours.
;;      defines it only if it doesn't already exist.
;; [3]  defines a generic with a typed arg; could have been just a fn.
;;      this guy is no pedagogue. but it works, fair enough. power.
;; [4]  in-memory persistence to start with.
;; [5]  written immediately to encapsulate because we know we'll later
;;      move to a db.
;; [6]  find takes an item and a sequence. this says find the item
;;      of name name, testing using string-equal.
;; [7]  if game-from-name returns nil, that means game isn't stored.
;;      this fn is written just to clarify intent.
;; [8]  return games sorted by votes. sort is destructive, hence copy-list.
;; [9]  add a game to storage. push is destructive. not functional.
;;      lisp is multi-paradigm, can program functionally if wish.
;; [10] add to the generic print-object for our given type
;;      with-slots lets us reference slots as if they were variables.
;;      without with-slots, would have to access game object twice, like so
;;      (format stream "name: ~s with ~d votes"
;;        (name object) (votes object))   ; <- just as good, really; simper.
;;      He comments that often lisp has evolved to remove all duplication like this.


(setf (html-mode) :html5)                                        ; [11]

(with-html-output
  (*standard-output* nil :prologue t :indent t)                  ; [12]
  (:html
    (:head
      (:title "A title"))
    (:body
      (:p "A test page from cl-who"))))

(defmacro standard-page ((&key title) &body body)                ; [13]
  `(with-html-output-to-string
     (*standard-output* nil :prologue t :indent t)               ; [14]
     (:html :lang "en"
        (:head
          (:meta :charset "utf-8")
          (:title ,title)
          (:link :type "text/css"
                 :rel "stylesheet"
                 :href "/retro.css"))
        (:body
          (:div :id "header" ; Retro games header
            (:img :src "/logo.jpg"
                  :alt "Commodore 64"
                  :class "logo")
            (:span :class "strapline"
                   "Vote on your favourite Retro Game"))
          ,@body))))


;; [11] Set up cl-who, specify doctype. write the prologue, & indent.
;;      Note webtales used sexml, not cl-who, these do the same thing.
;;      Tantalisingly he says cl-who "also allows us to embed lisp expressions"
;;      "setting the scene for dynamic web pages".
;; [12] Odd syntax - standard-output is in operator position, but with-html-output
;;      is a macro, and not treated as one by it. isn't this bad style?
;; [13] Generate a standard-page, i.e. generate bunch of code plus inject given
;;      title & body into it. So in lisp we make a layout file by writing a macro.
;;      N.B. In webtales, we did layout files WITHOUT using a macro.
;;      Note the *macro* has keyword args, here; & args to macro are code.
;; [14] Usage: (standard-page (:title "Title") (..any num body fns))
;;      Q. Why do macro's have a non s-expr-like syntax. A. READ 'ON LISP'.


(defun start-server (port)
  (start (make-instance 'easy-acceptor :port port)))

(push (create-prefix-dispatcher "/retro-games"
                                'retro-games)
      *dispatch-table*)

(defun retro-games ()
  (standard-page (:title "Retro Games")
                 (:h1 "Top Retro Games")
                 (:p "We'll write this later...")))





