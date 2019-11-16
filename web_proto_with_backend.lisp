;; (ql:quickload '(cl-who hunchentoot parenscript))

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
;; [10]