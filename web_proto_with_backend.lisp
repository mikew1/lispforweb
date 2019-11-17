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

(defmacro standard-page ((&key title script) &body body)         ; [13]
  `(with-html-output-to-string
     (*standard-output* nil :prologue t :indent t)               ; [14]
     (:html :lang "en"
        (:head
          (:meta :charset "utf-8")
          (:title ,title)
          (:link :type "text/css"
                 :rel "stylesheet"
                 :href "/retro.css")
          ,(when script
             `(:script :type "text/javascript"
                       (str ,script))))
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

(define-easy-handler (retro-games :uri "/retro-games") ()      ; [15]
  (standard-page
    (:title "Top Retro Games")
    (:h1 "Vote on your all time favourite retro games!")
    (:p "Missing a game? Make it available for votes "
        (:a :href "new-game" "here"))
    (:h2 "Current stand")
    (:div :id "chart" ; for css styling of links
      (:ol
        (dolist (game (games))
          (htm                                                ; [16]
            (:li (:a :href (format nil "vote?name=~a"
                              (url-encode ; avoid injection attacks
                                (name game))) "Vote!")
                 (fmt "~A with ~d votes" (escape-string (name game))
                                         (votes game)))))))))

(define-easy-handler (vote :uri "/vote") (name)               ; [17]
  (when (game-stored? name)
    (vote-for (game-from-name name)))
  (redirect "/retro-games"))

(define-easy-handler (new-game :uri "/new-game") ()           ; [18]
  (standard-page (:title "Add a new game"
                 :script (ps
                           (defvar add-form nil)
                           (defun validate-game-name (evt)          ; [21]
                             (when (= (@ add-form name value) "")
                               (chain evt (prevent-default))
                               (alert "Please enter a name.")))
                           (defun init ()                           ; [22]
                             (setf add-form (chain document
                                                   (get-element-by-id "addform")))
                             (chain add-form
                                    (add-event-listener "submit"
                                                validate-game-name false)))
                           (setf (chain window onload) init)))
    (:h1 "Add a new game to the chart")
    (:form :action "/game-added" :method "post" :id "addform"
      (:p "What is the name of the game?" (:br)
          (:input :type "text" :name "name" :class "txt"))
      (:p (:input :type "submit" :value "Add" :class "btn")))))

(define-easy-handler (game-added :uri "/game-added") (name)   ; [19]
  (unless (or (null name) (zerop (length name)))
    (add-game name))
  (redirect "/retro-games"))

;; [15] This macro defuns the fn retro-games for us, & registers
;;      it as a handler to the uri given. see hunchentoot docs for more.
;; [16] Omitting this strange 'htm' operator gives error e.g. :li fn undefined.
;;      cl-who docs 'syntax & semantics' explain it. like another invocation
;;      of with-html-output. seems needed when you loop.
;; [17] easy-handler with get param, name. it then checks for name,
;;      applies a vote, and redirects.
;; [18] handler for new-game which contains a form.
;; [19] form target, so a post route, note with parm, though a post param.
;; [20] handlers register on all verbs unless specialised.

;; [21] This is to do javascript validation in lisp.
;;      You could just write javascript. You could just use html validation.
;;      The '@' macro accesses object properties.
;;      The 'chain' macro chains function calls on an object instance.
;;      expands to: if(addForm.name.value === '') { evt.preventDefault(); ... }
;; [22] We could put js inline above, but let's not. Instead, we use onload.
;;      We're setting a lisp variable add-form to the dom element.
;;      expands to: addForm = document.getElementById("addform")
;;                  addForm.addEventListener('submit', validateGameName, false)
;;                  window.onload = init;
;;      Note how doing this has full prerequisite of knowledge of javascript.
;;      It also means people who don't know lisp cannot read your code.
;;      If it came to that though, could translate it out; useful exercise.
;;      To compile js for any of these, just wrap the fn defs above in (ps ...)
;; [23] On moving the js up into the easy handler: this is very bad, we need
;;      a way out of this, the 'handler' is overloaded with js.
;; [24] Note also the actual js here (and the game-added handler) still accept
;;      an empty space; the validation is incomplete.

