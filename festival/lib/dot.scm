;;; $Id: dot.scm,v 1.4 2007/06/07 13:09:28 emarsi Exp $
;;;
;;; by Erwin marsi
;;; for the NeXTenS project
;;;
;;; Functions for drawing and viewing several Festival relations,
;;; specific to the Dutch voices, with dot (part of the graphviz
;;; package)

;;; TODO:
;;; - add doc strings
;;; - deletion of tmp files optional; dot and png file names configurable by user
;;; - find a better solution instead of sleep


;;  default configuration

(defvar dot_exec "dot"
  "path to dot graph drawing program")

(defvar png_viewer "mozilla"
  "path to a program capable of displaying png graphics, for
instance Mozilla")


(defvar sleep_time 2
  "no. of seconds to wait before the png file is deleted")


(defvar node_att 
  "shape=plaintext,fontname=Helvetica,fontsize=10,height=0.1"
  "dot node attributes")
;; e.g. fontname=Helvetiva,fontsize=11

(defvar edge_att 
  "dir=none,fontname=Helvetica,fontsize=10"
  "dot edge attributes")


;; platform specific configuration

(cond
 ;; Mac OS X
 ((string-matches *ostype* ".*Darwin.*")
  (begin
    ;; assume Graphviz is installed
    (set! dot_exec  "/Applications/Graphviz.app/Contents/MacOS/dot")
    (set! png_viewer "open")
    (set! node_att "shape=plaintext,fontname=HelveticaNeue,fontsize=10,height=0.1")
    (set! edge_att "dir=none,fontname=HelveticaNeue,fontsize=10")))
 ;; Cygwin at MS Windows
 ((string-matches *ostype* ".*CYGWIN.*")
  ;; TODO
  )
 )


(define (show_prostree_graph utt)
  (show_graph write_prostree_to_graph utt))

(define (show_syntree_graph utt)
  (show_graph write_syntree_to_graph utt))

(define (show_token_graph utt)
  (show_graph write_token_to_graph utt))

(define (show_inton_graph utt)
  (show_graph write_inton_to_graph utt))

(define (show_graph write_fun utt)
  (let ((inp_fn (make_tmp_filename))
	(out_fn (string-append (make_tmp_filename) ".png")))
    (apply write_fun (list utt inp_fn))
    (system
     (format nil "%s -Tpng %s -o %s"
	     dot_exec
	     inp_fn
	     out_fn))
    (system 
     (format nil "%s %s && sleep %d"
	     png_viewer
	     out_fn
	     sleep_time))
    (delete-file inp_fn)
    (delete-file out_fn)
    ))

  

(define (write_prostree_to_graph utt fn)
  (write_to_graph prostree_to_graph utt fn))

(define (write_syntree_to_graph utt fn)
  (write_to_graph syntree_to_graph utt fn))

(define (write_token_to_graph utt fn)
  (write_to_graph token_to_graph utt fn))

(define (write_inton_to_graph utt fn)
  (write_to_graph inton_to_graph utt fn))

(define (write_to_graph write_fun utt fn)
  (let (fd)
    (set! fd (fopen fn "w"))
    (graph_header fd)
    (apply write_fun (list utt fd))
    (graph_trailer fd)
    (fclose fd)))


(define (syntree_to_graph utt fd)
  ;; align terminals at same rank
  (format fd "{rank = same;\n")
  (mapcar 
   (lambda (term)
     (format fd "\"%s\"\n; " (item.feat term 'id)))
   (utt.relation.leafs utt 'SynTree))
  (format fd "};\n")
  (mapcar
   (lambda (node)
     (format fd "%s [label=\"%s\"];\n"
	     (item.feat node 'id)
	     ;; escape double quotes in output to dot
	     (cond
	      ((string-equal (item.name node) "\"-")
	       "\\\"-")
	      ((string-equal (item.name node) "-\"")
	       "-\\\"")
	      (t 
	       (item.name node))))
     (mapcar
      (lambda (dtr)
	(format fd "%s -> %s;\n"
		(item.feat node 'id)
		(item.feat dtr 'id))
	)
      (item.daughters node)))
   (utt.relation.items utt 'SynTree)))


(define (prostree_to_graph utt fd)
  ;; align terminals at same rank
  (format fd "{rank = same;\n")
  (mapcar 
   (lambda (term)
     (format fd "\"%s\"\n; " (item.feat term 'id)))
   (utt.relation.leafs utt 'ProsTree))
  (format fd "};\n")
  ;; non-terminals
  (mapcar
   (lambda (node)
     (format fd "%s [label=\"%s\"];\n"
	     (item.feat node 'id)
	     (item.name node))
     (mapcar
      (lambda (dtr)
	(format fd "%s -> %s %s;\n"
		(item.feat node 'id)
		(item.feat dtr 'id)
		(if (member_string (item.name dtr) 
				   '("Prosword1" "ProsWord2" "Foot" 
				     "Appendix" "Syllable"))
		    (if (string-equal (item.feat dtr 'metrical) "strong")
			"[label=s]"
			"[label=w]")
		    "")))
      (item.daughters node)))
   (utt.relation.items utt 'ProsTree)))


(define (token_to_graph utt fd)
  ;; align terminals at same rank
  (format fd "{rank = same;\n")
  (mapcar 
   (lambda (term)
     (format fd "\"%s\"\n; " (item.feat term 'id)))
   (utt.relation.leafs utt 'Token))
  (format fd "};\n")
  (mapcar
   (lambda (node)
     (format fd "%s [label=\"%s\"];\n"
	     (item.feat node 'id)
	     (item.name node))
     (mapcar
      (lambda (dtr)
	(format fd "%s -> %s;\n"
		(item.feat node 'id)
		(item.feat dtr 'id)))
      (item.daughters node)))
   (utt.relation.items utt 'Token)))


(define (inton_to_graph utt fd)
  ;; align terminals at same rank
  (format fd "{rank = same;\n")
  (mapcar 
   (lambda (term)
     (format fd "\"%s\"\n; " (item.feat term 'id)))
   (utt.relation.items utt 'Word))
  (format fd "};\n")
  (mapcar
   (lambda (node)
     (format fd "%s [label=\"%s\"];\n"
	     (item.feat node 'id)
	     (item.name node))
     (mapcar
      (lambda (dtr)
	(format fd "%s -> %s;\n"
		(item.feat node 'id)
		(item.feat dtr 'id)))
      (item.daughters node)))
   (utt.relation.items utt 'Word-Int)))


(define (graph_header fd)
  (format fd "digraph G {\n")
  (format fd "ordering = \"out\";\n")
  (format fd "nodesep = .1;\n")
  (format fd "ranksep = .1;\n")
  (format fd "node [%s];\n" node_att)
  (format fd "edge [%s];\n" edge_att))


(define (graph_trailer fd)
  (format fd "}\n"))

(provide 'dot)  


