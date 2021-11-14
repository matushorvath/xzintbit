#!/usr/bin/env -S sbcl --control-stack-size 1024 --script

(load "~/.sbclrc")
(ql:quickload "str" :silent :true)

(defun run (path)
    (execute-program (list-to-vector (load-program path)) 0 0)
)

(defun list-to-vector (list)
    (make-array (list (length list)) :initial-contents list :adjustable t)
)

(defun load-program (path)
    (mapcar #'parse-integer
        (str:split ","
            (uiop:read-file-string path)
        )
    )
)

(defun execute-program (mem ip rb)
    (execute-program-instruction (get-mem mem ip) mem ip rb)
)

(defun execute-program-instruction (inst mem ip rb)
    (cond
        ((= (mod inst 100) 1) ;; add
            (set-param 2 inst mem ip rb (+ (get-param 0 inst mem ip rb) (get-param 1 inst mem ip rb)))
            (execute-program mem (+ ip 4) rb)
        )
        ((= (mod inst 100) 2) ;; mul
            (set-param 2 inst mem ip rb (* (get-param 0 inst mem ip rb) (get-param 1 inst mem ip rb)))
            (execute-program mem (+ ip 4) rb)
        )
        ((= (mod inst 100) 3) ;; in
            (set-param 0 inst mem ip rb (get-input))
            (execute-program mem (+ ip 2) rb)
        )
        ((= (mod inst 100) 4) ;; out
            (set-output (get-param 0 inst mem ip rb))
            (execute-program mem (+ ip 2) rb)
        )
        ((= (mod inst 100) 5) ;; jnz
            (execute-program
                mem
                (cond
                    ((/= 0 (get-param 0 inst mem ip rb)) (get-param 1 inst mem ip rb))
                    (t (+ ip 3))
                )
                rb
            )
        )
        ((= (mod inst 100) 6) ;; jz
            (execute-program
                mem
                (cond
                    ((= 0 (get-param 0 inst mem ip rb)) (get-param 1 inst mem ip rb))
                    (t (+ ip 3))
                )
                rb
            )
        )
        ((= (mod inst 100) 7) ;; lt
            (set-param
                2 inst mem ip rb
                (cond
                    ((< (get-param 0 inst mem ip rb) (get-param 1 inst mem ip rb)) 1)
                    (t 0)
                )
            )
            (execute-program mem (+ ip 4) rb)
        )
        ((= (mod inst 100) 8) ;; eq
            (set-param
                2 inst mem ip rb
                (cond
                    ((= (get-param 0 inst mem ip rb) (get-param 1 inst mem ip rb)) 1)
                    (t 0)
                )
            )
            (execute-program mem (+ ip 4) rb)
        )
        ((= (mod inst 100) 9) ;; arb
            (execute-program mem (+ ip 2) (+ rb (get-param 0 inst mem ip rb)))
        )
        ((= (mod inst 100) 99) ;; hlt
            (list mem ip rb)
        )
        (t nil)
    )
)

(defun get-param (idx inst mem ip rb)
    (get-param-in-mode
        (mod (truncate (/ inst (nth idx (list 100 1000 10000)))) 10)
        (get-mem mem (+ ip idx 1))
        mem ip rb
    )
)

(defun get-param-in-mode (mode raw mem ip rb)
    (cond
        ((= mode 0) (get-mem mem raw)) ;; position mode
        ((= mode 1) raw) ;; immediate mode
        ((= mode 2) (get-mem mem (+ rb raw))) ;; relative mode
        (t nil)
    )
)

(defun set-param (idx inst mem ip rb val)
    (set-param-in-mode
        (mod (truncate (/ inst (nth idx (list 100 1000 10000)))) 10)
        (get-mem mem (+ ip idx 1))
        mem ip rb val
    )
)

(defun set-param-in-mode (mode raw mem ip rb val)
    (cond
        ((= mode 0) (set-mem mem raw val)) ;; position mode
        ((= mode 2) (set-mem mem (+ rb raw) val)) ;; relative mode
        (t nil)
    )
)

(defun calc-mem-size (addr new-size)
    (cond
        ((< addr new-size) new-size)
        (t (calc-mem-size addr (* 2 new-size)))
    )
)

(defun get-mem (mem addr)
    (cond ((>= addr (length mem))
        (adjust-array mem (calc-mem-size addr (length mem)) :initial-element 0)
    ))

    (aref mem addr)
)

(defun set-mem (mem addr val)
    (cond ((>= addr (length mem))
        (adjust-array mem (calc-mem-size addr (length mem)) :initial-element 0)
    ))

    (setf (aref mem addr) val)
)

(defun get-input ()
    (char-code (read-char t))
)

(defun set-output (val)
    (princ (code-char val))
)

(run (car (uiop:command-line-arguments)))
