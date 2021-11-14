#!/usr/bin/env -S sbcl --control-stack-size 1024 --script

(load "~/.sbclrc")
(ql:quickload "str" :silent :true)

(defun run (path get-input set-output)
    (execute-program (load-program path) 0 0 get-input set-output)
)

(defun load-program (path)
    (mapcar #'parse-integer
        (str:split ","
            (uiop:read-file-string path)
        )
    )
)

(defun execute-program (mem ip rb get-input set-output)
    (execute-program-instruction (get-mem mem ip) mem ip rb get-input set-output)
)

(defun execute-program-instruction (inst mem ip rb get-input set-output)
    (cond
        ((= (mod inst 100) 1) ;; add
            (execute-program
                (set-param 2 inst mem ip rb (+ (get-param 0 inst mem ip rb) (get-param 1 inst mem ip rb)))
                (+ ip 4) rb get-input set-output
            )
        )
        ((= (mod inst 100) 2) ;; mul
            (execute-program
                (set-param 2 inst mem ip rb (* (get-param 0 inst mem ip rb) (get-param 1 inst mem ip rb)))
                (+ ip 4) rb get-input set-output
            )
        )
        ((= (mod inst 100) 3) ;; in
            (execute-program
                (set-param 0 inst mem ip rb (funcall get-input))
                (+ ip 2) rb get-input set-output
            )
        )
        ((= (mod inst 100) 4) ;; out
            (funcall set-output (get-param 0 inst mem ip rb))
            (execute-program mem (+ ip 2) rb get-input set-output)
        )
        ((= (mod inst 100) 5) ;; jnz
            (execute-program
                mem
                (cond
                    ((/= 0 (get-param 0 inst mem ip rb)) (get-param 1 inst mem ip rb))
                    (t (+ ip 3))
                )
                rb get-input set-output
            )
        )
        ((= (mod inst 100) 6) ;; jz
            (execute-program
                mem
                (cond
                    ((= 0 (get-param 0 inst mem ip rb)) (get-param 1 inst mem ip rb))
                    (t (+ ip 3))
                )
                rb get-input set-output
            )
        )
        ((= (mod inst 100) 7) ;; lt
            (execute-program
                (set-param
                    2 inst mem ip rb
                    (cond
                        ((< (get-param 0 inst mem ip rb) (get-param 1 inst mem ip rb)) 1)
                        (t 0)
                    )
                )
                (+ ip 4) rb get-input set-output
            )
        )
        ((= (mod inst 100) 8) ;; eq
            (execute-program
                (set-param
                    2 inst mem ip rb
                    (cond
                        ((= (get-param 0 inst mem ip rb) (get-param 1 inst mem ip rb)) 1)
                        (t 0)
                    )
                )
                (+ ip 4) rb get-input set-output
            )
        )
        ((= (mod inst 100) 9) ;; arb
            (execute-program
                mem (+ ip 2)
                (+ rb (get-param 0 inst mem ip rb))
                get-input set-output
            )
        )
        ((= (mod inst 100) 99) ;; hlt
            (list mem ip rb get-input set-output)
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

(defun get-mem (mem addr)
    (cond
        ((endp mem) 0)
        ((= addr 0) (car mem))
        (t (get-mem (cdr mem) (- addr 1)))
    )
)

(defun set-mem (mem addr val)
    (cond
        ((= addr 0) (cons val (cdr mem)))
        ((endp mem) (append (make-list addr :initial-element 0) (list val)))
        (t (cons (car mem) (set-mem (cdr mem) (- addr 1) val)))
    )
)

(defun get-stdin ()
    (char-code (read-char t))
)

(defun set-stdout (val)
    (princ (code-char val))
)

(run (car (uiop:command-line-arguments)) #'get-stdin #'set-stdout)
