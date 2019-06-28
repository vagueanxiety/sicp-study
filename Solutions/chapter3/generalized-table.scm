
; a table in which values are stored under an arbitrary number of keys and
; different values may be stored under different numbers of keys.

;;
; arbituary number of keys: list of keys used for look-up and insert
; recusively create subtables as necessary
;
; different values are stored under different numbers of keys:
;
; table = list (head + key value pairs)
; a key value pair = a record or a subtable
; a subtable is a table
; a record is a **simple** cons

;;
; same-key? determines the equality of two keys
(define (make-table same-key?)
  ;(let ((local-table (list '*table*)))
  (let ((local-table
          (list 't1 (cons 'a 'b) (list 't2 (cons 'c 'd)))
          ))

    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (caar records))
             (car records))
            (else (assoc key (cdr records)))))

    (define (get-entries dimension) (cdr dimension))

    ;; either record or table
    (define (record? dimension) (not (list? dimension)))

    ;; it does not differentiate table and record in its return value
    ;; it does not return record or table for partially correct keylist
    (define (dimension-assoc keylist dimension)
      (cond ((null? dimension) false)
            ((null? keylist) dimension)
            ((record? dimension) false)
            (else
              (let ((subdimension (assoc (car keylist) (get-entries dimension))))
                (cond (subdimension (dimension-assoc (cdr keylist) subdimension))
                      (else false)
                      )
                )
              ))
      )

    ;; lookup is responsible for differentiating between table and record
    (define (lookup keylist)
      (cond ((null? keylist) false)
            (else
              (let ((dimension (dimension-assoc keylist local-table)))
                (if (and dimension (record? dimension))
                    (cdr dimension)
                    false)
                )
              )
            )
      )


    ;; since we rely the shape of the data to differentiate types
    ;; insert is responsible for limiting user inputs so that they
    ;; don't "confuse" the system

    ;; todo check value
    (define (insert! keylist value) (insert-helper keylist local-table value))

    (define (insert-dimensions dimension keylist value)
      (if (null? keylist)
          (begin
            (set-cdr! dimension value)
            'ok)
          (begin
            (set-cdr!
              dimension
              (cons (list (car keylist))
                    (cdr dimension)))
            (insert-dimensions (cadr dimension) (cdr keylist) value)
            )
          )
      )

    (define (insert-helper keylist dimension value)
      (cond ((null? dimension) false)
            ((null? keylist)
             (if (record? dimension)
                 (begin (set-cdr! dimension value)
                        'ok)
                 false
                 ))
            ;; found the record/table
            ;; users are not allowed to change a table to a record

            ((record? dimension) false)
            ;; users are not allowed to change a record to a table

            (else
              (let ((subdimension (assoc (car keylist) (get-entries dimension))))
                (cond (subdimension (insert-helper (cdr keylist) subdimension
                                                   value))
                      (else (insert-dimensions dimension keylist value))))
              ;; stop midway because of lack of subtables
              )
            ))


    ;(let ((subtable
    ;(assoc key-1 (cdr local-table))))
    ;(if subtable
    ;(let ((record
    ;(assoc key-2
    ;(cdr subtable))))
    ;(if record
    ;(set-cdr! record value)
    ;(set-cdr!
    ;subtable
    ;(cons (cons key-2 value)
    ;(cdr subtable)))))
    ;(set-cdr!
    ;local-table
    ;(cons (list key-1
    ;(cons key-2 value))
    ;(cdr local-table)))))
    ;'ok)


    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'table-proc!) local-table)
            (else (error "Unknown operation:
                         TABLE" m))))
    dispatch))

;;
; by equality of pointers.
(define operation-table (make-table eq?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
(define table (operation-table 'table-proc!))


(get '())
(get '(c))
(get '(a))
(get '(t2))
(get '(t2 c))
(get '(t2 d))
(get '(t2 c a))

table

(put '(t1 a) 'c)
(get '(t1 a))
table


(put '(t1 t2 t3 d) 'e)
(get '(t1 t2 t3 d))
table

(put '(t1 a e) 'f)
(put '(t1 t2) 'f)
table
