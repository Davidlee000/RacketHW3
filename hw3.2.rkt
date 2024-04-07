#lang racket
;;written with help from microsoft bing ai
(require racket/format)

;;structs to store data in 
(struct account (number name balance) #:transparent)
(define-struct Transaction
  (transaction-type account-number transaction-id description amount))


;;function to read account and store in struct
(define (read-accounts file-path)
  (map (lambda (line)
         (define fields (regexp-split #px"  " line))
         (account (string->number (first fields))
                  (second fields)
                  (string->number (string-trim (third fields)))))
       (file->lines file-path)))

;;function to read transactions and store data
(define (read-transactions file-path)
  (define file-contents (file->string file-path))
  (map (lambda (line)
         (define fields (regexp-match* #px"\"[^\"]*\"|\\S+" line)) ; Split by tabs or consecutive spaces
         (define transaction-type (first fields))
         (define account-number (string->number (second fields)))
         (define transaction-id (string->number (third fields)))
         (define description (fourth fields))
         (define amount
           (if (>= (length fields) 6)
               (string->number (sixth fields))
               (string->number (fifth fields))))
       (make-Transaction transaction-type account-number transaction-id description amount))
       (string-split file-contents "\n" #:trim? #t)))



;;error handling files
(define (checkfile filename)
  (if (file-exists? filename)
           ;; File exists, do nothing
           (void)
           ;; File does not exist, print an error message
           (displayln (format"Could not find file "))))
(checkfile "ACCOUNTS.TXT")
(checkfile "TRANSACTIONS.TXT")

;;read files
(define accounts (read-accounts "ACCOUNTS.TXT"))
(define transactions (read-transactions "TRANSACTIONS.TXT"))


;;misc
(define (sum lst)
  (if (null? lst)
      0
      (+ (car lst) (sum (cdr lst)))))

(define (format-float num)
  (string-append (~r num #:precision '(= 2))))



;; Function to generate the statement for an account
(define (generate-statement account transactions)
  (define account-transactions (filter (lambda (t) (= (Transaction-account-number t) (account-number account))) transactions))
  (define purchases (filter (lambda (t) (string=? (Transaction-transaction-type t) "Purchase")) account-transactions))
  (define payments (filter (lambda (t) (string=? (Transaction-transaction-type t) "Payment")) account-transactions))
  (define total-purchases (if (empty? purchases) 0 (sum (map Transaction-amount purchases))))
  (define total-payments (if (empty? payments) 0 (sum (map Transaction-amount payments))))
  (define ending-balance (+ (account-balance account) total-purchases (- total-payments)))

  ;;Formatting
  (string-append
   "STATEMENT OF ACCOUNT\n"
   (number->string(account-number account)) "        " (~a(account-name account)#:max-width 40) "        Starting Balance:   " (format-float(account-balance account)) "\n\n"
   (apply string-append
          (map (lambda (t)
                 (string-append (number->string(Transaction-transaction-id t)) "   " (~a(Transaction-transaction-type t)#:min-width 10 #:align 'left) "      " (~a(string-replace(Transaction-description t)"\"" "")#:min-width 20 #:align 'left) "                  " (~a(format-float (Transaction-amount t))#:align 'right #:min-width 10) "\n"))
               account-transactions))
   "\nTotal Purchases:        " (~a(format-float total-purchases)#:min-width 10 #:align 'right)
   "\nTotal Payments:         " (~a(format-float total-payments)#:min-width 10 #:align 'right)
   "\nEnding Balance:         "  (~a(format-float ending-balance)#:min-width 10 #:align 'right)
   "\n\n*********************************************************\n"))


;; Function to write the statements to a file
(define (write-statements file-path)
  (define statements (apply string-append (map (lambda (account) (generate-statement account transactions)) accounts)))
  (call-with-output-file file-path
    #:exists 'replace
    (lambda (out) (display statements out))))

; Write the statements to a file
(write-statements "statement.txt")