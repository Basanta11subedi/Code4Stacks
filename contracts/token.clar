
;; title: token
;; version:1
;; summary:This is the fungible token for the code4stx project 


;; traits
(impl-trait 'ST1NXBK3K5YYMD6FD41MVNP3JS1GABZ8TRVX023PT.sip-010-trait-ft-standard.sip-010-trait);;

;; token definitions
(define-fungible-token BScoin u1000000000000)
;;

;; constants
(define-constant err-not-token-owner (err u101))
(define-constant err-insufficient-amount (err u102))
(define-constant err-owner-only (err u103))

;;

;; data vars
(define-data-var contract-owner principal  tx-sender)
;;

;; data maps
;;

;; public functions
;;

;; read only functions
(define-read-only (get-name) (ok "BScoin"))
(define-read-only (get-symbol) (ok "BSC"))
(define-read-only (get-decimals) (ok u6))
(define-read-only (get-balance (who principal)) (ok (ft-get-balance BScoin who)))
(define-read-only (get-token-uri) (ok none))
(define-read-only (get-total-supply) (ok (ft-get-supply BScoin)))

;; Public function
(define-public (transfer (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34))))
    (begin
       (asserts! (is-eq tx-sender  sender) err-not-token-owner)
       (asserts! (> amount u0) err-insufficient-amount)
       (try! (ft-transfer? BScoin amount sender recipient))
       (match memo to-print (print to-print) 0x)
       (ok true)
    )
)

(define-public (mint (amount uint) (recipient principal))
    (begin 
        (asserts! (is-eq tx-sender (var-get contract-owner)) err-owner-only)
        (ft-mint? BScoin amount recipient) 
    )
)

(define-public (burn (amount uint) (recipient principal))
    (begin 
        (asserts! (is-eq tx-sender (var-get contract-owner)) err-owner-only)
        (ft-burn? BScoin amount recipient) 
    )
)
