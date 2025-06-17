
;; title: token
;; version:1
;; summary:This is the fungible token for the code4stx project 


;; traits
(impl-trait 'ST1NXBK3K5YYMD6FD41MVNP3JS1GABZ8TRVX023PT.sip-010-trait-ft-standard.sip-010-trait);;

;; Token definitions
(define-fungible-token BScoin u1000000000000)
;;

;; constants
(define-constant err-not-token-owner (err u101))
(define-constant err-insufficient-amount (err u102))
(define-constant err-owner-only (err u103))
(define-constant err-blacklisted (err u104))

;;

;; data variables
(define-data-var contract-owner principal  tx-sender)
;;

;;Map
(define-map blacklist principal bool)


;; Read only functions
(define-read-only (get-name) (ok "BScoin"))
(define-read-only (get-symbol) (ok "BSC"))
(define-read-only (get-decimals) (ok u6))
(define-read-only (get-balance (who principal)) (ok (ft-get-balance BScoin who)))
(define-read-only (get-token-uri) (ok none))
(define-read-only (get-total-supply) (ok (ft-get-supply BScoin)))
(define-read-only (get-contract-owner) (ok (var-get contract-owner)))

;; Public function

;;Function to transfer the token
(define-public (transfer (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34))))
    (begin
       (asserts! (is-eq tx-sender  sender) err-not-token-owner)
       (asserts! (not (default-to false (map-get? blacklist sender))) err-blacklisted)
       (asserts! (not (default-to false (map-get? blacklist recipient))) err-blacklisted)
       (asserts! (> amount u0) err-insufficient-amount)
       (try! (ft-transfer? BScoin amount sender recipient))
       (print { event-type: "Transfer", amount: amount, sender: sender, recipient: recipient })
       (match memo to-print (print to-print) 0x)
       (ok true)
    )
)

;;Function to mint the tokens (only by owner)
(define-public (mint (amount uint) (recipient principal))
    (begin 
        (asserts! (is-eq tx-sender (var-get contract-owner)) err-owner-only)
        (asserts! (not (default-to false (map-get? blacklist recipient))) err-blacklisted)
        (asserts! (> amount u0) err-insufficient-amount)
        (try! (ft-mint? BScoin amount recipient)) 
        (print { event-type: "Mint", amount: amount, recipient: recipient})
        (ok true)
    )
)

;; Function to burn the tokens
(define-public (burn (amount uint))
    (begin 
        (asserts! (> amount u0) err-insufficient-amount)
        (try! (ft-burn? BScoin amount tx-sender))
        (print { event-type: "burn", amount: amount, recipient: tx-sender})
        (ok true)

    )
)

;;Transfer ownership
(define-public (transfer-ownership (new-owner principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) err-owner-only)
        (var-set contract-owner new-owner)
        (ok true)
    )
)

;; Function to add in the whitelist by the owner
(define-public (add-to-blacklist (user principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) err-owner-only)
        (map-set blacklist user true)
        (ok true)
    )
)

;; Function to remove from blacklist by owner
(define-public (remove-from-blacklist (user principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) err-owner-only)
        (map-delete blacklist user)
        (ok true)
    )
)