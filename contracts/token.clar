;; title: token
;; version:2
;; summary:This is the fungible token for the code4stx project 

;; traits
(impl-trait 'ST1NXBK3K5YYMD6FD41MVNP3JS1GABZ8TRVX023PT.sip-010-trait-ft-standard.sip-010-trait)

;; Token definitions
(define-fungible-token BScoin u1000000000000)

;; constants
(define-constant err-not-token-owner (err u101))
(define-constant err-insufficient-amount (err u102))
(define-constant err-owner-only (err u103))
(define-constant err-not-whitelisted (err u104))
(define-constant err-kyc-not-found (err u105))
(define-constant err-kyc-already-approved (err u106))
(define-constant err-kyc-not-pending (err u107))

;; data variables
(define-data-var contract-owner principal tx-sender)

;; Maps
(define-map whitelist principal bool)
(define-map kyc-submissions principal {ipfs-hash: (string-utf8 100), status: (string-utf8 20)})

;; Read only functions
(define-read-only (get-name) (ok "BScoin"))
(define-read-only (get-symbol) (ok "BSC"))
(define-read-only (get-decimals) (ok u6))
(define-read-only (get-balance (who principal)) (ok (ft-get-balance BScoin who)))
(define-read-only (get-token-uri) (ok none))
(define-read-only (get-total-supply) (ok (ft-get-supply BScoin)))
(define-read-only (get-contract-owner) (ok (var-get contract-owner)))
(define-read-only (get-kyc-status (user principal)) (ok (map-get? kyc-submissions user)))

;; Public functions

;; Function to transfer the token
(define-public (transfer (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34))))
    (begin
       (asserts! (is-eq tx-sender sender) err-not-token-owner)
       (asserts! (default-to false (map-get? whitelist sender)) err-not-whitelisted)
       (asserts! (default-to false (map-get? whitelist recipient)) err-not-whitelisted)
       (asserts! (> amount u0) err-insufficient-amount)
       (try! (ft-transfer? BScoin amount sender recipient))
       (print { event-type: "Transfer", amount: amount, sender: sender, recipient: recipient })
       (match memo to-print (print to-print) 0x)
       (ok true)
    )
)

;; Function to mint the tokens (only by owner)
(define-public (mint (amount uint) (recipient principal))
    (begin 
        (asserts! (is-eq tx-sender (var-get contract-owner)) err-owner-only)
        (asserts! (default-to false (map-get? whitelist recipient)) err-not-whitelisted)
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

;; Transfer ownership
(define-public (transfer-ownership (new-owner principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) err-owner-only)
        (var-set contract-owner new-owner)
        (ok true)
    )
)

;; Function to submit KYC documents (CID from IPFS)
(define-public (submit-kyc (ipfs-hash (string-utf8 100)))
  (begin
    (let ((empty-kyc (tuple (ipfs-hash u"")  (status u"none"))))
    (asserts! (is-eq (get status (default-to empty-kyc (map-get? kyc-submissions tx-sender))) u"none") err-kyc-already-approved))
    (map-set kyc-submissions tx-sender (tuple (ipfs-hash ipfs-hash) (status u"pending")))
    (print (tuple (event-type u"KYCSubmitted") (user tx-sender) (ipfs-hash ipfs-hash)))
    (ok true)
  )
)

;; Function to approve KYC (admin only)
(define-public (approve-kyc (user principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) err-owner-only)
    (let ((kyc-data (unwrap! (map-get? kyc-submissions user) err-kyc-not-found)))
      (asserts! (is-eq (get status kyc-data) u"pending") err-kyc-not-pending)
      (map-set kyc-submissions user (tuple (ipfs-hash (get ipfs-hash kyc-data)) (status u"approved")))
      (map-set whitelist user true)
      (print {event-type: "KYCApproved", user: user, by: tx-sender})
      (ok true)
    )
  )
)

;; Function to reject KYC (admin only)
(define-public (reject-kyc (user principal) (reason (string-utf8 1000)))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) err-owner-only)
    (let ((kyc-data (unwrap! (map-get? kyc-submissions user) err-kyc-not-found)))
      (asserts! (is-eq (get status kyc-data) u"pending") err-kyc-not-pending)
      (map-set kyc-submissions user (tuple (ipfs-hash (get ipfs-hash kyc-data)) (status u"rejected")))
      (print {event-type: "KYCRejected", user: user, by: tx-sender, reason: reason})
      (ok true)
    )
  )
)

;; Function to remove from whitelist (admin only)
(define-public (remove-from-whitelist (user principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) err-owner-only)
    (map-delete whitelist user)
    (ok true)
  )
)
;; ;; Function to add to whitelist (admin only)
;; (define-public (add-to-whitelist (user principal))
;;   (begin
;;     (asserts! (is-eq tx-sender (var-get admin)) err-not-admin)
;;     (map-set whitelist user true)
;;     (ok true)
;;   )
;; )

;; ;; Function to add in the blacklist by the owner
;; (define-public (add-to-blacklist (user principal))
;;     (begin
;;         (asserts! (is-eq tx-sender (var-get contract-owner)) err-owner-only)
;;         (map-set blacklist user true)
;;         (ok true)
;;     )
;; )

;; ;; Function to remove from blacklist by owner
;; (define-public (remove-from-blacklist (user principal))
;;     (begin
;;         (asserts! (is-eq tx-sender (var-get contract-owner)) err-owner-only)
;;         (map-delete blacklist user)
;;         (ok true)
;;     )
;; )