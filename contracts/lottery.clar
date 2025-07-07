;; title: BScoin Lottery
;; version: 1.0
;; summary: A decentralized lottery where users buy tickets with BScoin and one winner takes 90% of the pool.

(define-constant contract 'ST390VFVZJA4WP7QSZN0RTSGQDAG2P9NPN3X1ATDX.BScoin)
(use-trait sip-010-trait 'ST1NXBK3K5YYMD6FD41MVNP3JS1GABZ8TRVX023PT.sip-010-trait-ft-standard.sip-010-trait)

;;errors
(define-constant err-lottery-not-active (err u501))
(define-constant err-lottery-active (err u502))
(define-constant err-not-enough-tokens (err u503))
(define-constant err-lottery-ended (err u504))
(define-constant err-no-tickets (err u505))
(define-constant err-not-owner (err u506))

;;variable
(define-data-var lottery-active bool false)
(define-data-var lottery-start-block uint u0)
(define-data-var lottery-end-block uint u0)
(define-data-var ticket-price uint u1000000) ;; 1 bscoin = 1000000( 6 decimals)
(define-data-var fee-percentage uint u10)
(define-data-var owner principal tx-sender)
(define-data-var total-tickets uint u0)
(define-data-var last-winner principal tx-sender)
(define-data-var last-prize uint u0)

;; maping
(define-map tickets principal uint) ;; this tracks tickets per user
(define-map ticket-entries { user: principal, ticket-number: uint } principal) ;; this maps ticket number to users

;; Public functions

;;start a new lottery which will be done by owner only
(define-public (start-lottery (duration uint)) ;; durationn in blocks (1 block ~ 10-30 sec)
    (begin 
        (asserts! (is-eq tx-sender (var-get owner)) err-not-owner)
        (asserts! (not (var-get lottery-active)) err-lottery-active)
        (var-set lottery-active true)
        (var-set lottery-start-block stacks-block-height)
        (var-set lottery-end-block (+ stacks-block-height duration))
        (var-set total-tickets u0)
        (ok true)
    
    )
)