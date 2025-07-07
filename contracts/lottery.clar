;; title: BScoin Lottery
;; version: 1.0
;; summary: A decentralized lottery where users buy tickets with BScoin and one winner takes 90% of the pool.

(define-constant contract 'ST390VFVZJA4WP7QSZN0RTSGQDAG2P9NPN3X1ATDX.BScoin)
(use-trait sip-010-trait 'ST1NXBK3K5YYMD6FD41MVNP3JS1GABZ8TRVX023PT.sip-010-trait-ft-standard.sip-010-trait)

;;variable
(define-data-var lottery-active bool false)
(define-data-var lottery-start-block uint u0)
(define-data-var lottery-end-block uint u0)
(define-data-var ticket-price uint u1000000) ;; 1 bscoin = 1000000( 6 decimals)
(define-data-var fee-percentage uint u10)
(define-data-var owner principal tx-sender)