;; title: Lottery Contract
;; version: 1
;; summary: A lottery game using BScoin tokens

(define-constant contract 'ST390VFVZJA4WP7QSZN0RTSGQDAG2P9NPN3X1ATDX.BScoin)
(use-trait sip-010-trait 'ST1NXBK3K5YYMD6FD41MVNP3JS1GABZ8TRVX023PT.sip-010-trait-ft-standard.sip-010-trait)

;; Constants
(define-constant err-not-owner (err u100))
(define-constant err-lottery-active (err u101))
(define-constant err-lottery-not-active (err u102))
(define-constant err-not-enough-tickets (err u103))
(define-constant err-already-participated (err u104))
(define-constant err-invalid-amount (err u105))
(define-constant err-lottery-not-ended (err u106))
(define-constant err-no-winner (err u107))
(define-constant err-no-funds (err u108))
(define-constant err-cannot-terminate (err u109))

;; Data variables
(define-data-var owner principal tx-sender)
(define-data-var lottery-active bool false)
(define-data-var ticket-price uint u0)
(define-data-var end-block-height uint u0)
(define-data-var treasury uint u0)
(define-data-var total-tickets uint u0)

;; Maps
(define-map tickets uint principal)  ;; ticket number -> participant
(define-map participants principal uint)  ;; participant -> ticket number
;; (define-map next-ticket uint u1)  ;; counter for next ticket number

;; Read-only functions
(define-read-only (get-owner) (ok (var-get owner)))
(define-read-only (is-lottery-active) (ok (var-get lottery-active)))
(define-read-only (get-ticket-price) (ok (var-get ticket-price)))
(define-read-only (get-end-block) (ok (var-get end-block-height)))
(define-read-only (get-treasury) (ok (var-get treasury)))
(define-read-only (get-total-tickets) (ok (var-get total-tickets)))
(define-read-only (get-ticket-owner (ticket-number uint)) 
  (ok (map-get? tickets ticket-number)))
(define-read-only (get-user-ticket (user principal))
  (ok (map-get? participants user)))
  

;; Start a new lottery (owner only)
(define-public (start-lottery (price uint) (duration uint))
  (begin
    (asserts! (is-eq tx-sender (var-get owner)) err-not-owner)
    (asserts! (not (var-get lottery-active)) err-lottery-active)
    (asserts! (> price u0) err-invalid-amount)
    (asserts! (> duration u0) err-invalid-amount)
    
    (var-set lottery-active true)
    (var-set ticket-price price)
    (var-set end-block-height (+ stacks-block-height duration))
    (var-set total-tickets u0)
    
    (ok true)
  )
)

;; Buy a lottery ticket
(define-public (buy-ticket)
  (let (
      (current-price (var-get ticket-price))
      (active (var-get lottery-active))
      (end-block (var-get end-block-height))
    )
    (begin
      (asserts! active err-lottery-not-active)
      (asserts! (<= stacks-block-height end-block) err-lottery-not-active)
      (asserts! (is-none (map-get? participants tx-sender)) err-already-participated)
      
      ;; Transfer tokens from user to contract
      (try! (contract-call? contract transfer current-price tx-sender (as-contract tx-sender) none))
      
      ;; Assign ticket
      (let ((ticket-num (+ (var-get total-tickets) u1)))
        (map-set tickets ticket-num tx-sender)
        (map-set participants tx-sender ticket-num)
        (var-set total-tickets ticket-num)
        (ok ticket-num)
      )
    )
  )
)

;; Declare winner (can be called by anyone after lottery ends)
(define-public (declare-winner)
  (let (
      (active (var-get lottery-active))
      (end-block (var-get end-block-height))
      (total-tickets-sold (var-get total-tickets))
    )
    (begin
      (asserts! active err-lottery-not-active)
      (asserts! (> stacks-block-height end-block) err-lottery-not-ended)
      (asserts! (> total-tickets-sold u0) err-no-winner)

      (let (
          (block-hash (unwrap! (get-stacks-block-info? id-header-hash (- stacks-block-height u1)) err-no-winner))
      )
      (let (
            (truncated-hash (unwrap! (slice? block-hash u0 u16) err-no-winner))
      )

      (let (
          (hash-uint (buff-to-uint-le truncated-hash))
      )
      
      (let (
          (winning-ticket (+ (mod hash-uint total-tickets-sold) u1))
      )

      (let ((winner (unwrap! (map-get? tickets winning-ticket) err-no-winner)))
          (let (
              (total-prize (* (var-get ticket-price) total-tickets-sold))
              (winner-prize (/ (* total-prize u90) u100))
              (treasury-cut (/ (* total-prize u10) u100))
            )
            ;; Transfer 90% to winner
            (try! (as-contract (contract-call? contract transfer winner-prize (as-contract tx-sender) winner none)))
            
            ;; Add 10% to treasury
            (var-set treasury (+ (var-get treasury) treasury-cut))
            
            ;; Reset lottery
            (var-set lottery-active false)
            (var-set ticket-price u0)
            (var-set end-block-height u0)
            
            (ok (tuple (winner winner) (ticket winning-ticket) (amount winner-prize)))
          )
        )
      )
   
      )
      
      )
      
      )
    )

  )
)




;; Terminate lottery early (owner only) - refunds participants
(define-public (terminate-lottery)
  (let (
      (active (var-get lottery-active))
      (end-block (var-get end-block-height))
      (total-tickets-sold (var-get total-tickets))
      (refund-amount (var-get ticket-price))
    )
    (begin
      (asserts! (is-eq tx-sender (var-get owner)) err-not-owner)
      (asserts! active err-lottery-not-active)
      (asserts! (< total-tickets-sold u3) err-cannot-terminate)  ;; Only allow termination if less than 3 tickets sold
      
      ;; Refund all participants
      (if (> total-tickets-sold u0)
        (begin
          (let ((user1 (unwrap! (map-get? tickets u1) err-no-winner)))
            (try! (as-contract (contract-call? contract transfer refund-amount (as-contract tx-sender) user1 none)))
            (map-delete participants user1)
          )
          (if (> total-tickets-sold u1)
            (begin
              (let ((user2 (unwrap! (map-get? tickets u2) err-no-winner)))
                (try! (as-contract (contract-call? contract transfer refund-amount (as-contract tx-sender) user2 none)))
                (map-delete participants user2)
              )
              none
            )
            none
          )
        )
        none
      )
      
      ;; Reset lottery
      (var-set lottery-active false)
      (var-set ticket-price u0)
      (var-set end-block-height u0)
      (var-set total-tickets u0)
      
      (ok true)
    )
  ))

;; Withdraw treasury funds (owner only)
(define-public (withdraw-treasury (amount uint))
  (begin
    (asserts! (is-eq tx-sender (var-get owner)) err-not-owner)
    (asserts! (<= amount (var-get treasury)) err-no-funds)
    
    (var-set treasury (- (var-get treasury) amount))
    (as-contract (contract-call? contract transfer amount (as-contract tx-sender) tx-sender none))
  )
)

;; Transfer ownership (owner only)
(define-public (transfer-ownership (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get owner)) err-not-owner)
    (var-set owner new-owner)
    (ok true)
  )
)