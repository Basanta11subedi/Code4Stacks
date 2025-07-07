;; Title: BScoin/STX Liquidity Pool
;; Traits: Implement SIP-010 (fungible token) for LP tokens

(define-constant bscoin-contract 'ST390VFVZJA4WP7QSZN0RTSGQDAG2P9NPN3X1ATDX.BScoin)

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-insufficient-balance (err u101))
(define-constant err-insufficient-liquidity (err u102))
(define-constant err-zero-amount (err u103))
(define-constant err-slippage-too-high (err u104))
(define-constant err-deadline-expired (err u105))
(define-constant err-invalid-token (err u106))
(define-constant err-not-authorized (err u107))

;; Fee is 0.3% (30 basis points) - industry standard
(define-constant fee-numerator u3)
(define-constant fee-denominator u1000)

;; Define fungible tokens for LP tokens
(define-fungible-token stx-bscoin-lp)

;; Define data variables
(define-data-var total-stx uint u0)
(define-data-var total-bscoin uint u0)
(define-data-var last-price-cumulative uint u0)
(define-data-var last-block-height uint u0)

;; Helper functions for math calculations

;; Calculate the amount of tokens to be received in a swap
(define-read-only (get-amount-out (amount-in uint) (reserve-in uint) (reserve-out uint))
  (let
    (
      (amount-in-with-fee (* amount-in (- fee-denominator fee-numerator)))
      (numerator (* amount-in-with-fee reserve-out))
      (denominator (+ (* reserve-in fee-denominator) amount-in-with-fee))
    )
    (/ numerator denominator)
  )
)

;; Calculate the amount of tokens needed to be sent for desired output
(define-read-only (get-amount-in (amount-out uint) (reserve-in uint) (reserve-out uint))
  (let
    (
      (numerator (* reserve-in amount-out fee-denominator))
      (denominator (* (- reserve-out amount-out) (- fee-denominator fee-numerator)))
    )
    (+ (/ numerator denominator) u1)
  )
)



;; Calculate the minimum of two numbers
(define-read-only (min (a uint) (b uint))
  (if (<= a b) a b)
)

;; Get current reserves
(define-read-only (get-reserves)
  (ok {
    stx-reserve: (var-get total-stx),
    bscoin-reserve: (var-get total-bscoin)
  })
)

;; Get LP token total supply
(define-read-only (get-lp-token-supply)
  (ok (ft-get-supply stx-bscoin-lp))
)

;; Get user's LP token balance
(define-read-only (get-lp-balance (user principal))
  (ok (ft-get-balance stx-bscoin-lp user))
)

(define-public (add-liquidity 
  (stx-amount uint) 
  (bscoin-amount uint) 
  (min-stx-amount uint) 
  (min-bscoin-amount uint)
  (deadline uint)
  (recipient principal)
)
  (let (
    (stx-reserve (var-get total-stx))
    (bscoin-reserve (var-get total-bscoin))
    (total-liquidity (ft-get-supply stx-bscoin-lp))
  )
  (begin
    ;; Check deadline expiration
    (asserts! (<= stacks-block-height deadline) err-deadline-expired)
    
    ;; Validate input amounts
    (asserts! (and (> stx-amount u0) (> bscoin-amount u0)) err-zero-amount)
    
    (if (is-eq total-liquidity u0)
      ;; Initial liquidity provision
      (let (
        (liquidity-minted (sqrti (* stx-amount bscoin-amount)))
      )
      (begin
        ;; Verify minimum amounts for initial deposit
        (asserts! (and (>= stx-amount min-stx-amount) 
                      (>= bscoin-amount min-bscoin-amount)) 
                  err-slippage-too-high)
        
        ;; Update reserves
        (var-set total-stx stx-amount)
        (var-set total-bscoin bscoin-amount)
        
        ;; Mint LP tokens
        (try! (ft-mint? stx-bscoin-lp liquidity-minted recipient))
        
        ;; Return success with liquidity details
        (ok { 
          liquidity-minted: liquidity-minted, 
          stx-amount: stx-amount, 
          bscoin-amount: bscoin-amount 
        })
      ))
      ;; Subsequent liquidity additions
      (let (
        (stx-optimal (/ (* bscoin-amount stx-reserve) bscoin-reserve))
        (bscoin-optimal (/ (* stx-amount bscoin-reserve) stx-reserve))
        (liquidity-minted 
          (if (<= stx-optimal stx-amount)
            (/ (* stx-optimal total-liquidity) stx-reserve)
            (/ (* bscoin-optimal total-liquidity) bscoin-reserve)
          )
        )
      )
      (begin
        ;; Verify minimum amounts based on optimal ratio
        (asserts! 
          (if (<= stx-optimal stx-amount)
            (>= stx-optimal min-stx-amount)
            (>= bscoin-optimal min-bscoin-amount)
          )
          err-slippage-too-high
        )
        
        ;; Update reserves
        (var-set total-stx 
          (if (<= stx-optimal stx-amount)
            (+ stx-reserve stx-optimal)
            (+ stx-reserve stx-amount)
          )
        )
        (var-set total-bscoin 
          (if (<= stx-optimal stx-amount)
            (+ bscoin-reserve bscoin-amount)
            (+ bscoin-reserve bscoin-optimal)
          )
        )
        
        ;; Transfer tokens from user
        (try! (stx-transfer? stx-amount tx-sender (as-contract tx-sender)))
        (try! (contract-call? bscoin-contract transfer bscoin-amount tx-sender (as-contract tx-sender) none))
        
        ;; Mint LP tokens
        (try! (ft-mint? stx-bscoin-lp liquidity-minted recipient))
        
        ;; Update price tracking
        (var-set last-block-height stacks-block-height)
        
        (ok { 
          liquidity-minted: liquidity-minted,
          stx-amount: (if (<= stx-optimal stx-amount) stx-optimal stx-amount),
          bscoin-amount: (if (<= stx-optimal stx-amount) bscoin-amount bscoin-optimal)
        })
      ))
    )
  )
))

;; Remove liquidity from the pool
(define-public (remove-liquidity
  (liquidity uint)
  (min-stx-amount uint)
  (min-bscoin-amount uint)
  (deadline uint)
  (recipient principal)
)
  (let
    (
      (stx-reserve (var-get total-stx))
      (bscoin-reserve (var-get total-bscoin))
      (total-liquidity (ft-get-supply stx-bscoin-lp))
      (stx-amount (/ (* liquidity stx-reserve) total-liquidity))
      (bscoin-amount (/ (* liquidity bscoin-reserve) total-liquidity))
    )
    
    ;; Check deadline
    (asserts! (<= stacks-block-height deadline) err-deadline-expired)
    
    ;; Check liquidity > 0
    (asserts! (> liquidity u0) err-zero-amount)
    
    ;; Check slippage tolerance
    (asserts! (and (>= stx-amount min-stx-amount) (>= bscoin-amount min-bscoin-amount)) err-slippage-too-high)
    
    ;; Burn LP tokens
    (try! (ft-burn? stx-bscoin-lp liquidity tx-sender))
    
    ;; Update reserves
    (var-set total-stx (- stx-reserve stx-amount))
    (var-set total-bscoin (- bscoin-reserve bscoin-amount))
    
    ;; Transfer tokens to recipient
    (try! (as-contract (stx-transfer? stx-amount tx-sender recipient)))
    (try! (as-contract (contract-call? bscoin-contract transfer bscoin-amount tx-sender recipient none)))
    
    ;; Update last price info
    (var-set last-block-height stacks-block-height)
    
    (ok {
      stx-amount: stx-amount,
      bscoin-amount: bscoin-amount,
      liquidity-burned: liquidity
    })
  )
)

;; Swap STX for bscoin
(define-public (swap-stx-for-bscoin
  (stx-amount uint)
  (min-bscoin-out uint)
  (deadline uint)
  (recipient principal)
)
  (let
    (
      (stx-reserve (var-get total-stx))
      (bscoin-reserve (var-get total-bscoin))
      (bscoin-amount (get-amount-out stx-amount stx-reserve bscoin-reserve))
    )
    
    ;; Check deadline
    (asserts! (<= stacks-block-height deadline) err-deadline-expired)
    
    ;; Check amount > 0
    (asserts! (> stx-amount u0) err-zero-amount)
    
    ;; Check slippage tolerance
    (asserts! (>= bscoin-amount min-bscoin-out) err-slippage-too-high)
    
    ;; Update reserves
    (var-set total-stx (+ stx-reserve stx-amount))
    (var-set total-bscoin (- bscoin-reserve bscoin-amount))
    
    ;; Transfer tokens
    (try! (stx-transfer? stx-amount tx-sender (as-contract tx-sender)))
    (try! (as-contract (contract-call? bscoin-contract transfer bscoin-amount tx-sender recipient none)))
    
    ;; Update price accumulator
    (var-set last-block-height stacks-block-height)
    
    (ok { bscoin-amount: bscoin-amount })
  )
)

;; Swap bscoin for STX
(define-public (swap-bscoin-for-stx
  (bscoin-amount uint)
  (min-stx-out uint)
  (deadline uint)
  (recipient principal)
)
  (let
    (
      (stx-reserve (var-get total-stx))
      (bscoin-reserve (var-get total-bscoin))
      (stx-amount (get-amount-out bscoin-amount bscoin-reserve stx-reserve))
    )
    
    ;; Check deadline
    (asserts! (<= stacks-block-height deadline) err-deadline-expired)
    
    ;; Check amount > 0
    (asserts! (> bscoin-amount u0) err-zero-amount)
    
    ;; Check slippage tolerance
    (asserts! (>= stx-amount min-stx-out) err-slippage-too-high)
    
    ;; Update reserves
    (var-set total-stx (- stx-reserve stx-amount))
    (var-set total-bscoin (+ bscoin-reserve bscoin-amount))
    
    ;; Transfer tokens
    (try! (contract-call? bscoin-contract transfer bscoin-amount tx-sender (as-contract tx-sender) none))
    (try! (as-contract (stx-transfer? stx-amount tx-sender recipient)))
    
    ;; Update price accumulator
    (var-set last-block-height stacks-block-height)
    
    (ok { stx-amount: stx-amount })
  )
)

;; Initialize the pool
(define-public (initialize-pool)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set last-block-height stacks-block-height)
    (ok true)
  )
)