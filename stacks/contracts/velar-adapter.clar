;; velar-adapter.clar
;; Adapter for Velar DEX integration
;; Implements dex-adapter-trait for swapping xUSDC -> USDCx

(impl-trait .dex-adapter-trait.dex-adapter-trait)

;; ============================================
;; CONSTANTS
;; ============================================

(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-SWAP-FAILED (err u501))
(define-constant ERR-INSUFFICIENT-OUTPUT (err u502))
(define-constant ERR-POOL-NOT-FOUND (err u503))
(define-constant ERR-NOT-CONFIGURED (err u504))
(define-constant ERR-INVALID-TOKEN (err u505))
(define-constant ERR-PAUSED (err u506))
(define-constant ERR-INVALID-SLIPPAGE (err u507))
(define-constant ERR-INSUFFICIENT-ALLOWANCE (err u508))
(define-constant ERR-TRANSFER-FAILED (err u509))

;; Velar Mainnet Router Contract
(define-constant VELAR-MAINNET-ROUTER 'SP1Y5YSTAHZ88XYK1VPDH24GY0HPX5J4JECTMY4A1)

;; Velar Router Functions
(define-constant SWAP-EXACT-TOKENS-FOR-TOKENS "swap-exact-tokens-for-tokens")

;; ============================================
;; DATA VARIABLES (Keep as is)
;; ============================================

(define-data-var router-contract principal VELAR-MAINNET-ROUTER)
(define-data-var router-configured bool false)
(define-data-var contract-owner principal tx-sender)
(define-data-var xusdc-token-contract principal tx-sender)
(define-data-var usdcx-token-contract principal USDCX-MAINNET)
(define-data-var pool-id uint u0)
(define-data-var pool-configured bool false)
(define-data-var slippage-tolerance uint DEFAULT-SLIPPAGE-TOLERANCE)
(define-data-var paused bool false)

;; ============================================
;; ADMIN FUNCTIONS (Keep as is)
;; ============================================

;; ... (keep all admin functions unchanged)

;; ============================================
;; UPDATED DEX ADAPTER IMPLEMENTATION
;; ============================================

(define-public (swap-exact-tokens
  (amount-in uint)
  (min-amount-out uint)
  (token-in principal)
  (token-out principal))
  (let (
    (expected-out (calculate-output-amount amount-in))
    (actual-min-out (calculate-min-output amount-in))
  )
    (asserts! (not (var-get paused)) ERR-PAUSED)
    (asserts! (var-get router-configured) ERR-NOT-CONFIGURED)
    (asserts! (var-get pool-configured) ERR-NOT-CONFIGURED)

    (asserts! (is-eq token-in (var-get xusdc-token-contract)) ERR-INVALID-TOKEN)
    (asserts! (is-eq token-out (var-get usdcx-token-contract)) ERR-INVALID-TOKEN)

    ;; Enforce adapter slippage protection
    (asserts! (>= min-amount-out actual-min-out) ERR-INSUFFICIENT-OUTPUT)

    ;; 1. Transfer xUSDC from user to this contract
    (try! (ft-transfer?
      (var-get xusdc-token-contract)
      amount-in
      tx-sender
      (as-contract tx-sender)
      none
    ))

    ;; 2. Approve Velar router to spend xUSDC
    (try! (as-contract
      (ft-transfer?
        (var-get xusdc-token-contract)
        amount-in
        tx-sender
        (var-get router-contract)
        none
      )
    ))

    ;; 3. Execute swap through Velar router
    (match (try! (contract-call?
      (var-get router-contract)
      SWAP-EXACT-TOKENS-FOR-TOKENS
      (var-get pool-id)
      (var-get xusdc-token-contract)
      (var-get usdcx-token-contract)
      amount-in
      actual-min-out
      tx-sender
    )) as (swap-result { amount-out: uint })
      ;; 4. Verify swap output meets minimum
      (asserts! (>= amount-out min-amount-out) ERR-INSUFFICIENT-OUTPUT)
      
      ;; Log successful swap
      (print {
        event: "swap-executed",
        router: (var-get router-contract),
        pool-id: (var-get pool-id),
        token-in: token-in,
        token-out: token-out,
        amount-in: amount-in,
        amount-out: amount-out,
        min-amount-out: min-amount-out,
        slippage: (var-get slippage-tolerance),
        sender: tx-sender
      })
      
      (ok amount-out)
    )
  )
)

;; ============================================
;; UPDATED QUOTE FUNCTION (Query actual pool)
;; ============================================

(define-read-only (get-swap-quote
  (amount-in uint)
  (token-in principal)
  (token-out principal))
  (begin
    (asserts! (is-eq token-in (var-get xusdc-token-contract)) ERR-INVALID-TOKEN)
    (asserts! (is-eq token-out (var-get usdcx-token-contract)) ERR-INVALID-TOKEN)
    
    ;; Try to get quote from actual Velar pool
    (match (contract-call?
      (var-get router-contract)
      "get-amounts-out"
      (var-get pool-id)
      (var-get xusdc-token-contract)
      (var-get usdcx-token-contract)
      amount-in
    ) as (quote-response { amounts: (list 2 uint) })
      (ok (element-at amounts u1))
      ;; Fallback to local calculation if router doesn't respond
      (ok (calculate-output-amount amount-in))
    )
  )
)

;; ============================================
;; NEW: EMERGENCY WITHDRAW FUNCTION
;; ============================================

(define-public (withdraw-tokens
  (token principal)
  (amount uint)
  (recipient principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    
    ;; Transfer tokens from contract to recipient
    (try! (as-contract
      (ft-transfer?
        token
        amount
        tx-sender
        recipient
        none
      )
    ))
    
    (print {
      event: "tokens-withdrawn",
      token: token,
      amount: amount,
      recipient: recipient
    })
    
    (ok true)
  )
)

;; ============================================
;; HELPER FUNCTIONS (Keep as is)
;; ============================================

(define-private (calculate-output-amount (amount-in uint))
  (/ (* amount-in FEE-NUMERATOR) FEE-DENOMINATOR))

(define-private (calculate-min-output (amount-in uint))
  (let (
    (expected (calculate-output-amount amount-in))
    (slippage (var-get slippage-tolerance))
  )
    (/ (* expected (- SLIPPAGE-DENOMINATOR slippage)) SLIPPAGE-DENOMINATOR)))

;; ============================================
;; ADDITIONAL VIEW FUNCTIONS
;; ============================================

(define-read-only (get-pool-info)
  (ok {
    pool-id: (var-get pool-id),
    xusdc-token: (var-get xusdc-token-contract),
    usdcx-token: (var-get usdcx-token-contract),
    configured: (var-get pool-configured)
  })
)

(define-read-only (get-token-balance (token principal))
  (ft-get-balance token tx-sender)
)
;; ============================================
;; VIEW FUNCTIONS
;; ============================================

(define-read-only (get-router-contract) (var-get router-contract))
(define-read-only (is-router-configured) (var-get router-configured))
(define-read-only (is-pool-configured) (var-get pool-configured))
(define-read-only (get-slippage-tolerance) (var-get slippage-tolerance))
(define-read-only (get-paused-status) (var-get paused))
