;; CHRONOS VAULT - TIME-LOCKED STX ESCROW CONTRACT
;; A secure smart contract that enables users to create time-locked STX vaults
;; with customizable unlock periods, partial withdrawals, and multi-vault management.
;; Perfect for savings goals, vesting schedules, and delayed payments.

;; ERROR CONSTANTS
(define-constant ERROR-UNAUTHORIZED-ACCESS (err u1001))
(define-constant ERROR-VAULT-NOT-FOUND (err u1002))
(define-constant ERROR-VAULT-STILL-LOCKED (err u1003))
(define-constant ERROR-INSUFFICIENT-STX-BALANCE (err u1004))
(define-constant ERROR-INVALID-DEPOSIT-AMOUNT (err u1005))
(define-constant ERROR-INVALID-LOCK-DURATION (err u1006))
(define-constant ERROR-VAULT-LIMIT-EXCEEDED (err u1007))
(define-constant ERROR-ZERO-BALANCE-VAULT (err u1008))
(define-constant ERROR-WITHDRAWAL-AMOUNT-EXCEEDS-BALANCE (err u1009))
(define-constant ERROR-INVALID-VAULT-ID (err u1010))
(define-constant ERROR-INVALID-STRING-INPUT (err u1011))

;; CONTRACT CONFIGURATION CONSTANTS
(define-constant CONTRACT-DEPLOYER tx-sender)
(define-constant MAXIMUM-VAULTS-PER-USER u100)
(define-constant MINIMUM-LOCK-BLOCKS u144) ;; ~1 day minimum lock
(define-constant VAULT-CREATION-FEE u1000000) ;; 1 STX fee
(define-constant MAXIMUM-VAULT-ID u999999999) ;; Maximum allowed vault ID

;; CONTRACT STATE VARIABLES
(define-data-var global-vault-counter uint u0)
(define-data-var total-locked-stx-value uint u0)
(define-data-var is-contract-paused bool false)

;; DATA STRUCTURE MAPS
;; Primary vault storage with comprehensive metadata
(define-map time-locked-vault-registry
  { vault-unique-identifier: uint }
  {
    vault-owner-principal: principal,
    locked-stx-amount: uint,
    unlock-block-height: uint,
    creation-block-height: uint,
    last-interaction-block-height: uint,
    vault-description-label: (string-ascii 50)
  }
)

;; User's vault collection for efficient querying
(define-map user-vault-collection-registry
  { account-holder-principal: principal }
  { 
    owned-vault-identifiers-list: (list 100 uint),
    total-user-locked-stx: uint,
    total-vault-count: uint
  }
)

;; Vault activity log for transparency and audit trail
(define-map vault-activity-audit-log
  { vault-unique-identifier: uint, activity-sequence-index: uint }
  {
    transaction-action-type: (string-ascii 20),
    stx-amount-involved: uint,
    block-timestamp: uint,
    transaction-initiator-principal: principal
  }
)

;; INPUT VALIDATION HELPER FUNCTIONS
;; Validate vault ID is within acceptable range
(define-private (is-valid-vault-identifier (vault-identifier-to-check uint))
  (and (> vault-identifier-to-check u0) (<= vault-identifier-to-check MAXIMUM-VAULT-ID))
)

;; Validate vault exists and get its data
(define-private (get-validated-vault-data-record (vault-identifier-to-lookup uint))
  (if (is-valid-vault-identifier vault-identifier-to-lookup)
    (map-get? time-locked-vault-registry { vault-unique-identifier: vault-identifier-to-lookup })
    none
  )
)

;; VAULT CREATION & MANAGEMENT FUNCTIONS
;; Create a new time-locked vault with custom parameters
(define-public (create-new-time-locked-vault 
  (initial-deposit-stx-amount uint) 
  (lock-duration-in-blocks uint) 
  (vault-description-label (string-ascii 50)))
  (let
    (
      (new-vault-unique-identifier (+ (var-get global-vault-counter) u1))
      (calculated-vault-unlock-height (+ stacks-block-height lock-duration-in-blocks))
      (vault-creator-principal tx-sender)
      (existing-user-vault-data (default-to 
        { owned-vault-identifiers-list: (list), total-user-locked-stx: u0, total-vault-count: u0 }
        (map-get? user-vault-collection-registry { account-holder-principal: vault-creator-principal })))
      (vault-description-to-use "Custom Vault")
    )
    
    ;; Comprehensive input validation checks
    (asserts! (not (var-get is-contract-paused)) ERROR-UNAUTHORIZED-ACCESS)
    (asserts! (> initial-deposit-stx-amount u0) ERROR-INVALID-DEPOSIT-AMOUNT)
    (asserts! (>= lock-duration-in-blocks MINIMUM-LOCK-BLOCKS) ERROR-INVALID-LOCK-DURATION)
    (asserts! (>= (stx-get-balance vault-creator-principal) (+ initial-deposit-stx-amount VAULT-CREATION-FEE)) ERROR-INSUFFICIENT-STX-BALANCE)
    (asserts! (< (get total-vault-count existing-user-vault-data) MAXIMUM-VAULTS-PER-USER) ERROR-VAULT-LIMIT-EXCEEDED)
    (asserts! (is-valid-vault-identifier new-vault-unique-identifier) ERROR-INVALID-VAULT-ID)
    
    ;; Process STX transfers for deposit and fee
    (try! (stx-transfer? initial-deposit-stx-amount vault-creator-principal (as-contract tx-sender)))
    (try! (stx-transfer? VAULT-CREATION-FEE vault-creator-principal CONTRACT-DEPLOYER))
    
    ;; Register new vault with validated data
    (map-set time-locked-vault-registry
      { vault-unique-identifier: new-vault-unique-identifier }
      {
        vault-owner-principal: vault-creator-principal,
        locked-stx-amount: initial-deposit-stx-amount,
        unlock-block-height: calculated-vault-unlock-height,
        creation-block-height: stacks-block-height,
        last-interaction-block-height: stacks-block-height,
        vault-description-label: vault-description-to-use
      }
    )
    
    ;; Update user's vault collection registry
    (map-set user-vault-collection-registry
      { account-holder-principal: vault-creator-principal }
      {
        owned-vault-identifiers-list: (unwrap! (as-max-len? 
          (append (get owned-vault-identifiers-list existing-user-vault-data) new-vault-unique-identifier) u100)
          ERROR-VAULT-LIMIT-EXCEEDED),
        total-user-locked-stx: (+ (get total-user-locked-stx existing-user-vault-data) initial-deposit-stx-amount),
        total-vault-count: (+ (get total-vault-count existing-user-vault-data) u1)
      }
    )
    
    ;; Update global contract state variables
    (var-set global-vault-counter new-vault-unique-identifier)
    (var-set total-locked-stx-value (+ (var-get total-locked-stx-value) initial-deposit-stx-amount))
    
    ;; Log vault creation activity with validated ID
    (try! (log-vault-transaction-activity new-vault-unique-identifier u0 "VAULT_CREATED" initial-deposit-stx-amount vault-creator-principal))
    
    (ok new-vault-unique-identifier)
  )
)

;; Deposit additional STX into existing vault
(define-public (deposit-additional-stx-funds (target-vault-identifier uint) (additional-stx-amount uint))
  (let
    (
      (depositor-principal tx-sender)
    )
    
    ;; Initial validation checks for deposit
    (asserts! (not (var-get is-contract-paused)) ERROR-UNAUTHORIZED-ACCESS)
    (asserts! (is-valid-vault-identifier target-vault-identifier) ERROR-INVALID-VAULT-ID)
    (asserts! (> additional-stx-amount u0) ERROR-INVALID-DEPOSIT-AMOUNT)
    (asserts! (>= (stx-get-balance depositor-principal) additional-stx-amount) ERROR-INSUFFICIENT-STX-BALANCE)
    
    (let
      (
        (existing-vault-data (unwrap! (get-validated-vault-data-record target-vault-identifier) ERROR-VAULT-NOT-FOUND))
      )
      
      ;; Additional validation with vault data
      (asserts! (is-eq (get vault-owner-principal existing-vault-data) depositor-principal) ERROR-UNAUTHORIZED-ACCESS)
      
      ;; Transfer additional STX to contract
      (try! (stx-transfer? additional-stx-amount depositor-principal (as-contract tx-sender)))
      
      (let
        (
          (updated-vault-balance (+ (get locked-stx-amount existing-vault-data) additional-stx-amount))
        )
        
        ;; Update vault record with new balance
        (map-set time-locked-vault-registry
          { vault-unique-identifier: target-vault-identifier }
          (merge existing-vault-data { 
            locked-stx-amount: updated-vault-balance,
            last-interaction-block-height: stacks-block-height
          })
        )
        
        ;; Update global total locked value
        (var-set total-locked-stx-value (+ (var-get total-locked-stx-value) additional-stx-amount))
        
        ;; Update user's total locked amount in collection
        (match (map-get? user-vault-collection-registry { account-holder-principal: depositor-principal })
          existing-user-data (map-set user-vault-collection-registry
            { account-holder-principal: depositor-principal }
            (merge existing-user-data { total-user-locked-stx: (+ (get total-user-locked-stx existing-user-data) additional-stx-amount) }))
          false
        )
        
        ;; Log deposit activity transaction
        (try! (log-vault-transaction-activity target-vault-identifier u1 "FUNDS_DEPOSITED" additional-stx-amount depositor-principal))
        
        (ok updated-vault-balance)
      )
    )
  )
)

;; WITHDRAWAL FUNCTIONS
;; Withdraw full vault balance (only when unlocked)
(define-public (withdraw-complete-vault-balance (target-vault-identifier uint))
  (let 
    (
      (withdrawal-requester-principal tx-sender)
    )
    
    ;; Initial validation checks for withdrawal
    (asserts! (not (var-get is-contract-paused)) ERROR-UNAUTHORIZED-ACCESS)
    (asserts! (is-valid-vault-identifier target-vault-identifier) ERROR-INVALID-VAULT-ID)
    
    (let
      (
        (existing-vault-data (unwrap! (get-validated-vault-data-record target-vault-identifier) ERROR-VAULT-NOT-FOUND))
      )
      
      ;; Additional validation with vault data
      (asserts! (is-eq (get vault-owner-principal existing-vault-data) withdrawal-requester-principal) ERROR-UNAUTHORIZED-ACCESS)
      (asserts! (>= stacks-block-height (get unlock-block-height existing-vault-data)) ERROR-VAULT-STILL-LOCKED)
      (asserts! (> (get locked-stx-amount existing-vault-data) u0) ERROR-ZERO-BALANCE-VAULT)
      
      (let
        (
          (total-withdrawal-stx-amount (get locked-stx-amount existing-vault-data))
        )
        
        ;; Transfer complete STX balance back to vault owner
        (try! (as-contract (stx-transfer? total-withdrawal-stx-amount tx-sender withdrawal-requester-principal)))
        
        ;; Update vault to zero balance
        (map-set time-locked-vault-registry
          { vault-unique-identifier: target-vault-identifier }
          (merge existing-vault-data { 
            locked-stx-amount: u0,
            last-interaction-block-height: stacks-block-height
          })
        )
        
        ;; Update global contract state
        (var-set total-locked-stx-value (- (var-get total-locked-stx-value) total-withdrawal-stx-amount))
        
        ;; Update user's vault collection totals
        (match (map-get? user-vault-collection-registry { account-holder-principal: withdrawal-requester-principal })
          existing-user-data (map-set user-vault-collection-registry
            { account-holder-principal: withdrawal-requester-principal }
            (merge existing-user-data { total-user-locked-stx: (- (get total-user-locked-stx existing-user-data) total-withdrawal-stx-amount) }))
          false
        )
        
        ;; Log complete withdrawal activity
        (try! (log-vault-transaction-activity target-vault-identifier u2 "FULL_WITHDRAWAL" total-withdrawal-stx-amount withdrawal-requester-principal))
        
        (ok total-withdrawal-stx-amount)
      )
    )
  )
)

;; Partial withdrawal from unlocked vault
(define-public (withdraw-partial-stx-amount (target-vault-identifier uint) (requested-withdrawal-amount uint))
  (let
    (
      (withdrawal-requester-principal tx-sender)
    )
    
    ;; Initial validation checks for partial withdrawal
    (asserts! (not (var-get is-contract-paused)) ERROR-UNAUTHORIZED-ACCESS)
    (asserts! (is-valid-vault-identifier target-vault-identifier) ERROR-INVALID-VAULT-ID)
    (asserts! (> requested-withdrawal-amount u0) ERROR-INVALID-DEPOSIT-AMOUNT)
    
    (let
      (
        (existing-vault-data (unwrap! (get-validated-vault-data-record target-vault-identifier) ERROR-VAULT-NOT-FOUND))
      )
      
      ;; Additional validation with vault data
      (asserts! (is-eq (get vault-owner-principal existing-vault-data) withdrawal-requester-principal) ERROR-UNAUTHORIZED-ACCESS)
      (asserts! (>= stacks-block-height (get unlock-block-height existing-vault-data)) ERROR-VAULT-STILL-LOCKED)
      (asserts! (>= (get locked-stx-amount existing-vault-data) requested-withdrawal-amount) ERROR-WITHDRAWAL-AMOUNT-EXCEEDS-BALANCE)
      
      (let
        (
          (calculated-remaining-balance (- (get locked-stx-amount existing-vault-data) requested-withdrawal-amount))
        )
        
        ;; Transfer requested STX amount to vault owner
        (try! (as-contract (stx-transfer? requested-withdrawal-amount tx-sender withdrawal-requester-principal)))
        
        ;; Update vault balance after partial withdrawal
        (map-set time-locked-vault-registry
          { vault-unique-identifier: target-vault-identifier }
          (merge existing-vault-data { 
            locked-stx-amount: calculated-remaining-balance,
            last-interaction-block-height: stacks-block-height
          })
        )
        
        ;; Update global contract state
        (var-set total-locked-stx-value (- (var-get total-locked-stx-value) requested-withdrawal-amount))
        
        ;; Update user's vault collection totals
        (match (map-get? user-vault-collection-registry { account-holder-principal: withdrawal-requester-principal })
          existing-user-data (map-set user-vault-collection-registry
            { account-holder-principal: withdrawal-requester-principal }
            (merge existing-user-data { total-user-locked-stx: (- (get total-user-locked-stx existing-user-data) requested-withdrawal-amount) }))
          false
        )
        
        ;; Log partial withdrawal activity
        (try! (log-vault-transaction-activity target-vault-identifier u3 "PARTIAL_WITHDRAWAL" requested-withdrawal-amount withdrawal-requester-principal))
        
        (ok calculated-remaining-balance)
      )
    )
  )
)

;; QUERY & ANALYTICS READ-ONLY FUNCTIONS
;; Get comprehensive vault information with validation
(define-read-only (get-vault-complete-details (vault-identifier-to-query uint))
  (if (is-valid-vault-identifier vault-identifier-to-query)
    (map-get? time-locked-vault-registry { vault-unique-identifier: vault-identifier-to-query })
    none
  )
)

;; Get user's complete vault portfolio summary
(define-read-only (get-user-complete-vault-portfolio (account-holder-principal-to-query principal))
  (map-get? user-vault-collection-registry { account-holder-principal: account-holder-principal-to-query })
)

;; Check if specific vault is ready for withdrawal
(define-read-only (is-vault-unlocked-for-withdrawal (vault-identifier-to-check uint))
  (match (get-validated-vault-data-record vault-identifier-to-check)
    existing-vault-data (>= stacks-block-height (get unlock-block-height existing-vault-data))
    false
  )
)

;; Calculate remaining lock time for vault in blocks
(define-read-only (get-remaining-lock-blocks-count (vault-identifier-to-check uint))
  (match (get-validated-vault-data-record vault-identifier-to-check)
    existing-vault-data (if (>= stacks-block-height (get unlock-block-height existing-vault-data))
                        u0
                        (- (get unlock-block-height existing-vault-data) stacks-block-height))
    u0
  )
)

;; Get contract-wide analytics and statistics
(define-read-only (get-contract-global-analytics)
  {
    total-vaults-created-count: (var-get global-vault-counter),
    total-stx-value-locked: (var-get total-locked-stx-value),
    current-block-height: stacks-block-height,
    contract-stx-balance: (stx-get-balance (as-contract tx-sender)),
    contract-operational-status: (if (var-get is-contract-paused) "PAUSED" "ACTIVE")
  }
)

;; Calculate user's total locked STX value across all owned vaults
(define-read-only (calculate-user-total-locked-stx-value (account-holder-principal-to-query principal))
  (match (map-get? user-vault-collection-registry { account-holder-principal: account-holder-principal-to-query })
    existing-user-data (get total-user-locked-stx existing-user-data)
    u0
  )
)

;; UTILITY & HELPER FUNCTIONS
;; Log vault activity for transparency and audit trail
(define-private (log-vault-transaction-activity 
  (vault-identifier-for-logging uint) 
  (activity-sequence-index uint) 
  (transaction-action-type (string-ascii 20)) 
  (stx-amount-involved uint) 
  (transaction-initiator-principal principal))
  (begin
    (asserts! (is-valid-vault-identifier vault-identifier-for-logging) ERROR-INVALID-VAULT-ID)
    (map-set vault-activity-audit-log
      { vault-unique-identifier: vault-identifier-for-logging, activity-sequence-index: activity-sequence-index }
      {
        transaction-action-type: transaction-action-type,
        stx-amount-involved: stx-amount-involved,
        block-timestamp: stacks-block-height,
        transaction-initiator-principal: transaction-initiator-principal
      }
    )
    (ok true)
  )
)

;; Emergency pause/unpause function (deployer only)
(define-public (toggle-contract-operational-pause)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-DEPLOYER) ERROR-UNAUTHORIZED-ACCESS)
    (var-set is-contract-paused (not (var-get is-contract-paused)))
    (ok (var-get is-contract-paused))
  )
)

;; Get vault activity history with validation
(define-read-only (get-vault-transaction-activity-history (vault-identifier-to-query uint) (activity-sequence-index uint))
  (if (is-valid-vault-identifier vault-identifier-to-query)
    (map-get? vault-activity-audit-log { vault-unique-identifier: vault-identifier-to-query, activity-sequence-index: activity-sequence-index })
    none
  )
)