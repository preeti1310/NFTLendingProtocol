 ;; NFT Lending Protocol
;; A collateralized lending platform using NFTs as collateral for borrowing cryptocurrencies
;; Borrowers can deposit NFTs and borrow STX tokens against their collateral

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-loan-owner (err u101))
(define-constant err-insufficient-collateral (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-loan-not-found (err u104))
(define-constant err-nft-transfer-failed (err u105))
(define-constant err-loan-already-exists (err u106))

;; Loan to Value Ratio (70% - borrowers can borrow up to 70% of NFT value)
(define-constant ltv-ratio u70)
(define-constant max-ltv u100)

;; Data structures
(define-map loans 
  uint 
  {
    borrower: principal,
    nft-contract: principal,
    nft-id: uint,
    loan-amount: uint,
    nft-value: uint,
    timestamp: uint,
    is-active: bool
  })

(define-data-var loan-counter uint u0)
(define-data-var total-loans-issued uint u0)

;; Function 1: Create Loan - Deposit NFT as collateral and borrow STX
(define-public (create-loan (nft-contract principal) (nft-id uint) (nft-value uint) (loan-amount uint))
  (let 
    (
      (loan-id (+ (var-get loan-counter) u1))
      (max-loan-amount (/ (* nft-value ltv-ratio) max-ltv))
    )
    (begin
      ;; Validate inputs
      (asserts! (> loan-amount u0) err-invalid-amount)
      (asserts! (> nft-value u0) err-invalid-amount)
      (asserts! (<= loan-amount max-loan-amount) err-insufficient-collateral)
      
      ;; Check if loan already exists for this NFT
      (asserts! (is-none (map-get? loans loan-id)) err-loan-already-exists)
      
      ;; Transfer NFT from borrower to contract as collateral
      ;; Note: In a real implementation, you would use the actual NFT contract transfer function
      ;; For this example, we assume the transfer is successful
      
      ;; Transfer STX loan amount to borrower
      (try! (as-contract (stx-transfer? loan-amount tx-sender (as-contract tx-sender))))
      (try! (stx-transfer? loan-amount (as-contract tx-sender) tx-sender))
      
      ;; Create loan record
      (map-set loans loan-id
        {
          borrower: tx-sender,
          nft-contract: nft-contract,
          nft-id: nft-id,
          loan-amount: loan-amount,
          nft-value: nft-value,
          timestamp: stacks-block-height,
          is-active: true
        })
      
      ;; Update counters
      (var-set loan-counter loan-id)
      (var-set total-loans-issued (+ (var-get total-loans-issued) loan-amount))
      
      (ok loan-id))))

;; Function 2: Repay Loan - Repay borrowed amount and retrieve NFT collateral
(define-public (repay-loan (loan-id uint))
  (let 
    (
      (loan-data (unwrap! (map-get? loans loan-id) err-loan-not-found))
    )
    (begin
      ;; Validate loan exists and is active
      (asserts! (get is-active loan-data) err-loan-not-found)
      (asserts! (is-eq tx-sender (get borrower loan-data)) err-not-loan-owner)
      
      ;; Transfer repayment amount from borrower to contract
      (try! (stx-transfer? (get loan-amount loan-data) tx-sender (as-contract tx-sender)))
      
      ;; Return NFT to borrower
      ;; Note: In a real implementation, you would use the actual NFT contract transfer function
      ;; For this example, we assume the NFT transfer back is successful
      
      ;; Mark loan as repaid (inactive)
      (map-set loans loan-id
        (merge loan-data { is-active: false }))
      
      ;; Update total loans (reduce by repaid amount)
      (var-set total-loans-issued (- (var-get total-loans-issued) (get loan-amount loan-data)))
      
      (ok true))))

;; Read-only functions for data retrieval
(define-read-only (get-loan (loan-id uint))
  (map-get? loans loan-id))

(define-read-only (get-total-loans-issued)
  (ok (var-get total-loans-issued)))

(define-read-only (get-loan-counter)
  (ok (var-get loan-counter)))

(define-read-only (calculate-max-loan (nft-value uint))
  (ok (/ (* nft-value ltv-ratio) max-ltv)))