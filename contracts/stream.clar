(define-constant ERR_UNAUTHORISED (err u0))
(define-constant ERR_INVALID_SIGNATURE (err u1))
(define-constant ERR_STREAM_STILL_ACTIVE (err u2))
(define-constant ERR_INVALID_STREAM_ID (err u3))
(define-constant ERR_STREAM_INACTIVE (err u4))
(define-constant ERR_NO_BALANCE_TO_REFUND (err u5))

;; Data vars
(define-data-var latest-stream-id uint u0)

;; Stream mapping
(define-map streams
    uint ;; Stream-id
    {
        sender: principal,
        recipient: principal,
        balance: uint,
        withdrawn-balance: uint,
        payment-per-block: uint,
        timeframe: {
            start-block: uint,
            stop-block: uint,
        },
    }
)

(define-public (stream-to
        (recipient principal)
        (initial-balance uint)
        (timeframe {
            start-block: uint,
            stop-block: uint,
        })
        (payment-per-block uint)
    )
    ;; #[allow(unchecked_data)]
    (let (
            (stream {
                sender: contract-caller,
                recipient: recipient,
                balance: initial-balance,
                withdrawn-balance: u0,
                payment-per-block: payment-per-block,
                timeframe: timeframe,
            })
            (current-stream-id (var-get latest-stream-id))
        )
        ;; stx-transfer takes in (amount, sender, recipient) arguments
        ;; for the `recipient` - we do `(as-contract tx-sender)`
        ;; `as-contract` switches the `tx-sender` variable to be the contract principal
        ;; inside it's scope
        ;; so doing `as-contract tx-sender` gives us the contract address itself
        ;; this is like doing address(this) in Solidity
        (asserts! (> (get stop-block timeframe) (get start-block timeframe))
            ERR_INVALID_STREAM_ID
        )
        (try! (stx-transfer? initial-balance contract-caller (as-contract tx-sender)))
        (map-set streams current-stream-id stream)
        (var-set latest-stream-id (+ current-stream-id u1))
        (ok current-stream-id)
    )
)

;; Increase the locked STX balance for a stream
(define-public (refuel
        (stream-id uint)
        (amount uint)
    )
    ;; #[allow(unchecked_data)]
    (let ((current-stream (unwrap! (map-get? streams stream-id) ERR_INVALID_STREAM_ID)))
        (asserts! (is-eq contract-caller (get sender current-stream))
            ERR_UNAUTHORISED
        )
        (try! (stx-transfer? amount contract-caller (as-contract tx-sender)))
        (map-set streams stream-id
            (merge current-stream { balance: (+ amount (get balance current-stream)) })
        )
        (ok amount)
    )
)

;; Calculate the number of blocks a stream has been active
(define-read-only (calculate-block-delta (timeframe {
    start-block: uint,
    stop-block: uint,
}))
    (let (
            (start-block (get start-block timeframe))
            (stop-block (get stop-block timeframe))
            (delta (if (<= stacks-block-height start-block)
                ;; then
                u0
                ;; else
                (if (< stacks-block-height stop-block)
                    ;; then
                    (- stacks-block-height start-block)
                    ;; else
                    (- stop-block start-block)
                )
            ))
        )
        delta
    )
)

;; Read only function to get-balance
(define-read-only (balance-of
        (stream-id uint)
        (who principal)
    )
    (let (
            (current-stream (unwrap! (map-get? streams stream-id) u0))
            (block-delta (calculate-block-delta (get timeframe current-stream)))
            (recipient-balance (* block-delta (get payment-per-block current-stream)))
        )
        (if (is-eq who (get recipient current-stream))
            (- recipient-balance (get withdrawn-balance current-stream))
            (if (is-eq who (get sender current-stream))
                (- (get balance current-stream) recipient-balance)
                u0
            )
        )
    )
)

;; Function to withdraw received tokens
(define-public (withdraw (stream-id uint))
    ;; #[allow(unchecked_data)]
    (let (
            (current-stream (unwrap! (map-get? streams stream-id) ERR_INVALID_STREAM_ID))
            (recipient (get recipient current-stream))
        )
        (asserts! (is-eq contract-caller recipient) ERR_UNAUTHORISED)

        (let (
                (balance (balance-of stream-id recipient))
                (updated-stream (merge current-stream { withdrawn-balance: (+ balance (get withdrawn-balance current-stream)) }))
            )
            (map-set streams stream-id updated-stream)
            (try! (as-contract (stx-transfer? balance tx-sender recipient)))
            (ok balance)
        )
    )
)

;;;;  Withdraw received tokens
;; (define-public (withdraw
;;     (stream-id uint)
;;   )
;;   (let (
;;     (stream (unwrap! (map-get? streams stream-id) ERR_INVALID_STREAM_ID))
;;     (balance (balance-of stream-id contract-caller))
;;   )
;;     (asserts! (is-eq contract-caller (get recipient stream)) ERR_UNAUTHORIZED)
;;     (map-set streams stream-id 
;;       (merge stream {withdrawn-balance: (+ (get withdrawn-balance stream) balance)})
;;     )
;;     (try! (as-contract (stx-transfer? balance tx-sender (get recipient stream))))
;;     (ok balance)
;;   )
;; )

(define-public (refund (stream-id uint))
    (let (
            (current-stream (unwrap! (map-get? streams stream-id) ERR_INVALID_STREAM_ID))
            (balance (balance-of stream-id (get sender current-stream)))
            (delta (calculate-block-delta (get timeframe current-stream)))
            (timeframe (get timeframe current-stream))
        )
        (asserts! (is-eq contract-caller (get sender current-stream))
            ERR_UNAUTHORISED
        )
        (asserts! (< (get stop-block timeframe) stacks-block-height)
            ERR_STREAM_STILL_ACTIVE
        )
        (asserts! (> balance u0) ERR_NO_BALANCE_TO_REFUND)
        (map-set streams stream-id
            (merge current-stream { balance: (- (get balance current-stream) balance) })
        )
        ;; https://claude.ai/share/7d993e1a-52c2-4307-af84-e44214be4c4e
        ;; https://docs.stacks.co/reference/functions#stx-transfer
        (try! (as-contract (stx-transfer? balance tx-sender (get sender current-stream))))

        (ok balance)
    )
)

;; Get the hash of the stream
(define-read-only (hash-stream
        (stream-id uint)
        (new-payment-per-block uint)
        (new-timeframe {
            start-block: uint,
            stop-block: uint,
        })
    )
    (let (
            (current-stream (unwrap! (map-get? streams stream-id) (sha256 0)))
            (msg (concat
                (concat (unwrap-panic (to-consensus-buff? current-stream))
                    (unwrap-panic (to-consensus-buff? new-payment-per-block))
                )
                (unwrap-panic (to-consensus-buff? new-timeframe))
            ))
        )
        (sha256 msg)
    )
)

;; Signature verification
(define-read-only (validate-signature
        (hash (buff 32))
        (signature (buff 65))
        (signer principal)
    )
    (is-eq (principal-of? (unwrap! (secp256k1-recover? hash signature) false))
        (ok signer)
    )
)

(define-public (update-details
        (stream-id uint)
        (payment-per-block uint)
        (timeframe {
            start-block: uint,
            stop-block: uint,
        })
        (signer principal)
        (signature (buff 65))
    )
    (let (
            (current-stream (unwrap! (map-get? streams stream-id) ERR_INVALID_STREAM_ID))
            (hash (hash-stream stream-id payment-per-block timeframe))
        )
        (asserts! (validate-signature hash signature signer)
            ERR_INVALID_SIGNATURE
        )
        ;; The Rule: Executor != Signer
        ;; One party EXECUTES - The OTHER party must have SIGNED
        ;; If Sender executes - Recipient must sign
        ;; If Recipient executes - Sender must sign
        (asserts!
            (or
                (and (is-eq (get sender current-stream) contract-caller) (is-eq (get recipient current-stream) signer))
                (and (is-eq (get sender current-stream) signer) (is-eq (get recipient current-stream) contract-caller))
            )
            ERR_UNAUTHORISED
        )
        (map-set streams stream-id
            (merge current-stream {
                payment-per-block: payment-per-block,
                timeframe: timeframe,
            })
        )
        (ok true)
    )
)