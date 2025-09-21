
;; title: DataProvider
;; version: 1.0.0
;; summary: Address reputation system for off-chain data provider accuracy and reliability scoring
;; description: This contract manages reputation scores for data providers based on their accuracy and reliability in providing off-chain data

;; traits
;;

;; token definitions
;;

;; constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u401))
(define-constant ERR_PROVIDER_NOT_FOUND (err u404))
(define-constant ERR_PROVIDER_ALREADY_EXISTS (err u409))
(define-constant ERR_INVALID_SCORE (err u400))
(define-constant ERR_INVALID_PERCENTAGE (err u402))
(define-constant MIN_SCORE u0)
(define-constant MAX_SCORE u100)
(define-constant INITIAL_SCORE u50)

;; data vars
(define-data-var contract-owner principal CONTRACT_OWNER)
(define-data-var total-providers uint u0)

;; data maps
;; Provider information storage
(define-map providers
    principal
    {
        name: (string-ascii 64),
        registered-at: uint,
        is-active: bool,
        total-submissions: uint,
        accurate-submissions: uint
    }
)

;; Provider reputation scores
(define-map provider-scores
    principal
    {
        accuracy-score: uint,
        reliability-score: uint,
        overall-score: uint,
        last-updated: uint
    }
)

;; Admin whitelist for score updates
(define-map authorized-admins principal bool)

;; public functions

;; Register a new data provider
(define-public (register-provider (provider principal) (name (string-ascii 64)))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
        (asserts! (is-none (map-get? providers provider)) ERR_PROVIDER_ALREADY_EXISTS)

        (map-set providers provider {
            name: name,
            registered-at: block-height,
            is-active: true,
            total-submissions: u0,
            accurate-submissions: u0
        })

        (map-set provider-scores provider {
            accuracy-score: INITIAL_SCORE,
            reliability-score: INITIAL_SCORE,
            overall-score: INITIAL_SCORE,
            last-updated: block-height
        })

        (var-set total-providers (+ (var-get total-providers) u1))
        (ok true)
    )
)

;; Update provider activity status
(define-public (set-provider-status (provider principal) (active bool))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
        (asserts! (is-some (map-get? providers provider)) ERR_PROVIDER_NOT_FOUND)

        (map-set providers provider
            (merge (unwrap-panic (map-get? providers provider)) {is-active: active})
        )
        (ok true)
    )
)

;; Record a data submission and its accuracy
(define-public (record-submission (provider principal) (is-accurate bool))
    (begin
        (asserts! (or (is-eq tx-sender (var-get contract-owner))
                     (default-to false (map-get? authorized-admins tx-sender))) ERR_UNAUTHORIZED)
        (asserts! (is-some (map-get? providers provider)) ERR_PROVIDER_NOT_FOUND)

        (let (
            (current-data (unwrap-panic (map-get? providers provider)))
            (new-total (+ (get total-submissions current-data) u1))
            (new-accurate (if is-accurate
                            (+ (get accurate-submissions current-data) u1)
                            (get accurate-submissions current-data)))
            (accuracy-percentage (if (> new-total u0)
                                   (/ (* new-accurate u100) new-total)
                                   u0))
        )
            (map-set providers provider
                (merge current-data {
                    total-submissions: new-total,
                    accurate-submissions: new-accurate
                })
            )
            (try! (update-accuracy-score provider accuracy-percentage))
            (ok true)
        )
    )
)

;; Update provider reliability score
(define-public (update-reliability-score (provider principal) (score uint))
    (begin
        (asserts! (or (is-eq tx-sender (var-get contract-owner))
                     (default-to false (map-get? authorized-admins tx-sender))) ERR_UNAUTHORIZED)
        (asserts! (is-some (map-get? providers provider)) ERR_PROVIDER_NOT_FOUND)
        (asserts! (and (>= score MIN_SCORE) (<= score MAX_SCORE)) ERR_INVALID_SCORE)

        (let (
            (current-scores (unwrap-panic (map-get? provider-scores provider)))
            (accuracy (get accuracy-score current-scores))
            (new-overall (/ (+ accuracy score) u2))
        )
            (map-set provider-scores provider
                (merge current-scores {
                    reliability-score: score,
                    overall-score: new-overall,
                    last-updated: block-height
                })
            )
            (ok true)
        )
    )
)

;; Add or remove authorized admin
(define-public (set-admin-authorization (admin principal) (authorized bool))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
        (map-set authorized-admins admin authorized)
        (ok true)
    )
)

;; Transfer contract ownership
(define-public (transfer-ownership (new-owner principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
        (var-set contract-owner new-owner)
        (ok true)
    )
)

;; read only functions

;; Get provider information
(define-read-only (get-provider-info (provider principal))
    (map-get? providers provider)
)

;; Get provider scores
(define-read-only (get-provider-scores (provider principal))
    (map-get? provider-scores provider)
)

;; Get provider overall score
(define-read-only (get-provider-overall-score (provider principal))
    (match (map-get? provider-scores provider)
        scores (some (get overall-score scores))
        none
    )
)

;; Get provider accuracy percentage
(define-read-only (get-provider-accuracy (provider principal))
    (match (map-get? providers provider)
        data (let (
            (total (get total-submissions data))
            (accurate (get accurate-submissions data))
        )
            (if (> total u0)
                (some (/ (* accurate u100) total))
                (some u0)
            )
        )
        none
    )
)

;; Check if provider is active
(define-read-only (is-provider-active (provider principal))
    (match (map-get? providers provider)
        data (get is-active data)
        false
    )
)

;; Check if admin is authorized
(define-read-only (is-admin-authorized (admin principal))
    (default-to false (map-get? authorized-admins admin))
)

;; Get contract owner
(define-read-only (get-contract-owner)
    (var-get contract-owner)
)

;; Get total number of providers
(define-read-only (get-total-providers)
    (var-get total-providers)
)

;; private functions

;; Update accuracy score based on submission data
(define-private (update-accuracy-score (provider principal) (accuracy-percentage uint))
    (begin
        (asserts! (and (>= accuracy-percentage MIN_SCORE) (<= accuracy-percentage MAX_SCORE)) ERR_INVALID_PERCENTAGE)

        (let (
            (current-scores (unwrap-panic (map-get? provider-scores provider)))
            (reliability (get reliability-score current-scores))
            (new-overall (/ (+ accuracy-percentage reliability) u2))
        )
            (map-set provider-scores provider
                (merge current-scores {
                    accuracy-score: accuracy-percentage,
                    overall-score: new-overall,
                    last-updated: block-height
                })
            )
            (ok true)
        )
    )
)
