;; DecentralizedSocialHub Smart Contract
;; A comprehensive decentralized social networking platform built on Stacks blockchain
;; Enables users to create profiles, publish content, build social connections, and engage
;; with community-driven interactions in a trustless, censorship-resistant environment

;; ERROR CONSTANTS & VALIDATION

(define-constant contract-deployer tx-sender)
(define-constant ERR-UNAUTHORIZED-ACCESS (err u100))
(define-constant ERR-USER-PROFILE-NOT-FOUND (err u101))
(define-constant ERR-USER-PROFILE-ALREADY-EXISTS (err u102))
(define-constant ERR-CONTENT-POST-NOT-FOUND (err u103))
(define-constant ERR-ALREADY-FOLLOWING-USER (err u104))
(define-constant ERR-NOT-FOLLOWING-USER (err u105))
(define-constant ERR-CANNOT-FOLLOW-YOURSELF (err u106))
(define-constant ERR-INVALID-INPUT-DATA (err u107))
(define-constant ERR-CONTENT-EXCEEDS-MAX-LENGTH (err u108))
(define-constant ERR-USERNAME-EXCEEDS-MAX-LENGTH (err u109))
(define-constant ERR-POST-ALREADY-LIKED (err u110))
(define-constant ERR-POST-NOT-PREVIOUSLY-LIKED (err u111))
(define-constant ERR-POST-ALREADY-REPOSTED (err u112))
(define-constant ERR-INVALID-URL-FORMAT (err u113))
(define-constant ERR-INVALID-NUMERIC-INPUT (err u114))

;; =================================
;; PLATFORM CONFIGURATION VARIABLES
;; =================================

(define-data-var current-user-identifier uint u1)
(define-data-var current-post-identifier uint u1)
(define-data-var network-participation-fee uint u1000000) ;; 1 STX in microSTX
(define-data-var maximum-post-character-limit uint u280)
(define-data-var maximum-username-character-limit uint u30)
(define-data-var default-user-reputation-score uint u100)
(define-data-var platform-verification-threshold uint u500)

;; CORE DATA STRUCTURES

;; User profile registry with comprehensive social metrics
(define-map user-profile-registry 
    { user-identifier: uint }
    {
        display-username: (string-ascii 30),
        profile-biography: (string-utf8 200),
        profile-avatar-image-url: (string-ascii 200),
        total-followers-count: uint,
        total-following-count: uint,
        total-published-posts-count: uint,
        account-creation-block-height: uint,
        verification-status: bool,
        community-reputation-score: uint,
        last-activity-block-height: uint
    }
)

;; Reverse lookup mapping for username uniqueness
(define-map username-identifier-mapping 
    { display-username: (string-ascii 30) }
    { user-identifier: uint }
)

;; Principal address to user ID association
(define-map wallet-address-user-mapping 
    { wallet-principal: principal }
    { user-identifier: uint }
)

;; Content posts with rich metadata and engagement tracking
(define-map published-content-registry 
    { post-identifier: uint }
    {
        content-author-identifier: uint,
        post-text-content: (string-utf8 280),
        publication-block-height: uint,
        total-likes-received: uint,
        total-replies-received: uint,
        total-reposts-received: uint,
        parent-thread-post-identifier: (optional uint),
        is-reply-to-another-post: bool,
        attached-media-urls: (list 4 (string-ascii 200)),
        content-visibility-status: bool
    }
)

;; Social connection relationships with timestamps
(define-map social-following-relationships 
    { follower-user-identifier: uint, followed-user-identifier: uint }
    { relationship-established-block-height: uint, relationship-active-status: bool }
)

;; Post engagement tracking for likes
(define-map content-appreciation-registry 
    { post-identifier: uint, appreciating-user-identifier: uint }
    { appreciation-block-height: uint }
)

;; Post sharing and amplification tracking
(define-map content-amplification-registry 
    { post-identifier: uint, amplifying-user-identifier: uint }
    { amplification-block-height: uint }
)

;; User mentions and notifications system
(define-map user-mention-notifications 
    { post-identifier: uint, mentioned-user-identifier: uint }
    { mention-block-height: uint, notification-read-status: bool }
)

;; INTERNAL UTILITY FUNCTIONS

(define-private (validate-username-availability (proposed-username (string-ascii 30)))
    (and 
        (> (len proposed-username) u0)
        (<= (len proposed-username) (var-get maximum-username-character-limit))
        (is-none (map-get? username-identifier-mapping { display-username: proposed-username }))
    )
)

(define-private (validate-content-requirements (post-content (string-utf8 280)))
    (and 
        (> (len post-content) u0)
        (<= (len post-content) (var-get maximum-post-character-limit))
    )
)

(define-private (validate-biography-input (biography (string-utf8 200)))
    (and 
        (<= (len biography) u200)
        (> (len biography) u0)
    )
)

(define-private (validate-url-format (url (string-ascii 200)))
    (and 
        (<= (len url) u200)
        (> (len url) u0)
    )
)

(define-private (validate-media-urls (media-list (list 4 (string-ascii 200))))
    (fold validate-single-media-url media-list true)
)

(define-private (validate-single-media-url (url (string-ascii 200)) (acc bool))
    (and acc (validate-url-format url))
)

(define-private (validate-post-identifier (post-id uint))
    (and 
        (> post-id u0)
        (< post-id (var-get current-post-identifier))
    )
)

(define-private (validate-user-identifier (user-id uint))
    (and 
        (> user-id u0)
        (< user-id (var-get current-user-identifier))
    )
)

(define-private (validate-fee-amount (fee uint))
    (and 
        (>= fee u0)
        (<= fee u100000000) ;; Maximum reasonable fee (100 STX)
    )
)

(define-private (validate-character-limit (limit uint))
    (and 
        (>= limit u1)
        (<= limit u1000) ;; Reasonable maximum character limit
    )
)

(define-private (extract-user-identifier-from-principal (target-principal principal))
    (match (map-get? wallet-address-user-mapping { wallet-principal: target-principal })
        existing-mapping (some (get user-identifier existing-mapping))
        none
    )
)

(define-private (update-user-engagement-metrics (metric-category (string-ascii 20)) (target-user-identifier uint) (increment-value bool))
    (match (map-get? user-profile-registry { user-identifier: target-user-identifier })
        existing-profile
        (let (
            (current-followers (get total-followers-count existing-profile))
            (current-following (get total-following-count existing-profile))
            (current-posts (get total-published-posts-count existing-profile))
        )
            (if (is-eq metric-category "followers")
                (map-set user-profile-registry { user-identifier: target-user-identifier }
                    (merge existing-profile { 
                        total-followers-count: (if increment-value 
                                                  (+ current-followers u1) 
                                                  (if (> current-followers u0) (- current-followers u1) u0))
                    }))
                (if (is-eq metric-category "following")
                    (map-set user-profile-registry { user-identifier: target-user-identifier }
                        (merge existing-profile { 
                            total-following-count: (if increment-value 
                                                      (+ current-following u1) 
                                                      (if (> current-following u0) (- current-following u1) u0))
                        }))
                    (if (is-eq metric-category "posts")
                        (map-set user-profile-registry { user-identifier: target-user-identifier }
                            (merge existing-profile { 
                                total-published-posts-count: (+ current-posts u1),
                                last-activity-block-height: stacks-block-height
                            }))
                        false))))
        false
    )
)

(define-private (verify-user-existence (user-identifier uint))
    (is-some (map-get? user-profile-registry { user-identifier: user-identifier }))
)

(define-private (verify-post-existence (post-identifier uint))
    (is-some (map-get? published-content-registry { post-identifier: post-identifier }))
)

;; USER PROFILE MANAGEMENT

(define-public (establish-user-profile 
    (desired-username (string-ascii 30)) 
    (user-biography (string-utf8 200)) 
    (avatar-image-url (string-ascii 200)))
    (let (
        (new-user-identifier (var-get current-user-identifier))
        (requesting-principal tx-sender)
    )
        (asserts! (validate-username-availability desired-username) ERR-INVALID-INPUT-DATA)
        (asserts! (validate-biography-input user-biography) ERR-INVALID-INPUT-DATA)
        (asserts! (validate-url-format avatar-image-url) ERR-INVALID-URL-FORMAT)
        (asserts! (is-none (extract-user-identifier-from-principal requesting-principal)) ERR-USER-PROFILE-ALREADY-EXISTS)
        
        (map-set user-profile-registry 
            { user-identifier: new-user-identifier }
            {
                display-username: desired-username,
                profile-biography: user-biography,
                profile-avatar-image-url: avatar-image-url,
                total-followers-count: u0,
                total-following-count: u0,
                total-published-posts-count: u0,
                account-creation-block-height: stacks-block-height,
                verification-status: false,
                community-reputation-score: (var-get default-user-reputation-score),
                last-activity-block-height: stacks-block-height
            }
        )
        
        (map-set username-identifier-mapping 
            { display-username: desired-username } 
            { user-identifier: new-user-identifier })
        (map-set wallet-address-user-mapping 
            { wallet-principal: requesting-principal } 
            { user-identifier: new-user-identifier })
        
        (var-set current-user-identifier (+ new-user-identifier u1))
        (ok new-user-identifier)
    )
)

(define-public (modify-user-profile-information 
    (updated-biography (string-utf8 200)) 
    (updated-avatar-url (string-ascii 200)))
    (let (
        (requesting-principal tx-sender)
        (user-identifier-result (extract-user-identifier-from-principal requesting-principal))
    )
        (asserts! (validate-biography-input updated-biography) ERR-INVALID-INPUT-DATA)
        (asserts! (validate-url-format updated-avatar-url) ERR-INVALID-URL-FORMAT)
        
        (match user-identifier-result
            existing-user-identifier
            (match (map-get? user-profile-registry { user-identifier: existing-user-identifier })
                current-profile
                (begin
                    (map-set user-profile-registry { user-identifier: existing-user-identifier }
                        (merge current-profile { 
                            profile-biography: updated-biography, 
                            profile-avatar-image-url: updated-avatar-url,
                            last-activity-block-height: stacks-block-height
                        }))
                    (ok true))
                ERR-USER-PROFILE-NOT-FOUND)
            ERR-USER-PROFILE-NOT-FOUND)
    )
)
;; CONTENT CREATION & MANAGEMENT

(define-public (publish-content-post 
    (post-content (string-utf8 280)) 
    (media-attachments (list 4 (string-ascii 200))) 
    (parent-post-reference (optional uint)))
    (let (
        (new-post-identifier (var-get current-post-identifier))
        (requesting-principal tx-sender)
        (author-identifier-result (extract-user-identifier-from-principal requesting-principal))
        (is-threaded-reply (is-some parent-post-reference))
    )
        (asserts! (validate-content-requirements post-content) ERR-INVALID-INPUT-DATA)
        (asserts! (validate-media-urls media-attachments) ERR-INVALID-URL-FORMAT)
        
        (match author-identifier-result
            verified-author-identifier
            (begin
                (if is-threaded-reply
                    (match parent-post-reference
                        parent-identifier
                        (begin
                            (asserts! (validate-post-identifier parent-identifier) ERR-INVALID-NUMERIC-INPUT)
                            (asserts! (verify-post-existence parent-identifier) ERR-CONTENT-POST-NOT-FOUND))
                        true)
                    true)
                
                (map-set published-content-registry 
                    { post-identifier: new-post-identifier }
                    {
                        content-author-identifier: verified-author-identifier,
                        post-text-content: post-content,
                        publication-block-height: stacks-block-height,
                        total-likes-received: u0,
                        total-replies-received: u0,
                        total-reposts-received: u0,
                        parent-thread-post-identifier: parent-post-reference,
                        is-reply-to-another-post: is-threaded-reply,
                        attached-media-urls: media-attachments,
                        content-visibility-status: true
                    }
                )
                
                (update-user-engagement-metrics "posts" verified-author-identifier true)
                
                (if is-threaded-reply
                    (match parent-post-reference
                        parent-identifier
                        (match (map-get? published-content-registry { post-identifier: parent-identifier })
                            parent-post-data
                            (map-set published-content-registry { post-identifier: parent-identifier }
                                (merge parent-post-data { total-replies-received: (+ (get total-replies-received parent-post-data) u1) }))
                            false)
                        false)
                    false)
                
                (var-set current-post-identifier (+ new-post-identifier u1))
                (ok new-post-identifier))
            ERR-USER-PROFILE-NOT-FOUND)
    )
)

;; SOCIAL CONNECTION MANAGEMENT

(define-public (establish-user-following (target-username (string-ascii 30)))
    (let (
        (requesting-principal tx-sender)
        (follower-identifier-result (extract-user-identifier-from-principal requesting-principal))
        (target-identifier-result (map-get? username-identifier-mapping { display-username: target-username }))
    )
        (match follower-identifier-result
            verified-follower-identifier
            (match target-identifier-result
                target-mapping
                (let ((target-user-identifier (get user-identifier target-mapping)))
                    (asserts! (not (is-eq verified-follower-identifier target-user-identifier)) ERR-CANNOT-FOLLOW-YOURSELF)
                    (asserts! (is-none (map-get? social-following-relationships 
                        { follower-user-identifier: verified-follower-identifier, followed-user-identifier: target-user-identifier })) 
                        ERR-ALREADY-FOLLOWING-USER)
                    
                    (map-set social-following-relationships 
                        { follower-user-identifier: verified-follower-identifier, followed-user-identifier: target-user-identifier }
                        { relationship-established-block-height: stacks-block-height, relationship-active-status: true })
                    
                    (update-user-engagement-metrics "following" verified-follower-identifier true)
                    (update-user-engagement-metrics "followers" target-user-identifier true)
                    (ok true))
                ERR-USER-PROFILE-NOT-FOUND)
            ERR-USER-PROFILE-NOT-FOUND)
    )
)

(define-public (terminate-user-following (target-username (string-ascii 30)))
    (let (
        (requesting-principal tx-sender)
        (follower-identifier-result (extract-user-identifier-from-principal requesting-principal))
        (target-identifier-result (map-get? username-identifier-mapping { display-username: target-username }))
    )
        (match follower-identifier-result
            verified-follower-identifier
            (match target-identifier-result
                target-mapping
                (let ((target-user-identifier (get user-identifier target-mapping)))
                    (asserts! (is-some (map-get? social-following-relationships 
                        { follower-user-identifier: verified-follower-identifier, followed-user-identifier: target-user-identifier })) 
                        ERR-NOT-FOLLOWING-USER)
                    
                    (map-delete social-following-relationships 
                        { follower-user-identifier: verified-follower-identifier, followed-user-identifier: target-user-identifier })
                    
                    (update-user-engagement-metrics "following" verified-follower-identifier false)
                    (update-user-engagement-metrics "followers" target-user-identifier false)
                    (ok true))
                ERR-USER-PROFILE-NOT-FOUND)
            ERR-USER-PROFILE-NOT-FOUND)
    )
)

;; CONTENT ENGAGEMENT FUNCTIONS

(define-public (express-content-appreciation (target-post-identifier uint))
    (let (
        (requesting-principal tx-sender)
        (user-identifier-result (extract-user-identifier-from-principal requesting-principal))
        (post-data-result (map-get? published-content-registry { post-identifier: target-post-identifier }))
    )
        (asserts! (validate-post-identifier target-post-identifier) ERR-INVALID-NUMERIC-INPUT)
        
        (match user-identifier-result
            verified-user-identifier
            (match post-data-result
                existing-post-data
                (begin
                    (asserts! (is-none (map-get? content-appreciation-registry 
                        { post-identifier: target-post-identifier, appreciating-user-identifier: verified-user-identifier })) 
                        ERR-POST-ALREADY-LIKED)
                    
                    (map-set content-appreciation-registry 
                        { post-identifier: target-post-identifier, appreciating-user-identifier: verified-user-identifier }
                        { appreciation-block-height: stacks-block-height })
                    
                    (map-set published-content-registry { post-identifier: target-post-identifier }
                        (merge existing-post-data { total-likes-received: (+ (get total-likes-received existing-post-data) u1) }))
                    (ok true))
                ERR-CONTENT-POST-NOT-FOUND)
            ERR-USER-PROFILE-NOT-FOUND)
    )
)

(define-public (withdraw-content-appreciation (target-post-identifier uint))
    (let (
        (requesting-principal tx-sender)
        (user-identifier-result (extract-user-identifier-from-principal requesting-principal))
        (post-data-result (map-get? published-content-registry { post-identifier: target-post-identifier }))
    )
        (asserts! (validate-post-identifier target-post-identifier) ERR-INVALID-NUMERIC-INPUT)
        
        (match user-identifier-result
            verified-user-identifier
            (match post-data-result
                existing-post-data
                (begin
                    (asserts! (is-some (map-get? content-appreciation-registry 
                        { post-identifier: target-post-identifier, appreciating-user-identifier: verified-user-identifier })) 
                        ERR-POST-NOT-PREVIOUSLY-LIKED)
                    
                    (map-delete content-appreciation-registry 
                        { post-identifier: target-post-identifier, appreciating-user-identifier: verified-user-identifier })
                    
                    (map-set published-content-registry { post-identifier: target-post-identifier }
                        (merge existing-post-data { total-likes-received: (if (> (get total-likes-received existing-post-data) u0) 
                                                                             (- (get total-likes-received existing-post-data) u1) u0) }))
                    (ok true))
                ERR-CONTENT-POST-NOT-FOUND)
            ERR-USER-PROFILE-NOT-FOUND)
    )
)

(define-public (amplify-content-through-repost (target-post-identifier uint))
    (let (
        (requesting-principal tx-sender)
        (user-identifier-result (extract-user-identifier-from-principal requesting-principal))
        (post-data-result (map-get? published-content-registry { post-identifier: target-post-identifier }))
    )
        (asserts! (validate-post-identifier target-post-identifier) ERR-INVALID-NUMERIC-INPUT)
        
        (match user-identifier-result
            verified-user-identifier
            (match post-data-result
                existing-post-data
                (begin
                    (asserts! (is-none (map-get? content-amplification-registry 
                        { post-identifier: target-post-identifier, amplifying-user-identifier: verified-user-identifier })) 
                        ERR-POST-ALREADY-REPOSTED)
                    
                    (map-set content-amplification-registry 
                        { post-identifier: target-post-identifier, amplifying-user-identifier: verified-user-identifier }
                        { amplification-block-height: stacks-block-height })
                    
                    (map-set published-content-registry { post-identifier: target-post-identifier }
                        (merge existing-post-data { total-reposts-received: (+ (get total-reposts-received existing-post-data) u1) }))
                    (ok true))
                ERR-CONTENT-POST-NOT-FOUND)
            ERR-USER-PROFILE-NOT-FOUND)
    )
)

;; DATA RETRIEVAL FUNCTIONS

(define-read-only (retrieve-user-profile-by-username (target-username (string-ascii 30)))
    (match (map-get? username-identifier-mapping { display-username: target-username })
        username-mapping
        (map-get? user-profile-registry { user-identifier: (get user-identifier username-mapping) })
        none
    )
)

(define-read-only (retrieve-user-profile-by-identifier (target-user-identifier uint))
    (map-get? user-profile-registry { user-identifier: target-user-identifier })
)

(define-read-only (retrieve-user-profile-by-principal (target-principal principal))
    (match (extract-user-identifier-from-principal target-principal)
        found-user-identifier (map-get? user-profile-registry { user-identifier: found-user-identifier })
        none
    )
)

(define-read-only (retrieve-published-content-by-identifier (target-post-identifier uint))
    (map-get? published-content-registry { post-identifier: target-post-identifier })
)

(define-read-only (verify-following-relationship (follower-username (string-ascii 30)) (followed-username (string-ascii 30)))
    (match (map-get? username-identifier-mapping { display-username: follower-username })
        follower-mapping
        (match (map-get? username-identifier-mapping { display-username: followed-username })
            followed-mapping
            (is-some (map-get? social-following-relationships 
                { follower-user-identifier: (get user-identifier follower-mapping), 
                  followed-user-identifier: (get user-identifier followed-mapping) }))
            false)
        false
    )
)

(define-read-only (verify-content-appreciation-status (target-post-identifier uint) (username (string-ascii 30)))
    (match (map-get? username-identifier-mapping { display-username: username })
        user-mapping
        (is-some (map-get? content-appreciation-registry 
            { post-identifier: target-post-identifier, appreciating-user-identifier: (get user-identifier user-mapping) }))
        false
    )
)

(define-read-only (retrieve-platform-statistics)
    {
        total-registered-users: (- (var-get current-user-identifier) u1),
        total-published-content: (- (var-get current-post-identifier) u1),
        network-participation-fee: (var-get network-participation-fee),
        maximum-content-length: (var-get maximum-post-character-limit),
        platform-verification-threshold: (var-get platform-verification-threshold)
    }
)

;; ADMINISTRATIVE FUNCTIONS

(define-public (configure-network-participation-fee (updated-fee-amount uint))
    (begin
        (asserts! (is-eq tx-sender contract-deployer) ERR-UNAUTHORIZED-ACCESS)
        (asserts! (validate-fee-amount updated-fee-amount) ERR-INVALID-NUMERIC-INPUT)
        (var-set network-participation-fee updated-fee-amount)
        (ok true)
    )
)

(define-public (grant-user-verification-status (target-user-identifier uint))
    (begin
        (asserts! (is-eq tx-sender contract-deployer) ERR-UNAUTHORIZED-ACCESS)
        (asserts! (validate-user-identifier target-user-identifier) ERR-INVALID-NUMERIC-INPUT)
        
        (match (map-get? user-profile-registry { user-identifier: target-user-identifier })
            existing-profile
            (begin
                (map-set user-profile-registry { user-identifier: target-user-identifier }
                    (merge existing-profile { verification-status: true }))
                (ok true))
            ERR-USER-PROFILE-NOT-FOUND)
    )
)

(define-public (update-maximum-content-character-limit (new-character-limit uint))
    (begin
        (asserts! (is-eq tx-sender contract-deployer) ERR-UNAUTHORIZED-ACCESS)
        (asserts! (validate-character-limit new-character-limit) ERR-INVALID-NUMERIC-INPUT)
        (var-set maximum-post-character-limit new-character-limit)
        (ok true)
    )
)