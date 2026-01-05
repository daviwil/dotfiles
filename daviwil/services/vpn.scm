(define-module (daviwil services vpn)
  #:use-module (gnu)
  #:use-module (gnu services vpn)
  #:export (wireguard-0x11-client-service
            %zerocool-wg-peer
            %phantom-wg-peer
            %phone-wg-peer))

;; Predefined WireGuard peers for easy reference
(define %zerocool-wg-peer
  (wireguard-peer
   (name "zerocool")
   (public-key "eXEzSoFWgZrpHnuiaLotLeN38Sj0IlbF0QVKeUa+wnA=")
   (allowed-ips '("10.0.0.2/32"))))

(define %phantom-wg-peer
  (wireguard-peer
   (name "phantom")
   (public-key "5cRaB97LRNl/uhOI1KAcCvhr9kBa+1OpN1biaWuGV34=")
   (allowed-ips '("10.0.0.3/32"))))

(define %phone-wg-peer
  (wireguard-peer
   (name "phone")
   (public-key "I09CA7jT6tF3qruPdJwUkDByQQZVw7ABptZOg16lQF8=")
   (allowed-ips '("10.0.0.4/32"))))

(define* (wireguard-0x11-client-service client-ip #:optional (additional-peers '()))
  "Create a WireGuard client service for connecting to 0x11.run VPN server.
   CLIENT-IP should be in CIDR format (e.g., '10.0.0.2/24').
   ADDITIONAL-PEERS is an optional list of peer configurations for peer-to-peer connectivity."
  (service wireguard-service-type
           (wireguard-configuration
             (interface "wg0")
             (addresses (list client-ip))
             (private-key "/var/lib/wireguard/private-key")
             (peers (cons* (wireguard-peer
                             (name "0x11-server")
                             (public-key "8s4RuJjps2rf7LlcMTQvsKy5cs2KyayvuUNziRLR8Ds=")
                             (endpoint "0x11.run:51820")
                             (allowed-ips '("10.0.0.0/24")) ;; Hub and spoke
                             (keep-alive 25))
                           additional-peers)))))
