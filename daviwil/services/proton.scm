(define-module (daviwil services proton)
  #:use-module (gnu)
  #:use-module (gnu services vpn)
  #:use-module (daviwil services aws-ranges)
  #:export (wireguard-proton-split-service))

;; Split-tunnel WireGuard connection to ProtonVPN: only the AWS us-west-2
;; ranges go through it, everything else keeps the normal route.  Works
;; around packet loss on the ISP's path to that region.
;;
;; The private key comes from the Proton WireGuard config and lives outside
;; this repo, readable only by root.
(define* (wireguard-proton-split-service
          #:key
          (interface "wg-proton")
          (port 51821)              ; wg0 already listens on 51820
          (address "10.2.0.2/32")
          (private-key "/var/lib/wireguard/proton-key")
          (server-public-key "lhESmyI5iT1MdL/H4toiuDR2oovh/bDxvGvH1O2VYmQ=")
          (endpoint "149.88.102.46:51820"))
  (service wireguard-service-type
           (wireguard-configuration
            (interface interface)
            (port port)
            (addresses (list address))
            (private-key private-key)
            (peers (list (wireguard-peer
                          (name "proton")
                          (public-key server-public-key)
                          (endpoint endpoint)
                          (allowed-ips %aws-us-west-2-ipv4)
                          (keep-alive 25)))))))
