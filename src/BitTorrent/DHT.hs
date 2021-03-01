module BitTorrent.DHT (globalBootstrapAddrs) where

globalBootstrapAddrs =
  [ ("router.utorrent.com", "6881"),
    ("router.bittorrent.com", "6881"),
    ("dht.transmissionbt.com", "6881"),
    ("dht.aelitis.com", "6881"), -- Vuze
    ("router.silotis.us", "6881"), -- IPv6
    ("dht.libtorrent.org", "25401") -- @arvidn's
  ]
